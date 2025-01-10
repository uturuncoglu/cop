module cop_phases_catalyst

  !-----------------------------------------------------------------------------
  ! Phase for ParaView Catalyst interaction
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==), operator(-), operator(/)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF, only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE

  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC_Model, only: NUOPC_ModelGet

  use catalyst_api
  use catalyst_conduit
  !use conduit
  !use conduit_relay

  use cop_comp_shr, only: ChkErr
  use cop_comp_internalstate, only: InternalState

  use, intrinsic :: iso_c_binding, only: C_PTR

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: cop_phases_catalyst_run

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(cop_phases_catalyst)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine cop_phases_catalyst_run(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(C_PTR) :: node
    type(C_PTR) :: channel
    integer :: n, numScripts, step
    real(kind=8) :: time
    logical :: isPresent, isSet
    logical, save :: first_time = .true.
    type(InternalState) :: is_local
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState
    character(ESMF_MAXSTR) :: cvalue, tmpStr, scriptName
    character(len=ESMF_MAXSTR) :: timeStr
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_catalyst_run) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query component clock
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query current time
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalGet(currTime-startTime, s_r8=time, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize Catalyst
    if (first_time) then
       ! This node will hold the information nessesary to initialize ParaViewCatalyst
       node = catalyst_conduit_node_create()

       ! Query name of Catalyst script
       call NUOPC_CompAttributeGet(gcomp, name="CatalystScript", value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (isPresent .and. isSet) then
          scriptName = trim(cvalue)
          call ESMF_LogWrite(trim(subname)//": CatalystScript = "//trim(scriptName), ESMF_LOGMSG_INFO)
       endif

       ! Set script name
       ! TODO: Hard coded to be only one but this could be extended later
       numScripts = 1
       write(tmpStr, '(A,I1)') 'catalyst/scripts/script', numScripts
       do n = 1, numScripts
          call catalyst_conduit_node_set_path_char8_str(node, trim(tmpStr)//"/filename", trim(scriptName))
       end do

       ! Print out node for debugging
       call catalyst_conduit_node_print_detailed(node)

       ! Set flag
       first_time = .false.
    end if

    ! Add time/cycle information - Catalyst-specific variables
    step = int((currTime-startTime)/timeStep)
    call catalyst_conduit_node_set_path_int32(node, "catalyst/state/timestep", step)
    call catalyst_conduit_node_set_path_float64(node, "catalyst/state/time", time)

    ! Add channel
    channel = catalyst_conduit_node_fetch(node, "catalyst/channels/grid")



    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine cop_phases_catalyst_run

end module cop_phases_catalyst
