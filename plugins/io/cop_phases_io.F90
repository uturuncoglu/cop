module cop_phases_io

  !-----------------------------------------------------------------------------
  ! Write imported fields
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_State, ESMF_LogWrite
  use ESMF, only: ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_MAXSTR

  use NUOPC_Model, only: NUOPC_ModelGet

  use cop_comp_shr, only: ChkErr
  use cop_comp_shr, only: StateWrite
  use cop_comp_internalstate, only: InternalState

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: cop_phases_dump_all

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  integer :: dbug = 0
  character(len=*), parameter :: modName = "(cop_phases_io)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine cop_phases_dump_all(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    type(ESMF_Time) :: currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState
    character(len=ESMF_MAXSTR) :: timeStr
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_dump_all) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get internal state
    !nullify(is_local%wrap)
    !call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call StateWrite(importState, 'import_'//trim(timeStr), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine

end module cop_phases_io
