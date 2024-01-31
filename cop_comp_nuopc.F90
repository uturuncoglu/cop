module cop_comp_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for generic co-processing component (COP)
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Clock, ESMF_LogWrite
  use ESMF, only: ESMF_GridCompSetEntryPoint
  use ESMF, only: ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_MAXSTR

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompFilterPhaseMap, NUOPC_CompSetEntryPoint

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices
  use NUOPC_Model, only: label_Advertise
  use NUOPC_Model, only: label_ModifyAdvertised
  use NUOPC_Model, only: label_Advance

  !use catalyst_api
  !use catalyst_conduit
  !use conduit
  !use conduit_relay

  use cop_comp_shr, only: ChkErr

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: SetServices
  public :: SetVM

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  private :: InitializeP0

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(cop_comp_nuopc)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(SetServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! register the generic methods
    !------------------

    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! switching to IPD versions
    !------------------

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, userRoutine=InitializeP0, phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! specialize model
    !------------------
     
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertised, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv05 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv05p2"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeP0

  !-----------------------------------------------------------------------------

  subroutine Advertise(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(Advertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)




    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine ModifyAdvertised(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp  
    integer, intent(out) :: rc

    ! local variables
    integer :: n
    integer :: fieldCount
    type(ESMF_State) :: importState, exportState
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    character(len=*), parameter :: subname = trim(modName)//':(ModifyAdvertised) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! query for importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! loop over import fields
    !------------------

    call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)    
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
    
  end subroutine ModifyAdvertised

  !-----------------------------------------------------------------------------

  subroutine Advance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(Advance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    !------------------



    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advance

end module cop_comp_nuopc
