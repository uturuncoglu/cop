module cop_comp_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for generic co-processing component (COP)
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_LogWrite
  use ESMF, only: ESMF_LOGMSG_INFO, ESMF_SUCCESS

  use NUOPC, only : NUOPC_CompDerive

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: model_routine_SS => SetServices

  use catalyst_api
  use catalyst_conduit
  use conduit
  use conduit_relay

  use cop_comp_shr, only: ChkErr

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module data and routines
  !-----------------------------------------------------------------------------

  public :: SetServices
  public :: SetVM

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
    character(len=*), parameter  :: subname = trim(modName)//':(SetServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

end module cop_comp_nuopc
