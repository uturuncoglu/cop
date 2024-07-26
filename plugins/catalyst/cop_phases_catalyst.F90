module cop_phases_catalyst
#ifdef USE_CATALYST

  !-----------------------------------------------------------------------------
  ! Co-processing component (COP) phase for Insitu Analysis with Catalyst 
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp

  !use catalyst_api
  !use catalyst_conduit
  !use conduit
  !use conduit_relay

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
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_catalyst) '
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

    if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Clean memory
    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine cop_phases_catalyst_run

#endif
end module cop_phases_catalyst
