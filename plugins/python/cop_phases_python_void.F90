module cop_phases_python

  !-----------------------------------------------------------------------------
  ! Void phase for Python interaction 
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_LogWrite, ESMF_FAILURE
  use ESMF, only: ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: cop_phases_python_run

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  integer :: dbug = 0
  character(len=*), parameter :: modName = "(cop_phases_python)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine cop_phases_python_run(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_python_run) '
    !---------------------------------------------------------------------------

    rc = ESMF_FAILURE
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(subname//' Please recompile with Python support', ESMF_LOGMSG_ERROR)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine cop_phases_python_run

end module cop_phases_python
