module cop_python_interface

  !-----------------------------------------------------------------------------
  ! Void phase for Python interaction 
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO
  use, intrinsic :: iso_c_binding, only : C_PTR, C_CHAR

  implicit none

  !-----------------------------------------------------------------------------
  ! Public module interface
  !-----------------------------------------------------------------------------

  interface
    subroutine conduit_fort_to_py(cnode, py_script) bind(C, name="conduit_fort_to_py")
      use iso_c_binding
      implicit none
      type(C_PTR), value, intent(in) :: cnode
      character(kind=C_CHAR), intent(in) :: py_script(*)
    end subroutine conduit_fort_to_py

    function c_conduit_fort_from_py(name) result(res) bind(C, name="conduit_fort_from_py")
       use iso_c_binding
       implicit none
       character(kind=C_CHAR), intent(in) :: name(*)
       type(C_PTR) :: res
    end function c_conduit_fort_from_py
  end interface

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(cop_python_interface)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  function conduit_fort_from_py(name) result(res)
    use iso_c_binding
    implicit none

    ! input/output variables
    character(*), intent(in) :: name
    type(C_PTR) :: res

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(conduit_fort_from_py) '
    !---------------------------------------------------------------------------

    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    res = c_conduit_fort_from_py(trim(name) // C_NULL_CHAR)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end function conduit_fort_from_py

end module cop_python_interface
