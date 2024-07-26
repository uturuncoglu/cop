module cop_comp_internalstate

  !-----------------------------------------------------------------------------
  ! This is the module for shared routines 
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_State, ESMF_FieldBundle
  use ESMF, only: ESMF_LogWrite
  use ESMF, only: ESMF_SUCCESS, ESMF_LOGMSG_INFO

  use cop_comp_shr, only: ChkErr

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Private module routines and data structures
  !-----------------------------------------------------------------------------

  ! Internal state to keep instance data
  type InternalStateStruct
     type(ESMF_State), pointer :: NStateImp(:)   ! Import data from various component, on their grid
     type(ESMF_FieldBundle), pointer :: FBImp(:)
  end type InternalStateStruct

  !-----------------------------------------------------------------------------
  ! Public module routines and data structures
  !-----------------------------------------------------------------------------

  public :: InternalStateInit

  type, public :: InternalState
     type(InternalStateStruct), pointer :: wrap
  end type InternalState

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  ! TODO: The number of simultaneously connected component is fixed to 10. This
  ! needs to be run time configurable
  integer :: nconn = 10

  character(*), parameter :: modName =  "(cop_comp_types)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================  
  contains
!===============================================================================

  subroutine InternalStateInit(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(InternalState) :: is_local
    character(len=*), parameter :: subname = trim(modName)//':(InternalStateInit) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query for internal state 
    !------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Allocate memory
    !------------------

    !allocate(is_local%wrap%FBImp(nconn))
    allocate(is_local%wrap%NStateImp(nconn))

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InternalStateInit

end module cop_comp_internalstate
