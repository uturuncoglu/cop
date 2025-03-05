module geogate_share

  !-----------------------------------------------------------------------------
  ! This is the module for shared routines 
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==)
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF, only: ESMF_MAXSTR, ESMF_KIND_R8
  use ESMF, only: ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE

  use NUOPC, only: NUOPC_GetAttribute

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr
  public :: StringSplit
  public :: FB_init_pointer

  !-----------------------------------------------------------------------------
  ! Public module data
  !-----------------------------------------------------------------------------

  logical, public :: debugMode
  real(ESMF_KIND_R8), public, parameter :: CONST_PI = 3.14159265358979323846_ESMF_KIND_R8
  real(ESMF_KIND_R8), public, parameter :: CONST_RAD2DEG = 180.0_ESMF_KIND_R8/CONST_PI

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(*), parameter :: modName =  "(geogate_share)" 
  character(len=*), parameter :: u_FILE_u = __FILE__ 

!===============================================================================  
  contains
!===============================================================================

  logical function ChkErr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif
  end function ChkErr

  !-----------------------------------------------------------------------------

  function StringSplit(str, delim) result(parts)
    implicit none

    ! ----------------------------------------------
    ! The `split` function splits a given string into an array of substrings
    ! based on a specified delimiter.
    ! ----------------------------------------------

    ! input/output variables
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: delim
    character(len=:), allocatable :: parts(:)

    ! local variables
    integer :: i, start, count
    character(*), parameter :: subName = '(StringSplit)'
    !---------------------------------------------------------------------------

    ! Count the number of delimiters to determine the size of the parts array
    count = 0
    do i = 1, len(trim(str))
      if (str(i:i) == delim) count = count+1
    end do

    ! Allocate the parts array
    allocate(character(len=len(trim(str))) :: parts(count+1))

    ! Split the string
    start = 1
    count = 1
    do i = 1, len(trim(str))
      if (str(i:i) == delim) then
        parts(count) = str(start:i-1)
        start = i+1
        count = count+1
      end if
    end do
    parts(count) = str(start:)

    ! Trim the parts to remove any trailing spaces
    do i = 1, count
      parts(i) = trim(parts(i))
    end do

  end function StringSplit

  !-----------------------------------------------------------------------------

  subroutine FB_init_pointer(StateIn, FBout, name, rc)

    ! input/output variables
    type(ESMF_State), intent(in) :: StateIn
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*), intent(in) :: name
    integer, intent(out), optional :: rc

    ! local variables
    integer :: fieldCount
    character(ESMF_MAXSTR), allocatable :: lfieldNameList(:)
    character(len=*), parameter :: subname = trim(modName)//':(FB_init_pointer) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Create empty FBout
    FBout = ESMF_FieldBundleCreate(name=trim(name), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Get fields from state
    call ESMF_StateGet(StateIn, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldNameList(fieldCount))
    call ESMF_StateGet(StateIn, itemNameList=lfieldNameList, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Create field bundle
    !if (fieldCount > 0) then
       ! Get mesh from first non-scalar field in StateIn (assumes all the fields have the same mesh)
       !call ESMF_StateGet(StateIn, itemName=lfieldNameList(1), field=lfield, rc=rc)
       !if (chkerr(rc,__LINE__,u_FILE_u)) return
    !end if


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine FB_init_pointer

end module geogate_share
