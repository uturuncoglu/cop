module cop_comp_shr

  !-----------------------------------------------------------------------------
  ! This is the module for shared routines 
  !-----------------------------------------------------------------------------

  use ESMF , only: operator(==)
  use ESMF , only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF , only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF , only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF , only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF , only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF , only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF , only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE

  use NUOPC, only: NUOPC_GetAttribute

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr
  public :: FB_init_pointer
  public :: StringCountChar
  public :: StringListGetName
  public :: StringListGetNum

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(*), parameter :: modName =  "(cop_comp_shr)" 
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

  subroutine StringListGetName(list, k, name, delimiter, rc)

    ! ----------------------------------------------
    ! Get name of k-th field in list
    ! It is adapted from CDEPS, shr_string_listGetName
    ! ----------------------------------------------

    ! input/output variables
    character(len=*), intent(in)  :: list       ! list/string
    integer         , intent(in)  :: k          ! index of field
    character(len=*), intent(out) :: name       ! k-th name in list
    character(1)    , intent(in)  :: delimiter  ! char to search for splitting
    integer         , intent(out) :: rc

    ! local variables
    integer :: i,n     ! generic indecies
    integer :: kFlds   ! number of fields in list
    integer :: i0,i1   ! name = list(i0:i1)
    character(*), parameter :: subName = '(StringListGetName)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Check that this is a valid index ---
    kFlds = StringListGetNum(list, delimiter)
    if (k < 1 .or. kFlds < k) then
      call ESMF_LogWrite(trim(subname)//": ERROR invalid index ", ESMF_LOGMSG_INFO)
      rc = ESMF_FAILURE
    end if

    ! Start with whole list, then remove fields before and after desired field ---
    i0 = 1
    i1 = len_trim(list)

    ! Remove field names before desired field ---
    do n=2,k
       i = index(list(i0:i1), delimiter)
       i0 = i0 + i
    end do

    ! Remove field names after desired field ---
    if ( k < kFlds ) then
       i = index(list(i0:i1), delimiter)
       i1 = i0 + i - 2
    end if

    !Copy result into output variable ---
    name = list(i0:i1)//"   "

  end subroutine StringListGetName

  !-----------------------------------------------------------------------------

  integer function StringCountChar(str, delimiter)

    ! ----------------------------------------------
    ! Count number of occurances of a character
    ! It is adapted from CDEPS, shr_string_countChar
    ! ----------------------------------------------

    ! input/output variables
    character(len=*), intent(in) :: str        ! string to search
    character(1), intent(in)     :: delimiter  ! char to search for

    ! local variables
    integer :: count    ! counts occurances of char
    integer :: n        ! generic index
    character(len=*), parameter :: subName = '(StringCountChar)'
    !---------------------------------------------------------------------------

    count = 0
    do n = 1, len_trim(str)
      if (str(n:n) == delimiter) count = count + 1
    end do
    StringCountChar = count

  end function StringCountChar

  !-----------------------------------------------------------------------------

  integer function StringListGetNum(str, delimiter)

    ! ----------------------------------------------
    ! Get number of fields in a string list
    ! It is adapted from CDEPS, shr_string_listGetNum
    ! ----------------------------------------------

    ! input/output variables
    character(len=*), intent(in) :: str   ! string to search
    character(1), intent(in)     :: delimiter  ! char to search for

    ! local variables
    integer :: count ! counts occurances of char
    character(len=*), parameter :: subName = '(StringListGetNum)'
    !---------------------------------------------------------------------------

    StringListGetNum = 0

    if (len_trim(str) > 0) then
       count = StringCountChar(str, delimiter)
       StringListGetNum = count + 1
    endif

  end function StringListGetNum

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
    !   ! Get mesh from first non-scalar field in StateIn (assumes all the fields have the same mesh)
    !   call ESMF_StateGet(StateIn, itemName=lfieldNameList(1), field=lfield, rc=rc)
    !   if (chkerr(rc,__LINE__,u_FILE_u)) return
    !end if


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine FB_init_pointer

end module cop_comp_shr
