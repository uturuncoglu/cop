module cop_comp_shr

  !-----------------------------------------------------------------------------
  ! This is the module for shared routines 
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==)
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr
  public :: StringCountChar
  public :: StringListGetName
  public :: StringListGetNum
  public :: StateWrite

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

  subroutine StateWrite(state, prefix, rc)

    ! input/output variables
    type(ESMF_State) :: state
    character(len=*), intent(in) :: prefix 
    integer, intent(out), optional :: rc

    ! local variables
    integer :: n, fieldCount
    type(ESMF_Field) :: field
    type(ESMF_GeomType_Flag) :: geomType    
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(len=*), parameter :: subname = trim(modName)//':(StateWrite) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! loop over fields in state
    !------------------

    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))

    call ESMF_StateGet(state, itemNameList=lfieldnamelist, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       ! Get field from import state
       call ESMF_StateGet(state, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Get geom type
       call ESMF_FieldGet(field, geomtype=geomType, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Write field 
       if (geomtype == ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldWrite(field, trim(prefix)//'_'//trim(lfieldnamelist(n))//'.nc', variableName=trim(lfieldnamelist(n)), overwrite=.true., rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       elseif (geomtype == ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldWriteVTK(field, trim(prefix)//'_'//trim(lfieldnamelist(n)), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else
          call ESMF_LogWrite(trim(subname)//": ERROR geomType not supported ", ESMF_LOGMSG_INFO)
          rc=ESMF_FAILURE
          return
       end if ! geomType
    end do

    ! Clean memory
    deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine StateWrite

end module cop_comp_shr
