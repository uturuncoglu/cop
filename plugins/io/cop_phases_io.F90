module cop_phases_io

  !-----------------------------------------------------------------------------
  ! Write imported fields
  !-----------------------------------------------------------------------------

  use ESMF , only: operator(==)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF, only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE

  use NUOPC_Model, only: NUOPC_ModelGet

  use cop_comp_shr, only: ChkErr
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
    integer :: n
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
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query component clock
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query current time
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Loop over states
    do n = 1, is_local%wrap%numComp
       ! Write state
       call StateWrite(is_local%wrap%NStateImp(n), trim(is_local%wrap%compName(n))//'_import_'//trim(timeStr), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine StateWrite(state, prefix, rc)

    ! input/output variables
    type(ESMF_State) :: state
    character(len=*), intent(in) :: prefix
    integer, intent(out), optional :: rc

    ! local variables
    integer :: i, n
    integer :: itemCount, fieldCount
    type(ESMF_Field) :: field
    type(ESMF_State) :: nestedState
    type(ESMF_GeomType_Flag) :: geomType
    logical :: hasNested
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    character(ESMF_MAXSTR), allocatable :: fieldNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=*), parameter :: subname = trim(modName)//':(StateWrite) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Query state
    call ESMF_StateGet(state, nestedFlag=.false., itemCount=itemCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Allocate temporary data structures
    allocate(itemNameList(itemCount))
    allocate(itemTypeList(itemCount))

    ! Query state for items
    call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set flag if nested state is found
    hasNested = .false.
    do i = 1, itemCount
       if (itemTypeList(i) == ESMF_STATEITEM_STATE) hasNested = .true.
    end do

    ! Loop over states
    if (hasNested) then
       do i = 1, itemCount
          if (itemTypeList(i) == ESMF_STATEITEM_STATE) then
             ! Get the associated nested state
             call ESMF_StateGet(state, itemName=itemNameList(i), nestedState=nestedState, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Query nested state for fields
             call ESMF_StateGet(nestedState, itemCount=fieldCount, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Allocate temporary data structure
             allocate(fieldNameList(fieldCount))

             ! Query state for field list
             call ESMF_StateGet(nestedState, itemNameList=fieldNameList, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Loop over fields
             do n = 1, fieldCount
                ! Query state for field
                call ESMF_StateGet(nestedState, field=field, itemName=trim(fieldNameList(n)), rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Get geom type
                call ESMF_FieldGet(field, geomtype=geomType, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Write field
                if (geomtype == ESMF_GEOMTYPE_GRID) then
                   call ESMF_FieldWrite(field, trim(prefix)//'_'//trim(itemNameList(n))//'.nc', variableName=trim(fieldNameList(n)), overwrite=.true., rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return
                elseif (geomtype == ESMF_GEOMTYPE_MESH) then
                   call ESMF_FieldWriteVTK(field, trim(prefix)//'_'//trim(itemNameList(n)), rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return
                else
                   call ESMF_LogWrite(trim(subname)//": ERROR geomType not supported ", ESMF_LOGMSG_INFO)
                   rc=ESMF_FAILURE
                   return
                end if ! geomType
             end do

             ! Clear memeory
             deallocate(fieldNameList)
          end if
       end do
    else
       ! Loop over fields
       do n = 1, itemCount
          ! Query state for field
          call ESMF_StateGet(state, field=field, itemName=trim(itemNameList(n)), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Get geom type
          call ESMF_FieldGet(field, geomtype=geomType, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Write field
          if (geomtype == ESMF_GEOMTYPE_GRID) then
             call ESMF_FieldWrite(field, trim(prefix)//'_'//trim(itemNameList(n))//'.nc', variableName=trim(itemNameList(n)), overwrite=.true., rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          elseif (geomtype == ESMF_GEOMTYPE_MESH) then
             call ESMF_FieldWriteVTK(field, trim(prefix)//'_'//trim(itemNameList(n)), rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          else
             call ESMF_LogWrite(trim(subname)//": ERROR geomType not supported ", ESMF_LOGMSG_INFO)
             rc=ESMF_FAILURE
             return
          end if ! geomType
       end do ! itemCount
    end if

    ! Clean memory
    deallocate(itemNameList)
    deallocate(itemTypeList)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine StateWrite

end module cop_phases_io
