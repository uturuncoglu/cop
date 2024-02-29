module cop_comp_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for generic co-processing component (COP)
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==)
  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldEmptySet
  use ESMF, only: ESMF_State, ESMF_StateGet, ESMF_StateRemove
  use ESMF, only: ESMF_Clock, ESMF_LogWrite
  use ESMF, only: ESMF_GridCompSetEntryPoint
  use ESMF, only: ESMF_AttributeGet, ESMF_DistGridConnection
  use ESMF, only: ESMF_Grid, ESMF_GridCreate, ESMF_GridGet
  use ESMF, only: ESMF_DistGrid, ESMF_DistGridCreate, ESMF_DistGridGet
  use ESMF, only: ESMF_FAILURE, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_MAXSTR
  use ESMF, only: ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_EMPTY
  use ESMF, only: ESMF_FIELDSTATUS_COMPLETE
  use ESMF, only: ESMF_GeomType_Flag, ESMF_FieldStatus_Flag
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompFilterPhaseMap, NUOPC_CompSetEntryPoint
  use NUOPC, only: NUOPC_SetAttribute
  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC, only: NUOPC_Realize

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices
  use NUOPC_Model, only: label_Advertise
  use NUOPC_Model, only: label_ModifyAdvertised
  use NUOPC_Model, only: label_RealizeAccepted
  use NUOPC_Model, only: label_Advance

  !use catalyst_api
  !use catalyst_conduit
  !use conduit
  !use conduit_relay

  use cop_comp_shr, only: ChkErr
  use cop_comp_shr, only: StringListGetName
  use cop_comp_shr, only: StringListGetNum
  use cop_comp_shr, only: StateWrite

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

  !private :: InitializeP0

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
    ! specialize model
    !------------------
     
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! if we want to remove some fields from the list - we need to specialize this, otherwise not needed
    call NUOPC_CompSpecialize(gcomp, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertised, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeAccepted, specRoutine=RealizeAccepted, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState, exportState
    character(len=*), parameter :: subname = trim(modName)//':(Advertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! query for importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! set attribute for field mirroring
    !------------------

    ! It indicates that fields should be mirrored in the State of a connected component
    call NUOPC_SetAttribute(importState, "FieldTransferPolicy", "transferAll", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine ModifyAdvertised(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp  
    integer, intent(out) :: rc

    ! local variables
    integer :: n, m
    integer :: fieldCount
    logical :: isPresent, isSet
    type(ESMF_Field) :: field
    type(ESMF_State) :: importState, exportState
    character(ESMF_MAXSTR), allocatable :: rfieldnamelist(:)
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(ESMF_MAXSTR) :: message, cname, value, scalar_field_name = ''
    character(len=*), parameter :: subname = trim(modName)//':(ModifyAdvertised) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! query for ScalarFieldName
    !------------------

    scalar_field_name = ""
    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=value, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       scalar_field_name = trim(value)
       call ESMF_LogWrite(trim(subname)//":ScalarFieldName = "//trim(scalar_field_name), ESMF_LOGMSG_INFO)
    endif

    !------------------
    ! query for RemoveFieldList
    !------------------

    call NUOPC_CompAttributeGet(gcomp, name="RemoveFieldList", value=value, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       ! Get number of fields in the list
       m = StringListGetNum(value, ":")
       if (m > 0) then
          ! Allocate temporary array for field list
          allocate(rfieldnamelist(m))

          ! Loop over occurances and fill the field list
          do n = 1, m 
             call StringListGetName(value, n, cname, ':', rc)        
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             rfieldnamelist(n) = trim(cname)
             write(message, fmt='(A,I2.2,A)') trim(subname)//':RemoveFieldList(',n,') = '//trim(rfieldnamelist(n))
             call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          end do
       end if
    endif

    !------------------
    ! query for importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! loop over import fields and remove scalar field
    !------------------

    call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)    
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       ! Get field from import state
       call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Remove field if it is cpl_scalars
       if (trim(lfieldnamelist(n)) == trim(scalar_field_name)) then
          ! Print out mirrored field name
          call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
          
          ! Remove field from import state
          call ESMF_StateRemove(importState, itemNameList=(/trim(lfieldnamelist(n))/), relaxedFlag=.true., rc=rc) 
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    end do

    !------------------
    ! remove fields in the list
    !------------------

    if (m > 0) then
       ! Print out mirrored field name
       do n = 1, m
          call ESMF_LogWrite(trim(subname)//": "//trim(rfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
       end do

       ! Remove field/s from import state
       call ESMF_StateRemove(importState, itemNameList=rfieldnamelist, relaxedFlag=.true., rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! Clean memory
    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)
    if (allocated(rfieldnamelist)) deallocate(rfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
    
  end subroutine ModifyAdvertised

  !-----------------------------------------------------------------------------

  subroutine RealizeAccepted(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer :: n, m
    integer :: fieldCount, arbDimCount
    integer :: dimCount, tileCount, connectionCount
    integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
    type(ESMF_Grid) :: newgrid, grid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Field) :: field
    type(ESMF_GeomType_Flag) :: geomType
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_State) :: importState, exportState
    type(ESMF_DistGridConnection) , allocatable :: connectionList(:)
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(len=*), parameter :: subname = trim(modName)//':(RealizeAccepted) '
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
       ! Get field from import state
       call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Get field status
       call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
          ! Get geom type
          call ESMF_FieldGet(field, geomtype=geomType, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Get grid/mesh and update decomposition
          if (geomtype == ESMF_GEOMTYPE_GRID) then
             call ESMF_LogWrite(trim(subname)//": geomType is ESMF_GEOMTYPE_GRID for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

             ! Check ArbDimCount
             call ESMF_AttributeGet(field, name="ArbDimCount", value=arbDimCount, &
               convention="NUOPC", purpose="Instance", rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Make decision on whether the incoming Grid is arbDistr or not
             if (arbDimCount > 0) then
                ! Allocate required arrays
                allocate(minIndexPTile(arbDimCount,1))
                allocate(maxIndexPTile(arbDimCount,1))

                ! Query attributes
                call ESMF_AttributeGet(field, name="MinIndex", valueList=minIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
                call ESMF_AttributeGet(field, name="MaxIndex", valueList=maxIndexPTile(:,1), &
                  convention="NUOPC", purpose="Instance", rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Create DistGrid
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Create grid with new decomposition
                newgrid = ESMF_GridCreate(distgrid, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

             else ! arbdimcount <= 0
                ! Query field to get grid
                call ESMF_FieldGet(field, grid=grid, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Query grid
                call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Query DistGrid
                call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Allocate required arrays
                allocate(minIndexPTile(dimCount, tileCount))
                allocate(maxIndexPTile(dimCount, tileCount))

                ! Query DistGrid
                call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Create grid with new decomposition
                if (dimcount == 2) then
                   ! Query DistGrid
                   call ESMF_DistGridGet(distgrid, connectionCount=connectionCount, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Allocate connectionList and fill it
                   allocate(connectionList(connectionCount))
                   call ESMF_DistGridGet(distgrid, connectionList=connectionList, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Create DistGrid
                   distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Create new grid
                   newgrid = ESMF_GridCreate(distgrid, gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Remove temporary array
                   deallocate(connectionList)

                else
                   ! Create DistGrid
                   distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Create new grid
                   newgrid = ESMF_GridCreate(distgrid, gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                end if
             end if

             ! Remove temporary arrays
             deallocate(minIndexPTile, maxIndexPTile)

             ! Swap grid
             if (fieldStatus==ESMF_FIELDSTATUS_EMPTY .or. fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
                call ESMF_LogWrite(trim(subname)//": replace grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)
                call ESMF_FieldEmptySet(field, grid=newgrid, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
             else
                call ESMF_LogWrite(trim(subname)//": NOT replacing grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)
             end if

          elseif (geomtype == ESMF_GEOMTYPE_MESH) then
             call ESMF_LogWrite(trim(subname)//": geomType is ESMF_GEOMTYPE_MESH for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

          else
             call ESMF_LogWrite(trim(subname)//": ERROR geomType not supported ", ESMF_LOGMSG_INFO)
             rc=ESMF_FAILURE
             return

          end if ! geomType

       elseif (fieldStatus==ESMF_FIELDSTATUS_EMPTY) then
          call ESMF_LogWrite(trim(subname)//": provide grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)                 

       elseif (fieldStatus==ESMF_FIELDSTATUS_COMPLETE) then
          call ESMF_LogWrite(trim(subname)//": no grid provided for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

       else
          call ESMF_LogWrite(trim(subname)//": ERROR fieldStatus not supported ", ESMF_LOGMSG_INFO)
          rc=ESMF_FAILURE
          return 

       end if ! fieldStatus

       ! Realize the advertised field
       call NUOPC_Realize(importState, fieldName=trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

    end do

    ! Clean memory
    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
 
  end subroutine RealizeAccepted

  !-----------------------------------------------------------------------------

  subroutine Advance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Time) :: currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState
    character(len=ESMF_MAXSTR) :: timeStr
    character(len=*), parameter :: subname = trim(modName)//':(Advance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! query for clock, importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Write import state (for debugging)
    !------------------

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return  

    call StateWrite(importState, 'import_'//trim(timeStr), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return 

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advance

  ! different run phase for each workflow / very high level


end module cop_comp_nuopc
