module cop_comp_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for generic co-processing component (COP)
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==)
  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldEmptySet
  use ESMF, only: ESMF_State, ESMF_StateGet, ESMF_StateRemove
  use ESMF, only: ESMF_Clock, ESMF_LogWrite, ESMF_LogFoundAllocError
  use ESMF, only: ESMF_StateGet, ESMF_GridCompSetEntryPoint
  use ESMF, only: ESMF_GridCompGetInternalState, ESMF_GridCompSetInternalState
  use ESMF, only: ESMF_AttributeGet, ESMF_DistGridConnection
  use ESMF, only: ESMF_Grid, ESMF_GridCreate, ESMF_GridGet
  use ESMF, only: ESMF_DistGrid, ESMF_DistGridCreate, ESMF_DistGridGet
  use ESMF, only: ESMF_FAILURE, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_MAXSTR
  use ESMF, only: ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_EMPTY
  use ESMF, only: ESMF_FIELDSTATUS_COMPLETE, ESMF_StateItem_Flag
  use ESMF, only: ESMF_STATEITEM_STATE
  use ESMF, only: ESMF_GeomType_Flag, ESMF_FieldStatus_Flag
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompFilterPhaseMap, NUOPC_CompSetEntryPoint
  use NUOPC, only: NUOPC_SetAttribute, NUOPC_GetAttribute
  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC, only: NUOPC_Realize
  use NUOPC, only: NUOPC_AddNamespace
 
  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices
  use NUOPC_Model, only: model_routine_Run => routine_Run
  use NUOPC_Model, only: model_label_Advance => label_Advance
  use NUOPC_Model, only: label_Advertise
  use NUOPC_Model, only: label_ModifyAdvertised
  use NUOPC_Model, only: label_RealizeAccepted
  use NUOPC_Model, only: label_Advance

  use cop_comp_shr, only: ChkErr
  use cop_comp_shr, only: StringListGetName
  use cop_comp_shr, only: StringListGetNum
  use cop_comp_shr, only: StateWrite
  
  use cop_comp_internalstate, only: InternalState
  use cop_comp_internalstate, only: InternalStateInit 

  !use cop_phases_catalyst, only: cop_phases_catalyst_run

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

  integer :: dbug = 0
  !integer :: numProvider
  !integer :: maxNumProvider = 10
  !character(ESMF_MAXSTR) :: providerList(maxNumProvider)
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
    
    ! It is used to set FieldTransferPolicy 
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! It is used to remove some fields from the list of mirrored fields
    call NUOPC_CompSpecialize(gcomp, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertised, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! It is used to realized mirrored fields and also update decomposition
    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeAccepted, specRoutine=RealizeAccepted, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! It is used to run user specified phase to process the data
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! setup phases for Catalyst plugin
    !------------------

    !call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, phaseLabelList=(/"cop_phases_catalyst_run"/), &
    !   userRoutine=model_routine_Run, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
    !   specPhaseLabel="cop_phases_catalyst_run", specRoutine=cop_phases_catalyst_run, rc=rc)


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer :: stat
    type(InternalState) :: is_local
    type(ESMF_State) :: importState, exportState
    character(len=*), parameter :: subname = trim(modName)//':(Advertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Allocate memory for the internal state
    !------------------

    allocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
         msg="Allocation of the internal state memory failed.", line=__LINE__, file=u_FILE_u)) then
       return
    end if

    call ESMF_GridCompSetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call InternalStateInit(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Query for importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Set attribute for field mirroring
    ! It indicates that fields should be mirrored in the State of a connected component
    !------------------

    !call NUOPC_SetAttribute(importState, "FieldTransferPolicy", "transferAll", rc=rc)
    call NUOPC_SetAttribute(importState, "FieldTransferPolicy", "transferAllNested", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine ModifyAdvertised(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp  
    integer, intent(out) :: rc

    ! local variables
    integer :: k, n, m
    integer :: stat
    integer :: fieldCount, itemCount, importItemCount
    logical :: isPresent, isSet, isFound
    type(InternalState) :: is_local
    type(ESMF_Field) :: field
    type(ESMF_State) :: importState, exportState
    type(ESMF_State) :: importNestedState, exportNestedState
    character(ESMF_MAXSTR), allocatable :: kfieldnamelist(:)
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(ESMF_MAXSTR), allocatable :: rfieldnamelist(:)
    character(ESMF_MAXSTR) :: stateName 
    character(ESMF_MAXSTR) :: message, cname, cvalue, scalar_field_name = ''

    character(ESMF_MAXSTR), allocatable     :: importItemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: importItemTypeList(:)
    character(ESMF_MAXSTR), allocatable     :: exportItemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: exportItemTypeList(:)

    character(len=*), parameter :: subname = trim(modName)//':(ModifyAdvertised) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query internal state 
    !------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Query for ScalarFieldName
    !------------------

    scalar_field_name = ""
    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       scalar_field_name = trim(cvalue)
       call ESMF_LogWrite(trim(subname)//": ScalarFieldName = "//trim(scalar_field_name), ESMF_LOGMSG_INFO)
    endif

    !------------------
    ! Query for KeepFieldList (only list of fields will be abailable)
    !------------------

    call NUOPC_CompAttributeGet(gcomp, name="KeepFieldList", value=cvalue, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    m = 0
    if (isPresent .and. isSet) then
       ! Get number of fields in the list
       m = StringListGetNum(cvalue, ":")
       if (m > 0) then
          ! Allocate temporary array for field list
          allocate(kfieldnamelist(m))

          ! Loop over occurances and fill the field list
          do n = 1, m
             call StringListGetName(cvalue, n, cname, ':', rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             kfieldnamelist(n) = trim(cname)
             write(message, fmt='(A,I2.2,A)') trim(subname)//': KeepFieldList(',n,') = '//trim(kfieldnamelist(n))
             call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          end do
       end if
    endif

    !------------------
    ! Query for RemoveFieldList (remove fields from mirrored field list)
    !------------------

    call NUOPC_CompAttributeGet(gcomp, name="RemoveFieldList", value=cvalue, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    m = 0
    if (isPresent .and. isSet) then
       ! Get number of fields in the list
       m = StringListGetNum(cvalue, ":")
       if (m > 0) then
          ! Allocate temporary array for field list
          allocate(rfieldnamelist(m))

          ! Loop over occurances and fill the field list
          do n = 1, m 
             call StringListGetName(cvalue, n, cname, ':', rc)        
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             rfieldnamelist(n) = trim(cname)
             write(message, fmt='(A,I2.2,A)') trim(subname)//': RemoveFieldList(',n,') = '//trim(rfieldnamelist(n))
             call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          end do
       end if
    endif

    !------------------
    ! Query for importState and exportState
    !------------------

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Loop over import fields and remove/keep requested fields if there are
    !------------------

    call ESMF_StateGet(importState, nestedFlag=.false., name=StateName, itemCount=importItemCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Check for nested states
    if (importItemCount == 0) then
       call ESMF_StateGet(importState, nestedFlag=.true., name=StateName, itemCount=importItemCount, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if


!
!    allocate(importItemNameList(importItemCount))
!    allocate(importItemTypeList(importItemCount))
!
!    call ESMF_StateGet(importState, nestedFlag=.false., itemNameList=importItemNameList, itemTypeList=importItemTypeList, rc=rc)
!    if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
    write(message, fmt='(A,I5,A)') trim(subname)//': importItemCount = ', importItemCount, ' from state '//trim(StateName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!
!    if (importItemCount == 0) then
       ! Check for nested states
       
       

!    do n = 1, importItemCount
!       call ESMF_LogWrite(trim(subname)//": received "//trim(importItemNameList(n))//" from "//trim(cvalue), ESMF_LOGMSG_INFO)
!
!       if (importItemTypeList(n) == ESMF_STATEITEM_STATE) then
!          ! get nested state
!          call ESMF_StateGet(importState, itemName=importItemNameList(n), &
!            nestedState=importNestedState, rc=rc)
!          if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
!          ! remove fields from state




!          ! loop over import fields and remove scalar field
!          call ESMF_StateGet(importNestedState, itemCount=fieldCount, rc=rc)
!          if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
!          ! allocate field list
!          if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))
!
!          ! get field list
!          call ESMF_StateGet(importNestedState, itemNameList=lfieldnamelist, rc=rc)    
!          if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
!          do m = 1, fieldCount
!             ! get field from import state
!             call ESMF_StateGet(importNestedState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
!             if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
!             ! get provider name
!             call NUOPC_GetAttribute(field, name="ProviderCompName", value=cvalue, rc=rc)
!             if (ChkErr(rc,__LINE__,u_FILE_u)) return
!
!             ! print out mirrored field name
!             call ESMF_LogWrite(trim(subname)//": received "//trim(lfieldnamelist(n))//" from "//trim(cvalue), ESMF_LOGMSG_INFO)
!
!             ! Remove field if it is cpl_scalars
!             if (trim(lfieldnamelist(n)) == trim(scalar_field_name)) then
!                ! Print out mirrored field name
!                call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n))//" is removed", ESMF_LOGMSG_INFO)
!                
!                ! Remove field from import state
!                call ESMF_StateRemove(importState, itemNameList=(/trim(lfieldnamelist(n))/), relaxedFlag=.true., rc=rc) 
!                if (ChkErr(rc,__LINE__,u_FILE_u)) return
!             end if
!
!             ! Remove field if it is not in the KeepFieldList
!             do k = 1, size(kfieldnamelist, dim=1)
!                if (trim(lfieldnamelist(n)) == trim(kfieldnamelist(k))) then
!                   ! Match is found, skip removing
!                   cycle
!                else
!                   ! Remove field from import state
!                   call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
!                   call ESMF_StateRemove(importState, itemNameList=(/trim(lfieldnamelist(n))/), relaxedFlag=.true., rc=rc)   
!                   if (ChkErr(rc,__LINE__,u_FILE_u)) return
!                end if
!             end do
!          end do
!       end if
!    enddo

    !------------------
    ! Remove fields in the given list
    !------------------

!    if (size(rfieldnamelist, dim=1) > 0) then
!       ! Print out mirrored field name
!       do n = 1, size(rfieldnamelist, dim=1)
!          call ESMF_LogWrite(trim(subname)//": "//trim(rfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
!       end do
!
!       ! Remove field/s from import state
!       call ESMF_StateRemove(importState, itemNameList=rfieldnamelist, relaxedFlag=.true., rc=rc)
!       if (ChkErr(rc,__LINE__,u_FILE_u)) return
!    end if
!
!    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

    !------------------
    ! Find out number of provider and populate list
    !------------------

    !call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))

    !do n = 1, fieldCount
    !   ! Get field from import state
    !   call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
    !   if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !   ! Get attribute from field
    !   call NUOPC_GetAttribute(field, name="ProviderCompName", value=cvalue, &
    !     isPresent=isPresent, isSet=isSet, rc=rc)
    !   if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !   ! Check the list
    !   do i = 1, maxNumProvider
    !      if (trim(providerList(i)) == trim(cvalue)) then
    !         isFound = .true.
    !         exit
    !      end if
    !   end do

    !   ! If it is a new entry
    !   if (isFound) then
    !   end if


    !   ! Find out number of provider and their names
    !   !if (isPresent .and. isSet) then
    !   !
    !   !   print*, "Field, ProviderCompName = ", trim(lfieldnamelist(n)), trim(cvalue)
    !   !end if
    !end do

    !------------------
    ! Clean memory
    !------------------

    if (allocated(kfieldnamelist)) deallocate(kfieldnamelist)
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
    type(InternalState) :: is_local
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
    ! Query internal state 
    !------------------

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

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

       ! Check field status to modify decomposition
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
    logical :: isPresent, isSet 
    character(len=ESMF_MAXSTR) :: message
    character(len=ESMF_MAXSTR) :: cvalue 
    character(len=ESMF_MAXSTR) :: timeStr
    character(len=*), parameter :: subname = trim(modName)//':(Advance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! query for DebugLevel 
    !------------------

    call NUOPC_CompAttributeGet(gcomp, name='DebugLevel', value=cvalue, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (isPresent .and. isSet) then
       read(cvalue,*) dbug
    end if

    write(message, fmt='(A,L)') trim(subname)//': DebugLevel = ', dbug
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

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

    if (dbug > 5) then
       call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return  

       call StateWrite(importState, 'import_'//trim(timeStr), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    !call ESMF_StateGet(importState, name=cvalue, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !call ESMF_LogWrite(trim(subname)//' state name = '//trim(cvalue))
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advance

  ! different run phase for each workflow / very high level


end module cop_comp_nuopc
