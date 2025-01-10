module cop_phases_python

  !-----------------------------------------------------------------------------
  ! Phase for Python interaction 
  !-----------------------------------------------------------------------------

  use ESMF , only: operator(==)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF, only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE
  use ESMF, only: ESMF_Mesh, ESMF_MeshGet, ESMF_STATEITEM_FIELD
  use ESMF, only: ESMF_KIND_R8

  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC_Model, only: NUOPC_ModelGet

  use, intrinsic :: iso_c_binding, only : C_PTR
  use conduit

  use cop_comp_shr, only: ChkErr
  use cop_comp_internalstate, only: InternalState
  use cop_python_interface, only: conduit_fort_to_py

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

  type(C_PTR) :: cnode
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
    integer :: n, mpiComm, localPet, petCount
    type(InternalState) :: is_local
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState
    logical :: isPresent, isSet
    character(len=ESMF_MAXSTR) :: timeStr
    character(ESMF_MAXSTR) :: cvalue
    character(ESMF_MAXSTR) :: scriptName
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_python_run) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query component
    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query VM
    call ESMF_VMGet(vm, mpiCommunicator=mpiComm, localPet=localPet, petCount=petCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query current time
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query name of Python script
    call NUOPC_CompAttributeGet(gcomp, name="PythonScript", value=cvalue, &
      isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       scriptName = trim(cvalue)
       call ESMF_LogWrite(trim(subname)//": PythonScript = "//trim(scriptName), ESMF_LOGMSG_INFO)
    endif

    ! Create Conduit node
    cnode = conduit_node_create()

    ! Add information related to MPI
    call conduit_node_set_path_int32(cnode, "par/comm", mpiComm)
    call conduit_node_set_path_int32(cnode, "par/lpet", localPet)
    call conduit_node_set_path_int32(cnode, "par/npet", petCount)

    ! Loop over states
    do n = 1, is_local%wrap%numComp
       ! Add content of state to Conduit node
       call StateToNode(is_local%wrap%NStateImp(n), trim(is_local%wrap%compName(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Print node for debugging purpose
    !write(cvalue, fmt="(A,I5.5,A)") 'cop_node_', localPet, '.json'
    !call conduit_node_save(cnode, trim(cvalue), 'json')

    ! Pass node to Python
    call conduit_fort_to_py(cnode, trim(scriptName)//char(0))

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine cop_phases_python_run

  !-----------------------------------------------------------------------------

  subroutine StateToNode(state, compName, rc)
    use iso_c_binding
    implicit none

    ! input/output variables
    type(ESMF_State) :: state 
    character(len=*), intent(in) :: compName
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Mesh) :: mesh
    type(ESMF_Field) :: field
    integer :: n, m, rank, itemCount
    integer :: spatialDim, numOwnedElements
    logical, save :: firstTime = .true.
    integer, allocatable :: localElementCount(:)
    real(ESMF_KIND_R8), allocatable :: ownedElemCoords(:)
    real(ESMF_KIND_R8), pointer :: lats(:), lons(:)
    real(ESMF_KIND_R8), pointer :: fptr(:)
    character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=*), parameter :: subname = trim(modName)//':(StateToNode) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Query state
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (itemCount > 0) then
       ! Allocate temporary arrays
       allocate(itemNameList(itemCount))
       allocate(itemTypeList(itemCount))

       ! Query item names and types
       call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Loop over items
       do n = 1, itemCount
          ! Check if item is field
          if (itemTypeList(n) == ESMF_STATEITEM_FIELD) then
             ! Query field
             call ESMF_StateGet(state, itemName=itemNameList(n), field=field, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Extract mesh information and add it to the node
             ! This assumes all fields are on same mesh
             if (.not. conduit_node_has_path(cnode, trim(compName)//'/lon')) then
                ! Query mesh
                call ESMF_FieldGet(field, mesh=mesh, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Extract coordinate information
                call ESMF_MeshGet(mesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                allocate(lats(numOwnedElements))
                allocate(lons(numOwnedElements))
                allocate(ownedElemCoords(spatialDim*numOwnedElements))
                
                call ESMF_MeshGet(mesh, ownedElemCoords=ownedElemCoords, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                do m = 1, numOwnedElements
                   lons(m) = ownedElemCoords(2*m-1)
                   lats(m) = ownedElemCoords(2*m)
                end do

                ! Add coordinates to node
                call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lon', lons, int8(numOwnedElements))
                call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lat', lats, int8(numOwnedElements))

                ! Clean memory
                deallocate(ownedElemCoords)
                nullify(lons)
                nullify(lats)

             end if ! node check for coordinate info

             ! Query field
             call ESMF_FieldGet(field, rank=rank, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             allocate(localElementCount(rank))

             call ESMF_FieldGet(field, localElementCount=localElementCount, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Add field to node
             ! TODO: Assuming rank = 1 and also double
             call conduit_node_set_path_float64_ptr(cnode, &
               trim(compName)//'/'//trim(itemNameList(n)), fptr, int8(localElementCount(1)))

             ! Clean memory
             deallocate(localElementCount)
             nullify(fptr)

          end if ! itemTypeList
       end do
    end if ! itemCount

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine StateToNode

end module cop_phases_python
