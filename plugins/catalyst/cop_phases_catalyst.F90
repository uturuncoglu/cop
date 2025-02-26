module cop_phases_catalyst

  !-----------------------------------------------------------------------------
  ! Phase for ParaView Catalyst interaction
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==), operator(/=), operator(-), operator(/)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_VM, ESMF_VMGet
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_State, ESMF_StateGet, ESMF_StateItem_Flag
  use ESMF, only: ESMF_Field, ESMF_FieldGet
  use ESMF, only: ESMF_MAXSTR, ESMF_KIND_R8
  use ESMF, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_STATE
  use ESMF, only: ESMF_Mesh, ESMF_MeshGet
  use ESMF, only: ESMF_CoordSys_Flag, ESMF_COORDSYS_SPH_DEG

  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC_Model, only: NUOPC_ModelGet

  use catalyst_conduit
  use catalyst_api

  use cop_comp_shr, only: ChkErr, StringSplit
  use cop_comp_shr, only: CONST_RAD2DEG
  use cop_comp_internalstate, only: InternalState

  use, intrinsic :: iso_c_binding, only: C_PTR

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: cop_phases_catalyst_run

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(cop_phases_catalyst)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine cop_phases_catalyst_run(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(C_PTR) :: node
    type(C_PTR) :: scriptArgsItem, scriptArgs
    integer(kind(catalyst_status)) :: err
    integer :: n, numScripts, step
    integer :: mpiComm
    real(kind=8) :: time
    logical :: isPresent, isSet
    logical, save :: first_time = .true.
    type(InternalState) :: is_local
    type(ESMF_VM) :: vm
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState
    character(ESMF_MAXSTR) :: cvalue, tmpStr, scriptName
    character(ESMF_MAXSTR) :: timeStr
    character(ESMF_MAXSTR) :: message
    character(ESMF_MAXSTR) :: catalystImpl, paraviewImplDir
    character(ESMF_MAXSTR), allocatable :: scriptNames(:)
    character(len=*), parameter :: subname = trim(modName)//':(cop_phases_catalyst_run) '
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

    ! Query VM and communicator
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=mpiComm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Query current time
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalGet(currTime-startTime, s_r8=time, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Initialize
    if (first_time) then
       ! This node will hold the information nessesary to initialize ParaViewCatalyst
       node = catalyst_conduit_node_create()

       ! Query name of Catalyst script
       call NUOPC_CompAttributeGet(gcomp, name="CatalystScripts", value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (isPresent .and. isSet) then
          scriptNames = StringSplit(trim(cvalue), ":")
          do n = 1, size(scriptNames, dim=1)
             write(message, fmt='(A,I1,A)') trim(subname)//": CatalystScript (", n, ") = "//trim(scriptNames(n))
             call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          end do
       endif

       ! Set script name and arguments
       do n = 1, size(scriptNames, dim=1)
          ! Add script
          write(tmpStr, '(A,I1)') 'catalyst/scripts/script', n
          call catalyst_conduit_node_set_path_char8_str(node, trim(tmpStr)//"/filename", trim(scriptNames(n)))
          ! Add arguments
          scriptArgs = catalyst_conduit_node_fetch(node, trim(tmpStr)//"/args")
          scriptArgsItem = catalyst_conduit_node_append(scriptArgs)
          call catalyst_conduit_node_set_char8_str(scriptArgsItem, "--channel-name=grid")
       end do

       ! Set implementation type
       call get_environment_variable("CATALYST_IMPLEMENTATION_NAME", catalystImpl)
       if (trim(catalystImpl) == '') then
          call catalyst_conduit_node_set_path_char8_str(node, "catalyst_load/implementation", "paraview")
       else
          call ESMF_LogWrite(trim(subname)//": CATALYST_IMPLEMENTATION_NAME = "//trim(catalystImpl), ESMF_LOGMSG_INFO)
       end if

       ! Set Paraview/Catalyst search path
       call get_environment_variable("CATALYST_IMPLEMENTATION_PATHS", paraviewImplDir)
       if (trim(paraviewImplDir) == '') then
          call NUOPC_CompAttributeGet(gcomp, name="CatalystLoadPath", value=cvalue, &
            isPresent=isPresent, isSet=isSet, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          if (isPresent .and. isSet) then
             call catalyst_conduit_node_set_path_char8_str(node, "catalyst_load/search_paths/paraview", trim(cvalue))
             call ESMF_LogWrite(trim(subname)//": CatalystLoadPath = "//trim(cvalue), ESMF_LOGMSG_INFO)
          end if
       else
          call ESMF_LogWrite(trim(subname)//": CATALYST_IMPLEMENTATION_PATHS = "//trim(paraviewImplDir), ESMF_LOGMSG_INFO)
       end if

       ! Add MPI communicator
       call catalyst_conduit_node_set_path_int32(node, "catalyst/mpi_comm", mpiComm)

       ! Print out node for debugging
       !call catalyst_conduit_node_print_detailed(node)

       ! Initialize catalyst
       err = c_catalyst_initialize(node)
       if (err /= catalyst_status_ok) then
          write(message, fmt='(A,I)') trim(subname)//": Failed to initialize Catalyst: ", err
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
       end if

       ! Destroy node which is not required
       call catalyst_conduit_node_destroy(node)

       ! Set flag
       first_time = .false.
    end if

    ! This node will hold the information nessesary to execute ParaViewCatalyst
    node = catalyst_conduit_node_create()

    ! Add time/cycle information - Catalyst-specific variables
    step = int((currTime-startTime)/timeStep)
    call catalyst_conduit_node_set_path_int32(node, "catalyst/state/timestep", step)
    call catalyst_conduit_node_set_path_float64(node, "catalyst/state/time", time)

    ! Add channel for all components
    do n = 1, is_local%wrap%numComp
       ! Add content of state to Conduit node
       call StateToChannel(is_local%wrap%NStateImp(n), trim(is_local%wrap%compName(n)), node, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Print out node for debugging
    call catalyst_conduit_node_print_detailed(node)

    ! Execute catalyst
    err = c_catalyst_execute(node)
    if (err /= catalyst_status_ok) then
       write(message, fmt='(A,I)') trim(subname)//": Failed to execute Catalyst: ", err
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
       rc = ESMF_FAILURE
       return
    end if

    ! Destroy node
    call catalyst_conduit_node_destroy(node)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine cop_phases_catalyst_run

  !-----------------------------------------------------------------------------

  subroutine StateToChannel(state, compName, node, rc)

    ! input/output variables
    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: compName
    type(C_PTR), intent(inout) :: node
    integer, intent(out), optional :: rc

    ! local variables
    type(C_PTR) :: channel
    type(C_PTR) :: mesh
    type(C_PTR) :: fields
    integer :: n, m, itemCount
    integer :: spatialDim, numOwnedNodes, numOwnedElements
    integer :: numNodesMin, numNodesMax, dataSize
    type(ESMF_Field) :: field
    type(ESMF_Mesh) :: fmesh
    type(ESMF_CoordSys_Flag) :: coordSys
    integer, allocatable :: elemConn(:)
    integer, allocatable :: elementTypes(:)
    real(ESMF_KIND_R8), pointer :: farrayPtr(:)
    real(ESMF_KIND_R8), allocatable :: ownedNodeCoords(:)
    real(ESMF_KIND_R8), allocatable :: ownedNodeLats(:)
    real(ESMF_KIND_R8), allocatable :: ownedNodeLons(:)
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    character(ESMF_MAXSTR) :: message
    character(len=*), parameter :: subname = trim(modName)//':(StateToChannel) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called for '//trim(compName), ESMF_LOGMSG_INFO)

    ! Add channel
    channel = catalyst_conduit_node_fetch(node, "catalyst/channels/"//trim(compName))

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

             if (n == 1) then
                ! Add mesh to channel
                call catalyst_conduit_node_set_path_char8_str(channel, "type", "mesh")

                ! Create mesh
                mesh = catalyst_conduit_node_fetch(channel, "data")

                ! Set type of mesh, construct as an unstructured mesh
                call catalyst_conduit_node_set_path_char8_str(mesh, "coordsets/coords/type", "explicit")

                ! Query field mesh
                call ESMF_FieldGet(field, mesh=fmesh, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Extract coordinate information
                call ESMF_MeshGet(fmesh, spatialDim=spatialDim, numOwnedNodes=numOwnedNodes, &
                   numOwnedElements=numOwnedElements, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Allocate data structures
                allocate(elementTypes(numOwnedElements))
                allocate(ownedNodeLats(numOwnedNodes))
                allocate(ownedNodeLons(numOwnedNodes))
                allocate(ownedNodeCoords(spatialDim*numOwnedNodes))

                ! Get element coordinates and fill arrays
                call ESMF_MeshGet(fmesh, ownedNodeCoords=ownedNodeCoords, &
                   coordSys=coordSys, elementTypes=elementTypes, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                do m = 1, numOwnedNodes
                   ownedNodeLons(m) = ownedNodeCoords(2*m-1)
                   ownedNodeLats(m) = ownedNodeCoords(2*m)
                end do

                ! Convert coordinates from radian to degree
                if (coordSys /= ESMF_COORDSYS_SPH_DEG) then
                   ownedNodeLons = ownedNodeLons*CONST_RAD2DEG
                   ownedNodeLats = ownedNodeLats*CONST_RAD2DEG
                end if

                ! Add coordinates
                call catalyst_conduit_node_set_path_external_float64_ptr(mesh, "coordsets/coords/values/x", &
                   ownedNodeLons, int8(numOwnedNodes))
                call catalyst_conduit_node_set_path_external_float64_ptr(mesh, "coordsets/coords/values/y", &
                   ownedNodeLats, int8(numOwnedNodes))

                ! Add topology
                call catalyst_conduit_node_set_path_char8_str(mesh, "topologies/mesh/type", "unstructured")
                call catalyst_conduit_node_set_path_char8_str(mesh, "topologies/mesh/coordset", "coords")
                numNodesMin = minval(elementTypes, dim=1)
                numNodesMax = maxval(elementTypes, dim=1)
                if (numNodesMin == numNodesMax) then
                   if (numNodesMin == 3) then
                      allocate(elemConn(numOwnedElements*3))
                      call catalyst_conduit_node_set_path_char8_str(mesh, "topologies/mesh/elements/shape", "tri")
                   else if (numNodesMin == 4) then
                      allocate(elemConn(numOwnedElements*4))
                      call catalyst_conduit_node_set_path_char8_str(mesh, "topologies/mesh/elements/shape", "quad")
                   else
                      write(message, fmt='(A,I,A)') trim(subname)//": Failed to execute Catalyst: "// &
                         "only tri and quad are supported as element shape. the given mesh has ", &
                         numNodesMin, "nodes in each element."
                      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
                      rc = ESMF_FAILURE
                      return
                   end if
                else
                   ! call catalyst_conduit_node_set_path_char8_str(mesh, "topologies/mesh/elements/shape", "mixed")
                   call ESMF_LogWrite(trim(subname)//": Failed to execute Catalyst: mixed shape elements are not supported.", ESMF_LOGMSG_ERROR)
                   rc = ESMF_FAILURE
                   return
                end if

                call catalyst_conduit_node_set_path_external_int32_ptr(mesh, "topologies/mesh/elements/connectivity", &
                   elemConn, int8(size(elemConn, dim=1)))

                ! Create node for fields
                fields = catalyst_conduit_node_fetch(mesh, "fields")
             end if

             ! Query field pointer
             call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Add fields
             if (size(farrayPtr, dim=1) == numOwnedElements) then
                dataSize = numOwnedElements
                call catalyst_conduit_node_set_path_char8_str(fields, trim(itemNameList(n))//"/association", "element")
             else
                dataSize = numOwnedNodes
                call catalyst_conduit_node_set_path_char8_str(fields, trim(itemNameList(n))//"/association", "vertex")
             end if
             call catalyst_conduit_node_set_path_char8_str(fields, trim(itemNameList(n))//"/topology", "mesh")
             call catalyst_conduit_node_set_path_char8_str(fields, trim(itemNameList(n))//"/volume_dependent", "false")
             call catalyst_conduit_node_set_path_external_float64_ptr(fields, &
                trim(itemNameList(n))//"/values", farrayPtr, int8(dataSize))
          end if
       end do
    end if

    call ESMF_LogWrite(subname//' done for '//trim(compName), ESMF_LOGMSG_INFO)

  end subroutine StateToChannel

end module cop_phases_catalyst
