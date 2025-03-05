module geogate_phases_python

  !-----------------------------------------------------------------------------
  ! Phase for Python interaction 
  !-----------------------------------------------------------------------------

  use ESMF , only: operator(==), operator(/=)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompGetInternalState
  use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_VMBarrier
  use ESMF, only: ESMF_VMAllGatherV, ESMF_VMGatherV, ESMF_VMAllReduce
  use ESMF, only: ESMF_Time, ESMF_TimeGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet
  use ESMF, only: ESMF_LogFoundError, ESMF_FAILURE, ESMF_LogWrite
  use ESMF, only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_GeomType_Flag, ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_FieldGather
  use ESMF, only: ESMF_FieldWrite, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_FieldBundle, ESMF_FieldBundleCreate
  use ESMF, only: ESMF_MAXSTR, ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_STATE
  use ESMF, only: ESMF_Mesh, ESMF_MeshGet, ESMF_MeshWrite, ESMF_STATEITEM_FIELD
  use ESMF, only: ESMF_KIND_R8, ESMF_REDUCE_SUM
  use ESMF, only: ESMF_CoordSys_Flag, ESMF_COORDSYS_SPH_DEG

  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC_Model, only: NUOPC_ModelGet

  use, intrinsic :: iso_c_binding, only : C_PTR
  use conduit

  use geogate_share, only: ChkErr
  use geogate_share, only: CONST_RAD2DEG
  use geogate_internalstate, only: InternalState
  use geogate_python_interface, only: conduit_fort_to_py

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: geogate_phases_python_run

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  type(C_PTR) :: cnode
  logical :: allGatherToRoot = .false.
  character(ESMF_MAXSTR) :: scriptName


  character(len=*), parameter :: modName = "(geogate_phases_python)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine geogate_phases_python_run(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer, save :: timeStep = 0
    integer :: n, mpiComm, localPet, petCount
    type(InternalState) :: is_local
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState
    logical :: isPresent, isSet
    logical, save :: first_time = .true.
    character(len=ESMF_MAXSTR) :: timeStr
    character(ESMF_MAXSTR) :: cvalue
    character(ESMF_MAXSTR) :: message
    character(len=*), parameter :: subname = trim(modName)//':(geogate_phases_python_run) '
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

    ! Initialize
    if (first_time) then
       ! Query plugin specific attributes
       ! Python script/s
       call NUOPC_CompAttributeGet(gcomp, name="PythonScript", value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (isPresent .and. isSet) then
          scriptName = trim(cvalue)
          call ESMF_LogWrite(trim(subname)//": PythonScript = "//trim(scriptName), ESMF_LOGMSG_INFO)
       endif

       ! Gather
       call NUOPC_CompAttributeGet(gcomp, name="AllGatherToRoot", value=cvalue, &
         isPresent=isPresent, isSet=isSet, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (isPresent .and. isSet) then
          if (trim(cvalue) .eq. '.true.' .or. trim(cvalue) .eq. 'true') allGatherToRoot = .true.
       end if
       write(message, fmt='(A,L)') trim(subname)//': AllGatherToRoot = ', allGatherToRoot
       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

       ! Create Conduit node
       cnode = conduit_node_create()

       ! Set flag
       first_time = .false.
    end if

    ! Add time information
    call conduit_node_set_path_int32(cnode, 'core/time_step', timeStep)
    call conduit_node_set_path_char8_str(cnode, 'core/time_str', trim(timeStr)//char(0))

    ! Add MPI related information
    if (.not. allGatherToRoot) then
       call conduit_node_set_path_int32(cnode, "mpi/comm", mpiComm)
       call conduit_node_set_path_int32(cnode, "mpi/localPet", localPet)
       call conduit_node_set_path_int32(cnode, "mpi/petCount", petCount)
    end if

    ! Loop over states
    do n = 1, is_local%wrap%numComp
       ! Add content of state to Conduit node
       call StateToNode(is_local%wrap%NStateImp(n), vm, localPet, petCount, trim(is_local%wrap%compName(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Pass node to Python
    if (allGatherToRoot) then
       if (localPet == 0) then
          call conduit_fort_to_py(cnode, trim(scriptName)//char(0))
       end if
       ! PETs wait for root
       call ESMF_VMBarrier(vm, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call conduit_fort_to_py(cnode, trim(scriptName)//char(0))
    end if

    ! Reset node
    call conduit_node_reset(cnode)

    ! Increase time step
    timeStep = timeStep+1

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine geogate_phases_python_run

  !-----------------------------------------------------------------------------

  subroutine StateToNode(state, vm, localPet, petCount, compName, rc)
    use iso_c_binding
    implicit none

    ! input/output variables
    type(ESMF_State), intent(in) :: state
    type(ESMF_VM), intent(in) :: vm
    integer, intent(in) :: localPet
    integer, intent(in) :: petCount
    character(len=*), intent(in) :: compName
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Mesh) :: mesh
    type(ESMF_Field) :: field
    type(ESMF_CoordSys_Flag) :: coordSys
    integer :: i, n, m, rank, itemCount
    integer :: spatialDim, numOwnedElements
    integer :: numElements(1)
    integer, allocatable :: numOwnedElementsArr(:)
    integer, allocatable :: recvOffsets(:), recvCounts(:)
    real(ESMF_KIND_R8), allocatable   :: ownedElemCoords(:)
    real(ESMF_KIND_R8), allocatable :: ownedElemLats(:)
    real(ESMF_KIND_R8), allocatable :: ownedElemLons(:)
    real(ESMF_KIND_R8), allocatable :: elemLats(:)
    real(ESMF_KIND_R8), allocatable :: elemLons(:)
    real(ESMF_KIND_R8), allocatable :: farrayDst(:)
    real(ESMF_KIND_R8), pointer :: farrayPtr(:)
    character(ESMF_MAXSTR) :: message
    character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=*), parameter :: subname = trim(modName)//':(StateToNode) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called for '//trim(compName), ESMF_LOGMSG_INFO)

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
                ! Query field mesh
                call ESMF_FieldGet(field, mesh=mesh, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Extract coordinate information
                call ESMF_MeshGet(mesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Allocate data structures
                allocate(ownedElemLats(numOwnedElements))
                allocate(ownedElemLons(numOwnedElements))
                allocate(ownedElemCoords(spatialDim*numOwnedElements))

                ! Get element coordinates and fill arrays
                call ESMF_MeshGet(mesh, ownedElemCoords=ownedElemCoords, coordSys=coordSys, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                do m = 1, numOwnedElements
                   ownedElemLons(m) = ownedElemCoords(2*m-1)
                   ownedElemLats(m) = ownedElemCoords(2*m)
                end do

                ! Convert coordinates from radian to degree
                if (coordSys /= ESMF_COORDSYS_SPH_DEG) then
                   ownedElemLons = ownedElemLons*CONST_RAD2DEG
                   ownedElemLats = ownedElemLats*CONST_RAD2DEG
                end if

                ! Check the flag to gather data on PET 0
                if (allGatherToRoot) then
                   ! Find total number of elements
                   call ESMF_VMAllReduce(vm, (/ numOwnedElements /), numElements, 1, ESMF_REDUCE_SUM, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Collect numOwnedElements of each PET in a array
                   allocate(numOwnedElementsArr(petCount))
                   numOwnedElementsArr = 0
                   call ESMF_VMAllGatherV(vm, sendData=(/ numOwnedElements /), sendCount=1, &
                     recvData=numOwnedElementsArr, recvCounts=(/ (1, n = 0, petCount-1) /), &
                     recvOffsets=(/ (n, n = 0, petCount-1) /), rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Create recvOffsets array
                   allocate(recvOffsets(petCount))
                   recvOffsets = 0
                   recvOffsets = (/ (sum(numOwnedElementsArr(1:n))-numOwnedElementsArr(1), n=1, petCount) /)

                   ! Create recvCounts array
                   allocate(recvCounts(petCount))
                   recvCounts = (/ (numOwnedElementsArr(n), n=1, petCount) /)

                   ! Print out numOwnedElements, recvOffsets
                   !do i = 1, petCount
                   !   write(message, fmt='(A,3I8)') trim(subname)//': numOwnElem, Offset, Counts = ', &
                   !     numOwnedElementsArr(i), recvOffsets(i), recvCounts(i)
                   !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
                   !end do

                   ! Allocate arrays that will be used to gather data
                   if (localPet == 0) then
                      allocate(farrayDst(numElements(1)))
                      allocate(elemLons(numElements(1)))
                      allocate(elemLats(numElements(1)))
                   else
                      allocate(farrayDst(0))
                      allocate(elemLons(0))
                      allocate(elemLats(0))
                   end if

                   ! Gather coordinate data
                   call ESMF_VMGatherV(vm, ownedElemLons, numOwnedElements, elemLons, recvCounts, recvOffsets, 0, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return
                   call ESMF_VMGatherV(vm, ownedElemLats, numOwnedElements, elemLats, recvCounts, recvOffsets, 0, rc=rc)
                   if (ChkErr(rc,__LINE__,u_FILE_u)) return

                   ! Add coordinate information to node
                   call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lon', elemLons, int8(numElements(1)))
                   call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lat', elemLats, int8(numElements(1)))

                   ! Clean memory
                   deallocate(numOwnedElementsArr)
                   deallocate(recvCounts)
                   deallocate(recvOffsets)
                   deallocate(elemLons)
                   deallocate(elemLats)

                ! There is no need to gather data
                else
                   ! Add coordinate information to node
                   call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lon', ownedElemLons, int8(numOwnedElements))
                   call conduit_node_set_path_float64_ptr(cnode, trim(compName)//'/lat', ownedElemLats, int8(numOwnedElements))
                end if

                ! Clean memory
                deallocate(ownedElemCoords)
                deallocate(ownedElemLons)
                deallocate(ownedElemLats)
             end if

             ! Query field pointer
             call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return

             ! Add field data to node
             if (allGatherToRoot) then
                ! Gather field data
                call ESMF_FieldGather(field, farrayDst, rootPet=0, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Add to node
                call conduit_node_set_path_float64_ptr(cnode, &
                  trim(compName)//'/'//trim(itemNameList(n)), farrayDst, int8(numElements(1)))
             else
                ! Add to node
                call conduit_node_set_path_float64_ptr(cnode, &
                   trim(compName)//'/'//trim(itemNameList(n)), farrayPtr, int8(numOwnedElements))
             end if

             ! Disassociate pointer
             nullify(farrayPtr)

          end if ! itemTypeList
       end do

       ! Clean memory
       if (allGatherToRoot) deallocate(farrayDst)
       deallocate(itemNameList)
       deallocate(itemTypeList)

    end if ! itemCount

    call ESMF_LogWrite(subname//' done for '//trim(compName), ESMF_LOGMSG_INFO)

  end subroutine StateToNode

end module geogate_phases_python
