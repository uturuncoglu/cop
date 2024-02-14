module cop_comp_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for generic co-processing component (COP)
  !-----------------------------------------------------------------------------

  use ESMF , only : operator(==)
  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_Field, ESMF_FieldGet
  use ESMF, only: ESMF_State, ESMF_StateGet
  use ESMF, only: ESMF_Clock, ESMF_LogWrite
  use ESMF, only: ESMF_GridCompSetEntryPoint
  use ESMF, only: ESMF_FAILURE, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_METHOD_INITIALIZE, ESMF_MAXSTR
  use ESMF, only: ESMF_GEOMTYPE_GRID, ESMF_GEOMTYPE_MESH
  use ESMF, only: ESMF_FIELDSTATUS_GRIDSET, ESMF_FIELDSTATUS_EMPTY, ESMF_FIELDSTATUS_COMPLETE 
  use ESMF, only: ESMF_GeomType_Flag, ESMF_FieldStatus_Flag

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompFilterPhaseMap, NUOPC_CompSetEntryPoint
  use NUOPC, only: NUOPC_SetAttribute

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices
  use NUOPC_Model, only: label_Advertise
  use NUOPC_Model, only: label_ModifyAdvertised
  use NUOPC_Model, only: label_RealizeProvided
  use NUOPC_Model, only: label_Advance

  !use catalyst_api
  !use catalyst_conduit
  !use conduit
  !use conduit_relay

  use cop_comp_shr, only: ChkErr

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

  private :: InitializeP0

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
    ! switching to IPD versions
    !------------------

    !call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, userRoutine=InitializeP0, phase=0, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! specialize model
    !------------------
     
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advertise, specRoutine=Advertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertised, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_RealizeProvided, specRoutine=RealizeProvided, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, specRoutine=Advance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv05 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv05p2"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeP0

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
    integer :: n
    integer :: fieldCount
    type(ESMF_Field) :: field
    type(ESMF_State) :: importState, exportState
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(len=*), parameter :: subname = trim(modName)//':(ModifyAdvertised) '
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

    if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)    
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       ! Print out mirrored field name
       call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

       ! Get field from import state
       call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Overwrite "ProducerTransferOffer" attribute, default is "cannot provide"
       call NUOPC_SetAttribute(field, name="ProducerTransferOffer", value="will provide", rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Overwrite "SharePolicyGeomObject" attribute, default is "not share"
       call NUOPC_SetAttribute(field, name="SharePolicyGeomObject", value="share", rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Overwrite "SharePolicyField" attribute, default is "not share"
       call NUOPC_SetAttribute(field, name="SharePolicyField", value="share", rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Clean memory
    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
    
  end subroutine ModifyAdvertised

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer :: n
    integer :: fieldCount
    type(ESMF_Field) :: field
    type(ESMF_GeomType_Flag) :: geomType
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_State) :: importState, exportState
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    character(len=*), parameter :: subname = trim(modName)//':(RealizeProvided) '
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
    end do

    ! Clean memory
    if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
 
  end subroutine RealizeProvided

  !-----------------------------------------------------------------------------

  subroutine Advance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(Advance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    !------------------

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine Advance

end module cop_comp_nuopc
