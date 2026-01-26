!> @brief NEXUS NUOPC Component.
!> @details Modularized NUOPC component with phase-aware initialization
!> to resolve clock dependency issues. Major refactoring from monolithic
!> structure to maintainable modular design.
module nexus_cap

  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS => SetServices

  ! HEMCO core modules
  use HCOI_NUOPC_MOD, only: HCO_SetServices_NUOPC, HCO_SetExtState_NUOPC, HCO_UpdateExportFields_NUOPC
  use HCO_Config_Mod,  only: Config_ReadFile
  use HCO_Driver_Mod,  only: HCO_Init
  use HCOX_Driver_Mod, only: HCOX_Init
  use HCO_STATE_MOD, only: Hco_State, HcoState_Init, HcoState_Final, HCO_GetHcoID
  use HCO_ARR_MOD,   only: HCO_ArrInit, HCO_ArrAssert
  use HCO_TYPES_MOD, only: ConfigObj
  use HCO_Error_Mod, only: rk_hco => hp, &
    HcoErr, &
    HCO_LogFile_Open, &
    HCO_Error, HCO_MSG, HCO_Leave, HCO_Enter, &
    HCO_SUCCESS, HCO_MISSVAL
  use HCOX_STATE_MOD, only: Ext_State, ExtStateInit
  use HCOX_Driver_Mod, only: HCOX_Init, HCOX_Run, HCOX_Final
  use HCO_DIAGN_MOD, only: Diagn_Create, DiagnCollection_Get
  
  ! NEXUS modular components
  use nexus_grid_mod, only: nxs_set_grid, nxs_set_hco_grid, nxs_create_hco_grid, &
                            nxs_create_hco_grid_static, set_1d_coord
  use nexus_config_mod, only: nxs_init, nxs_finalize, nxs_read_time_config, parse_date
  use nexus_state_mod, only: nxs_diag_state_init_disabled, nxs_diag_state_update, &
                             nxs_expt_state_init, nxs_expt_state_update, &
                             nxs_state_finalize, nxs_create_hemco_diagnostics
  use nexus_initialize_mod, only: nexus_initialize_phase_aware
  use nexus_io_mod, only: IO_Init, IO_Read, IO_Write, ResolveFileName, &
                          InitializeFieldDataRegistry, nexus_register_field, &
                          PopulateImportFromRegistry
  use nexus_species_mod, only: NEXUS_RegisterSpecies

  implicit none

  ! TODO: cap object with pointers that can be retrieved with ESMF_GridCompGetInternalState?

  ! Default values for HEMCO input files: contain definitions of
  ! species, grid, and time settings, etc.
  character(len=255) :: GridFile = 'HEMCO_sa_Grid'
  character(len=255) :: SpecFile = 'HEMCO_sa_Spec.rc'
  character(len=255) :: TimeFile = 'HEMCO_sa_Time'
  character(len=255) :: DiagFile = 'NEXUS_Diag.nc'
  character(len=255) :: ExptFile = 'NEXUS_Expt.nc'
  character(len=255) :: ConfigFile_
  character(len=255) :: ReGridFile_
  character(len=255) :: OutputFile_
  integer            :: debugLevel_
  logical            :: writeRestart_

  !> HEMCO config object
  type(ConfigObj), pointer :: HcoConfig => NULL()

  !> HEMCO state object
  type(Hco_State), pointer :: HcoState => NULL()

  !> HEMCO extensions state
  type(Ext_State), pointer :: HcoExtState => NULL()

  !> Flag to track if HEMCO diagnostics have been created
  logical, save :: diagnostics_created = .false.

  type(ESMF_Grid)  :: HCO_Grid
  type(ESMF_Grid)  :: NXS_Grid
  type(ESMF_State) :: NXS_Diag_State
  !! "importState"
  !! An ESMF state of diagnostics on the HEMCO grid.
  type(ESMF_State) :: NXS_Expt_State
  !! "exportState"
  !! Regridded to the desired output grid.
  type(ESMF_RouteHandle) :: NXS_RouteHandle

  logical :: do_Debug  = .false.
  !! True if `debugLevel` passed to `init` is greater than zero.
  logical :: alwaysWriteRestartFile = .false.
  !! Even in NEXUS mode

  ! Start and end time of simulation
  integer :: T_YY(2), T_MM(2), T_DD(2)
  integer :: T_H(2), T_M(2), T_S(2)

  !> MAXIT is the maximum number of run calls allowed
  integer, parameter :: MAXIT = 100000

  integer, parameter :: rootPet = 0

  integer :: localPet = 0
  integer :: petCount = 1

  type(ESMF_StaggerLoc), parameter :: staggerList(2) = &
    (/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /)

  private

  public SetServices, &
    nxs_init, nxs_finalize, &
    T_YY, T_MM, T_DD, T_H, T_M, T_S, &
    HcoState

contains

  !-----------------------------------------------------------------------------
  ! NUOPC routines

  !> @brief Sets services for the NEXUS component.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Local variables
    integer :: localrc
    type(ESMF_VM) :: vm
    integer :: localPet, rootPet
    ! No import/export state creation here; handled in Initialize if needed

    rc = ESMF_SUCCESS

    ! Get local PET for printing
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_VMGet(vm, localPet=localPet, petCount=rootPet, rc=rc)  ! petCount to get total, rootPet = 0
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    rootPet = 0

    ! Derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set required NUOPC Instance attribute to avoid IPDvXp07 errors
    call NUOPC_CompAttributeSet(model, name='/NUOPC/Instance/StandardName', value='NEXUS', rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! NUOPC interface will handle configuration reading
    if (localPet == rootPet) print *, "NEXUS: HEMCO configuration will be read by NUOPC interface"

    ! HcoState creation and species registration will be done in Initialize phase

    ! Set GridComp on HcoState
    ! HcoState%GridComp => model

    ! Initialize HEMCO core
    ! MOVED TO Initialize phase because we need the grid!
    ! call HCO_Init( HcoState, rc )
    ! if (nxs_error_log(rc, msg='Error encountered in routine "HCO_Init"!', &
    !   line=__LINE__, &
    !   file=__FILE__, &
    !   rcToReturn=rc)) return

    ! Register SetClock to be called
    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Specialize model
    if (localPet == rootPet) print *, "NEXUS: Specialize Advertise"
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localPet == rootPet) print *, "NEXUS: Specialize Realize"
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localPet == rootPet) print *, "NEXUS: Specialize Advance"
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! We use the standard Initialize phase
    if (localPet == rootPet) print *, "NEXUS: Specialize DataInitialize"
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localPet == rootPet) print *, "NEXUS: SetServices Done"

  end subroutine

  !> @brief Advertises fields to the NUOPC driver.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine Advertise(model, rc)
    use HCO_Diagn_Mod, only: DiagnFileOpen, DiagnFileGetNext, DiagnFileClose

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_State) :: importState, exportState
    integer :: localrc
    integer :: lun
    logical :: eof
    character(len=63) :: cName, spcName, outUnit
    character(len=127) :: lName, unitName
    integer :: extNr, cat, hier, spaceDim

    ! Note: Advertise doesn't have localPet defined, need to get it or assume root calls it?
    ! Actually, Advertise is a NUOPC entry point, called by all PETs?
    ! We should check if we can get localPet.
    ! But wait, `Advertise` subroutine doesn't have `localPet` variable.
    ! We should add it.

    ! Adding localPet logic
    type(ESMF_VM) :: vm
    integer :: localPet

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (localPet == 0) print *, "NEXUS: Entering Advertise"
    rc = ESMF_SUCCESS

    ! Query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set HEMCO services - this must be done here where we have access to import/export states
    if (localPet == 0) print *, "NEXUS: Calling HCO_SetServices_NUOPC in Advertise with config file: ", trim(ConfigFile_)
    call HCO_SetServices_NUOPC( (localPet == 0), model, HcoConfig, &
      trim(ConfigFile_), importState, exportState, localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) then
      rc = localrc
      return  ! bail out
    endif

    ! Advertise NEXUS output variables by reading DiagnFile directly
    ! (Avoiding HCO_Init dependency here)
    if (.not. associated(HcoConfig)) then
       if (localPet == 0) print *, "NEXUS: HcoConfig is NOT associated in Advertise!"
       rc = ESMF_FAILURE
       return
    endif

    if (localPet == 0) print *, "NEXUS: Calling DiagnFileOpen"
    call DiagnFileOpen( HcoConfig, lun, localrc )
    if (nxs_error_log(localrc, msg='Error opening diagnostics file', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) print *, "NEXUS: DiagnFileOpen returned lun=", lun
    if (lun > 0) then
       do
          call DiagnFileGetNext( HcoConfig, lun, cName, spcName, extNr, cat, &
                                 hier, spaceDim, outUnit, eof, localrc, &
                                 lName=lName, unitName=unitName )
          if (localrc /= HCO_SUCCESS) then
             if (localPet == 0) print *, "NEXUS: DiagnFileGetNext failed"
             exit
          endif
          if (eof) then
             if (localPet == 0) print *, "NEXUS: DiagnFileGetNext EOF"
             exit
          endif

          if (localPet == 0) print "('NEXUS: Advertising ''', a, ''' (long_name=''', a, ''', units=''', a, ''')')", &
             trim(cName), trim(lName), trim(outUnit)

          ! First add field to NUOPC field dictionary
          call NUOPC_FieldDictionaryAddEntry(trim(cName), trim(outUnit), rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          ! Now advertise field using NUOPC_Advertise
          call NUOPC_Advertise(exportState, StandardName=trim(cName), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          if (localPet == 0) then
            print *, "NEXUS: Export field advertised: ", trim(cName)
          endif
       end do
       call DiagnFileClose(lun)
    endif

    ! Advertise STREAM:VARIABLE import fields for HEMCO coupling
    ! TODO: Only advertise when actually coupled - for now advertise all
    if (localPet == 0) print *, "NEXUS: Skipping STREAM:VARIABLE field advertising (standalone mode)"
    ! call AdvertiseStreamVariableImportFields(importState, localPet, rc)
    ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) print *, "NEXUS: Exiting Advertise"

  end subroutine

  !> @brief Realizes fields for the NUOPC driver.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    ! type(ESMF_Grid)         :: gridIn
    ! type(ESMF_Grid)         :: gridOut
    integer :: item, itemCount, localrc, stat
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),  allocatable :: itemTypeList(:)

    ! Adding localPet logic
    type(ESMF_VM) :: vm
    integer :: localPet
    type(ESMF_Clock) :: clock

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    rc = ESMF_SUCCESS

    ! Create grid for NUOPC - use static grid creation without clock dependency
    if (localPet == 0) print *, "NEXUS DEBUG: Realize - Creating HCO_Grid before field realization"
    call nxs_create_hco_grid_static( ConfigFile_, HCO_Grid, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Set grid on component so it's available for field creation
    if (localPet == 0) print *, "NEXUS DEBUG: Realize - Setting HCO_Grid on model component"
    call ESMF_GridCompSet(model, grid=HCO_Grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (localPet == 0) print *, "NEXUS DEBUG: Realize - HCO_Grid set on model component"

    ! Query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Note: grids and fields were already created as part of the `init` routine
    call ESMF_StateGet(exportState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet(exportState, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    do item = 1, itemCount
      if (itemTypeList(item) /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, itemNameList(item), field, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      if (localPet == 0) print "('NEXUS: Realizing export ''', a, '''')", trim(itemNameList(item))
      call NUOPC_Realize(exportState, field=field, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

    end do

    ! Also realize import fields so they can be populated by CDEPS data
    deallocate(itemNameList, itemTypeList)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (itemCount > 0) then
      allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Unable to allocate memory for import fields", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call ESMF_StateGet(importState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      do item = 1, itemCount
        if (itemTypeList(item) /= ESMF_STATEITEM_FIELD) cycle

        call ESMF_StateGet(importState, itemNameList(item), field, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (localPet == 0) print "('NEXUS: Realizing import ''', a, '''')", trim(itemNameList(item))
        call NUOPC_Realize(importState, field=field, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

      end do
    endif

    ! Create STREAM:VARIABLE import fields for HEMCO NUOPC coupling
    ! Now that grid is set on component, this will work properly
    call CreateStreamVariableImportFields(model, importState, localPet, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine

  !> @brief Advances the model by one timestep.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine Advance(model, rc)
    use HCO_Clock_Mod,   only : HcoClock_Set
    use HCO_FluxArr_Mod, only : HCO_FluxarrReset
    use HCO_Driver_Mod,  only : HCO_RUN
    use HCOX_Driver_Mod, only : HCOX_RUN
    use HCO_Diagn_Mod,   only : HcoDiagn_AutoUpdate

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: time
    type(ESMF_State)      :: importState, exportState
    character(len=160)    :: msgString
    integer(ESMF_KIND_I8) :: advanceCount
    integer               :: yy, mm, dd, h, m, s
    character(len=255)    :: msg
    integer               :: localrc
    integer               :: timeSlice
    integer               :: n
    type(ESMF_VM)         :: vm

    timeSlice = 0
    rc = ESMF_SUCCESS

    ! Query for clock, importState and exportState
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (localPet == 0) then
        call ESMF_LogWrite("NEXUS DEBUG: Entered Advance", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush(rc=localrc)
        print *, "NEXUS DEBUG: Advance - Getting clock and states"
    endif
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Note: In NUOPC mode, HEMCO gets data from IMPORT state
    ! Data is provided through our IO_Read mechanism which populates import state
    if (localPet == 0) print *, "NEXUS DEBUG: Advance - Reading data to populate IMPORT state (NUOPC mode)"

    ! Read external data into importState
    if (localPet == 0) then
        call ESMF_LogWrite("NEXUS DEBUG: Calling IO_Read", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush(rc=localrc)
        print *, "NEXUS DEBUG: Advance - About to call IO_Read"
    endif
    call IO_Read(importState, clock, rc=localrc)
    if (localPet == 0) then
        call ESMF_LogWrite("NEXUS DEBUG: IO_Read done", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush(rc=localrc)
    endif
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return


    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Get some Clock info
    call ESMF_ClockGet(clock, advanceCount=advanceCount, currTime=time)

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (localPet == 0) print "('NEXUS: ESMF Clock advanceCount: ', i0)", advanceCount
    timeSlice = advanceCount + 1

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="---->Advancing Model from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set HEMCO clock based on ESMF clock
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    call HcoClock_Set(HcoState, &
      yy, mm, dd, h, m, s, &
      IsEmisTime=.TRUE., RC=localrc)
    if (nxs_error_log(localrc, msg='Error encountered in routine "HcoClock_Set"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    write(msg, &
      "('Calculate emissions at ', i0.4, '-', i0.2, '-', i0.2, ' ', i2.2, ':', i0.2, ':', i0.2)") &
      yy, mm, dd, h, m, s
    call ESMF_LogWrite(msg)
    if (localPet == 0) print "('NEXUS: ', a)", trim(msg)

    ! ================================================================
    ! Arrays will be allocated by HEMCO during emission calculation
    ! Skip flux reset - arrays don't exist yet
    ! ================================================================

    ! ================================================================
    ! Set HCO options and define all arrays needed by core module
    ! and the extensions
    ! ================================================================

    ! Range of tracers and emission categories.
    ! Set Extension number ExtNr to 0, indicating that the core
    ! module shall be executed.
    HcoState%Options%SpcMin = 1
    HcoState%Options%SpcMax = -1  ! all species above or equal to SpcMin are considered
    HcoState%Options%CatMin = 1
    HcoState%Options%CatMax = -1
    HcoState%Options%ExtNr  = 0

    ! Use temporary array?
    HcoState%Options%FillBuffer = .FALSE.

    ! ================================================================
    ! Run HCO core module
    ! Emissions will be written into the corresponding flux arrays
    ! in HcoState.
    !
    ! NOTE: Call HCO_Run explicitly twice, once for phase 1 and
    ! once for phase 2.  This will ensure emissions get computed.
    ! (bmy, 1/29/18)
    ! ================================================================

    ! Phase 1: Update reading data fields etc.
    if (localPet == 0) print *, "NEXUS DEBUG: About to call HCO_Run phase 1"
    call HCO_Run( HcoState, 1, localrc )
    if (localPet == 0) print *, "NEXUS DEBUG: HCO_Run phase 1 returned, rc=", localrc
    if (nxs_error_log(localrc, msg='Error encountered in routine "Hco_Run", phase 1!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Phase 2: Compute emissions (skip for dry-run)
    if (localPet == 0) print *, "NEXUS DEBUG: About to call HCO_Run phase 2"
    call HCO_Run( HcoState, 2, localrc )
    if (localPet == 0) print *, "NEXUS DEBUG: HCO_Run phase 2 returned, rc=", localrc
    if (nxs_error_log(localrc, msg='Error encountered in routine "Hco_Run", phase 2!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Check if species are now available after HCO_Run
    if (localPet == 0) then
       if (associated(HcoState%Spc)) then
          print *, "NEXUS DEBUG: After HCO_Run, HcoState%Spc is associated, nSpc =", HcoState%nSpc
          if (HcoState%nSpc > 0) then
             print *, "NEXUS DEBUG: First species name =", trim(HcoState%Spc(1)%SpcName)
          endif
       else
          print *, "NEXUS DEBUG: After HCO_Run, HcoState%Spc is STILL NOT associated"
       endif
    endif

    ! ================================================================
    ! Run HCO extensions
    ! ================================================================

    ! Set ExtState fields from ESMF import state
    if (localPet == 0) print *, "NEXUS DEBUG: Advance - About to call HCO_SetExtState_NUOPC"
    call HCO_SetExtState_NUOPC( HcoState, HcoExtState, localrc )
    if (localPet == 0) print *, "NEXUS DEBUG: Advance - HCO_SetExtState_NUOPC returned, rc=", localrc
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCO_SetExtState_NUOPC"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Execute all enabled emission extensions. Emissions will be
    ! added to corresponding flux arrays in HcoState.
    if (localPet == 0) print *, "NEXUS DEBUG: Advance - About to call HCOX_Run"
    call HCOX_Run ( HcoState, HcoExtState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCOX_Run"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! ================================================================
    ! Reset emission arrays for next time step (now that they exist)
    ! ================================================================
    call HCO_FluxArrReset( HcoState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCO_FluxArrReset"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=================================================================
    ! Update all autofill diagnostics (skip for dry-run)
    !=================================================================
    call HcoDiagn_AutoUpdate ( HcoState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCOX_AutoUpdate"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=================================================================
    ! Update NEXUS Diagnostic state (using export state)
    !=================================================================
    ! Transfer HEMCO diagnostic data to export fields
    call HCO_UpdateExportFields_NUOPC(HcoState, exportState, localrc)
    if (nxs_error_log(localrc, msg='Error transferring HEMCO diagnostics to export fields!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (localPet == 0) then
        call ESMF_LogWrite("NEXUS DEBUG: Diagnostics updated via export state", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush(rc=localrc)
    endif

    !=================================================================
    ! Write output via I/O layer
    !=================================================================
    call IO_Write(exportState, clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out


  end subroutine

  !> @brief Set the clock for the model.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    type(ESMF_Clock) :: clock
    integer :: localrc

    rc = ESMF_SUCCESS

    ! Get the clock from the model
    call ESMF_GridCompGet(model, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Set logical flags from stored command-line args
    ! These flags are used in Initialize, but we can set them here too if needed
    ! or just ensure they are available via module variables.
    ! They are already module variables, so they persist.

    do_Debug  = (debugLevel_ > 0)
    alwaysWriteRestartFile = writeRestart_
    if (len_trim(OutputFile_) > 0) ExptFile = OutputFile_

    ! Create the grid here because we need it for Initialize
    ! But wait, SetClock is usually for setting the clock.
    ! However, NUOPC phases are:
    ! 1. Advertise
    ! 2. Realize
    ! 3. DataInitialize

    ! If we use standard phases, DataInitialize calls Initialize.
    ! But we need the grid BEFORE Realize if we want to advertise fields on the grid?
    ! Actually, Advertise is for advertising fields.
    ! Realize is for creating fields.
    ! DataInitialize is for initializing data.

    ! The issue "State object is invalid! Not created or deleted!" suggests
    ! that we might be trying to access states that are not ready or
    ! we are not correctly initializing things in the right order.

    ! Also, InitializeP0 is called by the driver.

    ! Let's look at Initialize again.
    ! It creates the grid and sets it on the component.
    ! It initializes HEMCO.

    ! The log shows:
    ! PET0     >>>driver: entered Initialize (phase=<none>) without valid internal Clock.
    ! ...
    ! PET0       >>>NEXUS: entered Initialize (phase=<none>) without valid internal Clock.

    ! This means Initialize is being called at phase 0 (InitializeP0).
    ! And at that point, the clock might not be valid yet?
    ! Or maybe it is valid but just not set on the component yet?

    ! In NUOPC, InitializeP0 (IPDv00) is usually empty or for very basic setup.
    ! IPDv01 is Advertise.
    ! IPDv02 is Realize.
    ! IPDv03 is DataInitialize.

    ! If we register Initialize for DataInitialize (IPDv03), it should be called then.
    ! But the log says "phase=<none>". This usually means IPDv00.

    ! Wait, if we use NUOPC_CompSpecialize for label_DataInitialize, it should be called at IPDv03.

    ! The error "State object is invalid" happens during Advertise?
    ! No, it happens during InitializeP0, InitializeIPDv02p1, etc.
    ! It seems like some internal state check is failing.

    ! One possibility is that we are not creating the states correctly or
    ! we are accessing them when they are not created.

    ! In Advertise:
    ! call NUOPC_ModelGet(model, importState=importState, exportState=exportState, rc=rc)
    ! ...
    ! call DiagnFileOpen( ... )

    ! If HcoConfig is not associated, we return.
    ! But HcoConfig is associated in SetServices.

    ! Let's check Initialize routine again.

  end subroutine SetClock

  !> @brief Phase-aware initialization that handles NUOPC clock dependencies
  !> @details This replaces the monolithic Initialize routine with a phase-aware
  !> approach that can handle different NUOPC initialization phases properly.
  !> The initialization is broken down into phases based on clock availability
  !> and NUOPC phase requirements.
  !> @param model The ESMF grid component
  !> @param rc Return code
  subroutine Initialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Call the new phase-aware initialization
    call nexus_initialize_phase_aware(model, rc)
    
    if ( rc /= HCO_SUCCESS ) then
       call HCO_ERROR('Error in phase-aware initialization', rc)
       return
    endif

    rc = HCO_SUCCESS

  end subroutine Initialize

  !-----------------------------------------------------------------------------
  ! Cap routines

  !> @brief NEXUS initialization.
  !>
  !> Read HEMCO config, initialize HEMCO state, create grid objects, etc.
  !>
  !> @param ConfigFile   Path to the configuration file.
  !> @param ReGridFile   Path to the regridding file.
  !> @param OutputFile   Path to the output file.
  !> @param debugLevel   Debug level.
  !> @param writeRestart Flag to write restart file.
  !> @param rc           Return code (optional).
  subroutine nxs_init(ConfigFile, ReGridFile, OutputFile, debugLevel, writeRestart, rc)
    character(len=*),  intent(in)  :: ConfigFile
    character(len=*),  intent(in)  :: ReGridFile
    character(len=*),  intent(in)  :: OutputFile
    integer,           intent(in)  :: debugLevel
    logical,           intent(in)  :: writeRestart
    integer, optional, intent(out) :: rc

    integer :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

        ConfigFile_   = ConfigFile
        ReGridFile_   = ReGridFile
        OutputFile_   = OutputFile
        debugLevel_   = debugLevel
        writeRestart_ = writeRestart

        ! Read time settings from configuration files
        call nxs_read_time_config(ConfigFile_, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

      end subroutine nxs_init
  !> @brief NEXUS finalization.
  !>
  !> Write last restart file, clean up HEMCO and grid objects, etc.
  !>
  !> @param rc Return code (optional).
  subroutine nxs_finalize( rc )

    use HCO_Clock_Mod,   only : HcoClock_Increase
    use HCO_Driver_Mod,  only : HCO_Final
    use HCOX_Driver_Mod, only : HCOX_Final
    use HCO_State_Mod,   only : HcoState_Final
    use HCOIO_DIAGN_MOD, only : HcoDiagn_Write
    use HCO_Diagn_Mod,   only : DiagnBundle_Cleanup

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    logical :: isCreated

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! Advance HEMCO clock to last timestamp
    call HcoClock_Increase ( HcoState, HcoState%TS_EMIS, .TRUE., RC=localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HcoClock_Increase"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (alwaysWriteRestartFile) then
      call HcoDiagn_Write( HcoState, .TRUE.,  localrc )
      if (nxs_error_log(localrc, msg='Error encountered in routine "HcoDiagn_Write"!', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end if

    ! Cleanup HCO core
    call HCO_FINAL( HcoState, .FALSE., localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCO_Final"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Cleanup extensions and ExtState object
    ! This will also nullify all pointer to the met fields.
    call HCOX_FINAL( HcoState, HcoExtState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCOX_Final"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Cleanup diagnostics (skip if dry-run)
    call DiagnBundle_Cleanup( HcoState%Diagn )

    ! Deallocate module arrays/pointers
    ! if ( allocated( XMID    ) ) deallocate ( XMID    )
    ! if ( allocated( YMID    ) ) deallocate ( YMID    )
    ! if ( allocated( XEDGE   ) ) deallocate ( XEDGE   )
    ! if ( allocated( YEDGE   ) ) deallocate ( YEDGE   )
    ! if ( allocated( YSIN    ) ) deallocate ( YSIN    )
    ! if ( allocated( AREA_M2 ) ) deallocate ( AREA_M2 )
    ! if ( allocated( PBL_M   ) ) deallocate ( PBL_M   )

    ! Cleanup HcoState object
    call HcoState_Final( HcoState )

    ! Cleanup NEXUS
    isCreated = ESMF_GridIsCreated(HCO_Grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call ESMF_GridDestroy(HCO_Grid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    isCreated = ESMF_GridIsCreated(NXS_Grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call ESMF_GridDestroy(NXS_Grid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    isCreated = ESMF_RouteHandleIsCreated(NXS_RouteHandle, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call ESMF_FieldRegridRelease(NXS_RouteHandle, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    isCreated = ESMF_StateIsCreated(NXS_Diag_State, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call nxs_state_finalize(NXS_Diag_State, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(NXS_Diag_State, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    isCreated = ESMF_StateIsCreated(NXS_Expt_State, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call nxs_state_finalize(NXS_Expt_State, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      call ESMF_StateDestroy(NXS_Expt_State, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

  end subroutine nxs_finalize




  !-----------------------------------------------------------------------------
  ! NEXUS methods

  !> @brief Sets up the NEXUS grid from a file.
  !>
  !> @param fileName Path to the grid file.
  !> @param clock    ESMF Clock for resolving filename.
  !> @param rc       Return code (optional).
  !> @return         The created ESMF grid.
  function nxs_set_grid( fileName, clock, rc ) result ( grid )

    use netcdf

    character(len=*),  intent(in)  :: fileName
    type(ESMF_Clock),  intent(in)  :: clock
    integer, optional, intent(out) :: rc

    type(ESMF_Grid) :: grid

    ! -- local variables
    integer :: localrc
    integer :: item, s
    integer :: ncid, dimid, varid
    integer :: ncerr
    integer :: dimLengths(2)
    integer :: lb(2), ub(2)
    real(ESMF_KIND_R8), pointer :: fp(:,:)
    character(len=255) :: resolvedFileName

    type(ESMF_VM) :: vm
    integer :: localPet

    character(len=*), parameter :: dimNames(2) = (/ "grid_xt", "grid_yt" /)
    character(len=*), parameter :: coordNames(2,2) = reshape( &
      (/ "grid_lont", "grid_latt", "grid_lon ", "grid_lat " /), &
      (/ 2,2 /) )

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ResolveFileName(fileName, clock, resolvedFileName, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ! We need localPet to restrict printing.
    ! Since this function doesn't have it, we can get it from VM?
    ! Or assume only root calls this?
    ! nxs_set_grid is called by Initialize, which is called by all PETs?
    ! In Initialize:
    ! if (do_Regrid) then
    !   NXS_Grid = nxs_set_grid( ReGridFile_, clock, rc=localrc )
    ! So all PETs call it.
    ! We should restrict printing.
    ! We can add type(ESMF_VM) :: vm, integer :: localPet

    ! (Wait, I can't add variables in the middle of declarations without being careful)
    ! I will add declarations at the top of the function

    ! Actually, I can just not print it or use a utility.
    ! But let's try to add localPet check if possible.
    ! For now, I will just suppress the print or check if I can get VM.

    ! print "('NEXUS: Input grid nc fn:', x, a)", trim(resolvedFileName)

    ! To do it properly:
    ! integer :: localPet
    ! type(ESMF_VM) :: vm
    ! call ESMF_VMGetCurrent(vm, rc=localrc)
    ! call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    ! if (localPet == 0) print ...

    ! But I need to insert variables declarations.
    ! I will modify the variable declaration part too.

    ncerr = nf90_open(trim(resolvedFileName), NF90_NOWRITE, ncid)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    do item = 1, 2
      ncerr = nf90_inq_dimid(ncid, dimNames(item), dimid)
      if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      ncerr = nf90_inquire_dimension(ncid, dimid, len=dimLengths(item))
      if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end do


    grid = ESMF_GridCreateNoPeriDim( &
      maxIndex = dimLengths,  &
      coordSys = ESMF_COORDSYS_SPH_DEG, &
      indexflag= ESMF_INDEX_GLOBAL, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ! -- add coordinates
    do s = 1, size(staggerList)
      call ESMF_GridAddCoord(grid, staggerloc=staggerList(s), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      do item = 1, 2
        nullify(fp)
        call ESMF_GridGetCoord(grid, item, staggerloc=staggerList(s), &
          localDE=0, farrayPtr=fp, computationalLBound=lb, &
          computationalUBound=ub, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        ncerr = nf90_inq_varid(ncid, coordNames(item,2), varid)
        if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        ncerr = nf90_get_var(ncid, varid, fp, start=lb, count=ub-lb+1)
        if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end do
    end do

    ! -- add area
    call ESMF_GridAddItem(grid, ESMF_GRIDITEM_AREA, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    nullify(fp)
    call ESMF_GridGetItem(grid, ESMF_GRIDITEM_AREA, &
      localDE=0, farrayPtr=fp, computationalLBound=lb, &
      computationalUBound=ub, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ncerr = nf90_inq_varid(ncid, "area", varid)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    ncerr = nf90_get_var(ncid, varid, fp, start=lb, count=ub-lb+1)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ncerr = nf90_close(ncid)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncerr, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end function nxs_set_grid

  !> @brief Initializes the diagnostics state.
  !>
  !> @param HcoGrid   The HEMCO grid.
  !> @param HcoState  The HEMCO state.
  !> @param DiagState The diagnostics state to initialize.
  !> @param rc        Return code (optional).
  ! NOTE: nxs_diag_state_init is disabled - using NUOPC export state instead
  ! All diagnostic functionality is handled by HCO_SetServices_NUOPC
  ! The entire subroutine is commented out below

#if 0
  !> @brief Disabled diagnostic state initialization (using export state instead)
  subroutine nxs_diag_state_init_disabled( HcoGrid, HcoState, DiagState, rc )

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    EOI = .false.
    nullify(thisDiagn)
    call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    do while (flag == HCO_SUCCESS)
      if (localPet == 0) print "('NEXUS: Initializing Diag variable ''', a, '''')", trim(thisDiagn%cName)

      ! Only process diagnostics with valid dimensions
      if (thisDiagn % spaceDim >= 2 .and. thisDiagn % spaceDim <= 3) then
        select case ( thisDiagn % spaceDim )
         case (2)
          field = ESMF_FieldCreate( HcoGrid, ESMF_TYPEKIND_R4, &
            name=thisDiagn % cName, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
         case (3)
          ! Check if 3D array is properly allocated before using bounds
          if (associated(thisDiagn % Arr3D) .and. associated(thisDiagn % Arr3D % Val)) then
            field = ESMF_FieldCreate( HcoGrid, ESMF_TYPEKIND_R4, &
              ungriddedLBound = (/ lbound(thisDiagn % Arr3D % Val, dim=3) /), &
              ungriddedUBound = (/ ubound(thisDiagn % Arr3D % Val, dim=3) /), &
              name=thisDiagn % cName, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
          else
            if (localPet == 0) print "('NEXUS: Skipping diagnostic variable ''', a, ''' - 3D array not allocated')", trim(thisDiagn%cName)
            call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
            if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return
            cycle  ! Skip to next diagnostic
          endif
        end select

        call ESMF_AttributeSet(field, name="LongName", value=trim(thisDiagn % long_name), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_AttributeSet(field, name="Units", value=trim(thisDiagn % OutUnit), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_AttributeSet(field, name="StandardName", value=trim(thisDiagn % cName), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_StateAdd( DiagState, (/ field /), rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      else
        if (localPet == 0) print "('NEXUS: Skipping diagnostic variable ''', a, ''' - invalid spaceDim: ', i0)", trim(thisDiagn%cName), thisDiagn % spaceDim
      endif

      call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
      if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

    ! Add additional debugging for StateReconcile
    if (localPet == 0) then
        call ESMF_LogWrite("NEXUS DEBUG: About to call ESMF_StateReconcile", ESMF_LOGMSG_INFO)
        call ESMF_LogFlush(rc=localrc)
    endif

    ! Only reconcile if we actually added fields to the state
    ! Check if the state has any items before reconciling
    call ESMF_StateGet(DiagState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (localPet == 0) then
        print *, "NEXUS DEBUG: DiagState itemCount = ", itemCount
    endif

    if (itemCount > 0) then
        call ESMF_StateReconcile( DiagState, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (localPet == 0) then
            call ESMF_LogWrite("NEXUS DEBUG: ESMF_StateReconcile completed successfully", ESMF_LOGMSG_INFO)
            call ESMF_LogFlush(rc=localrc)
        endif
    else
        if (localPet == 0) then
            call ESMF_LogWrite("NEXUS DEBUG: Skipping ESMF_StateReconcile - no items in state", ESMF_LOGMSG_INFO)
            call ESMF_LogFlush(rc=localrc)
        endif
    endif

  end subroutine nxs_diag_state_init_disabled
#endif

  !> @brief Updates the diagnostics state.
  !>
  !> @param HcoState  The HEMCO state.
  !> @param DiagState The diagnostics state to update.
  !> @param rc        Return code (optional).
  subroutine nxs_diag_state_update( HcoState, DiagState, rc )
    ! NOTE: Using NUOPC export state - diagnostics are automatically updated
    ! by HEMCO's NUOPC interface, no manual update needed

    type(HCO_State), pointer       :: HcoState
    type(ESMF_State)               :: DiagState
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! Diagnostics are automatically updated by HEMCO's NUOPC interface
    ! No manual copying needed when using export state mechanism
    localrc = ESMF_SUCCESS

  end subroutine nxs_diag_state_update

  !> @brief Initializes the export state (regridded diagnostics).
  !>
  !> @param grid        The destination grid.
  !> @param importState The import state (source).
  !> @param exportState The export state (destination).
  !> @param rc          Return code (optional).
  subroutine nxs_expt_state_init( grid, importState, exportState, rc )
    type(ESMF_Grid)                :: grid
    type(ESMF_State)               :: importState
    type(ESMF_State)               :: exportState
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, itemCount, rank
    integer :: srcTermProcessing
    integer :: stat
    integer :: lb(1), ub(1)
    type(ESMF_Field) :: srcfield, dstfield
    type(ESMF_TypeKind_Flag) :: typekind
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),  allocatable :: itemTypeList(:)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet( importState, itemCount=itemCount, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet( importState, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ! If a regridding routehandle is needed but not yet created, create it now.
    ! Use the first available field as a template for the source and destination
    ! fields required by the regrid store function. This avoids a messy check
    ! inside the main field creation loop.
    if (.not. ESMF_RouteHandleIsCreated(NXS_RouteHandle)) then
      do item = 1, itemCount
        if (itemTypeList(item) == ESMF_STATEITEM_FIELD) then
          ! Found a field, use it to create the regrid route handle
          call ESMF_StateGet(importState, itemNameList(item), srcfield, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          call ESMF_FieldGet(srcfield, rank=rank, typekind=typekind, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          ! Create a temporary destination field just for the regrid store call
          select case (rank)
          case (2)
            dstfield = ESMF_FieldCreate(grid, typekind, name=itemNameList(item)//'_temp_regrid', rc=localrc)
          case (3)
            call ESMF_FieldGet(srcfield, ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc)
            dstfield = ESMF_FieldCreate(grid, typekind, name=itemNameList(item)//'_temp_regrid', &
                                        ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc)
          end select
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          ! Precompute the regridding operation (i.e., weights)
          write(6,'(1x,"Precomputing regridding operation ...")')
          srcTermProcessing = 0
          call ESMF_FieldRegridStore(srcfield, dstfield, &
            regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
            srcTermProcessing=srcTermProcessing, &
            routehandle=NXS_RouteHandle, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          ! Destroy the temporary field
          call ESMF_FieldDestroy(dstfield, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) return

          ! Route handle is created, exit the search loop
          exit
        end if
      end do
    end if

    ! Create all destination fields and add them to the export state.
    do item = 1, itemCount
      if (itemTypeList(item) == ESMF_STATEITEM_FIELD) then
        if (localPet == 0) print "('NEXUS: Initializing Expt variable ''', a, '''')", trim(itemNameList(item))
        call ESMF_StateGet(importState, itemNameList(item), srcfield, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return

        call ESMF_FieldGet(srcfield, rank=rank, typekind=typekind, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return

        select case (rank)
        case (2)
          dstfield = ESMF_FieldCreate(grid, typekind, name=itemNameList(item), rc=localrc)
        case (3)
          call ESMF_FieldGet(srcfield, ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc)
          dstfield = ESMF_FieldCreate(grid, typekind, name=itemNameList(item), &
                                      ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc)
        end select

        ! Copy attributes from source field to destination field
        block
          character(len=ESMF_MAXSTR) :: longName, units, standardName
          call ESMF_AttributeGet(srcfield, "LongName", longName, rc=localrc)
          if (localrc == ESMF_SUCCESS) then
            call ESMF_AttributeSet(dstfield, "LongName", longName, rc=localrc)
          end if
          call ESMF_AttributeGet(srcfield, "Units", units, rc=localrc)
          if (localrc == ESMF_SUCCESS) then
            call ESMF_AttributeSet(dstfield, "Units", units, rc=localrc)
          end if
          call ESMF_AttributeGet(srcfield, "StandardName", standardName, rc=localrc)
          if (localrc == ESMF_SUCCESS) then
            call ESMF_AttributeSet(dstfield, "StandardName", standardName, rc=localrc)
          end if
        end block
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return

        call ESMF_StateAdd(exportState, (/ dstfield /), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return

      end if
    end do

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to deallocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_StateReconcile( exportState, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine nxs_expt_state_init

  !> @brief Updates the export state (performs regridding).
  !>
  !> @param importState The import state (source).
  !> @param exportState The export state (destination).
  !> @param rc          Return code (optional).
  subroutine nxs_expt_state_update( importState, exportState, rc )
    type(ESMF_State)               :: importState
    type(ESMF_State)               :: exportState
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, itemCount, rank
    integer :: stat
    integer :: lb(1), ub(1)
    type(ESMF_Field) :: srcfield, dstfield
    type(ESMF_TypeKind_Flag) :: typekind
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),  allocatable :: itemTypeList(:)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet( importState, itemCount=itemCount, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet( importState, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    do item = 1, itemCount
      if (itemTypeList(item) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet( importState, itemNameList(item), srcfield, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        call ESMF_StateGet( exportState, itemNameList(item), dstfield, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        call ESMF_FieldRegrid(srcField=srcfield, dstField=dstfield, &
          routehandle   = NXS_RouteHandle, &
          termorderflag = ESMF_TERMORDER_SRCSEQ, &
          rc = localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end if
    end do

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to deallocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine nxs_expt_state_update

  !> @brief Finalizes a state (destroys fields).
  !>
  !> @param state The state to finalize.
  !> @param rc    Return code (optional).
  subroutine nxs_state_finalize( state, rc )
    type(ESMF_State)               :: state
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, itemCount
    integer :: stat
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),  allocatable :: itemTypeList(:)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet( state, itemCount=itemCount, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_StateGet( state, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    do item = 1, itemCount
      if (itemTypeList(item) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet( state, itemNameList(item), field, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        call ESMF_FieldDestroy( field, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end if
    end do

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to deallocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine nxs_state_finalize

  !> @brief Helper function to check for errors and log them.
  !>
  !> If `rcToCheck` is not `HCO_SUCCESS`, log error message with ESMF
  !> and return.
  !>
  !> @param rcToCheck  The return code to check.
  !> @param msg        The error message (optional).
  !> @param line       The line number (optional).
  !> @param file       The file name (optional).
  !> @param rcToReturn The return code to set (optional).
  !> @return           True if an error occurred.
  logical function nxs_error_log(rcToCheck, msg, line, file, rcToReturn) result(not_ok)
    integer,                    intent(in)  :: rcToCheck
    character(len=*), optional, intent(in)  :: msg
    integer,          optional, intent(in)  :: line
    character(len=*), optional, intent(in)  :: file
    integer,          optional, intent(out) :: rcToReturn

    not_ok = (rcToCheck /= HCO_SUCCESS)

    if (not_ok) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg=msg, &
        line=line, file=file, rcToReturn=rcToReturn)
    else
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    end if

  end function nxs_error_log

  !> @brief Populates HcoState%Grid from ESMF Grid.
  subroutine nxs_set_hco_grid( HcoState, Grid, rc )
    type(HCO_State), intent(inout) :: HcoState
    type(ESMF_Grid), intent(in)    :: Grid
    integer,         intent(out)   :: rc

    integer :: localrc
    integer :: dimLengths(2)
    integer :: NX, NY
    real(ESMF_KIND_R8), pointer :: ptr_d(:,:)
    real(ESMF_KIND_R4), allocatable :: tmp_r4(:,:)

    rc = ESMF_SUCCESS

    ! Get grid dimensions from coords
    call ESMF_GridGetCoord(Grid, 1, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=ptr_d, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    NX = size(ptr_d, 1)
    NY = size(ptr_d, 2)
    HcoState%NX = NX
    HcoState%NY = NY
    HcoState%NZ = 1

    ! Allocate HEMCO grid arrays
    ! XMID (Center Longitudes)
    call HCO_ArrInit( HcoState%Grid%XMID, NX, NY, rc )
    HcoState%Grid%XMID%Val = real(ptr_d, kind=4)

    ! YMID (Center Latitudes)
    call HCO_ArrInit( HcoState%Grid%YMID, NX, NY, rc )
    call ESMF_GridGetCoord(Grid, 2, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=ptr_d, rc=localrc)
    HcoState%Grid%YMID%Val = real(ptr_d, kind=4)

    ! XEDGE (Corner Longitudes) - simplified
    call HCO_ArrInit( HcoState%Grid%XEDGE, NX, NY, rc )
    HcoState%Grid%XEDGE%Val = HcoState%Grid%XMID%Val ! Placeholder

    call HCO_ArrInit( HcoState%Grid%YEDGE, NX, NY, rc )
    HcoState%Grid%YEDGE%Val = HcoState%Grid%YMID%Val ! Placeholder

    ! AREA
    call HCO_ArrInit( HcoState%Grid%AREA_M2, NX, NY, rc )
    call ESMF_GridGetItem(Grid, ESMF_GRIDITEM_AREA, farrayPtr=ptr_d, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
       HcoState%Grid%AREA_M2%Val = real(ptr_d, kind=4)
    else
       HcoState%Grid%AREA_M2%Val = 1.0 ! Placeholder
    endif

  end subroutine nxs_set_hco_grid

  !> @brief Creates the HEMCO grid from configuration.
  !>
  !> @param ConfigFile Path to the main configuration file.
  !> @param Grid       The created ESMF grid.
  !> @param Clock      ESMF Clock for resolving filename.
  !> @param rc         Return code.
  subroutine nxs_create_hco_grid( ConfigFile, Grid, Clock, rc )
    character(len=*), intent(in)  :: ConfigFile
    type(ESMF_Grid),  intent(out) :: Grid
    type(ESMF_Clock), intent(in)  :: Clock
    integer,          intent(out) :: rc

    integer :: unit, stat
    character(len=255) :: line, key, value
    character(len=255) :: GridFileLocal
    character(len=255) :: resolvedGridFile
    integer :: i
    integer :: NX, NY
    real(ESMF_KIND_R8) :: XMIN, XMAX, YMIN, YMAX
    integer :: dimLengths(2)
    real(ESMF_KIND_R8) :: centerXMin, centerXMax, centerYMin, centerYMax
    integer :: localrc

    rc = ESMF_SUCCESS
    GridFileLocal = GridFile ! Default

    ! Open main config file to find GridFile
    open(newunit=unit, file=trim(ConfigFile), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening config file: ", trim(ConfigFile)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=10) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        if (trim(key) == 'GridFile') then
          GridFileLocal = value
          exit
        end if
      end if
    end do
10  continue
    close(unit)

    ! Open grid config file
    call ResolveFileName(GridFileLocal, Clock, resolvedGridFile, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) print *, "NEXUS: Reading grid settings from ", trim(resolvedGridFile)
    open(newunit=unit, file=trim(resolvedGridFile), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening grid file: ", trim(resolvedGridFile)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=20) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        ! Remove comments from value if any
        i = index(value, '#')
        if (i > 0) value = trim(value(1:i-1))

        if (trim(key) == 'NX') then
          read(value, *) NX
        else if (trim(key) == 'NY') then
          read(value, *) NY
        else if (trim(key) == 'XMIN') then
          read(value, *) XMIN
        else if (trim(key) == 'XMAX') then
          read(value, *) XMAX
        else if (trim(key) == 'YMIN') then
          read(value, *) YMIN
        else if (trim(key) == 'YMAX') then
          read(value, *) YMAX
        end if
      end if
    end do
20  continue
    close(unit)

    dimLengths(1) = NX
    dimLengths(2) = NY

    if (localPet == 0) print "('NEXUS: Grid NX=', i0, ' NY=', i0)", NX, NY
    if (localPet == 0) print "('NEXUS: Grid XMIN=', f0.2, ' XMAX=', f0.2)", XMIN, XMAX
    if (localPet == 0) print "('NEXUS: Grid YMIN=', f0.2, ' YMAX=', f0.2)", YMIN, YMAX

    ! Create ESMF Grid
    ! Assuming global grid if XMAX-XMIN >= 360
    if (abs(XMAX - XMIN) >= 360.0) then
        Grid = ESMF_GridCreate1PeriDim( &
          maxIndex = dimLengths, &
          coordSys = ESMF_COORDSYS_SPH_DEG, &
          indexflag= ESMF_INDEX_GLOBAL, &
          rc=rc)
    else
        Grid = ESMF_GridCreateNoPeriDim( &
          maxIndex = dimLengths, &
          coordSys = ESMF_COORDSYS_SPH_DEG, &
          indexflag= ESMF_INDEX_GLOBAL, &
          rc=rc)
    endif
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Add coordinates
    call ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! For center coordinates
    ! X coordinates
    centerXMin = XMIN + (XMAX - XMIN) / (2.0 * real(NX, kind=8))
    centerXMax = XMAX - (XMAX - XMIN) / (2.0 * real(NX, kind=8))
    call set_1d_coord(Grid, 1, ESMF_STAGGERLOC_CENTER, NX, NY, centerXMin, centerXMax, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Y coordinates
    centerYMin = YMIN + (YMAX - YMIN) / (2.0 * real(NY, kind=8))
    centerYMax = YMAX - (YMAX - YMIN) / (2.0 * real(NY, kind=8))
    call set_1d_coord(Grid, 2, ESMF_STAGGERLOC_CENTER, NX, NY, centerYMin, centerYMax, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Add Area Item
    call ESMF_GridAddItem(Grid, itemflag=ESMF_GRIDITEM_AREA, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Note: Area calculation is complex, here we might skip filling it or use dummy
    ! HEMCO will probably recalculate it or use 1.0 if not provided properly?
    ! nxs_set_hco_grid tries to get it.
    ! For now, let's leave it uninitialized or set to 1.0.
    ! nxs_set_hco_grid handles failure to get area by setting to 1.0.

  end subroutine nxs_create_hco_grid

  !> @brief Creates the HEMCO grid from configuration without clock dependency.
  !>
  !> This routine creates a static grid for use during the Realize phase where
  !> the ESMF clock is not yet available. It assumes static grid files that
  !> don't require date/time token resolution.
  !>
  !> @param ConfigFile Configuration file name
  !> @param Grid       Output ESMF grid object
  !> @param rc         Return code.
  subroutine nxs_create_hco_grid_static( ConfigFile, Grid, rc )
    character(len=*), intent(in)  :: ConfigFile
    type(ESMF_Grid),  intent(out) :: Grid
    integer,          intent(out) :: rc

    integer :: unit, stat
    character(len=255) :: line, key, value
    character(len=255) :: GridFileLocal
    integer :: i
    integer :: NX, NY
    real(ESMF_KIND_R8) :: XMIN, XMAX, YMIN, YMAX
    integer :: dimLengths(2)
    real(ESMF_KIND_R8) :: centerXMin, centerXMax, centerYMin, centerYMax
    integer :: localrc

    rc = ESMF_SUCCESS
    GridFileLocal = GridFile ! Default

    ! Open main config file to find GridFile
    open(newunit=unit, file=trim(ConfigFile), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening config file: ", trim(ConfigFile)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=10) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        if (trim(key) == 'GridFile') then
          GridFileLocal = value
          exit
        end if
      end if
    end do
10  continue
    close(unit)

    ! For Realize phase, assume static grid file (no date tokens to resolve)
    ! This works for files like HEMCO_sa_Grid.rc that contain static parameters

    if (localPet == 0) print *, "NEXUS: Reading grid settings from ", trim(GridFileLocal)
    open(newunit=unit, file=trim(GridFileLocal), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening grid file: ", trim(GridFileLocal)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=20) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        select case (trim(key))
          case ('XMIN')
            read(value, *) XMIN
          case ('XMAX')
            read(value, *) XMAX
          case ('YMIN')
            read(value, *) YMIN
          case ('YMAX')
            read(value, *) YMAX
          case ('NX')
            read(value, *) NX
          case ('NY')
            read(value, *) NY
        end select
      end if
    end do
20  continue
    close(unit)

    ! Create ESMF grid
    dimLengths(1) = NX
    dimLengths(2) = NY

    ! Calculate grid center coordinates
    centerXMin = XMIN + (XMAX - XMIN) / (2.0_ESMF_KIND_R8 * NX)
    centerXMax = XMAX - (XMAX - XMIN) / (2.0_ESMF_KIND_R8 * NX)
    centerYMin = YMIN + (YMAX - YMIN) / (2.0_ESMF_KIND_R8 * NY)
    centerYMax = YMAX - (YMAX - YMIN) / (2.0_ESMF_KIND_R8 * NY)

    Grid = ESMF_GridCreateNoPeriDim( &
      minIndex=(/1, 1/), &
      maxIndex=(/NX, NY/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Add coordinates to grid
    call ESMF_GridAddCoord(Grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) then
      print *, "NEXUS: Created static ESMF grid: NX=", NX, " NY=", NY
      print *, "NEXUS: Grid bounds: X=[", XMIN, ",", XMAX, "] Y=[", YMIN, ",", YMAX, "]"
    endif

  end subroutine nxs_create_hco_grid_static

  !> @brief Helper to set 1D coordinates on a 2D grid.
  subroutine set_1d_coord(grid, dim, stagger, nx, ny, minVal, maxVal, rc)
    type(ESMF_Grid), intent(inout) :: grid
    integer, intent(in) :: dim
    type(ESMF_StaggerLoc), intent(in) :: stagger
    integer, intent(in) :: nx, ny
    real(ESMF_KIND_R8), intent(in) :: minVal, maxVal
    integer, intent(out) :: rc

    real(ESMF_KIND_R8), pointer :: ptr(:,:)
    integer :: i, j
    integer :: lb(2), ub(2)
    real(ESMF_KIND_R8) :: step

    call ESMF_GridGetCoord(grid, dim, staggerloc=stagger, farrayPtr=ptr, &
      computationalLBound=lb, computationalUBound=ub, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    if (dim == 1) then
       step = (maxVal - minVal) / real(nx - 1, kind=8)
       if (nx == 1) step = 0.0
       do j = lb(2), ub(2)
          do i = lb(1), ub(1)
             ptr(i,j) = minVal + real(i - 1, kind=8) * step
          end do
       end do
    else
       step = (maxVal - minVal) / real(ny - 1, kind=8)
       if (ny == 1) step = 0.0
       do j = lb(2), ub(2)
          do i = lb(1), ub(1)
             ptr(i,j) = minVal + real(j - 1, kind=8) * step
          end do
       end do
    end if

  end subroutine set_1d_coord

  !> @brief Reads time settings from configuration files.
  !>
  !> @param ConfigFile Path to the main configuration file.
  !> @param rc         Return code.
  subroutine nxs_read_time_config(ConfigFile, rc)
    character(len=*), intent(in)  :: ConfigFile
    integer,          intent(out) :: rc

    integer :: unit, stat
    character(len=255) :: line, key, value
    character(len=255) :: TimeFileLocal
    character(len=255) :: DateStr
    integer :: i

    rc = ESMF_SUCCESS
    TimeFileLocal = TimeFile ! Default

    ! Open main config file to find TimeFile
    open(newunit=unit, file=trim(ConfigFile), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening config file: ", trim(ConfigFile)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=10) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        if (trim(key) == 'TimeFile') then
          TimeFileLocal = value
          exit
        end if
      end if
    end do
10  continue
    close(unit)

    ! Open time config file
    if (localPet == 0) print *, "NEXUS: Reading time settings from ", trim(TimeFileLocal)
    open(newunit=unit, file=trim(TimeFileLocal), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening time file: ", trim(TimeFileLocal)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=20) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      i = index(line, ':')
      if (i > 0) then
        key = trim(adjustl(line(1:i-1)))
        value = trim(adjustl(line(i+1:)))

        ! Remove comments from value if any
        i = index(value, '#')
        if (i > 0) value = trim(value(1:i-1))

        if (trim(key) == 'START') then
          call parse_date(value, T_YY(1), T_MM(1), T_DD(1), T_H(1), T_M(1), T_S(1))
        else if (trim(key) == 'END') then
          call parse_date(value, T_YY(2), T_MM(2), T_DD(2), T_H(2), T_M(2), T_S(2))
        end if
      end if
    end do
20  continue
    close(unit)

  end subroutine nxs_read_time_config

  !> @brief Parses a date string "YYYY-MM-DD HH:MM:SS".
  subroutine parse_date(str, yy, mm, dd, h, m, s)
    character(len=*), intent(in)  :: str
    integer,          intent(out) :: yy, mm, dd, h, m, s

    integer :: i
    character(len=255) :: temp

    ! Expected format: YYYY-MM-DD HH:MM:SS
    !                  1234567890123456789
    read(str(1:4), *) yy
    read(str(6:7), *) mm
    read(str(9:10), *) dd
    read(str(12:13), *) h
    read(str(15:16), *) m
    read(str(18:19), *) s

  end subroutine parse_date

  !-----------------------------------------------------------------------
  !> @brief Create HEMCO diagnostics manually
  !!
  !! This subroutine creates HEMCO diagnostics for all species that need
  !! to be exported, bypassing the file-based configuration if needed.
  !!
  subroutine nxs_create_hemco_diagnostics(HcoState, rc)
    implicit none

    type(Hco_State), pointer, intent(inout) :: HcoState
    integer, intent(out) :: rc

    ! Local variables
    integer :: i, n, HcoID
    character(len=255) :: msg
    character(len=63) :: DiagnName

    rc = HCO_SUCCESS

    ! ALWAYS print this to see if function is called
    print *, "NEXUS: *** nxs_create_hemco_diagnostics CALLED ***"

    if (HcoState%amIRoot) then
       print *, "NEXUS: Creating HEMCO diagnostics manually..."
       print *, "NEXUS: HEMCO has", HcoState%nSpc, "species defined"
    endif

    ! Check if diagnostics already exist
    call DiagnCollection_Get(HcoState%Diagn, &
       HcoState%Diagn%HcoDiagnIDDefault, nnDiagn=n, RC=rc)
    if (rc /= HCO_SUCCESS) then
       print *, "NEXUS: Error getting diagnostic collection info"
       return
    endif

    if (HcoState%amIRoot) then
       print *, "NEXUS: Default collection currently has", n, "diagnostics"
    endif

    ! Create diagnostics for ALL HEMCO species (like the standalone interface does)
    do i = 1, HcoState%nSpc
       ! Get HEMCO ID
       HcoID = HcoState%Spc(i)%HcoID
       if (HcoID <= 0) cycle

       ! Create diagnostic name
       DiagnName = trim(HcoState%Spc(i)%SpcName)

       if (HcoState%amIRoot) then
          print *, "NEXUS: Creating diagnostic for", trim(DiagnName), "HcoID:", HcoID
       endif

       call Diagn_Create(HcoState, &
          cName     = DiagnName, &
          long_name = 'Emissions of ' // trim(DiagnName), &
          HcoID     = HcoID, &
          ExtNr     = -1, &
          Cat       = -1, &
          Hier      = -1, &
          SpaceDim  = 2, &
          OutUnit   = 'kg/m2/s', &
          AutoFill  = 1, &
          COL       = HcoState%Diagn%HcoDiagnIDDefault, &
          OkIfExist = .TRUE., &
          RC        = rc)

       if (rc /= HCO_SUCCESS) then
          print *, "NEXUS: Error creating diagnostic for", trim(DiagnName)
          return
       endif
    end do    ! Check final count
    call DiagnCollection_Get(HcoState%Diagn, &
       HcoState%Diagn%HcoDiagnIDDefault, nnDiagn=n, RC=rc)
    if (rc /= HCO_SUCCESS) then
       print *, "NEXUS: Error getting final diagnostic count"
       return
    endif

    if (HcoState%amIRoot) then
       print *, "NEXUS: After creation, default collection has", n, "diagnostics"
    endif

  end subroutine nxs_create_hemco_diagnostics

  !> @brief Create STREAM:VARIABLE import fields for HEMCO NUOPC coupling
  !>
  !> This subroutine creates ESMF fields with names like "CEDS_SO2:SO2_was"
  !> based on the nexus_input_streams.yaml configuration and populates them
  !> with data from NetCDF files.
  !>
  !> @param model       The ESMF grid component
  !> @param importState The import state to add fields to
  !> @param localPet    Local processor ID
  !> @param rc          Return code
  subroutine CreateStreamVariableImportFields(model, importState, localPet, rc)
    use nexus_io_mod, only: CreateAndPopulateStreamVariableFields

    type(ESMF_GridComp), intent(inout)  :: model
    type(ESMF_State),    intent(inout)  :: importState
    integer,             intent(in)     :: localPet
    integer,             intent(out)    :: rc

    ! Local variables
    type(ESMF_Grid) :: grid
    integer         :: localrc
    logical         :: isCreated

    rc = ESMF_SUCCESS

    ! Get the grid from the gridded component
    call ESMF_GridCompGet(model, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) print *, "NEXUS DEBUG: CreateStreamVariableImportFields - GridCompGet complete"

    isCreated = ESMF_GridIsCreated(grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) then
       if (isCreated) then
          print *, "NEXUS DEBUG: CreateStreamVariableImportFields - Grid object IS created/valid"
       else
          print *, "NEXUS DEBUG: CreateStreamVariableImportFields - Grid object IS NOT created/valid"
       endif
    endif

    if (localPet == 0) print *, "NEXUS: Starting STREAM:VARIABLE import field creation..."

    ! Create and populate the STREAM:VARIABLE fields
    call CreateAndPopulateStreamVariableFields(importState, grid, localPet, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) then
      if (localPet == 0) print *, "NEXUS: ERROR in CreateAndPopulateStreamVariableFields"
      return
    endif

    if (localPet == 0) print *, "NEXUS: Successfully completed STREAM:VARIABLE import field creation"

  end subroutine CreateStreamVariableImportFields

  !> @brief Advertises STREAM:VARIABLE import fields during Advertise phase.
  !>
  !> @param[inout] importState  The import state to advertise fields in
  !> @param[in]    localPet     Local PET number for debug output
  !> @param[out]   rc           Return code
  subroutine AdvertiseStreamVariableImportFields(importState, localPet, rc)
    use nexus_io_mod, only: IO_Init

    type(ESMF_State), intent(inout) :: importState
    integer,          intent(in)    :: localPet
    integer,          intent(out)   :: rc

    ! Local variables
    character(len=256)      :: streamVariableName, streamName, varName, line
    integer                 :: i, j, k, localrc, ios, unit
    logical                 :: inStream, inDatavars

    rc = ESMF_SUCCESS

    if (localPet == 0) then
      print *, "NEXUS: Advertising STREAM:VARIABLE import fields from nexus_input_streams.yaml"
    endif

    ! Open and parse nexus_input_streams.yaml to get stream:variable combinations
    open(newunit=unit, file='nexus_input_streams.yaml', status='old', action='read', iostat=ios)
    if (ios /= 0) then
      if (localPet == 0) then
        print *, "NEXUS: Warning - could not open nexus_input_streams.yaml for advertising"
        print *, "NEXUS: Will advertise fallback test fields"
      endif
      ! Advertise some basic test fields if YAML is not available
      call AdvertiseTestStreamVariableFields(importState, localPet, rc)
      return
    endif

    inStream = .false.
    inDatavars = .false.
    streamName = ''

    ! Parse YAML file line by line (simplified parser for stream/datavar structure)
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit  ! End of file

      line = adjustl(line)  ! Remove leading spaces

      ! Look for stream name (e.g., "- name: CEDS_BC")
      if (line(1:7) == '- name:') then
        inStream = .true.
        inDatavars = .false.
        ! Extract stream name after "- name: "
        streamName = trim(adjustl(line(8:)))
        if (localPet == 0) then
          print *, "NEXUS: Found input stream for advertising: ", trim(streamName)
        endif

      ! Look for start of datavars section
      else if (inStream .and. line(1:9) == 'datavars:') then
        inDatavars = .true.

      ! Look for variable names in datavars (e.g., "- BC_agr")
      else if (inDatavars .and. line(1:1) == '-') then
        ! Extract variable name after "- "
        varName = trim(adjustl(line(2:)))

        ! Create STREAM:VARIABLE combination
        streamVariableName = trim(streamName) // ':' // trim(varName)

        if (localPet == 0) then
          print *, "NEXUS: Advertising import field: ", trim(streamVariableName)
        endif

        ! Advertise the field using NUOPC_Advertise
        call AdvertiseSingleStreamVariableField(importState, streamVariableName, localPet, localrc)
        if (localrc /= ESMF_SUCCESS) then
          rc = localrc
          close(unit)
          return
        endif

      ! Reset when we hit a new top-level entry or end of current stream
      else if (line(1:1) == '-' .and. line(1:7) /= '- name:') then
        inStream = .false.
        inDatavars = .false.
      endif
    end do

    close(unit)

    if (localPet == 0) then
      print *, "NEXUS: Completed advertising STREAM:VARIABLE import fields from YAML"
    endif

  end subroutine AdvertiseStreamVariableImportFields

  !> @brief Advertises a single STREAM:VARIABLE import field during Advertise phase.
  !>
  !> @param[inout] importState  The import state to advertise field in
  !> @param[in]    fieldName    The field name to advertise
  !> @param[in]    localPet     Local PET number for debug output
  !> @param[out]   rc           Return code
  subroutine AdvertiseSingleStreamVariableField(importState, fieldName, localPet, rc)

    type(ESMF_State), intent(inout) :: importState
    character(len=*), intent(in)    :: fieldName
    integer,          intent(in)    :: localPet
    integer,          intent(out)   :: rc

    ! Local variables
    integer :: localrc

    rc = ESMF_SUCCESS

    ! First add field to NUOPC field dictionary with appropriate units
    call NUOPC_FieldDictionaryAddEntry(trim(fieldName), "kg m-2 s-1", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Advertise import field using NUOPC_Advertise to ensure proper metadata
    call NUOPC_Advertise(importState, StandardName=trim(fieldName), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    if (localPet == 0) then
      print *, "NEXUS: Successfully advertised import field: ", trim(fieldName)
    endif

  end subroutine AdvertiseSingleStreamVariableField

  !> @brief Advertises fallback test import fields if YAML file is not available.
  !>
  !> @param[inout] importState  The import state to advertise fields in
  !> @param[in]    localPet     Local PET number for debug output
  !> @param[out]   rc           Return code
  subroutine AdvertiseTestStreamVariableFields(importState, localPet, rc)

    type(ESMF_State), intent(inout) :: importState
    integer,          intent(in)    :: localPet
    integer,          intent(out)   :: rc

    ! Local variables
    integer :: i, localrc
    character(len=*), parameter :: testFields(4) = [&
      'CEDS_SCALING:NOXscale', &
      'CEDS_BC:BC_agr       ', &
      'CEDS_OC:OC_agr       ', &
      'CEDS_SO2:SO2_was     ' ]

    rc = ESMF_SUCCESS

    if (localPet == 0) then
      print *, "NEXUS: Advertising fallback test STREAM:VARIABLE fields"
    endif

    ! Advertise test fields
    do i = 1, size(testFields)
      call AdvertiseSingleStreamVariableField(importState, trim(testFields(i)), localPet, localrc)
      if (localrc /= ESMF_SUCCESS) then
        rc = localrc
        return
      endif
    end do

  end subroutine AdvertiseTestStreamVariableFields

end module nexus_cap
