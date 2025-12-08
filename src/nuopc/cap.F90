!> @brief NEXUS NUOPC Component.
module nexus_cap

  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS => SetServices

  use HCOI_NUOPC_MOD, only: HCO_SetServices_NUOPC, HCO_SetExtState_NUOPC
  use HCO_Config_Mod,  only: Config_ReadFile
  use HCO_Driver_Mod,  only: HCO_Init
  use HCOX_Driver_Mod, only: HCOX_Init
  use HCO_STATE_MOD, only: Hco_State, HcoState_Init, HcoState_Final
  use HCO_TYPES_MOD, only: ConfigObj
  use HCO_Error_Mod, only: rk_hco => hp, &
    HcoErr, &
    HCO_LogFile_Open, &
    HCO_Error, HCO_MSG, HCO_Leave, HCO_Enter, &
    HCO_SUCCESS, HCO_MISSVAL
  use HCOX_STATE_MOD, only: Ext_State, ExtStateInit
  use HCOX_Driver_Mod, only: HCOX_Init, HCOX_Run, HCOX_Final
  use nexus_io_mod, only: IO_Init, IO_Read, IO_Write

  implicit none

  ! TODO: cap object with pointers that can be retrieved with ESMF_GridCompGetInternalState?

  ! Default values for HEMCO input files: contain definitions of
  ! species, grid, and time settings, etc.
  character(len=255) :: GridFile = 'HEMCO_sa_Grid'
  character(len=255) :: SpecFile = 'HEMCO_sa_Spec'
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

  type(ESMF_Grid)  :: HCO_Grid
  type(ESMF_Grid)  :: NXS_Grid
  type(ESMF_State) :: NXS_Diag_State
  !! "importState"
  !! An ESMF state of diagnostics on the HEMCO grid.
  type(ESMF_State) :: NXS_Expt_State
  !! "exportState"
  !! Regridded to the desired output grid.
  type(ESMF_RouteHandle) :: NXS_RouteHandle

  logical :: do_Regrid = .false.
  !! True if grid file path passed to `init` is not empty string.
  logical :: do_Debug  = .false.
  !! True if `debugLevel` passed to `init` is greater than zero.
  logical :: do_NEXUS  = .false.
  !! True if either `do_Regrid` or `do_Debug` is true.
  logical :: alwaysWriteRestartFile = .false.
  !! Even in NEXUS mode (`do_NEXUS`)

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

    rc = ESMF_SUCCESS

    ! Derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Initialize HEMCO services
    call HCO_SetServices_NUOPC( (localPet == rootPet), model, HcoConfig, &
      trim(ConfigFile_), rc )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Create HcoState
    call HcoState_Init( HcoState, HcoConfig, 0, rc )
    if (nxs_error_log(rc, msg='Error encountered in routine "HcoState_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Set GridComp on HcoState
    ! HcoState%GridComp => model

    ! Initialize HEMCO core
    call HCO_Init( HcoState, rc )
    if (nxs_error_log(rc, msg='Error encountered in routine "HCO_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! We use the standard Initialize phase
    call NUOPC_CompSpecialize(model, specLabel=label_Initialize, &
      specRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !> @brief Advertises fields to the NUOPC driver.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine Advertise(model, rc)
    use HCO_TYPES_MOD, only: DiagnCont
    use HCO_Diagn_Mod, only: Diagn_Get

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_State) :: importState, exportState
    integer :: localrc
    integer :: flag
    logical :: EOI
    type(DiagnCont), pointer :: thisDiagn
    character(len=:), allocatable :: name, long_name, units

    rc = ESMF_SUCCESS

    ! Query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Advertise NEXUS output variables
    EOI = .false.
    nullify(thisDiagn)
    call Diagn_Get(HcoState, EOI, thisDiagn, flag, localrc)
    if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    do while (flag == HCO_SUCCESS)

      name = trim(thisDiagn % cName)
      long_name = trim(thisDiagn % long_name)
      units = trim(thisDiagn % OutUnit)

      print "('NEXUS: Advertising ''', a, ''' (long_name=''', a, ''', units=''', a, ''')')", &
        name, long_name, units

      ! Add to field dictionary
      call NUOPC_FieldDictionaryAddEntry(name, units, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out

      ! Advertise field
      call NUOPC_Advertise(exportState, &
        name=name, &
        StandardName=name, &
        LongName=long_name, &
        Units=units, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      ! Next
      call Diagn_Get(HcoState, EOI, thisDiagn, flag, localrc)
      if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

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

    rc = ESMF_SUCCESS

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

      print "('NEXUS: Realizing ''', a, '''')", trim(itemNameList(item))
      call NUOPC_Realize(exportState, field=field, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

    end do

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

    timeSlice = 0
    rc = ESMF_SUCCESS

    ! Query for clock, importState and exportState
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Set import state for HEMCO
    HcoState%IMPORT = importState

    ! Read external data into importState
    call IO_Read(importState, clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return


    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Get some Clock info
    call ESMF_ClockGet(clock, advanceCount=advanceCount, currTime=time)

    print "('NEXUS: ESMF Clock advanceCount: ', i0)", advanceCount
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
    print "('NEXUS: ', a)", trim(msg)

    ! ================================================================
    ! Reset all emission and deposition values
    ! ================================================================
    call HCO_FluxArrReset( HcoState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCO_FluxArrReset"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

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
    call HCO_Run( HcoState, 1, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "Hco_Run", phase 1!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Phase 2: Compute emissions (skip for dry-run)
    call HCO_Run( HcoState, 2, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "Hco_Run", phase 2!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! ================================================================
    ! Run HCO extensions
    ! ================================================================

    ! Set ExtState fields from ESMF import state
    call HCO_SetExtState_NUOPC( HcoState, HcoExtState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCO_SetExtState_NUOPC"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Execute all enabled emission extensions. Emissions will be
    ! added to corresponding flux arrays in HcoState.
    call HCOX_Run ( HcoState, HcoExtState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCOX_Run"!', &
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
    ! Update NEXUS Diagnostic state
    !=================================================================
    if (do_NEXUS) then
      call nxs_diag_state_update( HcoState, NXS_Diag_State, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    !=================================================================
    ! Write output via I/O layer
    !=================================================================
    call IO_Write(exportState, clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out


  end subroutine

  !> @brief Initializes the model.
  !>
  !> @param model The ESMF grid component.
  !> @param rc    Return code.
  subroutine Initialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    integer :: localrc
    logical :: am_I_Root
    type(ESMF_VM) :: vm
    rc = ESMF_SUCCESS

    ! Initialize I/O
    call IO_Init(HCO_Grid, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! Get VM and pet info
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return
    am_I_Root = (localPet == rootPet)

    ! Set logical flags from stored command-line args
    do_Regrid = (len_trim(ReGridFile_) > 0)
    do_Debug  = (debugLevel_ > 0)
    do_NEXUS  = (do_Debug .or. do_Regrid)
    alwaysWriteRestartFile = writeRestart_
    if (len_trim(OutputFile_) > 0) ExptFile = OutputFile_

    ! Get grid from component
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGetGrid(exportState, HCO_Grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Create and initialize extensions.
    !=======================================================================
    ! call ExtState_Create( HcoExtState, HcoState%NX, HcoState%NY, HcoState%NZ, localrc )
    ! if (nxs_error_log(localrc, msg='Error encountered in routine "ExtState_Create"!', &
    !   line=__LINE__, &
    !   file=__FILE__, &
    !   rcToReturn=rc)) return
    call HCOX_Init( HcoState, HcoExtState, localrc )
    if (nxs_error_log(localrc, msg='Error encountered in routine "HCOX_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Start NEXUS Init
    !=======================================================================
    if (do_NEXUS) then
      NXS_Diag_State = ESMF_StateCreate( rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call nxs_diag_state_init( HCO_Grid, HcoState, NXS_Diag_State, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    if (do_Regrid) then
      NXS_Grid = nxs_set_grid( ReGridFile_, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      NXS_Expt_State = ESMF_StateCreate( rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call nxs_expt_state_init( NXS_Grid, NXS_Diag_State, NXS_Expt_State, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

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

    if (present(rc)) rc = ESMF_SUCCESS

        ConfigFile_   = ConfigFile
        ReGridFile_   = ReGridFile
        OutputFile_   = OutputFile
        debugLevel_   = debugLevel
        writeRestart_ = writeRestart

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

    if (do_NEXUS .and. alwaysWriteRestartFile) then
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
  !> @param rc       Return code (optional).
  !> @return         The created ESMF grid.
  function nxs_set_grid( fileName, rc ) result ( grid )

    use netcdf

    character(len=*),  intent(in)  :: fileName
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

    character(len=*), parameter :: dimNames(2) = (/ "grid_xt", "grid_yt" /)
    character(len=*), parameter :: coordNames(2,2) = reshape( &
      (/ "grid_lont", "grid_latt", "grid_lon ", "grid_lat " /), &
      (/ 2,2 /) )

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    print "('NEXUS: Input grid nc fn:', x, a)", trim(filename)
    ncerr = nf90_open(fileName, NF90_NOWRITE, ncid)
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
  subroutine nxs_diag_state_init( HcoGrid, HcoState, DiagState, rc )
    use HCO_TYPES_MOD, only: DiagnCont  ! diagnostics container
    use HCO_Diagn_Mod, only: Diagn_Get

    type(ESMF_Grid)                :: HcoGrid
    type(HCO_State), pointer       :: HcoState
    type(ESMF_State)               :: DiagState
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: flag
    logical :: EOI
    type(ESMF_Field) :: field
    type(DiagnCont), pointer :: thisDiagn

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
      print "('NEXUS: Initializing Diag variable ''', a, '''')", trim(thisDiagn%cName)
      select case ( thisDiagn % spaceDim )
       case (2)
        field = ESMF_FieldCreate( HcoGrid, ESMF_TYPEKIND_R4, &
          name=thisDiagn % cName, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
       case (3)
        field = ESMF_FieldCreate( HcoGrid, ESMF_TYPEKIND_R4, &
          ungriddedLBound = (/ lbound(thisDiagn % Arr3D % Val, dim=3) /), &
          ungriddedUBound = (/ ubound(thisDiagn % Arr3D % Val, dim=3) /), &
          name=thisDiagn % cName, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
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

      call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
      if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

    call ESMF_StateReconcile( DiagState, rc=localrc )
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine nxs_diag_state_init

  !> @brief Updates the diagnostics state.
  !>
  !> @param HcoState  The HEMCO state.
  !> @param DiagState The diagnostics state to update.
  !> @param rc        Return code (optional).
  subroutine nxs_diag_state_update( HcoState, DiagState, rc )
    use HCO_TYPES_MOD, only: DiagnCont
    use HCO_Diagn_Mod, only: Diagn_Get

    type(HCO_State), pointer       :: HcoState
    type(ESMF_State)               :: DiagState
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: flag
    integer :: lb(2), ub(2)
    logical :: EOI
    real(ESMF_KIND_R4), pointer :: fp2d(:,:), fp3d(:,:,:)
    type(ESMF_Field) :: field
    type(DiagnCont), pointer :: thisDiagn

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
      call ESMF_StateGet( DiagState, thisDiagn % cName, field, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      select case ( thisDiagn % spaceDim )
       case (2)
        call ESMF_FieldGet(field, farrayPtr=fp2d, &
          computationalLBound=lb, computationalUBound=ub, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        fp2d(lb(1):ub(1),lb(2):ub(2)) = thisDiagn % Arr2D % Val
       case (3)
        call ESMF_FieldGet(field, farrayPtr=fp3d, &
          computationalLBound=lb, computationalUBound=ub, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        fp3d(lb(1):ub(1),lb(2):ub(2),:) = thisDiagn % Arr3D % Val
      end select

      call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
      if (nxs_error_log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

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

    do item = 1, itemCount
      print "('NEXUS: Initializing Expt variable ''', a, '''')", trim(itemNameList(item))
      if (itemTypeList(item) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet( importState, itemNameList(item), srcfield, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_FieldGet( srcfield, rank=rank, typekind=typekind, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        select case (rank)
         case (2)
          dstfield = ESMF_FieldCreate( grid, typekind, name=itemNameList(item), rc=localrc )
         case (3)
          call ESMF_FieldGet( srcfield, ungriddedLBound=lb, ungriddedUBound=ub, &
            rc=localrc )
          dstfield = ESMF_FieldCreate( grid, typekind, name=itemNameList(item), &
            ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc )
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
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_StateAdd( exportState, (/ dstfield /), rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (.not.ESMF_RouteHandleIsCreated(NXS_RouteHandle)) then
          write(6,'(1x,"Precomputing regridding operation ...")')
          srcTermProcessing = 0
          call ESMF_FieldRegridStore(srcfield, dstfield, &
            regridmethod      = ESMF_REGRIDMETHOD_CONSERVE, &
            unmappedaction    = ESMF_UNMAPPEDACTION_IGNORE, &
            srcTermProcessing = srcTermProcessing,       &
            routehandle       = NXS_RouteHandle, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
        end if
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

end module nexus_cap

