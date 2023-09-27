module nexus_cap

  !-----------------------------------------------------------------------------
  ! NEXUS NUOPC Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS => SetServices

  use HCO_STATE_MOD, only: Hco_State
  use HCO_TYPES_MOD, only: ConfigObj
  use HCO_Error_Mod, only: rk_hco => hp, &
    HCO_LogFile_Open, &
    HCO_Error, HCO_MSG, &
    HCO_SUCCESS, HCO_MISSVAL
  use HCOX_STATE_MOD, only: Ext_State

  implicit none

  ! TOOD: cap object with pointers that can be retrieved with ESMF_GridCompGetInternalState?

  ! Default values for HEMCO input files: contain definitions of
  ! species, grid, and time settings, etc.
  character(len=255) :: GridFile = 'HEMCO_sa_Grid'
  character(len=255) :: SpecFile = 'HEMCO_sa_Spec'
  character(len=255) :: TimeFile = 'HEMCO_sa_Time'
  character(len=255) :: DiagFile = 'NEXUS_Diag.nc'
  character(len=255) :: ExptFile = 'NEXUS_Expt.nc'

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

  ! Start and end time of simulation
  integer :: T_YRS(2), T_MTS(2), T_DYS(2)
  integer :: T_HRS(2), T_MNS(2), T_SCS(2)

  ! Grid
  real(rk_hco), allocatable, target :: XMID   (:,:,:)
  real(rk_hco), allocatable, target :: YMID   (:,:,:)
  real(rk_hco), allocatable, target :: XEDGE  (:,:,:)
  real(rk_hco), allocatable, target :: YEDGE  (:,:,:)
  real(rk_hco), allocatable, target :: YSIN   (:,:,:)
  real(rk_hco), allocatable, target :: AREA_M2(:,:,:)
  real(rk_hco), allocatable, target :: PBL_M  (:,:)

  !> MAXIT is the maximum number of run calls allowed
  integer, parameter :: MAXIT = 100000

  integer, parameter :: rootPet = 0

  type(ESMF_StaggerLoc), parameter :: staggerList(2) = &
    (/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /)

  private

  public SetServices, init

contains

  !-----------------------------------------------------------------------------
  ! NUOPC routines

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize model
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

  end subroutine

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field
    call NUOPC_Advertise(exportState, &
      StandardName="the_best_variable_of_all", name="best", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut

    rc = ESMF_SUCCESS

    print *, HcoConfig%ConfigFileName

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! exportable field
    field = ESMF_FieldCreate(name="best", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString
    integer(ESMF_KIND_I8)       :: advanceCount

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Get some Clock info
    call ESMF_ClockGet(clock, advanceCount=advanceCount)

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

  end subroutine

  !-----------------------------------------------------------------------------
  ! Cap routines

  !> Cap initialization
  !> (read HEMCO config, initialize HEMCO state, create grid objects, etc.)
  subroutine init(ConfigFile, ReGridFile, OutputFile, debugLevel, rc)
    use HCO_Config_Mod,  only: Config_ReadFile
    use HCO_Driver_Mod,  only: HCO_Init
    use HCO_EXTLIST_Mod, only: GetExtOpt, CoreNr
    use HCO_State_Mod,   only: HcoState_Init
    use HCOX_Driver_Mod, only: HCOX_Init
    use HCOI_StandAlone_Mod, only: Get_nnMatch, &
      register_species, Define_Diagnostics, HCOI_SA_InitCleanup
    use NEXUS_Error_Mod, only: NEXUS_Error_Log

    character(len=*),  intent(in)  :: ConfigFile
    character(len=*),  intent(in)  :: ReGridFile
    character(len=*),  intent(in)  :: OutputFile
    integer,           intent(in)  :: debugLevel
    integer, optional, intent(out) :: rc

    integer :: localrc
    integer :: localPet
    logical :: am_I_Root
    integer :: nnMatch
    logical :: Dum, Found
    character(len=ESMF_MAXSTR) :: msgString
    type(ESMF_VM) :: vm

    if (present(rc)) rc = ESMF_SUCCESS

    ! -- determine whether I am root
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return

    am_I_Root = (localPet == rootPet)

    do_Regrid = (len_trim(ReGridFile) > 0)
    do_Debug  = (debugLevel > 0)
    do_NEXUS  = (do_Debug .or. do_Regrid)

    if (len_trim(OutputFile) > 0) ExptFile = OutputFile

    if ( am_I_Root ) then
      if ( do_Debug  ) print *,'Writing debug emissions to: '//trim(DiagFile)
      if ( do_Regrid ) print *,'Writing regridded emissions to: '//trim(ExptFile)
    end if

    !=======================================================================
    ! Read HEMCO configuration file and save into buffer. This also
    ! sets the HEMCO error properties (verbose mode? log file name,
    ! etc.) based upon the specifications in the configuration file.
    !=======================================================================
    call Config_ReadFile( am_I_Root, HcoConfig, ConfigFile,       &
      0,         localrc,   IsDryRun=.false. )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Config_Readfile!"', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Open logfile
    !======================================================================
    if ( am_I_Root ) then
      call HCO_LogFile_Open( HcoConfig%Err, RC=localrc )
      if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCO_Logfile_Open_Readfile!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end if

    !=======================================================================
    ! Initialize HEMCO state object and populate it
    !=======================================================================

    !-----------------------------------------------------------------------
    ! Extract species to use in HEMCO
    call Get_nnMatch( HcoConfig, nnMatch, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Get_nnMatch"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !-----------------------------------------------------------------------
    ! Initialize HCO state. Use only species that are used
    ! in HEMCO_sa_Spec.rc and are also found in the HEMCO config. file.
    call HcoState_Init( HcoState, HcoConfig, nnMatch, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HcoState_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !-----------------------------------------------------------------------
    ! Set grid
    call hco_set_grid ( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Set_Grid"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !-----------------------------------------------------------------------
    ! Create NEXUS grid and reset HEMCO grid as distributed
    if (do_NEXUS) then
      HCO_Grid = nxs_reset_hco_grid( HcoState, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    !-----------------------------------------------------------------------
    ! Register species
    call Register_Species( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Register_Species"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !-----------------------------------------------------------------------
    ! Read time information, incl. timesteps and simulation time(s)
    call hco_read_time( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Read_Time"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Set misc. parameter
    !=======================================================================

    ! Set ESMF flag
    HcoState%Options%isESMF = .FALSE.  ! TODO: should this still be the case?

    ! Let HEMCO schedule the diagnostics output
    HcoState%Options%HcoWritesDiagn = .not.do_NEXUS

    ! If not explicitly set, make sure that option Field2Diagn is true
    call GetExtOpt ( HcoState%Config, CoreNr, &
      'ConfigField to diagnostics', &
      OptValBool=Dum, Found=Found, RC=localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "GetExtOpt"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    if ( .NOT. Found ) HcoState%Options%Field2Diagn = .TRUE.

    !=======================================================================
    ! Are we running the HEMCO standalone in a dry-run mode?
    ! This is dictated by the HEMCO environment. If HEMCO is in a
    ! dry-run mode, no compute is performed and files are only "checked".
    ! Simulations will NOT stop on missing files. This is intended to be a
    ! quick sanity check to make sure that GEOS-Chem IO are all correctly
    ! set up, which is why most of the runs fail to complete successfully.
    ! (hplin, 11/2/19)
    !
    ! Dry-run simulations now send output to a log file that is separate
    ! from the HEMCO log files. (bmy, 11/11/19)
    !
    ! NOTE: The dry-run option is not invoked when we use HEMCO
    ! in external ESMs. (bmy, 11/13/19)
    !=======================================================================

    !=======================================================================
    ! Initialize HEMCO internal lists and variables. All data
    ! information is written into internal lists (ReadList) and
    ! the HEMCO configuration file is removed from buffer in this
    ! step. Also initializes the HEMCO clock
    !=======================================================================
    call HCO_Init( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCO_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Initialize extensions.
    ! This initializes all (enabled) extensions and selects all met.
    ! fields needed by them.
    !=======================================================================
    call HCOX_Init( HcoState, HcoExtState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCOX_Init"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Define diagnostics
    !=======================================================================

    !--------------------------------------------------------------------
    ! For regular simulations, read diagnostics configuration file
    ! and define diagnostic variables for output
    !--------------------------------------------------------------------
    call Define_Diagnostics( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Define_Diagnostics"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Leave HEMCO Init
    !=======================================================================
    call HCOI_SA_InitCleanup( localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCOI_SA_InitCleanup"!', &
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

    if (do_Debug) then
      call nxs_grid_write( HCO_Grid, DiagFile, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
    end if

    if (do_Regrid) then
      NXS_Grid = nxs_set_grid( ReGridFile, rc=localrc )
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

      if (do_Debug) then
        call nxs_grid_write( NXS_Grid, ExptFile, rc=localrc )
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
      end if
    end if

  end subroutine init

  !-----------------------------------------------------------------------------
  ! Selected HEMCO standalone routines
  ! (Copied here from nexus_methods_mod since they are private in
  ! hcoi_standalone_mod)

  subroutine hco_set_grid( HcoState, RC )
    !
    ! !USES:
    !
    use HCO_inquireMod,   only  : findFreeLUN
    use HCO_ExtList_Mod,  only  : HCO_GetOpt, GetExtOpt, CoreNr
    use HCO_VertGrid_Mod, only  : HCO_VertGrid_Define
    use HCO_GeoTools_Mod, only  : HCO_SetPBLm
    use HCO_CharTools_Mod, only : NextCharPos, HCO_SPC, GetNextLine

    !
    ! !INPUT/OUTPUT PARAMETERS:
    !
    type(HCO_STATE), pointer       :: HcoState
    integer,         intent(inout) :: RC
    !
    ! !REVISION HISTORY:
    !  13 Sep 2013 - C. Keller - Initial Version
    !  11 May 2015 - C. Keller - Now provide lon/lat edges instead of assuming
    !                            global grid.
    !  10 Sep 2015 - C. Keller - Allow to provide mid-points instead of edges.
    !EOP
    !------------------------------------------------------------------------------
    !BOC
    !
    ! LOCAL VARIABLES:
    !
    ! Scalars
    integer               :: NX, NY, NZ
    integer               :: I, J, N, LNG, LOW, UPP
    integer               :: IU_FILE, IOS, STRT
    real(rk_hco)          :: XMIN, XMAX
    real(rk_hco)          :: YMIN, YMAX
    real(rk_hco)          :: DVAL
    real(rk_hco)          :: DLON, DLAT
    real(rk_hco)          :: PI_180, YDGR, YSN, SIN_DELTA, AM2
    logical               :: FOUND,   EOF

    ! Arrays
    integer                   :: SZ(3)
    real(rk_hco)              :: RG(4)
    real(rk_hco), allocatable :: Ap(:), Bp(:)

    ! Strings
    character(len=255)    :: LOC
    character(len=  1)    :: COL
    character(len=255)    :: MyGridFile, ThisLoc
    character(len=4095)   :: DUM,        ErrMsg,  Msg

    !=================================================================
    ! SET_GRID begins here
    !=================================================================

    ! Initialize
    RC      = HCO_SUCCESS
    Msg     = ''
    ErrMsg  = ''
    ThisLoc = &
      'SET_GRID (in module HEMCO/Interfaces/hcoi_standalone_mod.F90)'

    ! Set PI_180
    PI_180 = HcoState%Phys%PI / 180.0_rk_hco

    ! Try to get GridFile from configuration file (in settings)
    call GetExtOpt ( HcoState%Config, CoreNr, 'GridFile', &
      OptValChar=MyGridFile,   Found=FOUND, RC=RC )
    if ( RC /= HCO_SUCCESS ) then
      ErrMsg = 'Error encountered in routine "GetExtOpt"!'
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if
    if ( FOUND ) GridFile = MyGridFile

    ! Write colon character to local variable
    COL = HCO_GetOpt( HcoState%Config%ExtList, 'Colon' )

    ! ------------------------------------------------------------------
    ! Open grid file
    ! ------------------------------------------------------------------

    ! Find a free file LUN
    IU_FILE = findFreeLUN()

    ! Open grid file
    OPEN( IU_FILE, FILE=trim(GridFile), STATUS='OLD', IOSTAT=IOS )
    if ( IOS /= 0 ) then
      ErrMsg = 'Error 1 reading ' // trim(GridFile)
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if

    ! ------------------------------------------------------------------
    ! Extract grid range
    ! The lon/lat grid ranges are expected to be provided first, with
    ! each range provided in a separate line:
    ! XMIN: -180.0
    ! XMAX:  180.0
    ! YMIN:  -90.0
    ! YMAX:   90.0
    ! ------------------------------------------------------------------
    do N = 1,4

      ! Get next valid line
      call GetNextLine( IU_FILE, DUM, EOF, RC )
      if ( RC /= HCO_SUCCESS .OR. EOF ) then
        ErrMsg= 'Error 2 reading ' // trim(GridFile)
        call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
        return
      end if

      ! Read integer after colon (this is the dimension size)
      LNG = len(trim(DUM))
      LOW = NextCharPos ( trim(DUM), COL, 1 )
      if ( LOW < 0 .OR. LOW == LNG ) then
        ErrMsg = 'Cannot extract size information from ' // trim(DUM)
        call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
        return
      end if
      LOW = LOW + 1
      READ( DUM(LOW:LNG), * ) RG(N)

    end do

    ! Pass to scalars
    XMIN = RG(1)
    XMAX = RG(2)
    YMIN = RG(3)
    YMAX = RG(4)

    ! Make sure values are in valid range
    if ( XMIN >= XMAX ) then
      write(ErrMsg,*) 'Lower lon must be smaller than upper lon: ', XMIN, XMAX
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if
    if ( YMIN >= YMAX ) then
      write(ErrMsg,*) 'Lower lat must be smaller than upper lat: ', YMIN, YMAX
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if

    ! Restrict latitude values to -90.0 and 90.0.
    if ( YMIN < -90.0_rk_hco ) then
      write(ErrMsg,*) 'Lower latitude must be between -90 and 90 degN: ', YMIN
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if
    if ( YMAX > 90.0_rk_hco ) then
      write(ErrMsg,*) 'Upper latitude must be between -90 and 90 degN: ', YMAX
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if

    ! ------------------------------------------------------------------
    ! Extract grid size (x,y,z)
    ! The grid sizes are expected to be provided in three separte lines:
    ! NX: 360
    ! NY: 180
    ! NZ: 1
    ! ------------------------------------------------------------------
    do N = 1,3

      ! Get next valid line
      call GetNextLine( IU_FILE, DUM, EOF, RC )
      if ( RC /= HCO_SUCCESS .OR. EOF ) then
        ErrMsg = 'Error 3 reading ' // trim(GridFile)
        call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
        return
      end if

      ! Read integer after colon (this is the dimension size)
      LNG = len(trim(DUM))
      LOW = NextCharPos ( trim(DUM), COL, 1 )
      if ( LOW < 0 .OR. LOW == LNG ) then
        ErrMsg = 'Cannot extract size information from ' // trim(DUM)
        call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
        return
      end if
      LOW = LOW + 1
      READ( DUM(LOW:LNG), * ) SZ(N)

    end do !N

    ! Grid dimensions
    NX = SZ(1)
    NY = SZ(2)
    NZ = SZ(3)

    ! ------------------------------------------------------------------
    ! Now that sizes are known, allocate all arrays
    ! ------------------------------------------------------------------
    allocate ( XMID     (NX,  NY,  1   ) )
    allocate ( YMID     (NX,  NY,  1   ) )
    allocate ( XEDGE    (NX+1,NY,  1   ) )
    allocate ( YEDGE    (NX,  NY+1,1   ) )
    allocate ( YSIN     (NX,  NY+1,1   ) )
    allocate ( AREA_M2  (NX,  NY,  1   ) )
    allocate ( AP       (          NZ+1) )
    allocate ( BP       (          NZ+1) )
    allocate ( PBL_M    ( NX, NY       ) )
    YSIN      = HCO_MISSVAL
    AREA_M2   = HCO_MISSVAL
    XMID      = HCO_MISSVAL
    YMID      = HCO_MISSVAL
    XEDGE     = HCO_MISSVAL
    YEDGE     = HCO_MISSVAL
    AP        = HCO_MISSVAL
    BP        = HCO_MISSVAL
    PBL_M     = HCO_MISSVAL

    ! ------------------------------------------------------------------
    ! Check if grid box edges and/or midpoints are explicitly given.
    ! Those need be provided on one line, e.g.:
    ! YEDGE: -90.0 -89.0 -86.0 ... 86.0 89.0 90.0
    ! ------------------------------------------------------------------
    do N = 1, 6 ! check for XEDGE, YEDGE, XMID, YMID

      ! Try to read line
      call GetNextLine( IU_FILE, DUM, EOF, RC )
      if ( RC /= HCO_SUCCESS ) then
        MSG = 'Error reading grid edges and/or midpoints in ' // trim(GridFile)
        call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
        return
      end if

      ! Exit loop here if end of file
      if ( EOF ) EXIT

      ! Read XEDGES or YEDGES
      LNG = -1
      if ( DUM(1:5) == 'XEDGE' .OR. DUM(1:5) == 'YEDGE' ) then
        LNG  = len(trim(DUM))
        STRT = 7 ! Start at string position 7 (e.g. 'XEDGE: XXX')
      else if ( DUM(1:4) == 'XMID' .OR. DUM(1:4) == 'YMID' ) then
        LNG = len(trim(DUM))
        STRT = 6 ! Start at string position 6 (e.g. 'XMID: XXX')
      else if ( DUM(1:2) == 'AP' .OR. DUM(1:2) == 'BP' ) then
        LNG = len(trim(DUM))
        STRT = 4 ! Start at string position 4 (e.g. 'AP: XXX')
      end if

      if ( LNG > 0 ) then

        LOW = -1
        UPP = -1
        I   = 0

        ! Walk through entire string
        do J = STRT, LNG

          ! Need to evaluate if this is the last string character and/or
          ! whitespace character
          if ( trim(DUM(J:J)) == HCO_SPC ) then

            ! If the lower substring bound is not set yet, assume that this
            ! is a lower substring bound, and continue search for upper bound
            if ( LOW == -1 ) LOW = J

            ! Make sure the substring bounds are valid values
            if ( (J-1) >= (LOW+1) ) then
              UPP = J
            else
              LOW = J
            end if

          end if

          ! If this is the last character, set upper substring bound to J
          if ( J == LNG ) UPP = J

          ! Read substring if both bounds are defined
          if ( UPP > LOW ) then

            ! Read value
            READ( DUM(LOW:UPP), * ) DVAL

            ! Index to fill
            I = I + 1

            ! Pass to XEDGE
            if ( trim(DUM(1:5)) == 'XEDGE' ) then
              if ( I > NX+1 ) then
                write(ErrMsg,*) 'More than ', NX+1, ' longitude edges found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              XEDGE(I,:,1) = DVAL

              ! Pass to YEDGE
            else if ( trim(DUM(1:5)) == 'YEDGE' ) then
              if ( I > NY+1 ) then
                write(ErrMsg,*) 'More than ', NY+1, ' latitude edges found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              YEDGE(:,I,1) = DVAL

              ! Pass to XMID
            else if ( trim(DUM(1:4)) == 'XMID' ) then
              if ( I > NX ) then
                write(ErrMsg,*) 'More than ', NX, ' latitude mid-points found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              XMID(I,:,1) = DVAL

              ! Pass to YMID
            else if ( trim(DUM(1:4)) == 'YMID' ) then
              if ( I > NY ) then
                write(ErrMsg,*) 'More than ', NY, ' latitude mid-points found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              YMID(:,I,1) = DVAL

              ! Pass to Ap
            else if ( trim(DUM(1:2)) == 'AP' ) then
              if ( I > (NZ+1) ) then
                write(ErrMsg,*) 'More than ', NZ+1, ' Ap values found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              AP(I) = DVAL

              ! Pass to Bp
            else if ( trim(DUM(1:2)) == 'BP' ) then
              if ( I > (NZ+1) ) then
                write(ErrMsg,*) 'More than ', NZ+1, ' Bp values found in ', trim(DUM)
                call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
                return
              end if
              BP(I) = DVAL
            end if

            ! Update bounds
            LOW = UPP
          end if
        end do

        ! Error check: all values must have been filled
        if ( trim(DUM(1:5)) == 'XEDGE' .AND. I /= NX+1 ) then
          write(ErrMsg,*) 'Error reading XEDGES: exactly ', NX+1, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if
        if ( trim(DUM(1:5)) == 'YEDGE' .AND. I /= NY+1 ) then
          write(ErrMsg,*) 'Error reading YEDGES: exactly ', NY+1, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if
        if ( trim(DUM(1:4)) == 'XMID' .AND. I /= NX ) then
          write(ErrMsg,*) 'Error reading XMID: exactly ', NX, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if
        if ( trim(DUM(1:4)) == 'YMID' .AND. I /= NY ) then
          write(ErrMsg,*) 'Error reading YMID: exactly ', NY, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if
        if ( trim(DUM(1:2)) == 'AP' .AND. I /= NZ+1 ) then
          write(ErrMsg,*) 'Error reading AP: exactly ', NZ+1, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if
        if ( trim(DUM(1:2)) == 'BP' .AND. I /= NZ+1 ) then
          write(ErrMsg,*) 'Error reading BP: exactly ', NZ+1, ' values must be given: ', trim(DUM)
          call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
          return
        end if

      end if
    end do

    ! Error check: if AP is given, Bp must be given as well
    if ( ALL(AP==HCO_MISSVAL) .AND. .NOT. ALL(BP==HCO_MISSVAL) ) then
      write(ErrMsg,*) 'At least a few AP values are missing, please provide exactly ', &
        NZ+1, 'AP and BP values.'
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    else if ( .NOT. ALL(AP==HCO_MISSVAL) .AND. ALL(BP==HCO_MISSVAL) ) then
      write(ErrMsg,*) 'At least a few BP values are missing, please provide exactly ', &
        NZ+1, 'AP and BP values.'
      call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
      return
    end if

    ! ------------------------------------------------------------------
    ! Close file
    ! ------------------------------------------------------------------
    CLOSE( IU_FILE )

    ! ------------------------------------------------------------------
    ! Fill grid box values
    ! ------------------------------------------------------------------
    DLAT = ( YMAX - YMIN ) / NY

    ! Now fill values
    do J = 1, NY
      do I = 1, NX

        ! Set longitude and latitude edge values if not read from disk
        if ( XEDGE(I,J,1) == HCO_MISSVAL ) then

          ! eventually get from mid-points
          if ( XMID(I,J,1) /= HCO_MISSVAL ) then
            if ( I > 1 ) then
              DLON         = XMID(I,J,1) - XMID(I-1,J,1)
            else
              DLON         = XMID(I+1,J,1) - XMID(I,J,1)
            end if
            XEDGE(I,J,1) = XMID(I,J,1) - DLON/2.0

            ! otherwise assume constant grid spacing
          else
            DLON = ( XMAX - XMIN ) / NX
            XEDGE(I,J,1) = XMIN + ( (I-1) * DLON )
          end if
        else
          DLON = XEDGE(I+1,J,1) - XEDGE(I,J,1)
        end if

        if ( YEDGE(I,J,1) == HCO_MISSVAL ) then

          ! eventually get from mid-points
          if ( YMID(I,J,1) /= HCO_MISSVAL ) then
            if ( J > 1 ) then
              DLAT         = YMID(I,J,1) - YMID(I,J-1,1)
            else
              DLAT         = YMID(I,J+1,1) - YMID(I,J,1)
            end if
            YEDGE(I,J,1) = YMID(I,J,1) - DLAT/2.0

            ! otherwise assume constant grid spacing
          else
            DLAT = ( YMAX - YMIN ) / NY
            YEDGE(I,J,1) = YMIN + ( (J-1) * DLAT )
          end if
        else
          DLAT = YEDGE(I,J+1,1) - YEDGE(I,J,1)
        end if

        ! Set mid values
        if ( XMID(I,J,1) == HCO_MISSVAL ) then
          XMID(I,J,1) = XEDGE(I,J,1) + ( DLON / 2.0_rk_hco )
        end if
        if ( YMID(I,J,1) == HCO_MISSVAL ) then
          YMID(I,J,1) = YEDGE(I,J,1) + ( DLAT / 2.0_rk_hco )
        end if

        ! Get sine of latitude edges
        YDGR        = PI_180 * YEDGE(I,J,1)  ! radians
        YSN         = SIN( YDGR )            ! sine
        YSIN(I,J,1) = YSN

        ! Eventually set uppermost edge
        if ( I == NX ) then
          if ( XEDGE(I+1,J,1) == HCO_MISSVAL ) then
            XEDGE(I+1,J,1) = XMIN + I * DLON
          end if
        end if
        if ( J == NY ) then
          if ( YEDGE(I,J+1,1) == HCO_MISSVAL ) then
            YEDGE(I,J+1,1) = YMIN + J * DLAT
          end if
          YDGR           = PI_180 * YEDGE(I,J+1,1)  ! radians
          YSN            = SIN( YDGR )              ! sine
          YSIN(I,J+1,1)  = YSN
        end if

      end do
    end do

    ! Calculate grid box areas. Follow calculation from grid_mod.F90
    ! of GEOS-Chem.
    do J = 1, NY

      ! delta latitude
      SIN_DELTA = YSIN(1,J+1,1) - YSIN(1,J,1)

      ! Grid box area.
      AM2 = DLON * PI_180 * HcoState%Phys%Re**2 * SIN_DELTA

      ! Pass to array
      AREA_M2(:,J,1) = AM2

    end do

    ! Set grid dimensions
    HcoState%NX = NX
    HcoState%NY = NY
    HcoState%NZ = NZ

    ! Vertical grid definition
    if ( ANY(AP/=HCO_MISSVAL) ) then
      call HCO_VertGrid_Define( HcoState%Config, &
        HcoState%Grid%zGrid, NZ, &
        Ap=Ap, Bp=Bp, RC=RC )
    else
      call HCO_VertGrid_Define( HcoState%Config, &
        HcoState%Grid%zGrid, NZ, RC=RC )
    end if
    if ( RC /= HCO_SUCCESS ) return

    ! Set pointers to grid variables
    HcoState%Grid%XMID%Val       => XMID   (:,:,1)
    HcoState%Grid%YMID%Val       => YMID   (:,:,1)
    HcoState%Grid%XEDGE%Val      => XEDGE  (:,:,1)
    HcoState%Grid%YEDGE%Val      => YEDGE  (:,:,1)
    HcoState%Grid%YSIN%Val       => YSIN   (:,:,1)
    HcoState%Grid%AREA_M2%Val    => AREA_M2(:,:,1)
    HcoState%Grid%PBLHEIGHT%Val  => PBL_M

    ! Define a default PBL height
    CALL HCO_SetPBLm( HcoState = HcoState, &
      FldName  ='PBL_HEIGHT', &
      PBLM     = HcoState%Grid%PBLHEIGHT%Val, &
      DefVal   = 1000.0_rk_hco, &
      RC       = RC )

    ! The pressure edges and grid box heights are obtained from
    ! an external file in ExtState_SetFields
    HcoState%Grid%PEDGE%Val      => NULL()
    HcoState%Grid%BXHEIGHT_M%Val => NULL()
    HcoState%Grid%ZSFC%Val       => NULL()
    HcoState%Grid%PSFC%Val       => NULL()

    ! Write grid information to log-file
    write(Msg,*) 'HEMCO grid definitions:'
    call HCO_MSG(HcoState%Config%Err,MSG)

    write(MSG,*) ' --> Number of longitude cells: ', NX
    call HCO_MSG(HcoState%Config%Err,MSG)
    write(MSG,*) ' --> Number of latitude cells : ', NY
    call HCO_MSG(HcoState%Config%Err,MSG)
    write(MSG,*) ' --> Number of levels         : ', NZ
    call HCO_MSG(HcoState%Config%Err,MSG)
    write(MSG,*) ' --> Lon range [deg E]        : ', XMIN, XMAX
    call HCO_MSG(HcoState%Config%Err,MSG)
    write(MSG,*) ' --> Lat range [deg N]        : ', YMIN, YMAX
    call HCO_MSG(HcoState%Config%Err,MSG)

    ! Cleanup
    if ( allocated(AP) ) deallocate(AP)
    if ( allocated(BP) ) deallocate(BP)

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine hco_set_grid

  subroutine hco_read_time( HcoState, RC )
    !
    ! !USES:
    !
    use HCO_inquireMod,  only : findfreeLUN
    use HCO_Extlist_Mod, only : HCO_GetOpt, GetExtOpt, CoreNr
    use HCO_CharTools_Mod, only : NextCharPos, GetNextLine
    !
    ! !INPUT PARAMETERS:
    !
    type(HCO_State), pointer       :: HcoState
    !
    ! !INPUT/OUTPUT PARAMETERS
    !
    integer,         intent(inout) :: RC          ! Success or failure?
    !
    ! !REVISION HISTORY:
    !  13 Sep 2013 - C. Keller - Initial Version
    !EOP
    !------------------------------------------------------------------------------
    !BOC
    !
    ! LOCAL VARIABLES:
    !
    ! Scalars
    integer             :: AS, IOS, IU_FILE
    integer             :: I,  N,   LNG, LOW
    logical             :: EOF, FOUND

    ! Strings
    character(len=  1)  :: COL
    character(len=255)  :: ErrMsg, ThisLoc, DUM
    character(len=255)  :: MyTimeFile

    !=================================================================
    ! READ_TIME begins here
    !=================================================================

    ! Initialize
    RC      = HCO_SUCCESS
    ErrMsg  = ''
    ThisLoc = &
      'READ_TIME (in module HEMCO/Standalone/hcoi_standalone_mod.F90)'

    ! Try to get TimeFile from configuration file (in settings)
    call GetExtOpt ( HcoState%Config, CoreNr, 'TimeFile', &
      OptValChar=MyTimeFile,   Found=FOUND, RC=RC )
    if ( RC /= HCO_SUCCESS ) then
      ErrMsg = 'Error encountered in routine "Hco_Run"!'
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if
    if ( FOUND ) TimeFile = MyTimeFile

    ! Find a free file LUN
    IU_FILE = findFreeLUN()

    ! Write colon character to local variable
    COL = HCO_GetOpt( HcoState%Config%ExtList, 'Colon' )

    ! Open time file
    OPEN( IU_FILE, FILE=trim(TimeFile), STATUS='OLD', IOSTAT=IOS )
    if ( IOS /= 0 ) then
      ErrMsg = 'Error 1 reading ' // trim(TimeFile)
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if

    ! Read start and end of simulation
    do N = 1,2

      call GetNextLine( IU_FILE, DUM, EOF, RC )
      if ( RC /= HCO_SUCCESS .OR. EOF ) then
        ErrMsg = 'Error reading time in ' // trim(TimeFile)
        call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
        return
      end if

      ! Remove 'BEGIN: ' or 'end: ' at the beginning
      LNG = len(trim(DUM))
      LOW = NextCharPos ( trim(DUM), COL, 1 )
      if ( LOW < 0 .OR. LOW == LNG ) then
        ErrMsg = 'Cannot extract index after colon: ' // trim(DUM)
        call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
        return
      end if
      LOW = LOW + 1
      DUM = ADJUSTL(DUM(LOW:LNG))
      LNG = len(trim(DUM))

      ! Times have to be stored as:
      ! YYYY-MM-DD HH:MM:SS
      ! --> read year from position 1:4, month from 6:7, etc.
      if ( LNG /= 19 ) then
        ErrMsg = 'Provided time stamp is not `YYYY-MM-DD HH:MM:SS`! ' // &
          trim(DUM)
        call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
        return
      end if

      READ ( DUM( 1: 4), * ) T_YRS(N)
      READ ( DUM( 6: 7), * ) T_MTS(N)
      READ ( DUM( 9:10), * ) T_DYS(N)
      READ ( DUM(12:13), * ) T_HRS(N)
      READ ( DUM(15:16), * ) T_MNS(N)
      READ ( DUM(18:19), * ) T_SCS(N)

    end do !I

    ! Get emission time step
    call GetNextLine( IU_FILE, DUM, EOF, RC )
    if ( (RC /= HCO_SUCCESS) .OR. EOF ) then
      ErrMsg = 'Cannot read emission time step from ' // trim(TimeFile)
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if

    ! Get index after colon
    LNG = len(trim(DUM))
    LOW = NextCharPos ( trim(DUM), COL, 1 )
    if ( LOW < 0 .OR. LOW == LNG ) then
      ErrMsg = 'Cannot extract index after colon: ' // trim(DUM)
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if
    LOW = LOW + 1
    READ( DUM(LOW:LNG), * ) HcoState%TS_EMIS

    ! Set same chemical and dynamic time step
    HcoState%TS_CHEM = HcoState%TS_EMIS
    HcoState%TS_DYN  = HcoState%TS_EMIS

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine hco_read_time

  !-----------------------------------------------------------------------------
  ! NEXUS methods

  function nxs_reset_hco_grid( HcoState, rc ) result ( grid )

    type(HCO_State),   pointer     :: HcoState
    integer, optional, intent(out) :: rc

    type(ESMF_Grid) :: grid

    ! -- local variables
    integer :: localrc, stat
    integer :: item, s
    integer :: ie, ux, uy
    integer :: nx, ny
    integer, dimension(2) :: lb, ub
    real(ESMF_KIND_R8), pointer :: fp(:,:)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (HcoState%Grid%XEDGE%Val(1,1) + 360. == &
      HcoState%Grid%XEDGE%Val(HcoState%NX,1)) then

      grid = ESMF_GridCreate1PeriDim( &
        maxIndex = (/ HcoState % NX, HcoState % NY /), &
        coordSys = ESMF_COORDSYS_SPH_DEG, &
        indexflag= ESMF_INDEX_GLOBAL, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

    else

      grid = ESMF_GridCreateNoPeriDim( &
        maxIndex = (/ HcoState % NX, HcoState % NY /), &
        coordSys = ESMF_COORDSYS_SPH_DEG, &
        indexflag= ESMF_INDEX_GLOBAL, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

    end if

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
        if      (staggerList(s) == ESMF_STAGGERLOC_CENTER) then
          nx = ub(1) - lb(1) + 1
          ny = ub(2) - lb(2) + 1
          select case (item)
           case (1)
            ! -- reset HEMCO center longitudes
            nullify(HcoState % Grid % XMID % Val)
            allocate(HcoState % Grid % XMID % Val(nx,ny), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Unable to allocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            HcoState % Grid % XMID % Alloc = .true.
            HcoState % Grid % XMID % Val = XMID(lb(1):ub(1),lb(2):ub(2),1)
            deallocate(XMID, stat=stat)
            if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
              msg="Unable to deallocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            fp(lb(1):ub(1),lb(2):ub(2)) = HcoState % Grid % XMID % Val
           case (2)
            ! -- reset HEMCO center latitudes
            nullify(HcoState % Grid % YMID % Val)
            allocate(HcoState % Grid % YMID % Val(nx,ny), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Unable to allocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            HcoState % Grid % YMID % Alloc = .true.
            HcoState % Grid % YMID % Val = YMID(lb(1):ub(1),lb(2):ub(2),1)
            deallocate(YMID, stat=stat)
            if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
              msg="Unable to deallocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            fp(lb(1):ub(1),lb(2):ub(2)) = HcoState % Grid % YMID % Val
          end select
        else if (staggerList(s) == ESMF_STAGGERLOC_CORNER) then
          ux = min(ub(1), HcoState % NX)
          nx = ux - lb(1) + 1
          uy = min(ub(2), HcoState % NY)
          ny = uy - lb(2) + 1
          select case (item)
           case (1)
            ! -- reset HEMCO edge longitudes
            nullify(HcoState % Grid % XEDGE % Val)
            allocate(HcoState % Grid % XEDGE % Val(nx+1,ny), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Unable to allocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            HcoState % Grid % XEDGE % Alloc = .true.
            HcoState % Grid % XEDGE % Val = XEDGE(lb(1):ux+1,lb(2):uy,1)
            fp(lb(1):ub(1),lb(2):uy) = XEDGE(lb(1):ub(1),lb(2):uy,1)
            ! -- fill missing edge points
            do ie = uy + 1, ub(2)
              fp(lb(1):ub(1),ie) = fp(lb(1):ub(1),uy)
            end do
            deallocate(XEDGE, stat=stat)
            if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
              msg="Unable to deallocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
           case (2)
            ! -- reset HEMCO edge latitudes
            nullify(HcoState % Grid % YEDGE % Val)
            allocate(HcoState % Grid % YEDGE % Val(nx,ny+1), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Unable to allocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
            HcoState % Grid % YEDGE % Alloc = .true.
            HcoState % Grid % YEDGE % Val = YEDGE(lb(1):ux,lb(2):uy+1,1)
            fp(lb(1):ux,lb(2):ub(2)) = YEDGE(lb(1):ux,lb(2):ub(2),1)
            ! -- fill missing edge points
            do ie = ux + 1, ub(1)
              fp(ie,lb(2):ub(2)) = fp(ux,lb(2):ub(2))
            end do
            deallocate(YEDGE, stat=stat)
            if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
              msg="Unable to deallocate memory", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) return  ! bail out
          end select
        end if
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

    ! -- reset HEMCO grid items
    nx = ub(1) - lb(1) + 1
    ny = ub(2) - lb(2) + 1
    nullify(HcoState % Grid % AREA_M2 % Val)
    nullify(HcoState % Grid % YSIN % Val)
    allocate(HcoState % Grid % AREA_M2 % Val(nx,ny), &
      HcoState % Grid % YSIN % Val(nx,ny+1), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    HcoState % Grid % AREA_M2 % Alloc = .true.
    HcoState % Grid % YSIN    % Alloc = .true.
    HcoState % Grid % AREA_M2 % Val = AREA_M2(lb(1):ub(1),lb(2):ub(2),1)
    HcoState % Grid % YSIN    % Val = YSIN   (lb(1):ub(1),lb(2):ub(2)+1,1)
    deallocate(AREA_M2, YSIN, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to deallocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    fp(lb(1):ub(1),lb(2):ub(2)) = HcoState % Grid % AREA_M2 % Val

    ! -- reset HEMCO grid size
    HcoState % NX = size(HcoState % Grid % XMID % Val, dim=1)
    HcoState % NY = size(HcoState % Grid % XMID % Val, dim=2)

  end function nxs_reset_hco_grid

  subroutine nxs_grid_write( grid, fileName, rc )
    type(ESMF_Grid)                :: grid
    character(len=*),  intent(in)  :: fileName
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer          :: localrc
    integer          :: item
    type(ESMF_Array) :: array

    character(len=*), parameter :: vNames(2) = (/ "lon", "lat" /)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    do item = 1, 2
      call ESMF_GridGetCoord(grid, item, array=array, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out
      call ESMF_ArrayWrite(array, fileName, variableName=vNames(item), &
        overwrite=.true., iofmt=ESMF_IOFMT_NETCDF, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out
    end do

  end subroutine nxs_grid_write

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

    print "('Input grid nc fn:', x, a)", trim(filename)
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

  subroutine nxs_diag_state_init( HcoGrid, HcoState, DiagState, rc )
    use HCO_TYPES_MOD, only: DiagnCont  ! diagnostics container
    use HCO_Diagn_Mod, only: Diagn_Get
    use NEXUS_Error_Mod, only: NEXUS_Error_Log

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
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    do while (flag == HCO_SUCCESS)
      print "('Initializing Diag variable ''', a, '''')", trim(thisDiagn%cName)
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

      call ESMF_StateAdd( DiagState, (/ field /), rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call Diagn_Get( HcoState, EOI, thisDiagn, flag, localrc )
      if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
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
      print "('Initializing Expt variable ''', a, '''')", trim(itemNameList(item))
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
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
         case (3)
          call ESMF_FieldGet( srcfield, ungriddedLBound=lb, ungriddedUBound=ub, &
            rc=localrc )
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
          dstfield = ESMF_FieldCreate( grid, typekind, name=itemNameList(item), &
            ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc )
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
        end select

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

end module nexus_cap
