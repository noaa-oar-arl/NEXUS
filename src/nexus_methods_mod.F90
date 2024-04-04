module NEXUS_Methods_Mod

  use ESMF

  use HCO_Error_Mod
  use HCO_Diagn_Mod
  use HCO_CharTools_Mod
  use HCO_Types_Mod
  use HCOX_State_Mod,      only : Ext_State
  use HCO_State_Mod,       only : HCO_State

  use NEXUS_Error_Mod

  implicit none

  type(ESMF_Grid)        :: HCO_Grid
  type(ESMF_Grid)        :: NXS_Grid
  type(ESMF_State)       :: NXS_Diag_State
  type(ESMF_State)       :: NXS_Expt_State
  type(ESMF_RouteHandle) :: NXS_RouteHandle

  logical :: do_Regrid = .false.
  logical :: do_Debug  = .false.
  logical :: do_NEXUS  = .false.
  logical :: alwaysWriteRestartFile = .false.


  ! Default values for HEMCO input files: contain definitions of
  ! species, grid, and time settings, etc.
  character(len=255)             :: GridFile  = 'HEMCO_sa_Grid'
  character(len=255)             :: SpecFile  = 'HEMCO_sa_Spec'
  character(len=255)             :: TimeFile  = 'HEMCO_sa_Time'
  character(len=255)             :: DiagFile  = 'NEXUS_Diag.nc'
  character(len=255)             :: ExptFile  = 'NEXUS_Expt.nc'

  ! HEMCO state
  type(HCO_State),       pointer :: HcoState  => NULL()

  ! HEMCO extensions state
  type(Ext_State),       pointer :: ExtState  => NULL()

  ! HEMCO config object
  type(ConfigObj),       pointer :: HcoConfig => NULL()

  ! Pointers used during initialization (for species matching)
  integer                        :: nHcoSpec
  character(len= 31),    pointer :: HcoSpecNames       (:) => NULL()
  integer                        :: nModelSpec
  character(len= 31),    pointer :: ModelSpecNames     (:) => NULL()
  integer,               pointer :: ModelSpecIDs       (:) => NULL()
  real(hp),              pointer :: ModelSpecMW        (:) => NULL()
  real(hp),              pointer :: ModelSpecK0        (:) => NULL()
  real(hp),              pointer :: ModelSpecCR        (:) => NULL()
  real(hp),              pointer :: ModelSpecPKA       (:) => NULL()
  integer,               pointer :: matchidx           (:) => NULL()

  ! Start and end time of simulation
  integer                        :: YRS(2), MTS(2), DYS(2)
  integer                        :: HRS(2), MNS(2), SCS(2)

  ! Grid
  real(hp), allocatable, target  :: XMID   (:,:,:)
  real(hp), allocatable, target  :: YMID   (:,:,:)
  real(hp), allocatable, target  :: XEDGE  (:,:,:)
  real(hp), allocatable, target  :: YEDGE  (:,:,:)
  real(hp), allocatable, target  :: YSIN   (:,:,:)
  real(hp), allocatable, target  :: AREA_M2(:,:,:)
  real(hp), allocatable, target  :: PBL_M  (:,:)

  ! MAXIT is the maximum number of run calls allowed
  integer, parameter             :: MAXIT = 100000

  integer, parameter             :: rootPet = 0

  type(ESMF_StaggerLoc), parameter :: staggerList(2) = &
    (/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /)

  interface NEXUS_Initialize
    module procedure Init
  end interface

  interface NEXUS_Run
    module procedure Run
  end interface

  interface NEXUS_Finalize
    module procedure Finalize
  end interface

  private

  public :: rootPet

  public :: NEXUS_Initialize
  public :: NEXUS_Run
  public :: NEXUS_Finalize

contains

  subroutine Init( ConfigFile, ReGridFile, OutputFile, debugLevel, writeRestart, rc )

    use HCO_Config_Mod,    only : Config_ReadFile
    use HCO_State_Mod,     only : HcoState_Init
    use HCO_Driver_Mod,    only : HCO_Init
    use HCOX_Driver_Mod,   only : HCOX_Init
    use HCO_EXTLIST_Mod,   only : GetExtOpt, CoreNr

    character(len=*),  intent(in)  :: ConfigFile
    character(len=*),  intent(in)  :: ReGridFile
    character(len=*),  intent(in)  :: OutputFile
    integer,           intent(in)  :: debugLevel
    logical,           intent(in)  :: writeRestart
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: localPet
    integer :: nnMatch
    logical :: am_I_Root
    logical :: Dum, Found
    character(len=ESMF_MAXSTR) :: msgString
    type(ESMF_VM) :: vm

    ! -- begin
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
    alwaysWriteRestartFile = writeRestart

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
    call Set_Grid ( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Set_Grid"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !-----------------------------------------------------------------------
    ! Create NEXUS grid and reset HEMCO grid as distributed
    if (do_NEXUS) then
      HCO_Grid = HCO_GridCreate( HcoState, rc=localrc )
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
    call Read_Time( HcoState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Read_Time"!', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    !=======================================================================
    ! Set misc. parameter
    !=======================================================================

    ! Set ESMF flag
    HcoState%Options%isESMF = .FALSE.

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
    call HCOX_Init( HcoState, ExtState, localrc )
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

      call NXS_DiagState_Init( HCO_Grid, HcoState, NXS_Diag_State, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out
    end if

!   if (do_Debug) then
!     call GridWrite( HCO_Grid, DiagFile, rc=localrc )
!     if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__,  &
!        file=__FILE__,  &
!        rcToReturn=rc)) return  ! bail out
!   end if

    if (do_Regrid) then
      NXS_Grid = GridCreate_GridSpec( ReGridFile, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out

      NXS_Expt_State = ESMF_StateCreate( rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out

      call NXS_ExptState_Init( NXS_Grid, NXS_Diag_State, NXS_Expt_State, rc=localrc )
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) return  ! bail out

!     call GridWrite( NXS_Grid, ExptFile, rc=localrc )
!     if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__,  &
!        file=__FILE__,  &
!        rcToReturn=rc)) return  ! bail out
    end if


  end subroutine Init


  subroutine Run( RC )

    use HCO_FluxArr_Mod, only : HCO_FluxarrReset
    use HCO_Clock_Mod,   only : HcoClock_Set
    use HCO_Clock_Mod,   only : HcoClock_Get
    use HCO_Clock_Mod,   only : HcoClock_Increase
    use HCO_Driver_Mod,  only : HCO_RUN
    use HCOX_Driver_Mod, only : HCOX_RUN

    integer, optional, intent(out) :: RC         ! Failure or success

    ! -- local variables
    integer            :: localrc
    integer            :: CNT
    integer            :: YR, MT, DY, HR, MN, SC
    character(len=255) :: Msg

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! Time step counter
    CNT = 0

    ! Do until end of simulation
    do while ( CNT < MAXIT )

       ! Increase counter by one
       CNT = CNT + 1

       !====================================================================
       ! Set HcoClock. On first call, use specified start date.
       ! Increase clock by one emission time step otherwise.
       !====================================================================
       if ( CNT == 1 ) then
          call HcoClock_Set ( HcoState,  YRS(1), MTS(1), &
                              DYS(1),    HRS(1), MNS(1), SCS(1), &
                              IsEmisTime=.TRUE., RC=localrc)
          if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HcoClock_Set"!', &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
       else
          call HcoClock_Increase ( HcoState, HcoState%TS_EMIS, .TRUE., RC=localrc )
          if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HcoClock_Increase"!', &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
       end if

       ! Get current time
       call HcoClock_Get ( HcoState%Clock, cYYYY=YR, &
                           cMM=MT, cDD=DY, cH=HR, cM=MN, cS=SC, RC=localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HcoClock_Get"!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       ! Leave loop if this is the end of the simulation
       if ( IsEndOfSimulation(YR,MT,DY,HR,MN,SC) ) EXIT

       ! Write to logfile and standard output (skip for dry-run)
       write( Msg, 100 ) YR, MT, DY, HR, MN, SC
100    FORMAT( 'Calculate emissions at ', i4,  '-', i2.2 ,'-', i2.2,' ',  &
                                          i2.2,':', i2.2, ':', i2.2      )
       call ESMF_LogWrite( Msg )
       write(*,*) trim( MSG )

       ! ================================================================
       ! Reset all emission and deposition values
       ! ================================================================
       call HCO_FluxArrReset( HcoState, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCO_FluxArrReset"!', &
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
       HcoState%Options%SpcMax = nModelSpec
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
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Hco_Run", phase 1!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       ! Phase 2: Compute emissions (skip for dry-run)
       call HCO_Run( HcoState, 2, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Hco_Run", phase 2!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       ! ================================================================
       ! Run HCO extensions
       ! ================================================================

       ! Set ExtState fields (skip for dry-run)
       call ExtState_SetFields ( HcoState, ExtState, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "ExtState_SetFields"!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       ! Update ExtState fields (skip for dry-run)
       call ExtState_UpdateFields( HcoState, ExtState, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "ExtState_Update_Fields"!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       ! Execute all enabled emission extensions. Emissions will be
       ! added to corresponding flux arrays in HcoState.
       call HCOX_Run ( HcoState, ExtState, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCOX_Run"!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       !=================================================================
       ! Update all autofill diagnostics (skip for dry-run)
       !=================================================================
       call HcoDiagn_AutoUpdate ( HcoState, localrc )
       if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCOX_AutoUpdate"!', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return

       !=================================================================
       ! Update NEXUS Diagnostic state
       !=================================================================
       if (do_NEXUS) then
         call NXS_DiagState_Update( HcoState, NXS_Diag_State, rc=localrc )
         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
       end if

       !=================================================================
       ! Write NEXUS Diagnostic state
       !=================================================================
       if (do_Debug) then
         call StateWrite( NXS_Diag_State, DiagFile, timeSlice=CNT, rc=localrc )
         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
       end if

       if (do_Regrid) then
         !=================================================================
         ! Update NEXUS Export state
         !=================================================================
         call NXS_ExptState_Update( NXS_Diag_State, NXS_Expt_State, rc=localrc )
         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out

         !=================================================================
         ! Write NEXUS Export state
         !=================================================================
         call StateWrite( NXS_Expt_State, ExptFile, timeSlice=CNT, rc=localrc )
         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
       end if

    end do

    ! Set iteration limit to avoid infinite runs
    if ( CNT == MAXIT ) then
      call ESMF_LogSetError(ESMF_RC_TIMEOUT, &
        msg='Counter limit reached - Increase MAXIT if you don`t like that!', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return
    end if


  end subroutine Run


  subroutine Finalize( rc )

    use HCO_Driver_Mod,  only : HCO_Final
    USE HCOIO_DIAGN_MOD, only : HcoDiagn_Write
    use HCOX_Driver_Mod, only : HCOX_Final
    use HCO_State_Mod,   only : HcoState_Final

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    logical :: isCreated

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (do_NEXUS .and. alwaysWriteRestartFile) then
      call HcoDiagn_Write( HcoState, .TRUE.,  localrc )
      if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HcoDiagn_Write"!', &
         line=__LINE__, &
         file=__FILE__, &
         rcToReturn=rc)) return
    end if

    ! Cleanup HCO core
    call HCO_FINAL( HcoState, .FALSE., localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCO_Final"!', &
       line=__LINE__, &
       file=__FILE__, &
       rcToReturn=rc)) return

    ! Cleanup extensions and ExtState object
    ! This will also nullify all pointer to the met fields.
    call HCOX_FINAL( HcoState, ExtState, localrc )
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "HCOX_Final"!', &
       line=__LINE__, &
       file=__FILE__, &
       rcToReturn=rc)) return

    ! Cleanup diagnostics (skip if dry-run)
    call DiagnBundle_Cleanup( HcoState%Diagn )

    ! Deallocate module arrays/pointers
    if ( allocated( XMID    ) ) deallocate ( XMID    )
    if ( allocated( YMID    ) ) deallocate ( YMID    )
    if ( allocated( XEDGE   ) ) deallocate ( XEDGE   )
    if ( allocated( YEDGE   ) ) deallocate ( YEDGE   )
    if ( allocated( YSIN    ) ) deallocate ( YSIN    )
    if ( allocated( AREA_M2 ) ) deallocate ( AREA_M2 )
    if ( allocated( PBL_M   ) ) deallocate ( PBL_M   )

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
      call StateFinalize(NXS_Diag_State, rc=localrc)
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
      call StateFinalize(NXS_Expt_State, rc=localrc)
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

  end subroutine Finalize
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Model_GetSpecies
!
! !DESCRIPTION: subroutine Model\_GetSpecies returns 'model' species
! information from the HEMCO standalone input file.
!\\
!\\
! !INTERFACE:
!
  subroutine Model_GetSpecies( HcoConfig,                          &
                               nModelSpec,     ModelSpecNames,     &
                               ModelSpecIDs,   ModelSpecMW,        &
                               ModelSpecK0,    ModelSpecCR,        &
                               ModelSpecPKA,   RC                   )
!
! !USES:
!
    use HCO_inquireMod,  only : findfreeLUN
    use HCO_EXTLIST_Mod, only : GetExtOpt, CoreNr
!
! !OUTPUT PARAMETERS:
!
    type(ConfigObj),    pointer     :: HcoConfig
    integer,            intent(OUT) :: nModelSpec
    character(len= 31), pointer     :: ModelSpecNames     (:)
    integer,            pointer     :: ModelSpecIDs       (:)
    real(hp),           pointer     :: ModelSpecMW        (:)
    real(hp),           pointer     :: ModelSpecK0        (:)
    real(hp),           pointer     :: ModelSpecCR        (:)
    real(hp),           pointer     :: ModelSpecPKA       (:)
    integer,            intent(OUT) :: RC
!
! !REVISION HISTORY:
!  13 Sep 2013 - C. Keller - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    integer             :: I, N, LNG, LOW, UPP
    integer             :: IU_FILE, IOS
    logical             :: FOUND,   EOF
    character(len=255)  :: MSG, LOC
    character(len=255)  :: MySpecFile
    character(len=2047) :: DUM

    !=================================================================
    ! Model_GetSpecies begins here
    !=================================================================

    ! For error handling
    LOC = 'Model_GetSpecies (hcoi_standalone_mod.F90)'

    ! Try to get SpecFile from configuration file (in settings)
    call GetExtOpt ( HcoConfig, CoreNr, 'SpecFile', &
                     OptValChar=MySpecFile,   Found=FOUND, RC=RC )
    !if ( RC /= HCO_SUCCESS ) return
    if ( FOUND ) then
       SpecFile = MySpecFile
    else
       MSG = 'Please provide filename with species definitions ' // &
             'in the configuration file settings, e.g. ' // &
             'SpecFile: MySpecies.rc'
       call HCO_Error ( HcoConfig%Err, MSG, RC, THISLOC=LOC )
       return
    end if

    ! Find a free file LUN
    IU_FILE = findFreeLUN()

    ! Open spec file
    OPEN( IU_FILE, FILE=trim(SpecFile), STATUS='OLD', IOSTAT=IOS )
    if ( IOS /= 0 ) then
       MSG = 'Error 1 reading ' // trim(SpecFile)
       call HCO_Error( HcoConfig%Err, MSG, RC, THISLOC=LOC )
       return
    end if

    ! Get number of species
    nModelSpec = 0
    do
       call GetNextLine( IU_FILE, DUM, EOF, RC )
       if ( EOF               ) EXIT
       if ( RC /= HCO_SUCCESS ) then
          MSG = 'Error encountered in reading SpecFile!.  Please ' // &
                'doublecheck that all species information has '    // &
                'been correctly entered.'
          call HCO_Error ( HcoConfig%Err, MSG, RC, THISLOC=LOC )
       end if
       nModelSpec = nModelSpec + 1
    end do

    ! Make sure we have one species
    if ( nModelSpec == 0 ) then
       MSG = 'Species file ' // trim(SpecFile)      // &
             ' does not seem to have any content. ' // &
             'You must define at least one species.'
       call HCO_Error( HcoConfig%Err, MSG, RC, THISLOC=LOC )
    end if

    ! Go back to line one
    REWIND( IU_FILE )

    ! Get next valid line
!    call GetNextLine( IU_FILE, DUM, EOF, RC )
!    if ( RC /= HCO_SUCCESS .OR. EOF ) then
!       MSG = 'Error 2 reading ' // trim(SpecFile)
!       call HCO_Error( MSG, RC, THISLOC=LOC )
!       return
!    end if
!
!    LNG = len(trim(DUM))
!    LOW = NextCharPos ( trim(DUM), HCO_COL(), 1 )
!    if ( LOW < 0 .OR. LOW == LNG ) then
!       MSG = 'Cannot extract index after colon: ' // trim(DUM)
!       call HCO_Error( MSG, RC, THISLOC=LOC )
!       return
!    end if
!    LOW = LOW + 1
!    READ ( DUM(LOW:LNG), * ) nModelSpec

    ! Allocate species arrays
    allocate(ModelSpecNames     (nModelSpec))
    allocate(ModelSpecIDs       (nModelSpec))
    allocate(ModelSpecMW        (nModelSpec))
    allocate(ModelSpecK0        (nModelSpec))
    allocate(ModelSpecCR        (nModelSpec))
    allocate(ModelSpecPKA       (nModelSpec))

    ! Assign variables to each species
    do N = 1, nModelSpec

       call GetNextLine( IU_FILE, DUM, EOF, RC )
       if ( RC /= HCO_SUCCESS .OR. EOF ) then
          write(MSG,100) N, trim(SpecFile)
          call HCO_Error( HcoConfig%Err, MSG, RC, THISLOC=LOC )
          return
       end if

       ! Start reading line from beginning
       LNG = len(trim(DUM))
       LOW = 0

       ! Read species ID, name, molecular weight, emitted molecular weight,
       ! molecular coefficient, and Henry coefficients K0, CR, pKa (in this
       ! order).
       do I = 1, 8

          ! Get lower and upper index of species ID (first entry in row).
          ! Skip all leading spaces.
          UPP = LOW

          do WHILE( UPP == LOW .AND. LOW /= LNG )
             LOW = LOW + 1
             if ( LOW > LNG ) then
                write(MSG,101) I, trim(DUM)
                call HCO_Error( MSG, RC, THISLOC=LOC )
                return
             end if
             UPP = NextCharPos( trim(DUM), HCO_SPC, LOW )
             if ( UPP < 0 ) UPP = LNG
          end do

          if ( I < 8 ) then
             UPP = UPP - 1 ! Don't read space
          end if

          ! Error check
          if ( UPP > LNG ) then
             write(MSG,*) 'Error reading species property ', I, &
                          ' on line ', trim(DUM), '. Each ', &
                          'species definition line is expected ', &
                          'to have 8 entries (ID, Name, MW, MWemis, ', &
                          'MolecRatio, K0, CR, PKA, e.g.: ', &
                          '1 CO   28.0 28.0 1.0 0.0 0.0 0.0'
             call HCO_Error ( HcoConfig%Err, MSG, RC, THISLOC=LOC )
             return
          end if

          ! Read into vector
          SELECT CASE ( I )
             CASE ( 1 )
                READ( DUM(LOW:UPP), * ) ModelSpecIDs(N)
             CASE ( 2 )
                READ( DUM(LOW:UPP), * ) ModelSpecNames(N)
             CASE ( 3 )
                READ( DUM(LOW:UPP), * ) ModelSpecMW(N)
             CASE ( 4 )
                ! EmMW - Do nothing
             CASE ( 5 )
                ! MolecRatio - Do nothing
             CASE ( 6 )
                READ( DUM(LOW:UPP), * ) ModelSpecK0(N)
             CASE ( 7 )
                READ( DUM(LOW:UPP), * ) ModelSpecCR(N)
             CASE ( 8 )
                READ( DUM(LOW:UPP), * ) ModelSpecPKA(N)
          end SELECT

          ! Continue from upper position (+1 to skip space). The
          ! while loop at the beginning of the do-loop will advance
          ! low by another one position, so the next character
          ! search will start at position UPP + 2, which is exactly
          ! what we want (UPP is the position BEFORE the space!).
          LOW = UPP + 1

       end do !I
    end do !N

    ! Close file
    CLOSE( IU_FILE )

    ! Make sure that the species indexing starts at 1
    if ( MINVAL( ModelSpecIDs ) /= 1 ) then
       MSG = 'Error encountered in reading SpecFile!.  The species '      // &
             'ID numbers do not start at 1!  Please check SpecFile '      // &
             'for typos.'
       call HCO_Error ( HcoConfig%Err, MSG, RC, THISLOC=LOC )
       return
    end if

    ! Make sure that the ID of the last species is the same as nModelSpec
    if ( MAXVAL( ModelSpecIDs ) /= nModelSpec ) then
       MSG = 'Error encountered in reading SpecFile!.  The ID number '    // &
             'of the last species does not match the number of species '  // &
             'that were read from SpecFile!  Please check SpecFile for '  //&
             'typos.'
       call HCO_Error ( HcoConfig%Err, MSG, RC, THISLOC=LOC )
       return
    end if

    ! Return w/ success
    RC = HCO_SUCCESS

100 FORMAT( 'Error reading species ', i3, ' in ', a )
101 FORMAT( 'Cannot extract element ', i1, ' in ', a )

  end subroutine Model_GetSpecies
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set_Grid
!
! !DESCRIPTION: subroutine SET\_GRID reads the grid information from the
!  HEMCO standalone grid file and sets all HEMCO grid arrays accordingly.
!  The grid file is expected to contain information on the grid edge lon/lat
!  range, as well as the number of grid cells in longitude and latitude
!  direction.
!\\
!\\
! !INTERFACE:
!
  subroutine SET_Grid( HcoState, RC )
!
! !USES:
!
    use HCO_inquireMod,   only  : findFreeLUN
    use HCO_ExtList_Mod,  only  : HCO_GetOpt, GetExtOpt, CoreNr
    use HCO_VertGrid_Mod, only  : HCO_VertGrid_Define
    use HCO_GeoTools_Mod, only  : HCO_SetPBLm
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
    real(hp)              :: XMIN, XMAX
    real(hp)              :: YMIN, YMAX
    real(hp)              :: DVAL
    real(hp)              :: DLON, DLAT
    real(hp)              :: PI_180, YDGR, YSN, SIN_DELTA, AM2
    logical               :: FOUND,   EOF

    ! Arrays
    integer               :: SZ(3)
    real(hp)              :: RG(4)
    real(hp), allocatable :: Ap(:), Bp(:)

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
    PI_180 = HcoState%Phys%PI / 180.0_hp

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
    if ( YMIN < -90.0_hp ) then
       write(ErrMsg,*) 'Lower latitude must be between -90 and 90 degN: ', YMIN
       call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
       return
    end if
    if ( YMAX > 90.0_hp ) then
       write(ErrMsg,*) 'Upper latitude must be between -90 and 90 degN: ', YMAX
       call HCO_Error( HcoState%Config%Err, ErrMsg, RC, THISLOC=ThisLoc )
       return
    end if

    ! ------------------------------------------------------------------
    ! Extract grid size (x,y,z)
    ! The grid sizes are expected to be provided in three separate lines:
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
          XMID(I,J,1) = XEDGE(I,J,1) + ( DLON / 2.0_hp )
       end if
       if ( YMID(I,J,1) == HCO_MISSVAL ) then
          YMID(I,J,1) = YEDGE(I,J,1) + ( DLAT / 2.0_hp )
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
                      DefVal   = 1000.0_hp, &
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

  end subroutine Set_Grid
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Get_nnMatch
!
! !DESCRIPTION: Subroutine Get\_nnMatch returns the number of species
! found in both the HEMCO configuration and the species input file.
!\\
!\\
! !INTERFACE:
!
  subroutine Get_nnMatch( HcoConfig, nnMatch, RC )
!
! !USES:
!
    use HCO_Config_Mod, only : Config_GetnSpecies
    use HCO_Config_Mod, only : Config_GetSpecNames
!
! !OUTPUT PARAMETERS:
!
    integer,         intent(  OUT)  :: nnMatch   ! Number of HEMCO species that are
                                         ! also species in the atm. model
!
! !INPUT/OUTPUT PARAMETERS:
!
    type(ConfigObj), pointer        :: HcoConfig ! Config object
    integer,         intent(inout)  :: RC        ! Success or failure?
!
! !REVISION HISTORY:
!  13 Sep 2013 - C. Keller   - Initial Version
!  18 Jan 2019 - R. Yantosca - Improve error trapping
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    integer            :: AS,     IDX
    character(len=255) :: ErrMsg, ThisLoc

    !=================================================================
    ! Get_nnMatch begins here
    !=================================================================

    ! Initialize
    RC      = HCO_SUCCESS
    ErrMsg  = ''
    ThisLoc = &
     'Get_nnMatch (in module HEMCO/Interfaces/hcoi_standalone_mod.F90)'

    ! Extract number of HEMCO species and corresponding species names
    ! as read from the HEMCO config. file.
    nHcoSpec = Config_GetnSpecies ( HcoConfig )
    call Config_GetSpecNames( HcoConfig, &
                              HcoSpecNames, nHcoSpec, RC )
    if ( RC /= HCO_SUCCESS ) then
       ErrMsg = 'Error encountered in routine "Config_GetSpecNames"!'
       call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
       return
    end if

    ! Extract species to be used from input file
    call Model_GetSpecies( HcoConfig,                           &
                           nModelSpec,     ModelSpecNames,      &
                           ModelSpecIDs,   ModelSpecMW,         &
                           ModelSpecK0,    ModelSpecCR,         &
                           ModelSpecPKA,   RC                    )
    if ( RC /= HCO_SUCCESS ) then
       ErrMsg = 'Error encountered in routine "Model_GetSpecies"!'
       call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
       return
    end if

    ! See how many species are also used in GEOS-Chem
    allocate(matchIDx(nHcoSpec),STAT=AS)
    if ( AS/=0 ) then
       ErrMsg = 'Allocation error matchIDx'
       call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
       return
    end if
    matchIDx(:) = -1
    call HCO_CharMatch( HcoSpecNames,   nHcoSpec,      &
                        ModelSpecNames, nModelSpec,    &
                        matchIDx,       nnMatch         )
    if ( nnMatch == 0 ) then
       ErrMsg = 'HCO_CharMatch returned found matching species!'
       call HCO_Error(HcoConfig%Err, ErrMsg, RC, ThisLoc )
       return
    end if

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine Get_nnMatch
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Register_Species
!
! !DESCRIPTION: Subroutine Register\_Species registers all species in the
!  HEMCO state object.
!\\
!\\
! !INTERFACE:
!
  subroutine Register_Species( HcoState, RC )
!
! !USES:
!
    use HCO_LogFile_Mod, only : HCO_SPEC2LOG
!
! !INPUT/OUTPUT PARAMETERS:
!
    type(HCO_STATE), pointer :: HcoState
    integer, intent(inout)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  13 Sep 2013 - C. Keller - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    integer :: CNT, I, IDX, CID

    !=================================================================
    ! REGISTER_SPECIES begins here
    !=================================================================

    ! Loop over all possible HEMCO species
    cnt = 0
    do I = 1, nHcoSpec

       ! Skip if this HEMCO species is not used in GEOS-Chem
       if ( MatchIDx(I) < 0 ) CYCLE

       ! increase counter: this is the index in HcoState%Spc!
       cnt                        = cnt + 1

       ! Set species name and GEOS-Chem tracer ID
       IDX                        = ModelSpecIDs(MatchIDx(I))
       HcoState%Spc(cnt)%SpcName  = HcoSpecNames(I)
       HcoState%Spc(cnt)%ModID    = IDX

       ! Molecular weights of species
       HcoState%Spc(cnt)%MW_g     = ModelSpecMW(IDX)

       ! Set Henry coefficients
       HcoState%Spc(cnt)%HenryK0  = ModelSpecK0(IDX)
       HcoState%Spc(cnt)%HenryCR  = ModelSpecCR(IDX)
       HcoState%Spc(cnt)%HenryPKA = ModelSpecPKA(IDX)

       ! Logfile I/O
       call HCO_SPEC2LOG( HcoState, Cnt )

    end do !I

    call HCO_MSG(HcoState%Config%Err,SEP1='-')

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine Register_Species
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Define_Diagnostics
!
! !DESCRIPTION: Subroutine Define\_Diagnostics defines all diagnostics to be
!  used in this simulation.
!\\
!\\
! !INTERFACE:
!
  subroutine Define_Diagnostics( HcoState, RC, SetDefault )
!
! !USES:
!
    use HCO_EXTLIST_MOD,   only : GetExtNr
!
! !INPUT PARAMETERS:
!
    type(HCO_STATE),  pointer                   :: HcoState
    logical,          intent(in   ), OPTIONAL   :: SetDefault  ! Define default diagnostics?
!
! !INPUT/OUTPUT PARAMETERS:
!
    integer,          intent(inout)             :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  13 Sep 2013 - C. Keller - Initial Version
!  05 Feb 2015 - C. Keller - Added SetDefault flag
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Scalars
    logical            :: SetDf
    integer            :: I, N, ExtNr, HcoID

    ! Strings
    character(len=31)  :: DiagnName
    character(len=255) :: ErrMsg,   ThisLoc

    !=================================================================
    ! DEFINE_DIAGNOSTICS begins here
    !=================================================================

    ! Initialize
    RC      =  HCO_SUCCESS
    ErrMsg  = ''
    ThisLoc = &
     'DEFINE_DIAGNOSTICS (in module HEMCO/Interfaces/hcoi_standalone_mod.F90'

    ! Get number of diagnostics currently defined in the default
    ! collection
    call DiagnCollection_Get( HcoState%Diagn, &
       HcoState%Diagn%HcoDiagnIDDefault, nnDiagn=N, RC=RC )
    if ( RC /= HCO_SUCCESS ) then
       ErrMsg = 'Error encountered in routine "DiagnCollection_Get"!'
       call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
       return
    end if

    print*, '### Define_Diagnostics: NNDIAGN: ', N

    ! If there are no diagnostics defined yet, define some default
    ! diagnostics below. These are simply the overall emissions
    ! (across all extensions, categories, hierarchies) for each
    ! HEMCO species.
    if ( PRESENT(SetDefault) ) then
       SetDf = SetDefault
    else
       SetDf = ( N == 0 )
    end if
    if ( SetDf ) then

       ! Loop over all HEMCO species
       do I = 1, HcoState%nSpc

          ! Get HEMCO ID
          HcoID = HcoState%Spc(I)%HcoID
          if ( HcoID <= 0 ) CYCLE

          ! Create diagnostics
          DiagnName = 'HEMCO__EMIS_' // trim(HcoState%Spc(I)%SpcName)
          call Diagn_Create ( HcoState,                               &
                              cName     = DiagnName,                  &
                              ExtNr     = -1,                         &
                              Cat       = -1,                         &
                              Hier      = -1,                         &
                              HcoID     = HcoID,                      &
                              SpaceDim  = 3,                          &
                              LevIDx    = -1,                         &
                              OutUnit   = 'kg/m2/s',                  &
                              AutoFill  = 1,                          &
                              COL = HcoState%Diagn%HcoDiagnIDDefault, &
                              OkIfExist = .TRUE.,                     &
                              RC        = RC                           )

          ! Trap potential errors
          if ( RC /= HCO_SUCCESS ) then
             ErrMsg = 'Error defining diagnostic: ' // trim( DiagnName )
             call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
             return
          end if
       end do !I
    end if

    !--------------------------------------------------------------------------
    ! Define some additional diagnostics
    !--------------------------------------------------------------------------
    ExtNr = GetExtNr ( HcoState%Config%ExtList, 'LightNOx' )
    if ( ExtNr > 0 ) then

       ! Loop over lighthing flash quantities
       do I = 1, 3

          ! Pick the proper diagnostic name
          SELECT CASE( I )
             CASE( 1 )
                DiagnName = 'LIGHTNING_TOTAL_FLASHRATE'
             CASE( 2 )
                DiagnName = 'LIGHTNING_INTRACLOUD_FLASHRATE'
             CASE( 3 )
                DiagnName = 'LIGHTNING_CLOUDGROUND_FLASHRATE'
          end SELECT

          ! Define diagnostics ID
          N = 56000 + I

          ! Create diagnostic container
          call Diagn_Create( HcoState,                               &
                             cName     = trim( DiagnName ),          &
                             cID       = N,                          &
                             ExtNr     = ExtNr,                      &
                             Cat       = -1,                         &
                             Hier      = -1,                         &
                             HcoID     = -1,                         &
                             SpaceDim  = 2,                          &
                             LevIDx    = -1,                         &
                             OutUnit   = 'flashes/min/km2',          &
                             OutOper   = 'Mean',                     &
                             COL = HcoState%Diagn%HcoDiagnIDDefault, &
                             AutoFill  = 0,                          &
                             RC        = RC                           )

          ! Trap potential errors
          if ( RC /= HCO_SUCCESS ) then
             ErrMsg = 'Error defining diagnostic: ' // trim( DiagnName )
             call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
             return
          end if
       end do

       ! ----------------------------------------------------------
       ! Diagnostics for convective cloud top height.
       ! ----------------------------------------------------------

       ! Define diagnostics name and ID
       DiagnName = 'LIGHTNING_CLOUD_TOP'
       N         = 56004

       ! Create diagnostic container
       call Diagn_Create( HcoState,                               &
                          cName     = trim( DiagnName ),          &
                          cID       = N,                          &
                          ExtNr     = ExtNr,                      &
                          Cat       = -1,                         &
                          Hier      = -1,                         &
                          HcoID     = -1,                         &
                          SpaceDim  = 2,                          &
                          LevIDx    = -1,                         &
                          OutUnit   = '1',                        &
                          OutOper   = 'Mean',                     &
                          COL = HcoState%Diagn%HcoDiagnIDDefault, &
                          AutoFill  = 0,                          &
                          RC        = RC                           )

       ! Trap potential errors
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Error defining diagnostic: ' // trim( DiagnName )
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          return
       end if

    end if ! Lightning NOx

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine Define_Diagnostics
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Read_Time
!
! !DESCRIPTION: Subroutine READ\_TIME reads the time information for the
!  HEMCO standalone from an input file.
!\\
!\\
! !INTERFACE:
!
  subroutine Read_Time( HcoState, RC )
!
! !USES:
!
    use HCO_inquireMod,  only : findfreeLUN
    use HCO_Extlist_Mod, only : HCO_GetOpt, GetExtOpt, CoreNr
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

       READ ( DUM( 1: 4), * ) YRS(N)
       READ ( DUM( 6: 7), * ) MTS(N)
       READ ( DUM( 9:10), * ) DYS(N)
       READ ( DUM(12:13), * ) HRS(N)
       READ ( DUM(15:16), * ) MNS(N)
       READ ( DUM(18:19), * ) SCS(N)

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

  end subroutine Read_Time
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtState_SetFields
!
! !DESCRIPTION: Subroutine ExtState\_SetFields fills the ExtState data fields
! with data read through the HEMCO configuration file.
!\\
!\\
! !INTERFACE:
!
  subroutine ExtState_SetFields ( HcoState, ExtState, RC )
!
! !USES:
!
    use HCO_ARR_MOD,        only : HCO_ArrAssert
    use HCO_GEOTOOLS_MOD,   only : HCO_GetSUNCOS
    use HCO_GEOTOOLS_MOD,   only : HCO_CalcVertGrid
    use HCOX_STATE_MOD,     only : ExtDat_Set
    use HCO_CLOCK_MOD,      only : HcoClock_First
!
! !INPUT/OUTPUT PARAMETERS
!
    type(HCO_STATE), pointer       :: HcoState
    type(EXT_STATE), pointer       :: ExtState
    integer,         intent(inout) :: RC          ! Success or failure?
!
! !REVISION HISTORY:
!  28 Jul 2014 - C. Keller   - Initial Version
!  06 Oct 2014 - M. Sulprizio- Remove PCENTER. Now calculate from pressure edges
!  09 Jul 2015 - E. Lundgren - Add MODIS Chlorophyll-a (CHLR)
!  26 Oct 2016 - R. Yantosca - Don't nullify local ptrs in declaration stmts
!  15 Jan 2019 - R. Yantosca - Update met field names to be consistent with
!                              those used for the FlexGrid update
!  18 Jan 2019 - R. Yantosca - Improve error trapping
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    ! Scalars
    logical            :: FIRST

    ! Strings
    character(len=255) :: Name, ErrMsg, ThisLoc

    ! Pointers
    real(hp), pointer  :: PSFC    (:,:  )
    real(hp), pointer  :: ZSFC    (:,:  )
    real(hp), pointer  :: TK      (:,:,:)
    real(hp), pointer  :: BXHEIGHT(:,:,:)
    real(hp), pointer  :: PEDGE   (:,:,:)

    !========================================================================
    ! ExtState_SetFields begins here
    !========================================================================

    ! Initialize
    RC       = HCO_SUCCESS
    ErrMsg   = ''
    ThisLoc  = &
     'ExtState_SetFields (in HEMCO/Interfaces/hcoi_standalone_mod.F90'

    ! Nullify pointers
    PSFC     => NULL()
    ZSFC     => NULL()
    TK       => NULL()
    BXHEIGHT => NULL()
    PEDGE    => NULL()

    ! Enter
    call HCO_Enter( HcoState%Config%Err, ThisLoc, RC )
    if ( RC /= HCO_SUCCESS ) then
      ErrMsg = 'Error encountered in "HCO_Enter"!'
      call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
      return
    end if

    ! First call?
    FIRST = HcoClock_First ( HcoState%Clock, .FALSE. )

    !------------------------------------------------------------------------
    ! %%%%% 2D fields %%%%%
    ! (1) Now use the same met field names as are specified in the
    !     the HEMCO_Config.rc file for the "FlexGrid" update
    ! (2) Not all extension fields are used for a given simulation type
    !------------------------------------------------------------------------

    !%%%%% 10-m winds %%%%%
    if ( ExtState%U10M%DoUse ) then
       Name = 'U10M'
       call ExtDat_Set( HcoState,     ExtState%U10M,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                    '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%V10M%DoUse ) then
       Name = 'V10M'
       call ExtDat_Set( HcoState,     ExtState%V10M,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Albedo %%%%%
    if ( ExtState%ALBD%DoUse ) then
       Name = 'ALBEDO'
       call ExtDat_Set( HcoState,     ExtState%ALBD,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Air and skin temperature %%%%%
    if ( ExtState%T2M%DoUse ) then
       Name = 'T2M'
       call ExtDat_Set( HcoState,     ExtState%T2M,                          &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%TSKIN%DoUse ) then
       Name = 'TS'
       call ExtDat_Set( HcoState,     ExtState%TSKIN,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Soil moisture %%%%%
    if ( ExtState%GWETROOT%DoUse ) then
       Name = 'GWETROOT'
       call ExtDat_Set( HcoState,     ExtState%GWETROOT,                     &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%GWETTOP%DoUse ) then
       Name = 'GWETTOP'
       call ExtDat_Set( HcoState,     ExtState%GWETTOP,                      &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Snow fields %%%%%
    if ( ExtState%SNOWHGT%DoUse ) then
       Name = 'SNOMAS'
       call ExtDat_Set( HcoState,     ExtState%SNOWHGT,                      &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%SNODP%DoUse ) then
       Name = 'SNODP'
       call ExtDat_Set( HcoState,     ExtState%SNODP,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
      end if
    end if

    !%%%%% Friction velocity %%%%%
    if ( ExtState%USTAR%DoUse ) then
       Name = 'USTAR'
       call ExtDat_Set( HcoState,     ExtState%USTAR,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Roughness height %%%%%
    if ( ExtState%Z0%DoUse ) then
       Name = 'Z0M'
       call ExtDat_Set( HcoState,     ExtState%Z0,                           &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg , RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Tropopause pressure %%%%%
    if ( ExtState%TROPP%DoUse ) then
       Name = 'TROPPT'
       call ExtDat_Set( HcoState,     ExtState%TROPP,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% PAR direct and diffuse %%%%%
    if ( ExtState%PARDR%DoUse ) then
       Name = 'PARDR'
       call ExtDat_Set( HcoState,     ExtState%PARDR,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%PARDF%DoUse ) then
       Name = 'PARDF'
       call ExtDat_Set( HcoState,     ExtState%PARDF,                        &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%RADSWG%DoUse ) then
       Name = 'SWGDN'
       call ExtDat_Set( HcoState,     ExtState%RADSWG,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Cloud fraction @ surface %%%%%
    if ( ExtState%CLDFRC%DoUse ) then
       Name = 'CLDTOT'
       call ExtDat_Set( HcoState,     ExtState%CLDFRC,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Leaf area index %%%%%
    if ( ExtState%LAI%DoUse ) then
       Name = 'LAI'
       call ExtDat_Set( HcoState,     ExtState%LAI,                          &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Flash density %%%%%
    if ( ExtState%FLASH_DENS%DoUse ) then
       Name = 'FLASH_DENS'
       call ExtDat_Set( HcoState,     ExtState%FLASH_DENS,                   &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Convective depth %%%%%
    if ( ExtState%CONV_DEPTH%DoUse ) then
       Name = 'CONV_DEPTH'
       call ExtDat_Set( HcoState,     ExtState%CONV_DEPTH,                   &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Fractional coverage fields %%%%%
    if ( ExtState%FRCLND%DoUse ) then
       Name = 'FRCLND'
       call ExtDat_Set( HcoState,     ExtState%FRCLND,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%FRLAND%DoUse ) then
       Name = 'FRLAND'
       call ExtDat_Set( HcoState,     ExtState%FRLAND,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%FROCEAN%DoUse ) then
       Name = 'FROCEAN'
       call ExtDat_Set( HcoState,     ExtState%FROCEAN,                      &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%FRLAKE%DoUse ) then
       Name = 'FRLAKE'
       call ExtDat_Set( HcoState,     ExtState%FRLAKE,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%FRLANDIC%DoUse ) then
       Name = 'FRLANDIC'
       call ExtDat_Set( HcoState,     ExtState%FRLANDIC,                     &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Solar zenith angle %%%%%
    if ( ExtState%SZAFACT%DoUse ) then
       Name = 'SZAFACT'
       call ExtDat_Set( HcoState,     ExtState%SZAFACT,                      &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Photolysis values %%%%%
    if ( ExtState%JNO2%DoUse ) then
       Name = 'JNO2'
       call ExtDat_Set( HcoState,     ExtState%JNO2,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
      if ( RC == HCO_SUCCESS ) then
         ErrMsg = 'Could not find quantity "' // trim( Name )             // &
                  '" for the HEMCO standalone simulation!'
         call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
         call HCO_Leave( HcoState%Config%Err, RC )
         return
      end if
   end if

   if ( ExtState%JOH%DoUse ) then
      Name = 'JOH'
      call ExtDat_Set( HcoState,     ExtState%JOH,                           &
                       trim( Name ), RC,       FIRST=FIRST                  )
      if ( RC == HCO_SUCCESS ) then
         ErrMsg = 'Could not find quantity "' // trim( Name )             // &
                  '" for the HEMCO standalone simulation!'
         call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
         call HCO_Leave( HcoState%Config%Err, RC )
         return
      end if
   end if

    !-----------------------------------------------------------------
    ! %%%%% 3D fields %%%%%
    ! (1) Now use the same met field names as are specified in the
    !     the HEMCO_Config.rc file for the "FlexGrid" update
    ! (2) Not all extension fields are used for a given simulation type
    !-----------------------------------------------------------------

    !%%%%% Cloud convection mass flux %%%%%
    if ( ExtState%CNV_MFC%DoUse ) then
       Name = 'CMFMC'
       call ExtDat_Set( HcoState,     ExtState%CNV_MFC,                      &
                        trim( Name ), RC,       FIRST=FIRST,                 &
                        OnLevEdge=.TRUE.                                    )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Specific humidity %%%%%
    if ( ExtState%SPHU%DoUse ) then
       Name = 'SPHU'
       call ExtDat_Set( HcoState,     ExtState%SPHU,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Temperature %%%%%
    if ( ExtState%TK%DoUse ) then
       Name = 'TMPU'
       call ExtDat_Set( HcoState,     ExtState%TK,                           &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Air mass, volume, density etc fields %%%%%
    if ( ExtState%AIR%DoUse ) then
       Name = 'AIR'
       call ExtDat_Set( HcoState,     ExtState%AIR,                          &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%AIRVOL%DoUse ) then
       Name = 'AIRVOL'
       call ExtDat_Set( HcoState,     ExtState%AIRVOL,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%AIRDEN%DoUse ) then
       Name = 'AIRDEN'
       call ExtDat_Set( HcoState,     ExtState%AIRDEN,                       &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Concentration fields %%%%%
    if ( ExtState%O3%DoUse ) then
       Name = 'O3'
       call ExtDat_Set( HcoState,     ExtState%O3,                           &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%NO%DoUse ) then
       Name = 'NO'
       call ExtDat_Set( HcoState,     ExtState%NO,                           &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%NO2%DoUse ) then
       Name = 'NO2'
       call ExtDat_Set( HcoState,     ExtState%NO2,                          &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%HNO3%DoUse ) then
       Name = 'HNO3'
       call ExtDat_Set( HcoState,     ExtState%HNO3,                         &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Deposition fields (for soil NOx) %%%%%
    if ( ExtState%DRY_TOTN%DoUse ) then
       Name = 'DRY_TOTN'
       call ExtDat_Set( HcoState,     ExtState%DRY_TOTN,                     &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    if ( ExtState%WET_TOTN%DoUse ) then
       Name = 'WET_TOTN'
       call ExtDat_Set( HcoState,     ExtState%WET_TOTN,                     &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !%%%%% Fraction of PBL field (for sea exchange only) %%%%%
    if ( ExtState%FRAC_OF_PBL%DoUse ) then
       Name = 'FRAC_OF_PBL'
       call ExtDat_Set( HcoState,     ExtState%FRAC_OF_PBL,                  &
                        trim( Name ), RC,       FIRST=FIRST                 )
       if ( RC == HCO_SUCCESS ) then
          ErrMsg = 'Could not find quantity "' // trim( Name )            // &
                   '" for the HEMCO standalone simulation!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !-----------------------------------------------------------------
    ! ==> DRYCOEFF must be read from the configuration file in module
    !     hcox_soilnox_mod.F90.
    !-----------------------------------------------------------------

    !-----------------------------------------------------------------
    ! Check for vertical grid update. This will try to read the
    ! vertical grid quantities from disk or calculate them from other
    ! quantities read from disk.
    !-----------------------------------------------------------------

    ! Eventually get temperature from disk
    if ( ExtState%TK%DoUse ) TK => ExtState%TK%Arr%Val

    ! Attempt to calculate vertical grid quantities
    call HCO_CalcVertGrid( HcoState, PSFC, ZSFC, TK, BXHEIGHT, PEDGE, RC )
    if ( RC /= HCO_SUCCESS ) return

    ! Reset pointers
    PSFC     => NULL()
    ZSFC     => NULL()
    TK       => NULL()
    BXHEIGHT => NULL()
    PEDGE    => NULL()

    !-----------------------------------------------------------------
    ! If needed, calculate SUNCOS values
    !-----------------------------------------------------------------
    if ( ExtState%SUNCOS%DoUse ) then
       if ( FIRST ) then
          call HCO_ArrAssert( ExtState%SUNCOS%Arr, HcoState%NX, HcoState%NY, RC )
          if ( RC /= HCO_SUCCESS ) then
             ErrMsg = 'SUNCOS array is not the expected dimensions!'
             call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
             call HCO_Leave( HcoState%Config%Err, RC )
             return
          end if
       end if

       call HCO_GetSUNCOS( HcoState, ExtState%SUNCOS%Arr%Val, 0, RC )
       if ( RC /= HCO_SUCCESS ) then
          ErrMsg = 'Error encountered in routine "HCO_GetSuncos"!'
          call HCO_Error( HcoConfig%Err, ErrMsg, RC, ThisLoc )
          call HCO_Leave( HcoState%Config%Err, RC )
          return
       end if
    end if

    !-----------------------------------------------------------------
    ! All done
    !-----------------------------------------------------------------

    ! Not first call any more
    FIRST = .FALSE.

    ! Leave w/ success
    call HCO_Leave( HcoState%Config%Err, RC )

  end subroutine ExtState_SetFields
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: ExtState_UpdateFields
!
! !DESCRIPTION: Subroutine ExtState\_UpdateFields makes sure that all local
! variables that ExtState is pointing to are up to date. For the moment, this
! is just a placeholder routine as none of the ExtState fields is filled by
! local module fields. Content can be added to it if there are variables that
! need to be updated manually, e.g. not through netCDF input data.
!\\
!\\
! !INTERFACE:
!
  subroutine ExtState_UpdateFields ( HcoState, ExtState, RC )
!
! !INPUT/OUTPUT PARAMETERS
!
    type(HCO_STATE),  pointer       :: HcoState
    type(EXT_STATE),  pointer       :: ExtState
    integer,          intent(inout) :: RC          ! Success or failure?
!
! !REVISION HISTORY:
!  28 Jul 2014 - C. Keller - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!

    !=================================================================
    ! ExtState_UpdateFields begins here
    !=================================================================

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine ExtState_UpdateFields
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: IsEndOfSimulation
!
! !DESCRIPTION: Function IsEndOfSimulation returns true if the passed date
! is beyond the end of the simulation date.
!\\
!\\
! !INTERFACE:
!
  function IsEndOfSimulation( Yr, Mt, Dy, Hr, Mn, Sc ) RESULT ( IsEnd )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    integer,          intent(in   ) :: YR
    integer,          intent(in   ) :: MT
    integer,          intent(in   ) :: DY
    integer,          intent(in   ) :: HR
    integer,          intent(in   ) :: MN
    integer,          intent(in   ) :: SC
!
! !OUTPUT PARAMETERS
!
    logical                         :: IsEnd
!
! !REVISION HISTORY:
!  08 Sep 2014 - C. Keller - Initial Version
!  13 Jul 2015 - C. Keller - Bug fix: now save YYYYMMDD and hhmmss in different
!                            variables to avoid integer truncation errors.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    integer       :: THISYYYYMMDD
    integer       :: THIShhmmss
    integer, save :: ENDYYYYMMDD = -1
    integer, save :: ENDhhmmss   = -1

    !=================================================================
    ! IsEndOfSimulation begins here
    !=================================================================

    ! Init
    IsEnd = .FALSE.

    ! Calculate simulation end datetime if not yet done so
    if ( ENDYYYYMMDD < 0 ) then
       ENDYYYYMMDD = YRS(2)*10000 + MTS(2)*100 + DYS(2)
       ENDhhmmss   = HRS(2)*10000 + MNS(2)*100 + SCS(2)
    end if

    ! Calculate current datetime
    THISYYYYMMDD = YR*10000 + MT*100 + DY
    THIShhmmss   = HR*10000 + MN*100 + SC

    ! Check if current datetime is beyond simulation end date
    if ( THISYYYYMMDD > ENDYYYYMMDD ) then
       IsEnd = .TRUE.
    else if ( (THISYYYYMMDD == ENDYYYYMMDD) .AND. (THIShhmmss >= ENDhhmmss) ) then
       IsEnd = .TRUE.
    end if

  end function IsEndOfSimulation
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOI_Sa_InitCleanup
!
! !DESCRIPTION: deallocates all local species arrays used during initialization.
!\\
!\\
! !INTERFACE:
!
  subroutine HCOI_SA_InitCleanup ( RC )
!
! !INPUT/OUTPUT PARAMETERS
!
    integer, intent(inout) :: RC          ! Success or failure?
!
! !REVISION HISTORY:
!  04 Feb 2016 - C. Keller - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    !=================================================================
    ! HCOI_SA_InitCleanup begins here
    !=================================================================

    ! Deallocate local variables (not used anymore)
    if ( associated(ModelSpecNames     ) ) deallocate(ModelSpecNames     )
    if ( associated(ModelSpecIDs       ) ) deallocate(ModelSpecIDs       )
    if ( associated(ModelSpecMW        ) ) deallocate(ModelSpecMW        )
    if ( associated(ModelSpecK0        ) ) deallocate(ModelSpecK0        )
    if ( associated(ModelSpecCR        ) ) deallocate(ModelSpecCR        )
    if ( associated(ModelSpecPKA       ) ) deallocate(ModelSpecPKA       )
    if ( associated(matchIDx           ) ) deallocate(matchIDx           )
    if ( associated(HcoSpecNames       ) ) deallocate(HcoSpecNames       )

    ! Return w/ success
    RC = HCO_SUCCESS

  end subroutine HCOI_SA_InitCleanup

  ! -- NEXUS methods

  function HCO_GridCreate( HcoState, rc ) result ( grid )

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

  end function HCO_GridCreate

  function GridCreate_GridSpec( fileName, rc ) result ( grid )

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

  end function GridCreate_GridSpec

  subroutine GridWrite( grid, fileName, rc )
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

  end subroutine GridWrite

  subroutine NXS_DiagState_Init( HcoGrid, HcoState, DiagState, rc )
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

  end subroutine NXS_DiagState_Init

  subroutine NXS_DiagState_Update( HcoState, DiagState, rc )
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
    if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
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
      if (NEXUS_Error_Log(localrc, msg='Error encountered in routine "Diagn_Get!"', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

  end subroutine NXS_DiagState_Update

  subroutine NXS_ExptState_Init( grid, importState, exportState, rc )
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

  end subroutine NXS_ExptState_Init

  subroutine NXS_ExptState_Update( importState, exportState, rc )
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

  end subroutine NXS_ExptState_Update

  subroutine StateWrite( state, fileName, timeSlice, rc )
    type(ESMF_State)               :: state
    character(len=*),  intent(in)  :: fileName
    integer, optional, intent(in)  :: timeSlice
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
        call ESMF_FieldWrite( field, fileName, overwrite=.true., &
          timeslice=timeSlice, iofmt=ESMF_IOFMT_NETCDF, rc=localrc )
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

  end subroutine StateWrite


  subroutine StateFinalize( state, rc )
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

  end subroutine StateFinalize

end module NEXUS_Methods_Mod
