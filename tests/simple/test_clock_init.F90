!------------------------------------------------------------------------------
! Test program for clock initialization bug condition exploration
!------------------------------------------------------------------------------
! This test verifies that calling HCO_Init without prior HcoClock_Init fails
! with "Clock not associated" error (the bug condition).
!
! Requirements: 1.4, 2.4
!------------------------------------------------------------------------------

program test_clock_init
  use ESMF
  use HCO_Error_Mod, only: HCO_SUCCESS, HCO_ERROR, HCO_MSG
  use HCO_Config_Mod, only: Config_ReadFile
  use HCO_Driver_Mod, only: HCO_Init
  use HCO_Clock_Mod, only: HcoClock_Init
  use HCO_STATE_MOD, only: Hco_State, HcoState_Init
  use HCO_TYPES_MOD, only: ConfigObj
  use nexus_grid_mod, only: nxs_set_hco_mesh
  use nexus_config_mod, only: nxs_read_config_file
  implicit none

  ! Test variables
  type(Hco_State), pointer :: HcoState => null()
  type(ConfigObj), pointer :: HcoConfig => null()
  type(ESMF_Grid) :: grid
  type(ESMF_Mesh) :: mesh
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: configFile, gridFile
  character(len=255) :: msg

  ! Test counters
  integer :: passed_tests, total_tests
  logical :: test_passed

  ! Initialize ESMF
  call ESMF_Initialize(logkind=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, 'ESMF_Initialize failed'
     stop 1
  endif

  call ESMF_VMGetCurrent(vm, rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

  ! Initialize counters
  passed_tests = 0
  total_tests = 0

  print *, '========================================'
  print *, 'Clock Initialization Bug Condition Test'
  print *, '========================================'
  print *, 'Testing that HCO_Init fails without prior HcoClock_Init'
  print *, ''

  ! Read configuration
  configFile = 'nexus.rc'
  call nxs_read_config_file(configFile, configFile, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, 'Warning: Could not read config file, using default'
     configFile = 'HEMCO_Config.rc'
  endif

  ! Read HEMCO configuration
  call Config_ReadFile((localPet == 0), HcoConfig, configFile, 0, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, 'Failed to read HEMCO config'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  !==============================================================================
  ! TEST 1: HCO_Init without HcoClock_Init (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: HCO_Init without HcoClock_Init'
  print *, '---------------------------------------'

  ! Initialize HEMCO state
  call HcoState_Init(HcoState, HcoConfig, 0, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, '  Failed to initialize HEMCO state'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create a simple grid/mesh for testing
  grid = ESMF_GridCreate(iDim=(/10, 10/), rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, '  Failed to create grid'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Set grid on HEMCO state
  call nxs_set_hco_mesh(HcoState, grid, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, '  Warning: Could not set mesh on HEMCO state'
  endif

  ! Check if clock is associated BEFORE HCO_Init
  if (associated(HcoState%Clock)) then
     print *, '  HcoState%Clock is associated (unexpected)'
  else
     print *, '  HcoState%Clock is NOT associated (expected - this is the bug condition)'
  endif

  ! Try to call HCO_Init without HcoClock_Init first
  call HCO_Init(HcoState, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [EXPECTED FAILURE] HCO_Init failed with rc=', localrc
     print *, '  This confirms the bug exists - clock not associated before HCO_Init'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] HCO_Init succeeded without HcoClock_Init'
     print *, '  This means the test does not detect the bug properly'
     print *, '  The clock may have been initialized automatically somewhere'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up
  call ESMF_GridDestroy(grid, rc=rc)
  call HcoState_Final(HcoState)
  HcoState => null()

  !==============================================================================
  ! TEST 2: HCO_Init WITH HcoClock_Init (should succeed)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: HCO_Init with HcoClock_Init (control test)'
  print *, '---------------------------------------------------'

  ! Re-initialize HEMCO state
  call HcoState_Init(HcoState, HcoConfig, 0, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, '  Failed to initialize HEMCO state'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create grid again
  grid = ESMF_GridCreate(iDim=(/10, 10/), rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, '  Failed to create grid'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Set grid on HEMCO state
  call nxs_set_hco_mesh(HcoState, grid, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, '  Warning: Could not set mesh on HEMCO state'
  endif

  ! Initialize clock BEFORE HCO_Init (the fix)
  call HcoClock_Init(HcoState, localrc)
  if (localrc /= HCO_SUCCESS) then
     print *, '  Failed to initialize clock'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Check if clock is associated
  if (associated(HcoState%Clock)) then
     print *, '  HcoState%Clock is associated (expected after HcoClock_Init)'
  else
     print *, '  HcoState%Clock is NOT associated (unexpected)'
  endif

  ! Now call HCO_Init with clock properly initialized
  call HCO_Init(HcoState, localrc)

  if (localrc == HCO_SUCCESS) then
     print *, '  [EXPECTED SUCCESS] HCO_Init succeeded with HcoClock_Init'
     print *, '  This confirms the fix works - clock must be initialized first'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED FAILURE] HCO_Init failed even with HcoClock_Init'
     print *, '  This may indicate an issue with the test or implementation'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up
  call ESMF_GridDestroy(grid, rc=rc)
  call HcoState_Final(HcoState)
  HcoState => null()

  !==============================================================================
  ! TEST SUMMARY
  !==============================================================================
  print *, '========================================'
  print *, 'TEST SUMMARY'
  print *, '========================================'
  print *, 'Total tests: ', total_tests
  print *, 'Passed: ', passed_tests
  print *, 'Failed: ', total_tests - passed_tests
  print *, ''

  if (passed_tests == total_tests) then
     print *, 'All tests PASSED - bug condition correctly detected!'
     print *, 'HCO_Init fails without HcoClock_Init, succeeds with it.'
  else
     print *, 'Some tests did not detect the expected behavior.'
     print *, 'This may indicate the test needs adjustment or the code has unexpected behavior.'
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  call ESMF_Finalize(rc=rc)

end program test_clock_init
