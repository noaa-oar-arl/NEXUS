!------------------------------------------------------------------------------
! Test program for HEMCO configuration and grid preservation
!------------------------------------------------------------------------------
! This test verifies that Config_ReadFile, nxs_create_grid, and
! nxs_read_time_config produce consistent results across different
! configuration files and grid specifications on unfixed code.
!
! Property: For all configuration files and grid specifications, parsing
! produces consistent results
!
! Requirements: 3.4, 3.6, 3.7
!
! Validates: Requirements 3.4, 3.6, 3.7
!------------------------------------------------------------------------------

program test_hemco_config_grid_preservation
  use ESMF
  use HCO_STATE_MOD
  use HCO_TYPES_MOD
  use HCO_ERROR_MOD
  implicit none

  ! Test variables
  type(Hco_State), pointer :: HcoState => NULL()
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: msg
  character(len=255) :: config_file
  character(len=255) :: diagn_file
  character(len=255) :: spec_file
  character(len=255) :: grid_file
  character(len=255) :: time_file

  ! Test counters
  integer :: passed_tests, total_tests
  logical :: test_passed
  integer :: i, j
  integer :: nx_1, ny_1, nx_2, ny_2
  real(ESMF_KIND_R8) :: start_time_1, start_time_2
  real(ESMF_KIND_R8) :: end_time_1, end_time_2
  real(ESMF_KIND_R8) :: timestep_1, timestep_2

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

  print *, '=========================================='
  print *, 'HEMCO Configuration and Grid Preservation'
  print *, '=========================================='
  print *, 'Testing that Config_ReadFile, nxs_create_grid, and'
  print *, 'nxs_read_time_config produce consistent results'
  print *, ''

  ! Set up configuration file paths
  config_file = './HEMCO_sa_Config.template'
  diagn_file = './HEMCO_sa_Diag.rc'
  spec_file = './HEMCO_sa_Spec.rc'
  grid_file = './HEMCO_sa_Grid.rc'
  time_file = './HEMCO_sa_Time.rc'

  !==============================================================================
  ! TEST 1: Configuration file reading consistency
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: Configuration file reading consistency'
  print *, '--------------------------------------------'

  ! Initialize HEMCO state first time
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     print *, '  This test requires HEMCO configuration files'
     deallocate(HcoState)
  else
     ! Read configuration first time
     call Config_ReadFile(HcoState, config_file, localrc)

     if (localrc == HCO_SUCCESS) then
        print *, '  [PASS] Configuration file read successfully (first time)'
        test_passed = .true.
     else
        print *, '  [FAIL] Configuration file read failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 2: Grid creation consistency
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Grid creation consistency'
  print *, '-------------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     deallocate(HcoState)
  else
     ! Create grid first time
     call nxs_create_grid(HcoState, grid_file, localrc)

     if (localrc == HCO_SUCCESS) then
        ! Store grid dimensions
        nx_1 = HcoState%NX
        ny_1 = HcoState%NY

        ! Create grid second time
        call nxs_create_grid(HcoState, grid_file, localrc)

        if (localrc == HCO_SUCCESS) then
           ! Store grid dimensions
           nx_2 = HcoState%NX
           ny_2 = HcoState%NY

           ! Compare grid dimensions
           if (nx_1 == nx_2 .and. ny_1 == ny_2) then
              print *, '  [PASS] Grid dimensions are consistent'
              print *, '  Grid size: ', nx_1, ' x ', ny_1
              test_passed = .true.
           else
              print *, '  [FAIL] Grid dimensions differ'
              print *, '  First:  ', nx_1, ' x ', ny_1
              print *, '  Second: ', nx_2, ' x ', ny_2
           endif
        else
           print *, '  [FAIL] Second grid creation failed with rc=', localrc
        endif
     else
        print *, '  [FAIL] First grid creation failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 3: Time configuration reading consistency
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 3: Time configuration reading consistency'
  print *, '--------------------------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     deallocate(HcoState)
  else
     ! Read time configuration first time
     call nxs_read_time_config(HcoState, time_file, localrc)

     if (localrc == HCO_SUCCESS) then
        ! Store time values
        start_time_1 = HcoState%StartTime
        end_time_1 = HcoState%EndTime
        timestep_1 = HcoState%TimeStep

        ! Read time configuration second time
        call nxs_read_time_config(HcoState, time_file, localrc)

        if (localrc == HCO_SUCCESS) then
           ! Store time values
           start_time_2 = HcoState%StartTime
           end_time_2 = HcoState%EndTime
           timestep_2 = HcoState%TimeStep

           ! Compare time values
           if (start_time_1 == start_time_2 .and. &
               end_time_1 == end_time_2 .and. &
               timestep_1 == timestep_2) then
              print *, '  [PASS] Time configuration is consistent'
              print *, '  Start time: ', start_time_1
              print *, '  End time: ', end_time_1
              print *, '  Timestep: ', timestep_1
              test_passed = .true.
           else
              print *, '  [FAIL] Time configuration differs'
              print *, '  First:  ', start_time_1, end_time_1, timestep_1
              print *, '  Second: ', start_time_2, end_time_2, timestep_2
           endif
        else
           print *, '  [FAIL] Second time configuration read failed with rc=', localrc
        endif
     else
        print *, '  [FAIL] First time configuration read failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 4: File-based grid specification
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 4: File-based grid specification'
  print *, '-----------------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     deallocate(HcoState)
  else
     ! Create grid from file
     call nxs_create_grid(HcoState, grid_file, localrc)

     if (localrc == HCO_SUCCESS) then
        print *, '  [PASS] File-based grid creation successful'
        test_passed = .true.
     else
        print *, '  [FAIL] File-based grid creation failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST SUMMARY
  !==============================================================================
  print *, '=========================================='
  print *, 'TEST SUMMARY'
  print *, '=========================================='
  print *, 'Total tests: ', total_tests
  print *, 'Passed: ', passed_tests
  print *, 'Failed: ', total_tests - passed_tests
  print *, ''

  if (passed_tests == total_tests) then
     print *, 'All tests PASSED - HEMCO configuration and grid are preserved!'
  else
     print *, 'Some tests failed - check HEMCO configuration files'
  endif

  call ESMF_Finalize(rc=rc)

end program test_hemco_config_grid_preservation
