!------------------------------------------------------------------------------
! Test program for HEMCO species registration and finalization preservation
!------------------------------------------------------------------------------
! This test verifies that NEXUS_RegisterSpecies and Finalize produce
! consistent results across different species configurations on unfixed code.
!
! Property: For all species configurations, registration and cleanup produce
! consistent results
!
! Requirements: 3.5, 3.8
!
! Validates: Requirements 3.5, 3.8
!------------------------------------------------------------------------------

program test_hemco_species_finalization_preservation
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
  integer :: num_species_1, num_species_2
  character(len=255), allocatable :: species_list_1(:)
  character(len=255), allocatable :: species_list_2(:)

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
  print *, 'HEMCO Species Registration and Finalization'
  print *, '=========================================='
  print *, 'Testing that NEXUS_RegisterSpecies and Finalize'
  print *, 'produce consistent results'
  print *, ''

  ! Set up configuration file paths
  config_file = './HEMCO_sa_Config.template'
  diagn_file = './HEMCO_sa_Diag.rc'
  spec_file = './HEMCO_sa_Spec.rc'
  grid_file = './HEMCO_sa_Grid.rc'
  time_file = './HEMCO_sa_Time.rc'

  !==============================================================================
  ! TEST 1: Species registration consistency
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: Species registration consistency'
  print *, '-------------------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     print *, '  This test requires HEMCO configuration files'
     deallocate(HcoState)
  else
     ! Register species first time
     call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

     if (localrc == HCO_SUCCESS) then
        ! Store number of species
        num_species_1 = HcoState%nSpecies

        ! Register species second time
        call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

        if (localrc == HCO_SUCCESS) then
           ! Store number of species
           num_species_2 = HcoState%nSpecies

           ! Compare species counts
           if (num_species_1 == num_species_2) then
              print *, '  [PASS] Species registration is consistent'
              print *, '  Number of species: ', num_species_1
              test_passed = .true.
           else
              print *, '  [FAIL] Species count differs'
              print *, '  First:  ', num_species_1
              print *, '  Second: ', num_species_2
           endif
        else
           print *, '  [FAIL] Second species registration failed with rc=', localrc
        endif
     else
        print *, '  [FAIL] First species registration failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 2: Finalization cleanup
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Finalization cleanup'
  print *, '----------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     deallocate(HcoState)
  else
     ! Register species
     call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

     if (localrc == HCO_SUCCESS) then
        ! Finalize HEMCO state
        call HcoState_Finalize(HcoState, localrc)

        if (localrc == HCO_SUCCESS) then
           print *, '  [PASS] Finalization completed successfully'
           test_passed = .true.
        else
           print *, '  [FAIL] Finalization failed with rc=', localrc
        endif
     else
        print *, '  [FAIL] Species registration failed'
        call HcoState_Finalize(HcoState, localrc)
     endif

     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 3: Species metadata preservation
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 3: Species metadata preservation'
  print *, '-----------------------------------'

  ! Initialize HEMCO state
  allocate(HcoState)
  call HcoState_Init(HcoState, config_file, localrc)

  if (localrc /= HCO_SUCCESS) then
     print *, '  [SKIP] Could not initialize HEMCO state'
     deallocate(HcoState)
  else
     ! Register species
     call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

     if (localrc == HCO_SUCCESS) then
        ! Allocate species list
        allocate(species_list_1(HcoState%nSpecies))

        ! Store species names
        do i = 1, HcoState%nSpecies
           species_list_1(i) = HcoState%Species(i)%Name
        enddo

        ! Register species again
        call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

        if (localrc == HCO_SUCCESS) then
           ! Allocate second species list
           allocate(species_list_2(HcoState%nSpecies))

           ! Store species names
           do i = 1, HcoState%nSpecies
              species_list_2(i) = HcoState%Species(i)%Name
           enddo

           ! Compare species lists
           test_passed = .true.
           do i = 1, HcoState%nSpecies
              if (species_list_1(i) /= species_list_2(i)) then
                 test_passed = .false.
                 exit
              endif
           enddo

           if (test_passed) then
              print *, '  [PASS] Species metadata is preserved'
              test_passed = .true.
           else
              print *, '  [FAIL] Species metadata differs'
           endif

           deallocate(species_list_2)
        else
           print *, '  [FAIL] Second species registration failed with rc=', localrc
        endif

        deallocate(species_list_1)
     else
        print *, '  [FAIL] First species registration failed with rc=', localrc
     endif

     call HcoState_Finalize(HcoState, localrc)
     deallocate(HcoState)
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 4: Multiple finalization cycles
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 4: Multiple finalization cycles'
  print *, '-----------------------------------'

  test_passed = .true.

  ! Perform multiple initialization and finalization cycles
  do i = 1, 3
     allocate(HcoState)
     call HcoState_Init(HcoState, config_file, localrc)

     if (localrc /= HCO_SUCCESS) then
        print *, '  [FAIL] Cycle ', i, ': HcoState_Init failed with rc=', localrc
        test_passed = .false.
        deallocate(HcoState)
        exit
     endif

     call NEXUS_RegisterSpecies(HcoState, spec_file, localrc)

     if (localrc /= HCO_SUCCESS) then
        print *, '  [FAIL] Cycle ', i, ': NEXUS_RegisterSpecies failed with rc=', localrc
        test_passed = .false.
        call HcoState_Finalize(HcoState, localrc)
        deallocate(HcoState)
        exit
     endif

     call HcoState_Finalize(HcoState, localrc)

     if (localrc /= HCO_SUCCESS) then
        print *, '  [FAIL] Cycle ', i, ': HcoState_Finalize failed with rc=', localrc
        test_passed = .false.
        deallocate(HcoState)
        exit
     endif

     deallocate(HcoState)
  enddo

  if (test_passed) then
     print *, '  [PASS] Multiple finalization cycles completed successfully'
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
     print *, 'All tests PASSED - HEMCO species registration and finalization are preserved!'
  else
     print *, 'Some tests failed - check HEMCO species configuration'
  endif

  call ESMF_Finalize(rc=rc)

end program test_hemco_species_finalization_preservation
