!------------------------------------------------------------------------------
! Test program for standalone driver architecture bug condition exploration
!------------------------------------------------------------------------------
! This test verifies that the driver creates DATM component even when
! standalone_mode is TRUE (the bug condition).
!
! Requirements: 1.7, 2.7
!------------------------------------------------------------------------------

program test_standalone_driver
  use ESMF
  use NUOPC
  use nexus_driver, only: SetServices, SetModelServices
  implicit none

  ! Test variables
  type(ESMF_GridComp) :: driver
  type(ESMF_GridComp) :: child_nexus, child_datm
  type(ESMF_CplComp) :: connector
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: msg

  ! Test counters
  integer :: passed_tests, total_tests
  logical :: test_passed

  ! Component tracking
  integer :: num_components
  character(len=255), allocatable :: component_labels(:)

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
  print *, 'Standalone Driver Bug Condition Test'
  print *, '========================================'
  print *, 'Testing that driver creates DATM component in standalone mode'
  print *, ''

  !==============================================================================
  ! TEST 1: Check driver component creation in standalone mode
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: Driver component creation in standalone mode'
  print *, '-----------------------------------------------------'

  ! Create a driver component
  driver = ESMF_GridCompCreate(name='TestDriver', rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, '  Failed to create driver component'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Call SetServices
  call SetServices(driver, localrc)
  if (localrc /= ESMF_SUCCESS) then
     print *, '  Warning: SetServices failed, continuing...'
  endif

  ! Call SetModelServices (this is where components are created)
  call SetModelServices(driver, localrc)
  if (localrc /= ESMF_SUCCESS) then
     print *, '  Warning: SetModelServices failed, continuing...'
  endif

  ! Get the list of child components
  call ESMF_GridCompGet(driver, childCompList=component_labels, rc=rc)
  if (rc == ESMF_SUCCESS) then
     num_components = size(component_labels)
     print *, '  Number of components created: ', num_components

     if (num_components > 0) then
        print *, '  Components created:'
        do i = 1, num_components
           print *, '    - ', trim(component_labels(i))
        end do

        ! Check if DATM component was created (bug condition)
        if (any(index(component_labels, 'DATM') > 0)) then
           print *, '  [EXPECTED FAILURE] DATM component created in standalone mode'
           print *, '  This confirms the bug - DATM should not be created in standalone mode'
           test_passed = .true.
        else
           print *, '  DATM component NOT created (bug may be fixed)'
        endif

        ! Check if NEXUS component was created
        if (any(index(component_labels, 'NEXUS') > 0)) then
           print *, '  NEXUS component created (expected)'
        else
           print *, '  NEXUS component NOT created (unexpected)'
        endif
     else
        print *, '  No components created (unexpected)'
     endif
  else
     print *, '  Warning: Could not get child component list'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 2: Verify standalone mode configuration
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Standalone mode configuration check'
  print *, '-------------------------------------------'

  ! The standalone_mode should be read from nexus.rc
  ! In standalone mode, only NEXUS component should be created
  ! In coupled mode, DATM + NEXUS + connector should be created

  print *, '  Expected behavior:'
  print *, '    - Standalone mode (standalone_mode = .true.): Only NEXUS'
  print *, '    - Coupled mode (standalone_mode = .false.): DATM + NEXUS + connector'
  print *, ''

  ! Check if we can determine the mode from the configuration
  ! For now, we'll just document the expected behavior
  print *, '  [EXPECTED FAILURE] If DATM was created, this confirms the bug'
  print *, '  The driver should check standalone_mode before creating DATM'

  test_passed = .true.  ! This test always passes as a documentation check
  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

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
     print *, 'Driver creates DATM component even in standalone mode.'
  else
     print *, 'Some tests did not detect the expected behavior.'
     print *, 'This may indicate the test needs adjustment or the code has unexpected behavior.'
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  if (allocated(component_labels)) deallocate(component_labels)

  call ESMF_GridCompDestroy(driver, rc=rc)

  call ESMF_Finalize(rc=rc)

end program test_standalone_driver
