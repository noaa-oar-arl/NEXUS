!------------------------------------------------------------------------------
! Test program for NUOPC phase ordering bug condition exploration
!------------------------------------------------------------------------------
! This test verifies that ModuleHcoState is NULL or improperly initialized
! in Advance phase (the bug condition).
!
! Requirements: 1.2, 1.6, 2.2, 2.6
!------------------------------------------------------------------------------

program test_nuopc_phase
  use ESMF
  use NUOPC
  use NUOPC_Model, only: NUOPC_ModelGet
  use HCO_Error_Mod, only: HCO_SUCCESS
  use HCO_STATE_MOD, only: Hco_State
  use nexus_cap, only: SetServices, Advertise, Realize, Advance
  use nexus_state_mod, only: ModuleHcoState, ModuleExtState
  implicit none

  ! Test variables
  type(ESMF_GridComp) :: model
  type(ESMF_State) :: importState, exportState
  type(ESMF_Clock) :: clock
  type(ESMF_Field) :: field
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: msg

  ! Test counters
  integer :: passed_tests, total_tests
  logical :: test_passed

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
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
  print *, 'NUOPC Phase Ordering Bug Condition Test'
  print *, '========================================'
  print *, 'Testing that ModuleHcoState is NULL in Advance phase (before fix)'
  print *, ''

  !==============================================================================
  ! TEST 1: Check ModuleHcoState before Realize phase
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: ModuleHcoState before Realize phase'
  print *, '--------------------------------------------'

  ! Check if ModuleHcoState is associated before any initialization
  if (.not. associated(ModuleHcoState)) then
     print *, '  [EXPECTED] ModuleHcoState is NOT associated before Realize'
     print *, '  This is the bug condition - state not initialized yet'
     test_passed = .true.
  else
     print *, '  ModuleHcoState is associated (unexpected before Realize)'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 2: Create NUOPC component and run through phases
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Run through NUOPC phases'
  print *, '---------------------------------'

  ! Create a simple NUOPC component
  model = ESMF_GridCompCreate(name='TestNEXUS', rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, '  Failed to create NUOPC component'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Call SetServices
  call SetServices(model, localrc)
  if (localrc /= ESMF_SUCCESS) then
     print *, '  Warning: SetServices failed, continuing...'
  endif

  ! Call Advertise
  call Advertise(model, localrc)
  if (localrc /= ESMF_SUCCESS) then
     print *, '  Warning: Advertise failed, continuing...'
  endif

  ! Call Realize
  call Realize(model, localrc)
  if (localrc /= ESMF_SUCCESS) then
     print *, '  Warning: Realize failed, continuing...'
  endif

  ! Check ModuleHcoState after Realize
  if (associated(ModuleHcoState)) then
     print *, '  ModuleHcoState is associated after Realize'
     if (associated(ModuleHcoState%Clock)) then
        print *, '  ModuleHcoState%Clock is associated'
     else
        print *, '  ModuleHcoState%Clock is NOT associated (bug condition)'
     endif
  else
     print *, '  ModuleHcoState is NOT associated after Realize (bug condition)'
  endif

  !==============================================================================
  ! TEST 3: Check ModuleHcoState in Advance phase
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 3: ModuleHcoState in Advance phase'
  print *, '----------------------------------------'

  ! Create a simple clock for Advance
  call ESMF_ClockCreate(name='TestClock', timeStep=ESMF_TimeInterval(h=1), rc=rc)
  if (rc == ESMF_SUCCESS) then
     call ESMF_GridCompSet(model, clock=clock, rc=rc)
  endif

  ! Call Advance
  call Advance(model, localrc)

  ! Check ModuleHcoState after Advance
  if (associated(ModuleHcoState)) then
     print *, '  ModuleHcoState is associated after Advance'
     if (associated(ModuleHcoState%Clock)) then
        print *, '  ModuleHcoState%Clock is associated'
        print *, '  [EXPECTED] State properly initialized in Realize phase'
        test_passed = .true.
     else
        print *, '  ModuleHcoState%Clock is NOT associated (bug condition)'
        print *, '  [EXPECTED FAILURE] This confirms the bug - clock not set'
     endif
  else
     print *, '  ModuleHcoState is NOT associated after Advance (bug condition)'
     print *, '  [EXPECTED FAILURE] This confirms the bug - state not initialized'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 4: Check for NULL pointer dereference in Advance
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 4: NULL pointer check in Advance'
  print *, '-------------------------------------'

  ! If ModuleHcoState is NULL, this would cause a segfault
  if (.not. associated(ModuleHcoState)) then
     print *, '  [EXPECTED FAILURE] ModuleHcoState is NULL'
     print *, '  In the unfixed code, this would cause NULL pointer dereference'
     print *, '  when Advance tries to access ModuleHcoState%Clock'
     test_passed = .true.
  else
     print *, '  ModuleHcoState is associated (bug may be fixed)'
  endif

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
     print *, 'ModuleHcoState is NULL or improperly initialized before Realize.'
  else
     print *, 'Some tests did not detect the expected behavior.'
     print *, 'This may indicate the test needs adjustment or the code has unexpected behavior.'
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  call ESMF_GridCompDestroy(model, rc=rc)
  call ESMF_ClockDestroy(clock, rc=rc)

  call ESMF_Finalize(rc=rc)

end program test_nuopc_phase
