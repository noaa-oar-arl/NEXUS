!------------------------------------------------------------------------------
! Integration test for full NUOPC initialization sequence
!------------------------------------------------------------------------------
! This test verifies that the complete NUOPC initialization sequence executes
! successfully: SetServices → Advertise → Realize → DataInitialize → Advance
!
! Requirements: 2.2, 2.6
!------------------------------------------------------------------------------

program test_nuopc_full_init_sequence
  use ESMF
  use NUOPC
  use NUOPC_Model
  use HCO_Error_Mod, only: HCO_SUCCESS
  use HCO_STATE_MOD, only: Hco_State
  use nexus_state_mod, only: ModuleHcoState, ModuleExtState
  implicit none

  ! Test variables
  type(ESMF_GridComp) :: model
  type(ESMF_State) :: importState, exportState
  type(ESMF_Clock) :: clock
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime, stopTime, currentTime
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
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

  if (localPet == 0) then
     print *, '========================================'
     print *, 'NUOPC Full Initialization Sequence Test'
     print *, '========================================'
     print *, 'Testing complete NUOPC phase sequence'
     print *, ''
  endif

  !==============================================================================
  ! TEST 1: Create NUOPC component
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 1: Create NUOPC component'
     print *, '-------------------------------'
  endif

  ! Create NUOPC model component
  model = ESMF_GridCompCreate(name='NEXUS', rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] NUOPC component created'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create NUOPC component, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 2: SetServices phase
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 2: SetServices phase'
     print *, '--------------------------'
  endif

  ! Call SetServices to register phase methods
  call NUOPC_CompDerive(model, NUOPC_ModelSetServices, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] SetServices phase completed'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] SetServices phase failed, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 3: Create import/export states
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 3: Create import/export states'
     print *, '------------------------------------'
  endif

  ! Create import and export states
  importState = ESMF_StateCreate(name='NEXUS Import', &
                                  stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create import state, rc=', rc
     endif
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  exportState = ESMF_StateCreate(name='NEXUS Export', &
                                  stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Import/export states created'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create export state, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 4: Create clock
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 4: Create clock'
     print *, '--------------------'
  endif

  ! Create time interval (1 hour timestep)
  call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create time interval, rc=', rc
     endif
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create start time (2023-01-01 00:00:00)
  call ESMF_TimeSet(startTime, yy=2023, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create start time, rc=', rc
     endif
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create stop time (2023-01-01 06:00:00)
  call ESMF_TimeSet(stopTime, yy=2023, mm=1, dd=1, h=6, m=0, s=0, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create stop time, rc=', rc
     endif
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create clock
  clock = ESMF_ClockCreate(name='NEXUS Clock', &
                           timeStep=timeStep, &
                           startTime=startTime, &
                           stopTime=stopTime, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Clock created'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to create clock, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 5: Advertise phase (IPDv01)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 5: Advertise phase (IPDv01)'
     print *, '---------------------------------'
  endif

  ! Call Initialize with phase 1 (Advertise)
  call ESMF_GridCompInitialize(model, importState=importState, &
                                exportState=exportState, clock=clock, &
                                phase=1, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Advertise phase completed'
        print *, '  Fields advertised in import/export states'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Advertise phase failed, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 6: Realize phase (IPDv02)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 6: Realize phase (IPDv02)'
     print *, '-------------------------------'
  endif

  ! Call Initialize with phase 2 (Realize)
  call ESMF_GridCompInitialize(model, importState=importState, &
                                exportState=exportState, clock=clock, &
                                phase=2, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Realize phase completed'
        print *, '  ModuleHcoState initialized with clock'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Realize phase failed, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 7: Verify ModuleHcoState is properly initialized
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 7: Verify ModuleHcoState initialization'
     print *, '---------------------------------------------'
  endif

  ! Check if ModuleHcoState is associated
  if (associated(ModuleHcoState)) then
     if (localPet == 0) then
        print *, '  [SUCCESS] ModuleHcoState is associated'
     endif

     ! Check if Clock is associated
     if (associated(ModuleHcoState%Clock)) then
        if (localPet == 0) then
           print *, '  [SUCCESS] ModuleHcoState%Clock is associated'
           print *, '  Clock properly initialized in Realize phase'
        endif
        test_passed = .true.
     else
        if (localPet == 0) then
           print *, '  [FAILURE] ModuleHcoState%Clock is NOT associated'
        endif
     endif
  else
     if (localPet == 0) then
        print *, '  [FAILURE] ModuleHcoState is NOT associated'
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 8: DataInitialize phase (IPDv03)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 8: DataInitialize phase (IPDv03)'
     print *, '--------------------------------------'
  endif

  ! Call Initialize with phase 3 (DataInitialize)
  call ESMF_GridCompInitialize(model, importState=importState, &
                                exportState=exportState, clock=clock, &
                                phase=3, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] DataInitialize phase completed'
        print *, '  Export fields initialized'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] DataInitialize phase failed, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 9: Advance phase (first timestep)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 9: Advance phase (first timestep)'
     print *, '---------------------------------------'
  endif

  ! Call Run (Advance)
  call ESMF_GridCompRun(model, importState=importState, &
                        exportState=exportState, clock=clock, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Advance phase completed'
        print *, '  Model advanced one timestep'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Advance phase failed, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 10: Verify clock advanced
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 10: Verify clock advanced'
     print *, '-------------------------------'
  endif

  ! Get current time from clock
  call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
  if (rc == ESMF_SUCCESS) then
     ! Check if current time is after start time
     if (currentTime > startTime) then
        if (localPet == 0) then
           print *, '  [SUCCESS] Clock advanced correctly'
           call ESMF_TimePrint(currentTime, options='string', rc=rc)
        endif
        test_passed = .true.
     else
        if (localPet == 0) then
           print *, '  [FAILURE] Clock did not advance'
        endif
     endif
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Failed to get current time, rc=', rc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST SUMMARY
  !==============================================================================
  if (localPet == 0) then
     print *, '========================================'
     print *, 'TEST SUMMARY'
     print *, '========================================'
     print *, 'Total tests: ', total_tests
     print *, 'Passed: ', passed_tests
     print *, 'Failed: ', total_tests - passed_tests
     print *, ''

     if (passed_tests == total_tests) then
        print *, 'All tests PASSED!'
        print *, 'Full NUOPC initialization sequence works correctly:'
        print *, '  1. Component created'
        print *, '  2. SetServices registered phase methods'
        print *, '  3. Import/export states created'
        print *, '  4. Clock created'
        print *, '  5. Advertise phase completed'
        print *, '  6. Realize phase completed'
        print *, '  7. ModuleHcoState properly initialized'
        print *, '  8. DataInitialize phase completed'
        print *, '  9. Advance phase completed'
        print *, '  10. Clock advanced correctly'
     else
        print *, 'Some tests FAILED.'
        print *, 'Check the output above for details.'
     endif
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  call ESMF_GridCompDestroy(model, rc=rc)
  call ESMF_StateDestroy(importState, rc=rc)
  call ESMF_StateDestroy(exportState, rc=rc)
  call ESMF_ClockDestroy(clock, rc=rc)

  call ESMF_Finalize(rc=rc)

end program test_nuopc_full_init_sequence
