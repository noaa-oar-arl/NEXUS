!==============================================================================
!
! Test: CF-compliant output module basic functionality
!
! Purpose: Verify that the nexus_output_mod module can be initialized,
!          configured, and finalized without errors
!
!==============================================================================

program test_cf_output_module

  use ESMF
  use nexus_output_mod

  implicit none

  ! Test variables
  integer :: rc, localPet
  type(ESMF_VM) :: vm
  type(ESMF_Grid) :: grid
  type(ESMF_Clock) :: clock
  type(ESMF_Time) :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep
  logical :: test_passed

  ! Initialize ESMF
  call ESMF_Initialize(vm=vm, defaultlogfilename="test_cf_output.log", &
                       logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, "ERROR: ESMF_Initialize failed"
     stop 1
  endif

  call ESMF_VMGet(vm, localPet=localPet, rc=rc)

  if (localPet == 0) then
     print *, "========================================="
     print *, "Test: CF-compliant output module"
     print *, "========================================="
  endif

  test_passed = .true.

  ! Create a simple grid for testing
  grid = ESMF_GridCreateNoPeriDim( &
       minIndex=(/1,1/), &
       maxIndex=(/10,10/), &
       regDecomp=(/1,1/), &
       name="test_grid", &
       rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) print *, "ERROR: Failed to create grid"
     test_passed = .false.
     goto 100
  endif

  ! Create a clock for testing
  call ESMF_TimeSet(startTime, yy=2020, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
  call ESMF_TimeSet(stopTime, yy=2020, mm=1, dd=2, h=0, m=0, s=0, rc=rc)
  call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)

  clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
                           stopTime=stopTime, name="test_clock", rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) print *, "ERROR: Failed to create clock"
     test_passed = .false.
     goto 100
  endif

  ! Test 1: Initialize output system with default config
  if (localPet == 0) print *, "Test 1: Initialize output system"
  call OutputInit('nonexistent_config.yaml', grid, clock, rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) print *, "ERROR: OutputInit failed"
     test_passed = .false.
     goto 100
  endif
  if (localPet == 0) print *, "  PASS: OutputInit succeeded"

  ! Test 2: Validate CF compliance (stub test)
  if (localPet == 0) print *, "Test 2: Validate CF compliance"
  call ValidateCFCompliance('test_output.nc', rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) print *, "ERROR: ValidateCFCompliance failed"
     test_passed = .false.
     goto 100
  endif
  if (localPet == 0) print *, "  PASS: ValidateCFCompliance succeeded"

  ! Test 3: Finalize output system
  if (localPet == 0) print *, "Test 3: Finalize output system"
  call OutputFinalize(rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) print *, "ERROR: OutputFinalize failed"
     test_passed = .false.
     goto 100
  endif
  if (localPet == 0) print *, "  PASS: OutputFinalize succeeded"

100 continue

  ! Cleanup
  call ESMF_ClockDestroy(clock, rc=rc)
  call ESMF_GridDestroy(grid, rc=rc)

  ! Print results
  if (localPet == 0) then
     print *, "========================================="
     if (test_passed) then
        print *, "RESULT: ALL TESTS PASSED"
        print *, "========================================="
     else
        print *, "RESULT: SOME TESTS FAILED"
        print *, "========================================="
     endif
  endif

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)

  if (test_passed) then
     stop 0
  else
     stop 1
  endif

end program test_cf_output_module
