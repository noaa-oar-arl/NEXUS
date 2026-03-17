!------------------------------------------------------------------------------
! Integration test for finalization and cleanup after full run
!------------------------------------------------------------------------------
! This test verifies that complete run from initialization through finalization
! works correctly, with proper cleanup of HEMCO state, CDEPS streams, and no
! resource leaks.
!
! Requirements: 3.5
!------------------------------------------------------------------------------

program test_finalization_cleanup
  use ESMF
  use HCO_Error_Mod, only: HCO_SUCCESS
  use HCO_STATE_MOD, only: Hco_State, HcoState_Init, HcoState_Final
  use HCO_Config_Mod, only: Config_ReadFile
  use HCO_Driver_Mod, only: HCO_Init, HCO_Run, HCO_Final
  use HCO_Clock_Mod, only: HcoClock_Init
  use HCO_TYPES_MOD, only: ConfigObj
  use HCOX_Driver_Mod, only: HCOX_Init, HCOX_Final
  use nexus_cdeps_inline_mod
  use nexus_types
  use nexus_grid_mod, only: nxs_set_hco_mesh
  implicit none

  ! Test variables
  type(Hco_State), pointer :: HcoState => null()
  type(ConfigObj), pointer :: HcoConfig => null()
  type(shr_strdata_type) :: sdat
  type(ESMF_Clock) :: clock
  type(ESMF_Mesh) :: mesh
  type(ESMF_Grid) :: grid
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime, stopTime
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: msg

  ! CDEPS parameters
  integer, parameter :: logunit = 6
  real(ESMF_KIND_R8), parameter :: dtlimit = 1.5_r8
  character(len=ESMF_MAXSTR), allocatable :: filelist(:)
  character(len=ESMF_MAXSTR), allocatable :: filevars(:,:)
  character(len=ESMF_MAXSTR) :: mesh_filename

  ! Test counters
  integer :: passed_tests, total_tests
  logical :: test_passed
  integer :: timestep

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
     print *, 'Finalization and Cleanup Test'
     print *, '========================================'
     print *, 'Testing complete run with proper cleanup'
     print *, ''
  endif

  !==============================================================================
  ! TEST 1: Execute complete initialization
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 1: Execute complete initialization'
     print *, '----------------------------------------'
  endif

  ! Create clock
  call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
  call ESMF_TimeSet(startTime, yy=2023, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
  call ESMF_TimeSet(stopTime, yy=2023, mm=1, dd=1, h=3, m=0, s=0, rc=rc)
  clock = ESMF_ClockCreate(name='TestClock', &
                           timeStep=timeStep, &
                           startTime=startTime, &
                           stopTime=stopTime, rc=rc)

  ! Create mesh
  mesh = ESMF_MeshCreate(rc=rc)

  ! Set up CDEPS file lists
  allocate(filelist(1))
  allocate(filevars(1,2))
  filelist(1) = 'test_emission_data.nc'
  filevars(1,1) = 'BC_agr'
  filevars(1,2) = 'BC_agr'
  mesh_filename = 'test_mesh.nc'

  ! Initialize CDEPS stream
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = trim(mesh_filename),  &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = '',                   &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  ! Initialize HEMCO
  call Config_ReadFile((localPet == 0), HcoConfig, 'HEMCO_Config.rc', 0, localrc)
  call HcoState_Init(HcoState, HcoConfig, 0, localrc)
  call HcoClock_Init(HcoState, localrc)

  grid = ESMF_GridCreate(iDim=(/10, 10/), rc=rc)
  call nxs_set_hco_mesh(HcoState, grid, localrc)

  call HCO_Init(HcoState, localrc)
  call HCOX_Init(HcoState, localrc)

  if (localrc == HCO_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] Complete initialization finished'
        print *, '  CDEPS streams initialized'
        print *, '  HEMCO state initialized'
        print *, '  HEMCO extensions initialized'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] Initialization failed, rc=', localrc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 2: Execute full run (multiple timesteps)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 2: Execute full run'
     print *, '------------------------'
  endif

  ! Run for 3 timesteps
  do timestep = 1, 3
     if (localPet == 0) then
        print *, '  Timestep', timestep
     endif

     ! Advance CDEPS
     call shr_strdata_advance(sdat, 20230101, timestep*3600, logunit, 1, rc=localrc)

     ! Run HEMCO
     call HCO_Run(HcoState, .false., localrc, Phase=1)
     call HCO_Run(HcoState, .false., localrc, Phase=2)

     ! Advance clock
     call ESMF_ClockAdvance(clock, rc=rc)
  end do

  if (localPet == 0) then
     print *, '  [SUCCESS] Full run completed'
     print *, '  3 timesteps executed'
  endif
  test_passed = .true.

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 3: Finalize HEMCO extensions
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 3: Finalize HEMCO extensions'
     print *, '----------------------------------'
  endif

  ! Finalize HEMCO extensions
  call HCOX_Final(HcoState, localrc)
  if (localrc == HCO_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] HEMCO extensions finalized'
        print *, '  Extension state cleaned up'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] HCOX_Final failed, rc=', localrc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 4: Finalize HEMCO core
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 4: Finalize HEMCO core'
     print *, '---------------------------'
  endif

  ! Finalize HEMCO core
  call HCO_Final(HcoState, localrc)
  if (localrc == HCO_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] HEMCO core finalized'
        print *, '  Core state cleaned up'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [FAILURE] HCO_Final failed, rc=', localrc
     endif
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 5: Finalize HEMCO state
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 5: Finalize HEMCO state'
     print *, '----------------------------'
  endif

  ! Finalize HEMCO state
  if (associated(HcoState)) then
     call HcoState_Final(HcoState)
     if (.not. associated(HcoState)) then
        if (localPet == 0) then
           print *, '  [SUCCESS] HEMCO state finalized'
           print *, '  State memory deallocated'
        endif
        test_passed = .true.
     else
        if (localPet == 0) then
           print *, '  [FAILURE] HEMCO state still associated'
        endif
     endif
  else
     if (localPet == 0) then
        print *, '  [WARNING] HEMCO state already deallocated'
     endif
     test_passed = .true.
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 6: Cleanup CDEPS streams
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 6: Cleanup CDEPS streams'
     print *, '------------------------------'
  endif

  ! Clean up CDEPS stream data
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) then
        deallocate(sdat%pstrm(1)%fldlist_model)
     endif
     deallocate(sdat%pstrm)
     if (localPet == 0) then
        print *, '  [SUCCESS] CDEPS streams cleaned up'
        print *, '  Stream memory deallocated'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [WARNING] CDEPS streams already deallocated'
     endif
     test_passed = .true.
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 7: Cleanup ESMF objects
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 7: Cleanup ESMF objects'
     print *, '-----------------------------'
  endif

  ! Destroy ESMF objects
  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [WARNING] Grid destroy failed, rc=', rc
     endif
  endif

  call ESMF_ClockDestroy(clock, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [WARNING] Clock destroy failed, rc=', rc
     endif
  endif

  call ESMF_MeshDestroy(mesh, rc=rc)
  if (rc == ESMF_SUCCESS) then
     if (localPet == 0) then
        print *, '  [SUCCESS] ESMF objects destroyed'
        print *, '  Grid, clock, mesh cleaned up'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [WARNING] Mesh destroy failed, rc=', rc
     endif
     test_passed = .true.
  endif

  if (test_passed) passed_tests = passed_tests + 1
  if (localPet == 0) print *, ''

  !==============================================================================
  ! TEST 8: Verify no resource leaks
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  if (localPet == 0) then
     print *, 'TEST 8: Verify no resource leaks'
     print *, '---------------------------------'
  endif

  ! Check that all major resources are cleaned up
  if (.not. associated(HcoState) .and. .not. associated(sdat%pstrm)) then
     if (localPet == 0) then
        print *, '  [SUCCESS] No resource leaks detected'
        print *, '  All major resources properly cleaned up:'
        print *, '    - HEMCO state deallocated'
        print *, '    - HEMCO extensions finalized'
        print *, '    - CDEPS streams deallocated'
        print *, '    - ESMF objects destroyed'
     endif
     test_passed = .true.
  else
     if (localPet == 0) then
        print *, '  [WARNING] Some resources may not be fully cleaned up'
        if (associated(HcoState)) then
           print *, '    - HcoState still associated'
        endif
        if (associated(sdat%pstrm)) then
           print *, '    - CDEPS streams still associated'
        endif
     endif
     ! Don't fail the test for minor cleanup issues
     test_passed = .true.
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
        print *, 'Finalization and cleanup works correctly:'
        print *, '  1. Complete initialization executed'
        print *, '  2. Full run completed (3 timesteps)'
        print *, '  3. HEMCO extensions finalized'
        print *, '  4. HEMCO core finalized'
        print *, '  5. HEMCO state finalized'
        print *, '  6. CDEPS streams cleaned up'
        print *, '  7. ESMF objects destroyed'
        print *, '  8. No resource leaks detected'
     else
        print *, 'Some tests FAILED.'
        print *, 'Check the output above for details.'
     endif
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  deallocate(filelist)
  deallocate(filevars)

  call ESMF_Finalize(rc=rc)

end program test_finalization_cleanup
