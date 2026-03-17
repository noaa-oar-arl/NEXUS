!------------------------------------------------------------------------------
! Test program for CDEPS initialization bug condition exploration
!------------------------------------------------------------------------------
! This test verifies that InitializeSingleCDEPSStream fails when required
! CDEPS parameters are missing (the bug condition).
!
! Requirements: 1.1, 1.3, 2.1, 2.3
!------------------------------------------------------------------------------

program test_cdeps_init
  use ESMF
  use nexus_cdeps_inline_mod
  use nexus_types
  implicit none

  ! Test variables
  type(shr_strdata_type) :: sdat
  type(ESMF_Clock) :: clock
  type(ESMF_Mesh) :: mesh
  type(ESMF_Field) :: field
  integer :: rc, localrc
  integer :: localPet, petCount
  type(ESMF_VM) :: vm
  character(len=255) :: msg

  ! Test data
  integer, parameter :: logunit = 6
  integer, parameter :: stream_idx = 1
  real(ESMF_KIND_R8), parameter :: dtlimit = 1.5_r8

  ! File lists for CDEPS initialization
  character(len=ESMF_MAXSTR), allocatable :: filelist(:)
  character(len=ESMF_MAXSTR), allocatable :: filevars(:,:)

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
  print *, 'CDEPS Initialization Bug Condition Test'
  print *, '========================================'
  print *, 'Testing that InitializeSingleCDEPSStream fails with missing parameters'
  print *, ''

  ! Create a simple clock for testing
  clock = ESMF_ClockCreate(name='TestClock', rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, 'Failed to create clock'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create a simple mesh for testing
  mesh = ESMF_MeshCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, 'Failed to create mesh'
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Allocate arrays for CDEPS initialization
  allocate(filelist(1))
  allocate(filevars(1,2))

  ! Set up test file paths
  filelist(1) = '/scratch2/NAQFC/Barry.Baker/emissions/CEDS/v2021-06-15/2023/BC-em-anthro_CEDS_global_2023.nc'
  filevars(1,1) = 'BC_agr'
  filevars(1,2) = 'BC_agr'

  !==============================================================================
  ! TEST 1: Missing stream_src_mask parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: Missing stream_src_mask parameter'
  print *, '----------------------------------------'

  ! Initialize CDEPS stream with incomplete parameters (missing stream_src_mask)
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         ! stream_src_mask     = 0,  ! <-- MISSING (BUG)
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_src_mask causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 2: Missing stream_dst_mask parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Missing stream_dst_mask parameter'
  print *, '----------------------------------------'

  ! Re-initialize with missing stream_dst_mask
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         ! stream_dst_mask     = 0,  ! <-- MISSING (BUG)
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_dst_mask causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 3: Missing stream_mapalgo parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 3: Missing stream_mapalgo parameter'
  print *, '---------------------------------------'

  ! Re-initialize with missing stream_mapalgo
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         ! stream_mapalgo      = 'bilinear',  ! <-- MISSING (BUG)
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_mapalgo causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 4: Missing stream_taxmode parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 4: Missing stream_taxmode parameter'
  print *, '---------------------------------------'

  ! Re-initialize with missing stream_taxmode
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         ! stream_taxmode      = 'cycle',  ! <-- MISSING (BUG)
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_taxmode causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 5: Missing stream_tintalgo parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 5: Missing stream_tintalgo parameter'
  print *, '----------------------------------------'

  ! Re-initialize with missing stream_tintalgo
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         ! stream_tintalgo     = 'linear',  ! <-- MISSING (BUG)
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_tintalgo causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 6: Missing stream_offset parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 6: Missing stream_offset parameter'
  print *, '--------------------------------------'

  ! Re-initialize with missing stream_offset
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         ! stream_offset       = 0,  ! <-- MISSING (BUG)
         stream_taxmode      = 'cycle',              &
         stream_dtlimit      = dtlimit,              &
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_offset causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  ! Clean up for next test
  if (associated(sdat%pstrm)) then
     if (allocated(sdat%pstrm(1)%fldlist_model)) deallocate(sdat%pstrm(1)%fldlist_model)
     deallocate(sdat%pstrm(1))
  endif

  !==============================================================================
  ! TEST 7: Missing stream_dtlimit parameter (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 7: Missing stream_dtlimit parameter'
  print *, '---------------------------------------'

  ! Re-initialize with missing stream_dtlimit
  call shr_strdata_init_from_inline(sdat,           &
         my_task             = localPet,             &
         logunit             = logunit,              &
         compname            = 'NEXUS',              &
         model_clock         = clock,                &
         model_mesh          = mesh,                 &
         stream_name         = 'test_stream',        &
         stream_meshfile     = 'unset',              &
         stream_filenames    = filelist,             &
         stream_yearFirst    = 2023,                 &
         stream_yearLast     = 2023,                 &
         stream_yearAlign    = 2023,                 &
         stream_fldlistFile  = filevars(:,1),        &
         stream_fldListModel = filevars(:,2),        &
         stream_lev_dimname  = 'unset',              &
         stream_mapalgo      = 'bilinear',           &
         stream_offset       = 0,                    &
         stream_taxmode      = 'cycle',              &
         ! stream_dtlimit      = dtlimit,  ! <-- MISSING (BUG)
         stream_tintalgo     = 'linear',             &
         stream_src_mask     = 0,                    &
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc /= ESMF_SUCCESS) then
     print *, '  [EXPECTED FAILURE] shr_strdata_init_from_inline failed with rc=', localrc
     print *, '  This confirms the bug exists - missing stream_dtlimit causes failure'
     test_passed = .true.
  else
     print *, '  [UNEXPECTED SUCCESS] Stream initialized successfully'
     print *, '  This means the test does not detect the bug properly'
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
     print *, 'The incomplete CDEPS parameter setup causes shr_strdata_init_from_inline to fail.'
  else
     print *, 'Some tests did not detect the expected failure.'
     print *, 'This may indicate the test needs adjustment or the code has unexpected behavior.'
  endif

  !==============================================================================
  ! CLEANUP
  !==============================================================================
  deallocate(filelist)
  deallocate(filevars)

  call ESMF_ClockDestroy(clock, rc=rc)
  call ESMF_MeshDestroy(mesh, rc=rc)

  call ESMF_Finalize(rc=rc)

end program test_cdeps_init
