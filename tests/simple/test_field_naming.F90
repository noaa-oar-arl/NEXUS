!------------------------------------------------------------------------------
! Test program for field naming consistency bug condition exploration
!------------------------------------------------------------------------------
! This test verifies that dshr_fldbun_getFldPtr fails when using HEMCO field
! names with CDEPS field bundles (the bug condition).
!
! Requirements: 1.5, 1.8, 2.5, 2.8
!------------------------------------------------------------------------------

program test_field_naming
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
  print *, 'Field Naming Bug Condition Test'
  print *, '========================================'
  print *, 'Testing that dshr_fldbun_getFldPtr fails with HEMCO field names'
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
  ! TEST 1: Initialize CDEPS stream with proper parameters
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 1: Initialize CDEPS stream (setup)'
  print *, '---------------------------------------'

  ! Initialize CDEPS stream with complete parameters
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
         stream_dst_mask     = 0,                    &
         rc                  = localrc)

  if (localrc == ESMF_SUCCESS) then
     print *, '  [OK] CDEPS stream initialized successfully'
     print *, '  Stream contains field: ', trim(filevars(1,2))
     test_passed = .true.
  else
     print *, '  [ERROR] CDEPS stream initialization failed with rc=', localrc
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 2: Try to get field with HEMCO name (should fail)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 2: Get field with HEMCO name (BC_agr)'
  print *, '-------------------------------------------'

  ! Try to get field using HEMCO field name
  call dshr_fldbun_getFldPtr(sdat%pstrm(1)%fldbun_model, 'BC_agr', dataPtr1d, rc=localrc)

  if (localrc == ESMF_SUCCESS) then
     print *, '  [OK] Field found with HEMCO name: BC_agr'
     print *, '  This may indicate the field naming is consistent'
  else
     print *, '  [EXPECTED FAILURE] dshr_fldbun_getFldPtr failed with rc=', localrc
     print *, '  This confirms the bug - HEMCO names don''t match CDEPS internal names'
     test_passed = .true.
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 3: Try to get field with CDEPS internal name (emission)
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 3: Get field with CDEPS internal name (emission)'
  print *, '-----------------------------------------------------'

  ! Try to get field using CDEPS internal name
  call dshr_fldbun_getFldPtr(sdat%pstrm(1)%fldbun_model, 'emission', dataPtr1d, rc=localrc)

  if (localrc == ESMF_SUCCESS) then
     print *, '  [OK] Field found with CDEPS name: emission'
     print *, '  CDEPS uses internal name "emission" for all fields'
     test_passed = .true.
  else
     print *, '  [EXPECTED FAILURE] dshr_fldbun_getFldPtr failed with rc=', localrc
     print *, '  This confirms the field naming mismatch'
  endif

  if (test_passed) passed_tests = passed_tests + 1
  print *, ''

  !==============================================================================
  ! TEST 4: List available fields in CDEPS stream
  !==============================================================================
  total_tests = total_tests + 1
  test_passed = .false.

  print *, 'TEST 4: List available fields in CDEPS stream'
  print *, '---------------------------------------------'

  ! Get field count
  call ESMF_FieldBundleGet(sdat%pstrm(1)%fldbun_model, fieldCount=fieldCount, rc=localrc)

  if (localrc == ESMF_SUCCESS .and. fieldCount > 0) then
     print *, '  Number of fields in CDEPS stream: ', fieldCount

     ! Get field names
     allocate(fieldNameList(fieldCount))
     call ESMF_FieldBundleGet(sdat%pstrm(1)%fldbun_model, fieldNameList=fieldNameList, rc=localrc)

     if (localrc == ESMF_SUCCESS) then
        print *, '  Available fields:'
        do i = 1, fieldCount
           print *, '    - ', trim(fieldNameList(i))
        end do

        ! Check if HEMCO names match
        if (any(index(fieldNameList, 'BC_agr') > 0)) then
           print *, '  HEMCO name BC_agr found in CDEPS fields'
        else
           print *, '  HEMCO name BC_agr NOT found in CDEPS fields (bug condition)'
           print *, '  CDEPS uses generic names like "emission" instead'
           test_passed = .true.
        endif
     else
        print *, '  Warning: Could not get field names'
     endif

     deallocate(fieldNameList)
  else
     print *, '  No fields in CDEPS stream or error'
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
     print *, 'CDEPS uses internal names (emission) while HEMCO expects specific names (BC_agr, etc.)'
  else
     print *, 'Some tests did not detect the expected behavior.'
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

end program test_field_naming
