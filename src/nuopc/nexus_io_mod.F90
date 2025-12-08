!
!==============================================================================
!
! !MODULE: nexus_io_mod
!
! !DESCRIPTION: Handles I/O operations for the NEXUS component, inspired by
!  MAPL ExtData and History components.
!
!==============================================================================

!> @brief Handles I/O operations for the NEXUS component.
!>
!> Inspired by MAPL ExtData and History components.
module nexus_io_mod

#ifdef USE_MPI
  use mpi
#endif
  use ESMF
  implicit none

  private

  public :: IO_Init, IO_Read, IO_Write

  !----------------------------------------------------------------------------
  ! Derived types for managing I/O streams
  !----------------------------------------------------------------------------

  !> @brief Type for managing history streams (output).
  type :: HistoryStream
    character(len=255) :: name
    character(len=255) :: fileName
    type(ESMF_TimeInterval) :: frequency
    character(len=10) :: mode ! "append" or "overwrite"
    character(len=255), allocatable :: variables(:)
    type(ESMF_Clock) :: clock
    logical :: initialized = .false.
  end type HistoryStream

  !> @brief Type for managing external data streams (input).
  type :: ExtDataStream
    character(len=255) :: name
    character(len=255) :: fileName
    integer :: filestream  ! For parallel I/O
    character(len=255), allocatable :: variables(:)
    character(len=20) :: timeInterpMethod
    type(ESMF_Grid) :: srcGrid
    type(ESMF_Field) :: srcField, srcField2
    type(ESMF_Field) :: dstField, dstField2
    type(ESMF_RouteHandle) :: routeHandle ! for regridding
  end type ExtDataStream

  ! Array to hold all streams
  type(HistoryStream), allocatable :: historyStreams(:)
  type(ExtDataStream), allocatable :: extDataStreams(:)

contains

  !> @brief Initializes the I/O layer by reading the io.rc file.
  !>
  !> @param dstGrid The destination grid to regrid to.
  !> @param rc      Return code.
  subroutine IO_Init(dstGrid, rc)
    use netcdf
#ifdef USE_MPI
    use mpi
#endif
    integer, intent(out) :: rc
    type(ESMF_Grid), intent(in) :: dstGrid

    integer :: unit, stat, num_hist_streams, num_ext_streams, i
    character(len=255) :: line, key, value
    character(len=255), allocatable :: hist_vars(:), ext_vars(:)
    logical :: in_history_block, in_extdata_block
    type(ESMF_VM) :: vm
    integer :: localPet

    ! For resizing arrays
    integer :: n
    character(len=255), allocatable :: temp_vars(:)

    ! For regridding setup
    integer :: ncid, dimid, varid, ncerr
    integer :: dimLengths(2)
    character(len=255) :: lon_var, lat_var
    real, pointer :: fp(:,:)
    integer :: root_rc

    rc = ESMF_SUCCESS
    in_history_block = .false.
    in_extdata_block = .false.
    num_hist_streams = 0
    num_ext_streams = 0

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (localPet == 0) then
      open(newunit=unit, file="io.rc", status='old', iostat=stat)
      if (stat /= 0) then
        rc = ESMF_FAILURE
      else
        ! First pass: count the number of streams
        do
          read(unit, '(a)', end=10) line
          if (trim(line) == ':: HISTORY ::') then
            num_hist_streams = num_hist_streams + 1
          else if (trim(line) == ':: EXTDATA ::') then
            num_ext_streams = num_ext_streams + 1
          end if
        end do
10      continue
        rewind(unit)
      endif
    endif

#ifdef USE_MPI
    call ESMF_VMBroadcast(vm, num_hist_streams, 1, 0, rc=rc)
    call ESMF_VMBroadcast(vm, num_ext_streams, 1, 0, rc=rc)
    call ESMF_VMBroadcast(vm, rc, 1, 0, rc=rc)
#endif
    if (rc == ESMF_FAILURE) return

    if (num_hist_streams > 0) allocate(historyStreams(num_hist_streams))
    if (num_ext_streams > 0) allocate(extDataStreams(num_ext_streams))

    ! Second pass: parse the streams
    if (localPet == 0) then
      num_hist_streams = 0
      num_ext_streams = 0
      do
        read(unit, '(a)', end=20) line
        if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

        if (in_history_block) then
          if (trim(line) == '::') then
            in_history_block = .false.
            if (allocated(hist_vars)) then
              historyStreams(num_hist_streams)%variables = hist_vars
              deallocate(hist_vars)
            end if
            cycle
          end if

          key = trim(adjustl(line(1:index(line,':')-1)))
          value = trim(adjustl(line(index(line,':')+1:)))

          select case (trim(key))
            case ('NAME')
              historyStreams(num_hist_streams)%name = value
            case ('FREQUENCY')
              read(value, *) stat
              call ESMF_TimeIntervalSet(historyStreams(num_hist_streams)%frequency, s=stat, rc=rc)
            case ('FILE')
              historyStreams(num_hist_streams)%fileName = value
            case ('MODE')
              historyStreams(num_hist_streams)%mode = value
            case ('VARIABLES')
              ! This is a label for the variable list, do nothing
            case default
              if (line(1:1) == '-') then
                if (allocated(hist_vars)) then
                  n = size(hist_vars)
                  call move_alloc(hist_vars, temp_vars)
                  allocate(hist_vars(n + 1))
                  hist_vars(1:n) = temp_vars
                  hist_vars(n + 1) = trim(adjustl(line(2:)))
                else
                  allocate(hist_vars(1))
                  hist_vars(1) = trim(adjustl(line(2:)))
                end if
              end if
          end select
        else if (in_extdata_block) then
          if (trim(line) == '::') then
            in_extdata_block = .false.
            if (allocated(ext_vars)) then
              extDataStreams(num_ext_streams)%variables = ext_vars
              deallocate(ext_vars)
            end if
            cycle
          end if

          key = trim(adjustl(line(1:index(line,':')-1)))
          value = trim(adjustl(line(index(line,':')+1:)))

          select case (trim(key))
            case ('NAME')
              extDataStreams(num_ext_streams)%name = value
            case ('FILE')
              extDataStreams(num_ext_streams)%fileName = value
            case ('TIME_INTERPOLATION')
              extDataStreams(num_ext_streams)%timeInterpMethod = value
            case ('VARIABLES')
              ! This is a label for the variable list, do nothing
            case default
              if (line(1:1) == '-') then
                if (allocated(ext_vars)) then
                  n = size(ext_vars)
                  call move_alloc(ext_vars, temp_vars)
                  allocate(ext_vars(n + 1))
                  ext_vars(1:n) = temp_vars
                  ext_vars(n + 1) = trim(adjustl(line(2:)))
                else
                  allocate(ext_vars(1))
                  ext_vars(1) = trim(adjustl(line(2:)))
                end if
              end if
          end select
        else
          if (trim(line) == ':: HISTORY ::') then
            in_history_block = .true.
            num_hist_streams = num_hist_streams + 1
          else if (trim(line) == ':: EXTDATA ::') then
            in_extdata_block = .true.
            num_ext_streams = num_ext_streams + 1
          end if
        end if
      end do
20    continue
      close(unit)
    endif

#ifdef USE_MPI
    ! Broadcast the stream definitions
    do i = 1, size(historyStreams)
        call ESMF_VMBroadcast(vm, historyStreams(i)%name, len(historyStreams(i)%name), 0, rc=rc)
        call ESMF_VMBroadcast(vm, historyStreams(i)%fileName, len(historyStreams(i)%fileName), 0, rc=rc)
        call ESMF_VMBroadcast(vm, historyStreams(i)%mode, len(historyStreams(i)%mode), 0, rc=rc)
        call ESMF_VMBroadcast(vm, historyStreams(i)%frequency, 1, 0, rc=rc)
        if (localPet /= 0) then
            if (allocated(historyStreams(i)%variables)) deallocate(historyStreams(i)%variables)
            allocate(historyStreams(i)%variables(1))
        endif
        n = size(historyStreams(i)%variables)
        call ESMF_VMBroadcast(vm, n, 1, 0, rc=rc)
        if (localPet /= 0) then
            deallocate(historyStreams(i)%variables)
            allocate(historyStreams(i)%variables(n))
        endif
        call ESMF_VMBroadcast(vm, historyStreams(i)%variables, n*len(historyStreams(i)%variables(1)), 0, rc=rc)
    enddo
    do i = 1, size(extDataStreams)
        call ESMF_VMBroadcast(vm, extDataStreams(i)%name, len(extDataStreams(i)%name), 0, rc=rc)
        call ESMF_VMBroadcast(vm, extDataStreams(i)%fileName, len(extDataStreams(i)%fileName), 0, rc=rc)
        call ESMF_VMBroadcast(vm, extDataStreams(i)%timeInterpMethod, len(extDataStreams(i)%timeInterpMethod), 0, rc=rc)
        if (localPet /= 0) then
            if (allocated(extDataStreams(i)%variables)) deallocate(extDataStreams(i)%variables)
            allocate(extDataStreams(i)%variables(1))
        endif
        n = size(extDataStreams(i)%variables)
        call ESMF_VMBroadcast(vm, n, 1, 0, rc=rc)
        if (localPet /= 0) then
            deallocate(extDataStreams(i)%variables)
            allocate(extDataStreams(i)%variables(n))
        endif
        call ESMF_VMBroadcast(vm, extDataStreams(i)%variables, n*len(extDataStreams(i)%variables(1)), 0, rc=rc)
    enddo
#endif

    ! Initialize initialized flag for each history stream
    do i = 1, size(historyStreams)
       historyStreams(i)%initialized = .false.
    end do

    ! Pre-compute regridding weights for ExtData streams
    do i = 1, size(extDataStreams)
      ! Create source grid from file in parallel
      lon_var = "lon"
      lat_var = "lat"

#ifdef USE_MPI
      call ESMF_VMBroadcast(vm, root_rc, 1, 0, rc=rc)
#endif
      if (root_rc /= ESMF_SUCCESS) cycle
#ifdef USE_MPI
      call ESMF_VMBroadcast(vm, dimLengths, 2, 0, rc=rc)
#endif

#ifdef USE_PNETCDF
      ! In parallel, all PETs open the file and read the metadata.
      ncerr = nf90_open_par(trim(extDataStreams(i)%fileName), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
      if (ncerr /= nf90_noerr) then
        print *, "Error opening ", trim(extDataStreams(i)%fileName)
        rc = ESMF_FAILURE
      else
        ncerr = nf90_inq_dimid(ncid, lon_var, dimid)
        ncerr = nf90_inquire_dimension(ncid, dimid, len=dimLengths(1))
        ncerr = nf90_inq_dimid(ncid, lat_var, dimid)
        ncerr = nf90_inquire_dimension(ncid, dimid, len=dimLengths(2))
        ncerr = nf90_close(ncid)
      endif
#else
      root_rc = ESMF_SUCCESS
      ! Read dimensions on root PET and broadcast
      if (localPet == 0) then
         ncerr = nf90_open(extDataStreams(i)%fileName, NF90_NOWRITE, ncid)
         if (ncerr /= nf90_noerr) then
           print *, "Error opening ", trim(extDataStreams(i)%fileName)
           root_rc = ESMF_FAILURE
         else
           ncerr = nf90_inq_dimid(ncid, lon_var, dimid)
           ncerr = nf90_inquire_dimension(ncid, dimid, len=dimLengths(1))
           ncerr = nf90_inq_dimid(ncid, lat_var, dimid)
           ncerr = nf90_inquire_dimension(ncid, dimid, len=dimLengths(2))
           ncerr = nf90_close(ncid)
         endif
      endif
#ifdef USE_MPI
      call ESMF_VMBroadcast(vm, root_rc, 1, 0, rc=rc)
#endif
      if (root_rc /= ESMF_SUCCESS) cycle
#ifdef USE_MPI
      call ESMF_VMBroadcast(vm, dimLengths, 2, 0, rc=rc)
#endif
#endif


      extDataStreams(i)%srcGrid = ESMF_GridCreateNoPeriDim(maxIndex=dimLengths, &
                                                           coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)

      ! Add coordinates to srcGrid
      call ESMF_GridAddCoord(extDataStreams(i)%srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)


#ifdef USE_PNETCDF
      ! In parallel, all PETs open the file and read the coordinates.
      ncerr = nf90_open_par(trim(extDataStreams(i)%fileName), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
      if (ncerr /= nf90_noerr) then
        print *, "Error opening ", trim(extDataStreams(i)%fileName)
        rc = ESMF_FAILURE
      else
        ! Get pointer to lon coord and read from file
        call ESMF_GridGetCoord(extDataStreams(i)%srcGrid, 1, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fp, rc=rc)
        ncerr = nf90_inq_varid(ncid, lon_var, varid)
        ncerr = nf90_get_var(ncid, varid, fp)

        ! Get pointer to lat coord and read from file
        call ESMF_GridGetCoord(extDataStreams(i)%srcGrid, 2, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fp, rc=rc)
        ncerr = nf90_inq_varid(ncid, lat_var, varid)
        ncerr = nf90_get_var(ncid, varid, fp)
        ncerr = nf90_close(ncid)
      endif
#else
      ! Read coordinates on root and broadcast. This part is not fully parallel
      ! but is required to set up the distributed grid correctly.
      if (localPet == 0) then
        ncerr = nf90_open(extDataStreams(i)%fileName, NF90_NOWRITE, ncid)
        ! Get pointer to lon coord and read from file
        call ESMF_GridGetCoord(extDataStreams(i)%srcGrid, 1, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fp, rc=rc)
        ncerr = nf90_inq_varid(ncid, lon_var, varid)
        ncerr = nf90_get_var(ncid, varid, fp)

        ! Get pointer to lat coord and read from file
        call ESMF_GridGetCoord(extDataStreams(i)%srcGrid, 2, staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=fp, rc=rc)
        ncerr = nf90_inq_varid(ncid, lat_var, varid)
        ncerr = nf90_get_var(ncid, varid, fp)
        ncerr = nf90_close(ncid)
      endif

#ifdef USE_MPI
      ! Broadcast coordinates to all PETs
      call ESMF_GridBroadcast(extDataStreams(i)%srcGrid, rootPet=0, rc=rc)
#endif
#endif

      ! Create source and destination fields
      extDataStreams(i)%srcField = ESMF_FieldCreate(extDataStreams(i)%srcGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="srcField", rc=rc)
      extDataStreams(i)%dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="dstField", rc=rc)
      extDataStreams(i)%srcField2 = ESMF_FieldCreate(extDataStreams(i)%srcGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="srcField2", rc=rc)
      extDataStreams(i)%dstField2 = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="dstField2", rc=rc)

      ! Pre-compute regridding weights
      call ESMF_FieldRegridStore(extDataStreams(i)%srcField, extDataStreams(i)%dstField, &
                                 regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                                 unmappedaction=ESMF_UNMAPPEDACTION_ERROR, &
                                 routehandle=extDataStreams(i)%routeHandle, rc=rc)
    end do

    print *, "NEXUS_IO: Initialized ", num_hist_streams, " history streams and ", &
             num_ext_streams, " extdata streams."

  end subroutine IO_Init

  !> @brief Reads data from external files into the importState.
  !>
  !> @param state The ESMF state to read data into (importState).
  !> @param clock The current ESMF clock.
  !> @param rc    Return code.
  subroutine IO_Read(state, clock, rc)
    use netcdf
#ifdef USE_MPI
    use mpi
#endif
    type(ESMF_State), intent(inout) :: state
    type(ESMF_Clock), intent(in)    :: clock
    integer, intent(out)           :: rc

    integer :: i, j, k, localrc
    type(ESMF_Time) :: currTime
    type(ESMF_VM) :: vm
    integer :: localPet

    ! For reading and interpolation
    integer :: ncid, varid, timeid, time_ndims, time_dimids(1)
    integer, allocatable :: time_dimlens(:)
    character(len=255) :: time_units
    integer :: start(3), count(3)
    real(ESMF_KIND_R8), allocatable :: data_slice(:)
    real(ESMF_KIND_R8), allocatable :: time_vals(:)
    integer :: t1_idx, t2_idx
    real(ESMF_KIND_R8) :: w1, w2
    type(ESMF_Field) :: final_field
    real, pointer :: final_ptr(:,:)
    real, pointer :: src_ptr_t1(:,:), src_ptr_t2(:,:)
    real, pointer :: dst_ptr_t1(:,:), dst_ptr_t2(:,:)
    type(ESMF_Time) :: base_time, t1, t2
    type(ESMF_TimeInterval) :: ti
    real(ESMF_KIND_R8) :: time_diff, total_diff
    integer :: root_rc
    type(ESMF_Field) :: tempField

    rc = ESMF_SUCCESS
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    do i = 1, size(extDataStreams)

#ifdef USE_PNETCDF
      ! In parallel, all PETs open the file and read the time metadata.
      localrc = nf90_open_par(trim(extDataStreams(i)%fileName), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
      if (localrc /= nf90_noerr) then
        root_rc = ESMF_FAILURE
      else
        ! Read time coordinate and units
        localrc = nf90_inq_varid(ncid, "time", timeid)
        if (localrc == nf90_noerr) then
          localrc = nf90_get_att(ncid, timeid, "units", time_units)
          call parse_time_units(time_units, base_time, rc=localrc)

          localrc = nf90_inquire_variable(ncid, timeid, ndims=time_ndims, dimids=time_dimids)
          allocate(time_dimlens(time_ndims))
          localrc = nf90_inquire_dimension(ncid, time_dimids(1), len=time_dimlens(1))
          allocate(time_vals(time_dimlens(1)))
          localrc = nf90_get_var(ncid, timeid, time_vals)

          ! Find bracketing indices and weights
          t1_idx = -1
          t2_idx = -1
          do k = 1, size(time_vals) - 1
            call ESMF_TimeIntervalSet(ti, s_r8=time_vals(k), rc=localrc)
            t1 = base_time + ti
            call ESMF_TimeIntervalSet(ti, s_r8=time_vals(k+1), rc=localrc)
            t2 = base_time + ti
            if (t1 <= currTime .and. currTime <= t2) then
              t1_idx = k
              t2_idx = k + 1
              exit
            end if
          end do

          if (t1_idx < 0) then
            if (currTime < t1) then
              t1_idx = 1; t2_idx = 1; w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
            else
              t1_idx = size(time_vals); t2_idx = size(time_vals); w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
            end if
          else if (t1_idx == t2_idx) then
            w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
          else
            call ESMF_TimeGet(t1, s_r8=time_diff, rc=localrc)
            call ESMF_TimeGet(t2, s_r8=total_diff, rc=localrc)
            total_diff = total_diff - time_diff
            call ESMF_TimeGet(currTime, s_r8=time_diff, rc=localrc)
            call ESMF_TimeGet(t1, s_r8=w1, rc=localrc)
            time_diff = time_diff - w1
            if (total_diff > 0) then
              w2 = time_diff / total_diff
              w1 = 1.0_ESMF_KIND_R8 - w2
            else
              w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
            endif
          endif
        endif
        localrc = nf90_close(ncid)
      endif
#else
      ! Time information is read on root PET and broadcasted
      if (localPet == 0) then
        localrc = nf90_open(extDataStreams(i)%fileName, NF90_NOWRITE, ncid)
        if (localrc /= nf90_noerr) then
          root_rc = ESMF_FAILURE
        else
          ! Read time coordinate and units
          localrc = nf90_inq_varid(ncid, "time", timeid)
          if (localrc == nf90_noerr) then
            localrc = nf90_get_att(ncid, timeid, "units", time_units)
            call parse_time_units(time_units, base_time, rc=localrc)

            localrc = nf90_inquire_variable(ncid, timeid, ndims=time_ndims, dimids=time_dimids)
            allocate(time_dimlens(time_ndims))
            localrc = nf90_inquire_dimension(ncid, time_dimids(1), len=time_dimlens(1))
            allocate(time_vals(time_dimlens(1)))
            localrc = nf90_get_var(ncid, timeid, time_vals)

            ! Find bracketing indices and weights
            t1_idx = -1
            t2_idx = -1
            do k = 1, size(time_vals) - 1
              call ESMF_TimeIntervalSet(ti, s_r8=time_vals(k), rc=localrc)
              t1 = base_time + ti
              call ESMF_TimeIntervalSet(ti, s_r8=time_vals(k+1), rc=localrc)
              t2 = base_time + ti
              if (t1 <= currTime .and. currTime <= t2) then
                t1_idx = k
                t2_idx = k + 1
                exit
              end if
            end do

            if (t1_idx < 0) then
              if (currTime < t1) then
                t1_idx = 1; t2_idx = 1; w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
              else
                t1_idx = size(time_vals); t2_idx = size(time_vals); w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
              end if
            else if (t1_idx == t2_idx) then
              w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
            else
              call ESMF_TimeGet(t1, s_r8=time_diff, rc=localrc)
              call ESMF_TimeGet(t2, s_r8=total_diff, rc=localrc)
              total_diff = total_diff - time_diff
              call ESMF_TimeGet(currTime, s_r8=time_diff, rc=localrc)
              call ESMF_TimeGet(t1, s_r8=w1, rc=localrc)
              time_diff = time_diff - w1
              if (total_diff > 0) then
                w2 = time_diff / total_diff
                w1 = 1.0_ESMF_KIND_R8 - w2
              else
                w1 = 1.0_ESMF_KIND_R8; w2 = 0.0_ESMF_KIND_R8
              endif
            endif
          endif
          localrc = nf90_close(ncid)
        endif
      endif
#endif

#ifdef USE_MPI
      ! Broadcast time interpolation data
      call ESMF_VMBroadcast(vm, root_rc, 1, 0, rc=rc)
#endif
      if (root_rc /= ESMF_SUCCESS) cycle
#ifdef USE_MPI
      call ESMF_VMBroadcast(vm, t1_idx, 1, 0, rc=rc)
      call ESMF_VMBroadcast(vm, t2_idx, 1, 0, rc=rc)
      call ESMF_VMBroadcast(vm, w1, 1, 0, rc=rc)
      call ESMF_VMBroadcast(vm, w2, 1, 0, rc=rc)
#endif

      do j = 1, size(extDataStreams(i)%variables)
        ! Read data for t1_idx
        call ESMF_FieldRead(extDataStreams(i)%srcField, trim(extDataStreams(i)%fileName), &
                             iofmt=ESMF_IOFMT_NETCDF, fieldName=trim(extDataStreams(i)%variables(j)), &
                             timesliceList=(/t1_idx/), rc=localrc)
        call ESMF_FieldRegrid(extDataStreams(i)%srcField, extDataStreams(i)%dstField, &
                              routehandle=extDataStreams(i)%routeHandle, rc=localrc)

        if (t1_idx /= t2_idx) then
          ! Read data for t2_idx
          call ESMF_FieldRead(extDataStreams(i)%srcField2, trim(extDataStreams(i)%fileName), &
                               iofmt=ESMF_IOFMT_NETCDF, fieldName=trim(extDataStreams(i)%variables(j)), &
                               timesliceList=(/t2_idx/), rc=localrc)
          call ESMF_FieldRegrid(extDataStreams(i)%srcField2, extDataStreams(i)%dstField2, &
                                routehandle=extDataStreams(i)%routeHandle, rc=localrc)
        endif

        ! Get pointers to destination fields
        call ESMF_FieldGet(extDataStreams(i)%dstField, farrayPtr=dst_ptr_t1, rc=localrc)
        if (t1_idx /= t2_idx) then
          call ESMF_FieldGet(extDataStreams(i)%dstField2, farrayPtr=dst_ptr_t2, rc=localrc)
        endif

        ! Get pointer to final field in import state
        call ESMF_StateGet(state, trim(extDataStreams(i)%variables(j)), final_field, rc=localrc)
        if (localrc /= ESMF_SUCCESS) cycle
        call ESMF_FieldGet(final_field, farrayPtr=final_ptr, rc=localrc)

        ! Time interpolate
        if (t1_idx == t2_idx) then
          final_ptr(:,:) = dst_ptr_t1
        else
          final_ptr(:,:) = w1 * dst_ptr_t1 + w2 * dst_ptr_t2
        endif

      enddo
    enddo

  end subroutine IO_Read

    !> @brief Writes data from the exportState to history files.
    !>
    !> @param state The ESMF state containing data to write (exportState).
    !> @param clock The current ESMF clock.
    !> @param rc    Return code.
    subroutine IO_Write(state, clock, rc)

      type(ESMF_State), intent(in) :: state

      type(ESMF_Clock), intent(in) :: clock

      integer, intent(out)        :: rc



      integer :: i, j, localrc
      type(ESMF_Time) :: currTime

      type(ESMF_Field) :: field

      logical :: isTime, isInit

      type(ESMF_FieldBundle) :: bundle

      type(ESMF_Field), allocatable :: fieldList(:)
      type(ESMF_Time) :: streamTime


      rc = ESMF_SUCCESS


      do i = 1, size(historyStreams)

        ! Check if it is time to write to this stream

        if (.not. historyStreams(i)%initialized) then

          call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)

          historyStreams(i)%clock = ESMF_ClockCreate(name=trim(historyStreams(i)%name)//"_clock", &
                                                     timeStep=historyStreams(i)%frequency, &
                                                     startTime=currTime, rc=localrc)
          historyStreams(i)%initialized = .true.

        end if

        call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
        call ESMF_ClockGet(historyStreams(i)%clock, currTime=streamTime, rc=localrc)

        if (currTime == streamTime) then

          print *, "NEXUS_IO: Writing to stream '", trim(historyStreams(i)%name), "'"



          ! Create a field bundle to write all variables at once

          allocate(fieldList(size(historyStreams(i)%variables)))

          do j = 1, size(historyStreams(i)%variables)

            call ESMF_StateGet(state, trim(historyStreams(i)%variables(j)), field, rc=localrc)

            if (localrc == ESMF_SUCCESS) then

              fieldList(j) = field

            else

              print *, "NEXUS_IO: Variable not found in export state: ", trim(historyStreams(i)%variables(j))

              ! Skip this variable

            end if

          end do



          bundle = ESMF_FieldBundleCreate(name="history_bundle", fieldList=fieldList, rc=localrc)

          call ESMF_FieldBundleWrite(bundle, historyStreams(i)%fileName, &

                                     overwrite=(historyStreams(i)%mode == "overwrite"), &

                                     iofmt=ESMF_IOFMT_NETCDF, rc=localrc)

          call ESMF_FieldBundleDestroy(bundle, rc=localrc)

          deallocate(fieldList)



          if (localrc /= ESMF_SUCCESS) then

            rc = localrc

            return

          end if



          ! Advance the stream's clock

          call ESMF_ClockAdvance(historyStreams(i)%clock, rc=localrc)

        end if

      end do



    end subroutine IO_Write



    !> @brief Parses a time unit string like "seconds since YYYY-MM-DD..."
    !>
    !> @param unit_string The time unit string to parse.
    !> @param base_time   The parsed base time.
    !> @param rc          Return code.
    subroutine parse_time_units(unit_string, base_time, rc)

      character(len=*), intent(in) :: unit_string

      type(ESMF_Time), intent(out) :: base_time

      integer, intent(out) :: rc



      integer :: yy, mm, dd, h, m, s

      character(len=10) :: date_str

      character(len=8) :: time_str



      rc = ESMF_SUCCESS



      ! Assuming format "seconds since YYYY-MM-DD HH:MM:SS"

      ! TODO: Make this more robust

      read(unit_string(15:24), '(a)') date_str

      read(unit_string(26:33), '(a)') time_str



      read(date_str(1:4), '(i4)') yy

      read(date_str(6:7), '(i2)') mm

      read(date_str(9:10), '(i2)') dd

      read(time_str(1:2), '(i2)') h

      read(time_str(4:5), '(i2)') m

      read(time_str(7:8), '(i2)') s



      call ESMF_TimeSet(base_time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)



    end subroutine parse_time_units



end module nexus_io_mod
