!> @file nexus_cdeps_inline_mod.F90
!> @brief NEXUS inline CDEPS module for direct data stream reading
!> @details Implements inline CDEPS capability following UFS atmospheric model pattern.
!> Bypasses NUOPC coupling infrastructure and reads data directly via CDEPS.

module nexus_cdeps_inline_mod

  use ESMF
  use dshr_mod,          only: dshr_pio_init
  use dshr_strdata_mod,  only: shr_strdata_type, &
                               shr_strdata_init_from_inline, &
                               shr_strdata_advance
  use dshr_stream_mod,   only: shr_stream_init_from_esmfconfig
  use dshr_methods_mod,  only: dshr_fldbun_getfldptr
  use shr_kind_mod,      only: r8 => shr_kind_r8

  implicit none

  private

  public :: nexus_cdeps_init
  public :: nexus_cdeps_run
  public :: nexus_cdeps_finalize
  public :: nexus_cdeps_get_data_pointer
  public :: nexus_cdeps_get_available_fields

  !> Grid and mesh objects for data interpolation
  type(ESMF_Mesh), save :: model_mesh

  !> Stream data configuration and instances
  type(shr_strdata_type), allocatable, save :: sdat(:)
  integer, save :: num_cdeps_streams = 0

  !> Helper type for parsing configuration
  type :: StreamConfig
    character(len=256) :: name
    character(len=512) :: datafile
    character(len=256), allocatable :: vars(:)
    integer :: num_vars
    ! Default properties
    integer :: year_first, year_last, year_align
  end type StreamConfig

  !> Module state
  integer :: debug_level = 0
  integer :: log_unit = 6
  logical, save :: initialized = .false.
  real(kind=8), parameter :: missing_value = 9.99d20

  !> Constants
  character(len=*), parameter :: CDEPS_CONFIG = "nexus_input_streams.yaml"

contains

  !> @brief Initialize inline CDEPS for NEXUS
  !> @param[in] mesh ESMF Mesh
  !> @param[in] clock Model clock
  !> @param[out] rc Return code
  subroutine nexus_cdeps_init(mesh, clock, rc)

    type(ESMF_Mesh),     intent(in)  :: mesh
    type(ESMF_Clock),    intent(in)  :: clock
    integer,             intent(out) :: rc

    ! Local variables
    integer :: local_pet, localrc
    integer :: i
    type(ESMF_VM) :: vm
    type(StreamConfig), allocatable :: configs(:)

    rc = ESMF_SUCCESS

    ! Get local PET
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_VMGet(vm, localPet=local_pet, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    if (local_pet == 0) then
      write(log_unit,*) 'NEXUS: Inline CDEPS initialization'
    end if

    ! store mesh
    model_mesh = mesh

    ! ------------------------------------------------------------------
    ! 1. Parse YAML configuration
    ! ------------------------------------------------------------------
    call ParseYAMLConfig(CDEPS_CONFIG, configs, num_cdeps_streams, rc)
    if (rc /= ESMF_SUCCESS) then
        if (local_pet == 0) print *, "nexus_cdeps_init: Failed to parse YAML config"
        return
    endif

    if (num_cdeps_streams > 0) then
        allocate(sdat(num_cdeps_streams))
        if (local_pet == 0) print *, "nexus_cdeps_init: Found ", num_cdeps_streams, " streams in YAML"

        ! Initialize each stream
        do i = 1, num_cdeps_streams
             call InitializeSingleCDEPSStream(i, configs(i), mesh, clock, localrc)
             if (localrc /= ESMF_SUCCESS) then
                 if (local_pet == 0) print *, "nexus_cdeps_init: Failed to initialize stream ", i
                 rc = localrc
                 return
             endif
        enddo
    else
        if (local_pet == 0) print *, "nexus_cdeps_init: No streams found"
    end if

    ! Cleanup configs
    if (allocated(configs)) then
        do i = 1, size(configs)
            if (allocated(configs(i)%vars)) deallocate(configs(i)%vars)
        enddo
        deallocate(configs)
    endif

    initialized = .true.
    if (local_pet == 0) then
      write(log_unit,*) 'NEXUS: Inline CDEPS initialization completed'
    end if

  end subroutine nexus_cdeps_init

  !> @brief Parse YAML configuration file
  subroutine ParseYAMLConfig(filename, configs, num_streams, rc)
    character(len=*), intent(in) :: filename
    type(StreamConfig), allocatable, intent(out) :: configs(:)
    integer, intent(out) :: num_streams
    integer, intent(out) :: rc

    integer :: unit_num, ios, i, n, v
    character(len=512) :: line
    character(len=256) :: temp_str
    integer :: parsing_stage
    logical :: file_exists

    rc = ESMF_SUCCESS
    num_streams = 0

    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
        print *, "ParseYAMLConfig: File not found: ", trim(filename)
        return
    endif

    ! First pass: Count streams
    open(newunit=unit_num, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "ParseYAMLConfig: Could not open ", trim(filename)
        rc = ESMF_FAILURE
        return
    endif

    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '- name:') > 0) then
            num_streams = num_streams + 1
        endif
    enddo
    rewind(unit_num)

    if (num_streams == 0) then
        close(unit_num)
        return
    endif

    allocate(configs(num_streams))

    ! Second pass: Parse data
    n = 0
    parsing_stage = 0 ! 0: searching, 1: in stream, 2: in datavars

    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit

        ! Check for new stream
        if (index(line, '- name:') > 0) then
            n = n + 1
            parsing_stage = 1
            ! Parse name
            i = index(line, ':')
            configs(n)%name = adjustl(trim(line(i+1:)))
            ! Defaults
            configs(n)%year_first = 2000
            configs(n)%year_last = 2023
            configs(n)%year_align = 2000
            configs(n)%num_vars = 0
            configs(n)%datafile = ""
            continue
        endif

        if (parsing_stage > 0) then
            if (index(line, 'datafiles:') > 0) then
                i = index(line, ':')
                temp_str = adjustl(trim(line(i+1:)))
                ! Remove potential quotes
                if (temp_str(1:1) == '"' .or. temp_str(1:1) == "'") then
                    configs(n)%datafile = temp_str(2:len_trim(temp_str)-1)
                else
                    configs(n)%datafile = trim(temp_str)
                endif
            elseif (index(line, 'datavars:') > 0) then
                parsing_stage = 2
            elseif (parsing_stage == 2 .and. trim(line) /= '' .and. index(line, '-') > 0) then
                ! Count vars first to allocate?
                ! Simplified: we assume vars are listed correctly
                ! We'll just count them in a buffer pass or implement dynamic list.
                ! For now, let's just count vars in a sub-loop or assume max vars.
                ! Re-reading logic is complex in Fortran.
                ! Let's just create a temporary large array for vars.
                ! We really need to count vars first or use a linked list.
                ! Given constraint, let's implement a pre-counter or fixed max size.
                ! Or re-allocate.

                ! Let's allocate with size 100 for now and resize if needed.
                if (.not. allocated(configs(n)%vars)) allocate(configs(n)%vars(100))

                configs(n)%num_vars = configs(n)%num_vars + 1
                i = index(line, '-')
                temp_str = adjustl(trim(line(i+1:)))
                configs(n)%vars(configs(n)%num_vars) = trim(temp_str)
            elseif (parsing_stage == 2 .and. trim(line) /= '' .and. index(line, '-') == 0 .and. index(line, ':') > 0) then
                ! New key found, stop parsing datavars
                parsing_stage = 1
            endif

            ! Parse years
            if (index(line, 'year_first:') > 0) read(line(index(line,':')+1:), *) configs(n)%year_first
            if (index(line, 'year_last:') > 0)  read(line(index(line,':')+1:), *) configs(n)%year_last
            if (index(line, 'year_align:') > 0) read(line(index(line,':')+1:), *) configs(n)%year_align
        endif
    enddo
    close(unit_num)

    ! Resize var arrays
    do i = 1, num_streams
        if (configs(i)%num_vars > 0) then
            character(len=256), allocatable :: temp_vars(:)
            allocate(temp_vars(configs(i)%num_vars))
            temp_vars = configs(i)%vars(1:configs(i)%num_vars)
            deallocate(configs(i)%vars)
            allocate(configs(i)%vars(configs(i)%num_vars))
            configs(i)%vars = temp_vars
            deallocate(temp_vars)
        else
             if (.not. allocated(configs(i)%vars)) allocate(configs(i)%vars(0))
        endif
    enddo

  end subroutine ParseYAMLConfig

  !> @brief Initialize a single CDEPS stream
  !> @param[in] stream_idx Stream index
  !> @param[in] config Stream config
  !> @param[in] mesh ESMF Mesh
  !> @param[in] clock ESMF clock
  !> @param[out] rc Return code
  subroutine InitializeSingleCDEPSStream(stream_idx, config, mesh, clock, rc)
    integer, intent(in) :: stream_idx
    type(StreamConfig), intent(in) :: config
    type(ESMF_Mesh), intent(in) :: mesh
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    character(len=ESMF_MAXSTR), allocatable :: filelist(:)
    character(len=ESMF_MAXSTR), allocatable :: filevars(:,:)
    character(len=64) :: stream_name
    integer :: localPet, localrc, i
    type(ESMF_VM) :: vm

    call ESMF_VMGetCurrent(vm, rc=localrc)
    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)

    rc = ESMF_SUCCESS

    ! Allocate file lists
    allocate(filelist(1)) ! Readmode single
    allocate(filevars(config%num_vars, 2))

    ! Set file path
    filelist(1) = trim(config%datafile)

    ! Set vars
    do i = 1, config%num_vars
        filevars(i,1) = trim(config%vars(i)) ! file name
        filevars(i,2) = trim(config%vars(i)) ! model name (assuming same for now)
    enddo

    if (localPet == 0) then
        print *, "InitializeSingleCDEPSStream: Initializing stream ", trim(config%name)
        print *, "  File: ", trim(filelist(1))
        print *, "  Vars: ", config%num_vars
    endif

    write(stream_name,fmt='(a,i2.2)') 'nexus_stream_', stream_idx
    sdat(stream_idx)%model_clock = clock

    ! Allocate and set up the pstrm structure
    allocate(sdat(stream_idx)%pstrm(1))
    allocate(sdat(stream_idx)%pstrm(1)%fldlist_model(config%num_vars))
    do i=1, config%num_vars
        sdat(stream_idx)%pstrm(1)%fldlist_model(i) = trim(filevars(i,2))
    enddo

    ! Initialize CDEPS stream
    call shr_strdata_init_from_inline(sdat(stream_idx),           &
           my_task             = localPet,                        &
           logunit             = log_unit,                        &
           compname            = 'NEXUS',                         &
           model_clock         = clock,                           &
           model_mesh          = mesh,                            &
           stream_name         = trim(stream_name),               &
           stream_meshfile     = 'unset',                         &
           stream_filenames    = filelist,                        &
           stream_yearFirst    = config%year_first,               &
           stream_yearLast     = config%year_last,                &
           stream_yearAlign    = config%year_align,               &
           stream_fldlistFile  = filevars(:,1),                   &
           stream_fldListModel = filevars(:,2),                   &
           stream_lev_dimname  = 'unset',                         &
           stream_mapalgo      = 'bilinear',                      &
           stream_offset       = 0,                               &
           stream_taxmode      = 'cycle',                         &
           stream_dtlimit      = 1.5_r8,                          &
           stream_tintalgo     = 'linear',                        &
           stream_src_mask     = 0,                               &
           stream_dst_mask     = 0,                               &
           rc                  = localrc)

    rc = localrc
    deallocate(filelist)
    deallocate(filevars)

  end subroutine InitializeSingleCDEPSStream

  !> @brief Run inline CDEPS to advance data streams
  !> @param[in] clock Model clock
  !> @param[out] rc Return code
  subroutine nexus_cdeps_run(clock, rc)

    type(ESMF_Clock), intent(in)  :: clock
    integer,          intent(out) :: rc

    integer :: local_pet, i, localrc
    type(ESMF_Time) :: currTime
    integer :: yy, mm, dd, h, m, s
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=local_pet, rc=rc)

    if (.not. initialized) return

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)

    do i = 1, num_cdeps_streams
        call shr_strdata_advance(sdat(i), ymd=yy*10000+mm*100+dd, tod=h*3600+m*60+s, &
                                 logunit=log_unit, istr='NEXUS', rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
             if (local_pet == 0) print *, "nexus_cdeps_run: Error advancing stream ", i
             rc = localrc
        endif
    enddo

  end subroutine nexus_cdeps_run

  !> @brief Get data pointer for a specific field from CDEPS
  !> @param[in] field_name Name of the field to retrieve
  !> @param[out] data_ptr Pointer to the data array (1D packed)
  !> @param[out] rc Return code
  subroutine nexus_cdeps_get_data_pointer(field_name, data_ptr, rc)

    character(len=*), intent(in) :: field_name
    real(kind=8), pointer, intent(out) :: data_ptr(:)
    integer, intent(out) :: rc

    integer :: i
    logical :: found

    rc = ESMF_SUCCESS
    data_ptr => null()
    found = .false.

    do i = 1, num_cdeps_streams
        ! Try to get pointer for the requested field name
        call dshr_fldbun_getFldPtr(sdat(i)%pstrm(1)%fldbun_model, trim(field_name), data_ptr, rc=rc)

        if (rc == ESMF_SUCCESS .and. associated(data_ptr)) then
            found = .true.
            return
        endif
    enddo

    if (.not. found) then
        rc = ESMF_RC_NOT_FOUND
    endif

  end subroutine nexus_cdeps_get_data_pointer

  !> @brief Get list of available fields from CDEPS streams
  !> @param[out] field_names Array of field names (allocatable)
  !> @param[out] num_fields Number of fields found
  !> @param[out] rc Return code
  subroutine nexus_cdeps_get_available_fields(field_names, num_fields, rc)
    character(len=ESMF_MAXSTR), allocatable, intent(out) :: field_names(:)
    integer, intent(out) :: num_fields
    integer, intent(out) :: rc

    integer :: i, j, k, total_vars
    integer :: count

    rc = ESMF_SUCCESS
    num_fields = 0

    if (.not. initialized) return

    ! First pass: count total fields
    total_vars = 0
    do i = 1, num_cdeps_streams
        if (allocated(sdat(i)%pstrm(1)%fldlist_model)) then
            total_vars = total_vars + size(sdat(i)%pstrm(1)%fldlist_model)
        endif
    enddo

    if (total_vars == 0) return

    allocate(field_names(total_vars))

    ! Second pass: collect names
    count = 0
    do i = 1, num_cdeps_streams
        if (allocated(sdat(i)%pstrm(1)%fldlist_model)) then
            do j = 1, size(sdat(i)%pstrm(1)%fldlist_model)
                count = count + 1
                field_names(count) = sdat(i)%pstrm(1)%fldlist_model(j)
            enddo
        endif
    enddo

    num_fields = count

  end subroutine nexus_cdeps_get_available_fields

  !> @brief Finalize inline CDEPS
  !> @param[out] rc Return code
  subroutine nexus_cdeps_finalize(rc)
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    if (.not. initialized) return

    if (allocated(sdat)) deallocate(sdat)

    initialized = .false.

  end subroutine nexus_cdeps_finalize

end module nexus_cdeps_inline_mod