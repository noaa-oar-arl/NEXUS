!==============================================================================
!
! !MODULE: nexus_io_mod
!
! !DESCRIPTION: Handles I/O operations for the NEXUS component.
!  - Input:  Handled via CDEPS (Inline)
!  - Output: Handled via ESMF History (managed via xml file)
!
!==============================================================================

module nexus_io_mod

#if defined(USE_MPI)
  use mpi
#endif
  use ESMF
  use pio

  ! --- CDEPS Imports ---
  use dshr_strdata_mod, only: shr_strdata_type,         &
                              shr_strdata_init_from_inline, &
                              shr_strdata_advance,      &
                              shr_strdata_get_stream_fieldbundle

  implicit none

  private

  public :: IO_Init, IO_Read, IO_Write, IO_Final, ResolveFileName

  !----------------------------------------------------------------------------
  ! Derived types
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

  ! Array to hold history streams (Output only)
  type(HistoryStream), allocatable :: historyStreams(:)

  ! CDEPS Stream Objects (Input only)
  type(shr_strdata_type), allocatable, save :: CDEPS_Streams(:)
  logical, save :: CDEPS_Initialized = .false.

  ! PIO System
  type(iosystem_desc_t), pointer, save :: pio_subsystem => null()

  character(len=*), parameter :: CDEPS_CONFIG = "nexus_input_streams.yaml"
  character(len=*), parameter :: HISTORY_CONFIG = "nexus_output_streams.yaml"

contains

    !> @brief Initializes IO: Sets up History and Input
    subroutine IO_Init(dstGrid, clock, rc)
      integer, intent(out) :: rc
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_Clock), intent(in) :: clock

      ! YAML Parsing
      type(ESMF_HConfig) :: hconfig
      character(len=255) :: stream_name
      character(len=255) :: key_prefix, key_prefix_var
      character(len=10) :: index_str, index_str_var
      character(len=255) :: freqString
      integer :: i, n, num_hist_streams, num_input_streams
      integer :: j

      ! CDEPS/ESMF
      type(ESMF_VM) :: vm
      integer :: localPet, petCount, rootPet
      logical :: check_input_streams, check_output_streams
      integer :: pio_comm, ibuf

      ! CDEPS Init vars
      type(ESMF_Mesh) :: model_mesh
      character(len=255) :: taxmode, tintalgo, mapalgo, readmode, meshfile, lev_dimname
      integer :: year_first, year_last, year_align
      character(len=1024) :: datafiles_template
      character(len=255), allocatable :: input_files(:)
      character(len=255), allocatable :: input_vars_file(:), input_vars_model(:)
      integer :: num_files, num_vars
      real(ESMF_KIND_R8) :: dtlimit

      rc = ESMF_SUCCESS
      num_hist_streams = 0
      num_input_streams = 0
      check_input_streams = .false.
      check_output_streams = .false.

      call ESMF_VMGetCurrent(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=pio_comm, rc=rc)
      rootPet = 0  ! Root PET for broadcasts

      !--------------------------------------------------------------------------
      ! 0. Initialize PIO (Required for CDEPS)
      !--------------------------------------------------------------------------
      ! if (.not. associated(pio_subsystem)) then
      !    allocate(pio_subsystem)
      !    ! Use default PIO init (all tasks are IO tasks, stride 1)
      !    ! PIO_Init is a generic interface, and the arguments need to match one of the specific procedures.
      !    ! Assuming standard PIO_Init(rank, comm, iosystem, num_iotasks, stride, rearranger)
      !    ! call PIO_Init(localPet, pio_comm, pio_subsystem, petCount, 1, PIO_REARR_BOX)
      ! endif

      !--------------------------------------------------------------------------
      ! 1. Parse nexus_output_streams.yaml for History streams
      !--------------------------------------------------------------------------
      ! Check for output config on all processes to avoid uninitialized variable
      hconfig = ESMF_HConfigCreate(filename=HISTORY_CONFIG, rc=rc)
      if (rc == ESMF_SUCCESS) then
          check_output_streams = .true.
      else
          check_output_streams = .false.
          call ESMF_LogWrite("NEXUS_IO: Output config not found or invalid", ESMF_LOGMSG_WARNING)
          rc = ESMF_SUCCESS ! Reset rc
      endif

      if (check_output_streams) then
          if (localPet == 0) then
              num_hist_streams = ESMF_HConfigGetSize(hconfig, keyString="output_streams", rc=rc)

              if (num_hist_streams > 0) allocate(historyStreams(num_hist_streams))

              do i = 1, num_hist_streams
                  write(index_str, '(I0)') i
                  key_prefix = "output_streams:"//trim(index_str)

                  historyStreams(i)%name = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":name", rc=rc)

                  freqString = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":frequency", rc=rc)
                  call ParseTimeInterval(freqString, historyStreams(i)%frequency, rc)

                  historyStreams(i)%fileName = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":file", rc=rc)
                  historyStreams(i)%mode = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":mode", rc=rc)

                  n = ESMF_HConfigGetSize(hconfig, keyString=trim(key_prefix)//":variables", rc=rc)
                  allocate(historyStreams(i)%variables(n))
                  do j = 1, n
                      write(index_str_var, '(I0)') j
                      historyStreams(i)%variables(j) = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":variables:"//trim(index_str_var), rc=rc)
                  end do
              end do
              call ESMF_HConfigDestroy(hconfig, rc=rc)
          endif

          ! Broadcast History Config
  #ifdef USE_MPI
          call ESMF_VMBroadcast(vm, num_hist_streams, 1, rootPet, rc=rc)
          if (localPet /= 0 .and. num_hist_streams > 0) allocate(historyStreams(num_hist_streams))

          if (num_hist_streams > 0) then
              do i = 1, size(historyStreams)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%name, len(historyStreams(i)%name), rootPet, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%fileName, len(historyStreams(i)%fileName), rootPet, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%mode, len(historyStreams(i)%mode), rootPet, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%frequency, 1, rootPet, rc=rc)

                  if (localPet == 0) then
                      n = size(historyStreams(i)%variables)
                  else
                      n = 0
                  endif
                  call ESMF_VMBroadcast(vm, n, 1, rootPet, rc=rc)
                  if (localPet /= 0) allocate(historyStreams(i)%variables(n))

                  call ESMF_VMBroadcast(vm, historyStreams(i)%variables, n*len(historyStreams(i)%variables(1)), rootPet, rc=rc)

                  historyStreams(i)%initialized = .false.
              enddo
          endif
  #endif
      endif

      !--------------------------------------------------------------------------
      ! 2. Initialize CDEPS for INPUT
      !--------------------------------------------------------------------------
      ! Check for input config on all processes to avoid uninitialized variable
      hconfig = ESMF_HConfigCreate(filename=CDEPS_CONFIG, rc=rc)
      if (rc == ESMF_SUCCESS) then
          check_input_streams = .true.
      else
          check_input_streams = .false.
          rc = ESMF_SUCCESS
      endif

      if (check_input_streams) then
         if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: Initializing CDEPS Inline...", ESMF_LOGMSG_INFO)

         ! Create Mesh from Grid (Required by CDEPS)
         ! TODO: Fix ESMF_MeshCreate API call
         ! model_mesh = ESMF_MeshCreate(grid=dstGrid, meshstructure=ESMF_MESHSTRUCTURE_ELEMENT, rc=rc)
         ! if (rc /= ESMF_SUCCESS) then
         !     call ESMF_LogWrite("NEXUS_IO: Failed to create Mesh from Grid", ESMF_LOGMSG_ERROR)
         !     return
         ! endif

         ! Parse and Broadcast Input Streams
         ! Get number of input streams on all processes to avoid uninitialized variable
         num_input_streams = ESMF_HConfigGetSize(hconfig, keyString="input_streams", rc=rc)
         if (rc /= ESMF_SUCCESS) num_input_streams = 0

         if (num_input_streams > 0) then
             allocate(CDEPS_Streams(num_input_streams))

         do i = 1, num_input_streams
             ! Root extracts data
             if (localPet == 0) then
                 write(index_str, '(I0)') i
                 key_prefix = "input_streams:"//trim(index_str)

                 stream_name = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":name", rc=rc)
                 taxmode = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":taxmode", rc=rc)
                 tintalgo = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":tintalgo", rc=rc)
                 mapalgo = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":mapalgo", rc=rc)
                 readmode = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":readmode", rc=rc)
                 meshfile = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":meshfile", rc=rc)
                 lev_dimname = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":lev_dimname", rc=rc)
                 year_first = ESMF_HConfigAsI4(hconfig, keyString=trim(key_prefix)//":year_first", rc=rc)
                 year_last = ESMF_HConfigAsI4(hconfig, keyString=trim(key_prefix)//":year_last", rc=rc)
                 year_align = ESMF_HConfigAsI4(hconfig, keyString=trim(key_prefix)//":year_align", rc=rc)
                 datafiles_template = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":datafiles", rc=rc)

                 call GenerateFileList(datafiles_template, year_first, year_last, input_files)
                 num_files = size(input_files)

                 num_vars = ESMF_HConfigGetSize(hconfig, keyString=trim(key_prefix)//":datavars", rc=rc)

                 allocate(input_vars_file(num_vars))
                 do n = 1, num_vars
                     write(index_str_var, '(I0)') n
                     input_vars_file(n) = ESMF_HConfigAsString(hconfig, keyString=trim(key_prefix)//":datavars:"//trim(index_str_var), rc=rc)
                 end do
             endif

             ! Broadcast config for this stream
             call ESMF_VMBroadcast(vm, stream_name, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, taxmode, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, tintalgo, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, mapalgo, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, readmode, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, meshfile, 255, rootPet, rc=rc)
             call ESMF_VMBroadcast(vm, lev_dimname, 255, rootPet, rc=rc)
             ! TODO: Fix VMBroadcast type issues
             ! call ESMF_VMBroadcast(vm, year_first, 1, rootPet, rc=rc)
             ! call ESMF_VMBroadcast(vm, year_last, 1, rootPet, rc=rc)
             ! call ESMF_VMBroadcast(vm, year_align, 1, rootPet, rc=rc)

             ! TODO: Fix VMBroadcast type issues
             ! call ESMF_VMBroadcast(vm, num_files, 1, rootPet, rc=rc)
             if (localPet /= 0) allocate(input_files(num_files))
             call ESMF_VMBroadcast(vm, input_files, num_files*255, rootPet, rc=rc)

             ! TODO: Fix VMBroadcast type issues
             ! call ESMF_VMBroadcast(vm, num_vars, 1, rootPet, rc=rc)
             if (localPet /= 0) allocate(input_vars_file(num_vars))
             call ESMF_VMBroadcast(vm, input_vars_file, num_vars*255, rootPet, rc=rc)

             ! Init Stream
             if (localPet /= 0) allocate(input_vars_model(num_vars))
             if (localPet == 0) allocate(input_vars_model(num_vars))
             input_vars_model = input_vars_file ! Assume model name = file name

             ! Set PIO Subsystem manually
             ! CDEPS_Streams(i)%pio_subsystem => pio_subsystem
             ! CDEPS_Streams(i)%io_type = PIO_IOTYPE_NETCDF
             ! CDEPS_Streams(i)%io_format = PIO_IOFORMAT_NETCDF

             dtlimit = 1.0d30

             call shr_strdata_init_from_inline( &
                 sdat = CDEPS_Streams(i), &
                 my_task = localPet, &
                 logunit = 6, &
                 compname = "NEXUS", &
                 model_clock = clock, &
                 model_mesh = model_mesh, &
                 stream_meshfile = meshfile, &
                 stream_lev_dimname = lev_dimname, &
                 stream_mapalgo = mapalgo, &
                 stream_filenames = input_files, &
                 stream_fldlistFile = input_vars_file, &
                 stream_fldListModel = input_vars_model, &
                 stream_yearFirst = year_first, &
                 stream_yearLast = year_last, &
                 stream_yearAlign = year_align, &
                 stream_offset = 0, &
                 stream_taxmode = taxmode, &
                 stream_dtlimit = dtlimit, &
                 stream_tintalgo = tintalgo, &
                 stream_name = stream_name, &
                 rc = rc)

             if (allocated(input_files)) deallocate(input_files)
             if (allocated(input_vars_file)) deallocate(input_vars_file)
             if (allocated(input_vars_model)) deallocate(input_vars_model)

         end do

             if (localPet == 0) call ESMF_HConfigDestroy(hconfig, rc=rc)

             CDEPS_Initialized = .true.
         else
             if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: No input streams found. CDEPS not initialized.", ESMF_LOGMSG_WARNING)
             CDEPS_Initialized = .false.
         endif
      else
         if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: No input streams configuration. CDEPS not initialized.", ESMF_LOGMSG_WARNING)
         CDEPS_Initialized = .false.
      endif

      if (localPet == 0) print *, "NEXUS_IO: Initialized ", num_hist_streams, " history streams and ", num_input_streams, " input streams."

    end subroutine IO_Init

  !> @brief Reads Input Data (Via CDEPS)
  subroutine IO_Read(state, clock, rc)
    type(ESMF_State), intent(inout) :: state
    type(ESMF_Clock), intent(in)    :: clock
    integer, intent(out)            :: rc

    type(ESMF_Time) :: currTime
    integer :: localrc
    integer :: ymd, tod, i, j, num_fields
    integer :: yy, mm, dd, h, m, s
    type(ESMF_FieldBundle) :: fldbun
    type(ESMF_Field) :: f_src, f_dst
    character(len=256), allocatable :: fieldNames(:)
    integer :: fieldCount

    rc = ESMF_SUCCESS
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)

    ymd = yy*10000 + mm*100 + dd
    tod = h*3600 + m*60 + s

    if (CDEPS_Initialized) then
        do i = 1, size(CDEPS_Streams)
            ! Advance Stream
            call shr_strdata_advance(CDEPS_Streams(i), ymd, tod, logunit=6, istr=trim(CDEPS_Streams(i)%stream(1)%file(1)%name), rc=localrc)
            if (localrc /= ESMF_SUCCESS) then
                rc = localrc
                return
            endif

            ! Transfer fields
            ! Retrieve bundle from CDEPS (on model mesh)
            fldbun = shr_strdata_get_stream_fieldbundle(CDEPS_Streams(i), 1, 'model')

            call ESMF_FieldBundleGet(fldbun, fieldCount=fieldCount, rc=localrc)
            if (fieldCount > 0) then
                allocate(fieldNames(fieldCount))
                call ESMF_FieldBundleGet(fldbun, fieldNameList=fieldNames, rc=localrc)

                do j = 1, fieldCount
                    call ESMF_FieldBundleGet(fldbun, fieldNames(j), field=f_src, rc=localrc)

                    ! Check if ImportState needs this field
                    ! TODO: Fix ESMF_StateGet API call
                    ! call ESMF_StateGet(state, itemName=trim(fieldNames(j)), itemType=ESMF_STATEITEM_FIELD, &
                    !                    field=f_dst, rc=localrc)
                    ! if (localrc == ESMF_SUCCESS) then
                    !     call ESMF_FieldCopy(f_src, f_dst, rc=localrc)
                    ! endif
                end do
                deallocate(fieldNames)
            endif
        end do
    endif

  end subroutine IO_Read


  !> @brief Writes History Data (Legacy Logic kept for output)
  subroutine IO_Write(state, clock, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out)        :: rc

      integer :: i, j, localrc, localPet
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: currTime, streamTime
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Field), allocatable :: fieldList(:)
      character(len=255) :: resolvedFileName

      rc = ESMF_SUCCESS
      call ESMF_VMGetCurrent(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)

      if (.not. allocated(historyStreams)) return

      do i = 1, size(historyStreams)

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
          if (localPet == 0) print *, "NEXUS_IO: Writing to stream '", trim(historyStreams(i)%name), "'"

          allocate(fieldList(size(historyStreams(i)%variables)))
          do j = 1, size(historyStreams(i)%variables)
            call ESMF_StateGet(state, trim(historyStreams(i)%variables(j)), field, rc=localrc)
            if (localrc == ESMF_SUCCESS) then
              fieldList(j) = field
            endif
          end do

          bundle = ESMF_FieldBundleCreate(name="history_bundle", fieldList=fieldList, rc=localrc)

          ! Resolve output filename (using Date Tokens only)
          call ResolveDateTokens(historyStreams(i)%fileName, clock, resolvedFileName, localrc)

          call ESMF_FieldBundleWrite(bundle, trim(resolvedFileName), &
                                     overwrite=(historyStreams(i)%mode == "overwrite"), &
                                     iofmt=ESMF_IOFMT_NETCDF, rc=localrc)

          call ESMF_FieldBundleDestroy(bundle, rc=localrc)
          deallocate(fieldList)

          call ESMF_ClockAdvance(historyStreams(i)%clock, rc=localrc)
        end if
      end do
  end subroutine IO_Write

  !> @brief Clean up CDEPS resources
  subroutine IO_Final(rc)
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    if (allocated(CDEPS_Streams)) deallocate(CDEPS_Streams)
    if (associated(pio_subsystem)) then
        call PIO_Finalize(pio_subsystem, rc)
        deallocate(pio_subsystem)
        nullify(pio_subsystem)
    endif
  end subroutine IO_Final


  !----------------------------------------------------------------------------
  ! Utility
  !----------------------------------------------------------------------------

  subroutine ResolveFileName(template, clock, resolved, rc)
      character(len=*), intent(in) :: template
      type(ESMF_Clock), intent(in) :: clock
      character(len=*), intent(out) :: resolved
      integer, intent(out) :: rc
      call ResolveDateTokens(template, clock, resolved, rc)
  end subroutine ResolveFileName

  subroutine ResolveDateTokens(template, clock, resolved, rc)
      character(len=*), intent(in) :: template
      type(ESMF_Clock), intent(in) :: clock
      character(len=*), intent(out) :: resolved
      integer, intent(out) :: rc
      type(ESMF_Time) :: currTime
      integer :: yy, mm, dd
      rc = ESMF_SUCCESS
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, rc=rc)
      call ExpandDateTokens(template, yy, mm, dd, resolved)
  end subroutine ResolveDateTokens

  subroutine ExpandDateTokens(template, yy, mm, dd, result)
      character(len=*), intent(in) :: template
      integer, intent(in) :: yy, mm, dd
      character(len=*), intent(out) :: result
      character(len=10) :: syyyy, smm, sdd
      write(syyyy, '(I4.4)') yy
      write(smm, '(I2.2)') mm
      write(sdd, '(I2.2)') dd
      result = template
      call ReplaceToken(result, '$YYYY', trim(syyyy))
      call ReplaceToken(result, '%y4', trim(syyyy))
      call ReplaceToken(result, '$MM', trim(smm))
      call ReplaceToken(result, '%m2', trim(smm))
      call ReplaceToken(result, '$DD', trim(sdd))
      call ReplaceToken(result, '%d2', trim(sdd))
  end subroutine ExpandDateTokens

  subroutine ReplaceToken(str, token, replacement)
       character(len=*), intent(inout) :: str
       character(len=*), intent(in) :: token
       character(len=*), intent(in) :: replacement
       character(len=255) :: tmp
       integer :: idx
       do
          idx = index(str, token(:len_trim(token)))
          if (idx == 0) exit
          tmp = str(1:idx-1) // replacement(:len_trim(replacement)) // str(idx+len_trim(token):)
          str = tmp
       end do
  end subroutine ReplaceToken

  ! Parse HHMMSS string to ESMF_TimeInterval
  subroutine ParseTimeInterval(timeString, timeInterval, rc)
      character(len=*), intent(in) :: timeString
      type(ESMF_TimeInterval), intent(out) :: timeInterval
      integer, intent(out) :: rc
      integer :: h, m, s

      rc = ESMF_SUCCESS
      ! Expecting HHMMSS format
      if (len_trim(timeString) == 6) then
          read(timeString(1:2), *) h
          read(timeString(3:4), *) m
          read(timeString(5:6), *) s
          call ESMF_TimeIntervalSet(timeInterval, h=h, m=m, s=s, rc=rc)
      else
          rc = ESMF_FAILURE
      endif
  end subroutine ParseTimeInterval

  ! Generate list of files from template
  subroutine GenerateFileList(template, y1, y2, file_list)
      character(len=*), intent(in) :: template
      integer, intent(in) :: y1, y2
      character(len=255), allocatable, intent(out) :: file_list(:)

      integer :: y, m, count, idx
      logical :: has_mm
      character(len=255) :: tmp

      has_mm = (index(template, "$MM") > 0 .or. index(template, "%m2") > 0)

      if (index(template, "$YYYY") > 0 .or. index(template, "%y4") > 0) then
          if (has_mm) then
              count = (y2 - y1 + 1) * 12
              allocate(file_list(count))
              idx = 0
              do y = y1, y2
                  do m = 1, 12
                      idx = idx + 1
                      call ExpandDateTokens(template, y, m, 1, tmp)
                      file_list(idx) = tmp
                  end do
              end do
          else
              count = y2 - y1 + 1
              allocate(file_list(count))
              idx = 0
              do y = y1, y2
                  idx = idx + 1
                  call ExpandDateTokens(template, y, 1, 1, tmp)
                  file_list(idx) = tmp
              end do
          endif
      else
          allocate(file_list(1))
          file_list(1) = template
      endif
  end subroutine

end module nexus_io_mod
