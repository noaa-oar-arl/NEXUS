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
  use FoX_DOM, only: extractDataContent, destroy, Node, NodeList, parseFile, &
                     getElementsByTagName, getLength, item, getAttribute

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

  character(len=*), parameter :: CDEPS_CONFIG = "nexus_input_streams.xml"
  character(len=*), parameter :: HISTORY_CONFIG = "nexus_output_streams.xml"

contains

    !> @brief Initializes IO: Sets up History and Input
    subroutine IO_Init(dstGrid, clock, rc)
      integer, intent(out) :: rc
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_Clock), intent(in) :: clock
  
      ! XML Parsing
      type(Node), pointer :: doc, stream_node, p
      type(NodeList), pointer :: stream_list, var_list
      character(len=255) :: stream_name
      integer :: i, n, num_hist_streams, num_input_streams
      character(len=255), allocatable :: temp_vars(:)
  
      ! CDEPS/ESMF
      type(ESMF_VM) :: vm
      integer :: localPet, petCount
      logical :: check_input_streams, check_output_streams
      integer :: pio_comm

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
  
      call ESMF_VMGetCurrent(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=pio_comm, rc=rc)
  
      !--------------------------------------------------------------------------
      ! 0. Initialize PIO (Required for CDEPS)
      !--------------------------------------------------------------------------
      if (.not. associated(pio_subsystem)) then
         allocate(pio_subsystem)
         ! Use default PIO init (all tasks are IO tasks, stride 1)
         call PIO_Init(localPet, pio_comm, pio_subsystem, petCount, 1, 1)
      endif

      !--------------------------------------------------------------------------
      ! 1. Parse nexus_output_streams.xml for History streams
      !--------------------------------------------------------------------------
      if (localPet == 0) then
        inquire(file=HISTORY_CONFIG, exist=check_output_streams)
      endif
      call ESMF_VMBroadcast(vm, check_output_streams, 1, 0, rc=rc)
  
      if (check_output_streams) then
          if (localPet == 0) then
              doc => parseFile(HISTORY_CONFIG)
              stream_list => getElementsByTagName(doc, "stream_info")
              num_hist_streams = getLength(stream_list)
  
              if (num_hist_streams > 0) allocate(historyStreams(num_hist_streams))
  
              do i = 1, num_hist_streams
                  stream_node => item(stream_list, i-1)
                  call getAttribute(stream_node, 'name', stream_name)
                  historyStreams(i)%name = stream_name
  
                  p => item(getElementsByTagName(stream_node, "frequency"), 0)
                  call extractDataContent(p, historyStreams(i)%frequency)
  
                  p => item(getElementsByTagName(stream_node, "file"), 0)
                  call extractDataContent(p, historyStreams(i)%fileName)
  
                  p => item(getElementsByTagName(stream_node, "mode"), 0)
                  call extractDataContent(p, historyStreams(i)%mode)
  
                  p => item(getElementsByTagName(stream_node, "variables"), 0)
                  var_list => getElementsByTagName(p, "var")
                  allocate(historyStreams(i)%variables(getLength(var_list)))
                  do n = 1, getLength(var_list)
                      p => item(var_list, n - 1)
                      call extractDataContent(p, historyStreams(i)%variables(n))
                  end do
              end do
              call destroy(doc)
          endif
  
          ! Broadcast History Config
  #ifdef USE_MPI
          call ESMF_VMBroadcast(vm, num_hist_streams, 1, 0, rc=rc)
          if (localPet /= 0 .and. num_hist_streams > 0) allocate(historyStreams(num_hist_streams))
  
          if (num_hist_streams > 0) then
              do i = 1, size(historyStreams)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%name, len(historyStreams(i)%name), 0, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%fileName, len(historyStreams(i)%fileName), 0, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%mode, len(historyStreams(i)%mode), 0, rc=rc)
                  call ESMF_VMBroadcast(vm, historyStreams(i)%frequency, 1, 0, rc=rc)
  
                  if (localPet == 0) then
                      n = size(historyStreams(i)%variables)
                  else
                      n = 0
                  endif
                  call ESMF_VMBroadcast(vm, n, 1, 0, rc=rc)
                  if (localPet /= 0) allocate(historyStreams(i)%variables(n))
  
                  call ESMF_VMBroadcast(vm, historyStreams(i)%variables, n*len(historyStreams(i)%variables(1)), 0, rc=rc)
  
                  historyStreams(i)%initialized = .false.
              enddo
          endif
  #endif
      endif
  
      !--------------------------------------------------------------------------
      ! 2. Initialize CDEPS for INPUT
      !--------------------------------------------------------------------------
      if (localPet == 0) then
         inquire(file=CDEPS_CONFIG, exist=check_input_streams)
      endif
      call ESMF_VMBroadcast(vm, check_input_streams, 1, 0, rc=rc)
  
      if (check_input_streams) then
         if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: Initializing CDEPS Inline...", ESMF_LOGMSG_INFO)

         ! Create Mesh from Grid (Required by CDEPS)
         model_mesh = ESMF_MeshCreate(grid=dstGrid, meshstructure=ESMF_MESHSTRUCTURE_ELEMENT, rc=rc)
         if (rc /= ESMF_SUCCESS) then
             call ESMF_LogWrite("NEXUS_IO: Failed to create Mesh from Grid", ESMF_LOGMSG_ERROR)
             return
         endif

         ! Parse and Broadcast Input Streams
         if (localPet == 0) then
             doc => parseFile(CDEPS_CONFIG)
             stream_list => getElementsByTagName(doc, "stream_info")
             num_input_streams = getLength(stream_list)
         endif

         call ESMF_VMBroadcast(vm, num_input_streams, 1, 0, rc=rc)

         allocate(CDEPS_Streams(num_input_streams))

         do i = 1, num_input_streams
             ! Root extracts data
             if (localPet == 0) then
                 stream_node => item(stream_list, i-1)

                 call getAttribute(stream_node, 'name', stream_name)
                 call ExtractChildText(stream_node, "taxmode", taxmode)
                 call ExtractChildText(stream_node, "tintalgo", tintalgo)
                 call ExtractChildText(stream_node, "mapalgo", mapalgo)
                 call ExtractChildText(stream_node, "readmode", readmode)
                 call ExtractChildText(stream_node, "meshfile", meshfile)
                 call ExtractChildText(stream_node, "lev_dimname", lev_dimname)
                 call ExtractChildInt(stream_node, "year_first", year_first)
                 call ExtractChildInt(stream_node, "year_last", year_last)
                 call ExtractChildInt(stream_node, "year_align", year_align)
                 call ExtractChildText(stream_node, "datafiles", datafiles_template)

                 call GenerateFileList(datafiles_template, year_first, year_last, input_files)
                 num_files = size(input_files)

                 p => item(getElementsByTagName(stream_node, "datavars"), 0)
                 var_list => getElementsByTagName(p, "var")
                 num_vars = getLength(var_list)

                 allocate(input_vars_file(num_vars))
                 do n = 1, num_vars
                     p => item(var_list, n-1)
                     call extractDataContent(p, input_vars_file(n))
                 end do
             endif

             ! Broadcast config for this stream
             call ESMF_VMBroadcast(vm, stream_name, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, taxmode, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, tintalgo, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, mapalgo, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, readmode, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, meshfile, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, lev_dimname, 255, 0, rc=rc)
             call ESMF_VMBroadcast(vm, year_first, 1, 0, rc=rc)
             call ESMF_VMBroadcast(vm, year_last, 1, 0, rc=rc)
             call ESMF_VMBroadcast(vm, year_align, 1, 0, rc=rc)

             call ESMF_VMBroadcast(vm, num_files, 1, 0, rc=rc)
             if (localPet /= 0) allocate(input_files(num_files))
             call ESMF_VMBroadcast(vm, input_files, num_files*255, 0, rc=rc)

             call ESMF_VMBroadcast(vm, num_vars, 1, 0, rc=rc)
             if (localPet /= 0) allocate(input_vars_file(num_vars))
             call ESMF_VMBroadcast(vm, input_vars_file, num_vars*255, 0, rc=rc)

             ! Init Stream
             if (localPet /= 0) allocate(input_vars_model(num_vars))
             if (localPet == 0) allocate(input_vars_model(num_vars))
             input_vars_model = input_vars_file ! Assume model name = file name

             ! Set PIO Subsystem manually
             CDEPS_Streams(i)%pio_subsystem => pio_subsystem
             CDEPS_Streams(i)%io_type = PIO_IOTYPE_NETCDF
             CDEPS_Streams(i)%io_format = PIO_IOFORMAT_NETCDF

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

         if (localPet == 0) call destroy(doc)

         CDEPS_Initialized = .true.
      else
         if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: No input streams found. CDEPS not initialized.", ESMF_LOGMSG_WARNING)
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
                    call ESMF_StateGet(state, trim(fieldNames(j)), itemType=ESMF_STATEITEM_FIELD, &
                                       field=f_dst, rc=localrc)
                    if (localrc == ESMF_SUCCESS) then
                        call ESMF_FieldCopy(f_src, f_dst, rc=localrc)
                    endif
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

  ! Helpers for XML extraction
  subroutine ExtractChildText(parent, tag, value)
      type(Node), pointer :: parent, p
      character(len=*), intent(in) :: tag
      character(len=*), intent(out) :: value
      p => item(getElementsByTagName(parent, tag), 0)
      call extractDataContent(p, value)
  end subroutine

  subroutine ExtractChildInt(parent, tag, value)
      type(Node), pointer :: parent, p
      character(len=*), intent(in) :: tag
      integer, intent(out) :: value
      p => item(getElementsByTagName(parent, tag), 0)
      call extractDataContent(p, value)
  end subroutine

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
