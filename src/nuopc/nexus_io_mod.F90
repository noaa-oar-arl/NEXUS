!==============================================================================
!
! !MODULE: nexus_io_mod
!
! !DESCRIPTION: Handles I/O operations for the NEXUS component.
!  - Input:  Handled via CDEPS (Community Data Models for Earth Predictive Systems)
!  - Output: Handled via ESMF History (managed via xml file)
!
!==============================================================================

module nexus_io_mod

#if defined(USE_MPI)
  use mpi
#endif
  use ESMF

  ! --- CDEPS Imports ---
  use cdeps_stream_mod, only: cdeps_stream_type,      &
                              cdeps_stream_init,      &
                              cdeps_stream_run,       &
                              cdeps_stream_final,     &
                              cdeps_stream_get_field, &
                              cdeps_stream_get_field_names
  use FoX_DOM, only: extractDataContent, destroy, Node, NodeList, parseFile, getElementsByTagName, getLength, item, getAttribute

  implicit none

  private

  public :: IO_Init, IO_Read, IO_Write, IO_Final

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

  ! CDEPS Stream Object (Input only)
  type(cdeps_stream_type), save :: MainCDEPS
  logical, save :: CDEPS_Initialized = .false.

contains

    !> @brief Initializes IO: Sets up History and Input from nexus_streams.xml
    subroutine IO_Init(dstGrid, clock, rc)
      integer, intent(out) :: rc
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_Clock), intent(in) :: clock
  
      ! XML Parsing
      type(Node), pointer :: doc, stream_node, p
      type(NodeList), pointer :: stream_list, var_list
      character(len=255) :: stream_name
      integer :: i, n, num_hist_streams
      character(len=255), allocatable :: temp_vars(:)
  
      ! CDEPS/ESMF
      type(ESMF_VM) :: vm
      integer :: localPet
      logical :: check_input_streams, check_output_streams
      character(len=*), parameter :: CDEPS_CONFIG = "nexus_input_streams.xml"
      character(len=*), parameter :: HISTORY_CONFIG = "nexus_output_streams.xml"
  
      rc = ESMF_SUCCESS
      num_hist_streams = 0
  
      call ESMF_VMGetCurrent(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  
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
  
         call cdeps_stream_init(stream      = MainCDEPS,    &
                                grid        = dstGrid,      &
                                config_file = CDEPS_CONFIG, &
                                rc          = rc)
  
         if (rc /= ESMF_SUCCESS) then
             call ESMF_LogWrite("NEXUS_IO: CDEPS Init Failed", ESMF_LOGMSG_ERROR)
             return
         endif
  
         CDEPS_Initialized = .true.
      else
         if (localPet == 0) call ESMF_LogWrite("NEXUS_IO: No input streams found in nexus_input_streams.xml. CDEPS not initialized.", ESMF_LOGMSG_WARNING)
         CDEPS_Initialized = .false.
      endif
  
      if (localPet == 0) print *, "NEXUS_IO: Initialized ", num_hist_streams, " history streams."
  
    end subroutine IO_Init

  !> @brief Reads Input Data (Via CDEPS)
  subroutine IO_Read(state, clock, rc)
    type(ESMF_State), intent(inout) :: state
    type(ESMF_Clock), intent(in)    :: clock
    integer, intent(out)            :: rc

    type(ESMF_Time) :: currTime
    integer :: localrc

    rc = ESMF_SUCCESS
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)

    if (CDEPS_Initialized) then
        ! 1. Advance Stream
        call cdeps_stream_run(stream = MainCDEPS, &
                              time   = currTime,  &
                              rc     = localrc)

        if (localrc /= ESMF_SUCCESS) then
             rc = localrc
             return
        endif

        ! 2. Transfer Data to ImportState
        call Transfer_CDEPS_Fields(MainCDEPS, state, clock, localrc)
        rc = localrc
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


  !> @brief Transfers fields from CDEPS stream to ImportState
  subroutine Transfer_CDEPS_Fields(stream, state, clock, rc)
    type(cdeps_stream_type), intent(inout) :: stream
    type(ESMF_State),        intent(inout) :: state
    type(ESMF_Clock),        intent(in)    :: clock
    integer,                 intent(out)   :: rc

    type(ESMF_Field) :: f_src, f_dst
    character(len=256), allocatable :: fieldNames(:)
    integer :: i, count

    rc = ESMF_SUCCESS

    call cdeps_stream_get_field_names(stream, fieldNames, rc)
    if (rc /= ESMF_SUCCESS .or. .not. allocated(fieldNames)) return

    count = size(fieldNames)

    do i = 1, count
        ! Check if ImportState needs this field
        call ESMF_StateGet(state, trim(fieldNames(i)), itemType=ESMF_STATEITEM_FIELD, &
                           field=f_dst, rc=rc)

        if (rc == ESMF_SUCCESS) then
            call cdeps_stream_get_field(stream, trim(fieldNames(i)), f_src, rc)
            if (rc == ESMF_SUCCESS) then
                 call ESMF_FieldCopy(f_src, f_dst, rc=rc)
            endif
        else
            rc = ESMF_SUCCESS ! Reset rc if field not found in State (safe to ignore)
        endif
    end do

    if (allocated(fieldNames)) deallocate(fieldNames)
  end subroutine Transfer_CDEPS_Fields


  !> @brief Clean up CDEPS resources
  subroutine IO_Final(rc)
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    if (CDEPS_Initialized) then
        call cdeps_stream_final(MainCDEPS, rc=rc)
    endif
  end subroutine IO_Final


  !----------------------------------------------------------------------------
  ! Utility: Date Token Parsing (Used only for History Filenames)
  !----------------------------------------------------------------------------
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

end module nexus_io_mod