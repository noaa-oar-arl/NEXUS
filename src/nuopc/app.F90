!> @brief Command-line interface for the NEXUS NUOPC Single-Model Driver.
program app

#ifdef USE_MPI
  use mpi
#endif

  use ESMF

  use nexus_cap, only: init_cap => nxs_init, finalize_cap => nxs_finalize
  use nexus_driver, only: driverSS => SetServices

  implicit none

  character(1), parameter :: newline = new_line('a')
  character(len=*), parameter :: description = &
    "NOAA Emission and Exchange Unified System (NEXUS)" // newline // &
    "(NUOPC Single-Model Driver application)" // newline // &
    "https://github.com/noaa-oar-arl/NEXUS"

  integer :: rc, localrc, userRc
  integer, parameter :: rootPet = 0
  integer :: localPet, petCount
  integer :: debugLevel
  logical :: writeRestart
  integer :: ibuf(2)
#ifdef USE_MPI
  integer :: mpi_ierr
#endif
  character(ESMF_MAXSTR) :: ConfigFile
  character(ESMF_MAXSTR) :: ReGridFile
  character(ESMF_MAXSTR) :: OutputFile
  character(ESMF_MAXSTR) :: sbuf(3)
  type(ESMF_VM) :: vm
  type(ESMF_GridComp) :: drvComp

#ifdef USE_MPI
  call MPI_Init(mpi_ierr)
#endif

  ! Initialize ESMF
  call ESMF_Initialize(defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("app STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Parse control file and share information with other PETs
  call ESMF_VMGetCurrent(vm, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ConfigFile = ""
  ReGridFile = ""
  OutputFile = ""

  debugLevel = 0
  writeRestart = .false.

  localrc = ESMF_SUCCESS

  if (localPet == rootPet) then
    call parse_control_file("nexus.rc", ConfigFile, ReGridFile, OutputFile, &
      debugLevel, writeRestart, localrc)

    call print_sep(char="=")
    print "(a)", description
    call print_sep()

    print "('ConfigFile = ', a)", trim(ConfigFile)
    print "('ReGridFile = ', a)", trim(ReGridFile)
    print "('debugLevel = ', i0)", debugLevel
    print "('OutputFile = ', a)", trim(OutputFile)
    print "('petCount   = ', i0)", petCount
    call print_sep()
  end if

  ! Broadcast settings to other PETs
  ibuf(1) = localrc
  ibuf(2) = debugLevel
  call ESMF_VMBroadcast(vm, ibuf, size(ibuf), rootPet, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  localrc    = ibuf(1)
  debugLevel = ibuf(2)
  if (ESMF_LogFoundError(localrc, msg="Failure reading control file", &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  sbuf(1) = ConfigFile
  sbuf(2) = ReGridFile
  sbuf(3) = OutputFile
  call ESMF_VMBroadcast(vm, sbuf, size(sbuf)*len(sbuf(1)), rootPet, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  ConfigFile = sbuf(1)
  ReGridFile = sbuf(2)
  OutputFile = sbuf(3)

  !-----------------------------------------------------------------------------

  call init_cap(ConfigFile, ReGridFile, OutputFile, debugLevel, writeRestart, rc=rc)

  ! -> CREATE THE DRIVER
  drvComp = ESMF_GridCompCreate(name="driver", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! -> SET DRIVER SERVICES
  call ESMF_GridCompSetServices(drvComp, driverSS, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! INITIALIZE THE DRIVER
  call ESMF_GridCompInitialize(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! RUN THE DRIVER
  call ESMF_GridCompRun(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! FINALIZE THE DRIVER
  call ESMF_GridCompFinalize(drvComp, userRc=userRc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=userRc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------

  call ESMF_LogWrite("NEXUS run finished", ESMF_LOGMSG_INFO, rc=rc)

  call finalize_cap(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("NEXUS finalized", ESMF_LOGMSG_INFO)

  ! Finalize ESMF
  call ESMF_Finalize()

#ifdef USE_MPI
  call MPI_Finalize(mpi_ierr)
#endif

  if (localPet == rootPet) print "('NEXUS: ', a)", "Done"

contains

  !> @brief Prints a separator line.
  !>
  !> @param char The character to use for the separator (default: "-").
  !> @param n    The length of the separator (default: 60).
  subroutine print_sep(char, n)
    character(len=1), intent(in), optional :: char
    integer, intent(in), optional :: n

    character(len=1) :: char_
    integer :: n_
    character(len=:), allocatable :: sep
    integer i
    type(ESMF_VM) :: vm
    integer :: localPet, localrc

    if (.not. present(char)) then
      char_ = "-"
    else
      char_ = char
    end if
    if (.not. present(n)) then
      n_ = 60
    else
      n_ = n
    end if

    allocate(character(len=n_) :: sep)
    do i = 1, n_
      sep(i:i) = char_
    end do

    call ESMF_VMGetCurrent(vm, rc=localrc)
    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    if (localPet == 0) print "(a)", sep
  end subroutine print_sep

  !> @brief Parses the control file.
  !>
  !> @param file         The path to the control file.
  !> @param ConfigFile   (Out) Path to the configuration file.
  !> @param ReGridFile   (Out) Path to the regridding file.
  !> @param OutputFile   (Out) Path to the output file.
  !> @param debugLevel   (Out) Debug level.
  !> @param writeRestart (Out) Flag to write restart file.
  !> @param rc           (Out) Return code.
  subroutine parse_control_file(file, ConfigFile, ReGridFile, OutputFile, &
    debugLevel, writeRestart, rc)
    character(len=*), intent(in) :: file
    character(len=*), intent(out) :: ConfigFile
    character(len=*), intent(out) :: ReGridFile
    character(len=*), intent(out) :: OutputFile
    integer, intent(out) :: debugLevel
    logical, intent(out) :: writeRestart
    integer, intent(out) :: rc

    integer :: unit, stat
    character(len=255) :: line, key, value

    type(ESMF_VM) :: vm
    integer :: localPet
    integer :: localrc

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)

    open(newunit=unit, file=trim(file), status='old', iostat=stat)
    if (stat /= 0) then
      if (localPet == 0) print *, "Error opening control file: ", trim(file)
      rc = ESMF_FAILURE
      return
    end if

    do
      read(unit, '(a)', end=10) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      key = trim(adjustl(line(1:index(line,':')-1)))
      value = trim(adjustl(line(index(line,':')+1:)))

      select case (key)
        case ('CONFIG_FILE')
          ConfigFile = value
        case ('REGRID_FILE')
          ReGridFile = value
        case ('OUTPUT_FILE')
          OutputFile = value
        case ('DEBUG_LEVEL')
          read(value, *) debugLevel
        case ('WRITE_RESTART')
          read(value, *) writeRestart
      end select
    end do
10  continue
    close(unit)

  end subroutine parse_control_file

end program app
