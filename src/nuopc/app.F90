program app

  !-----------------------------------------------------------------------------
  ! Command-line interface for the NEXUS NUOPC Single-Model Driver
  !-----------------------------------------------------------------------------

  use ESMF

  use nexus_cap, only: init_cap => init
  use nexus_driver, only: driverSS => SetServices

  implicit none

  character(len=*), parameter :: NEXUS_options(11,2) = reshape( &
    (/ &
    "-c           ", "c:           ", &
    "--config     ", "c:           ", &
    "--config-file", "c:           ", &
    "-r           ", "r:           ", &
    "--regrid-to  ", "r:           ", &
    "-d           ", "d            ", &
    "--debug      ", "d            ", &
    "-o           ", "o:           ", &
    "--output     ", "o:           ", &
    "-h           ", "h            ", &
    "--help       ", "h            " &
    /), (/ 11, 2 /), order=(/ 2, 1 /))

  character(len=*), parameter :: usage = &
    "Usage: nexus &
    [-c|--config-file <file>] [-r|--regrid-to <file>] [-o|--output <file>] &
    [-d|--debug] [-h|--help]"

  character(1), parameter :: newline = new_line('a')
  character(len=*), parameter :: description = &
    "NOAA Emission and Exchange Unified System (NEXUS)" // newline // &
    "(NUOPC Single-Model Driver application)" // newline // &
    "https://github.com/noaa-oar-arl/NEXUS"

  integer :: rc, localrc, userRc
  integer, parameter :: rootPet = 0
  integer :: localPet
  integer :: idx, ind, item
  integer :: debugLevel
  integer :: ibuf(2)
  character(ESMF_MAXSTR) :: ConfigFile
  character(ESMF_MAXSTR) :: ReGridFile
  character(ESMF_MAXSTR) :: OutputFile
  character(ESMF_MAXSTR) :: optarg
  character(ESMF_MAXSTR) :: sbuf(3)
  type(ESMF_VM) :: vm
  type(ESMF_GridComp) :: drvComp


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

  ! Parse command line arguments and share information with other PETs
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

  ConfigFile = ""
  ReGridFile = ""
  OutputFile = ""

  debugLevel = 0

  localrc = ESMF_SUCCESS

  if (localPet == 0) then
    do item = 1, size(NEXUS_options, dim=1)
      call ESMF_UtilGetArgIndex(NEXUS_options(item,1), argindex=ind, rc=localrc)
      if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        exit
      if (ind > -1) then
        idx = len_trim(NEXUS_options(item,2))
        if (NEXUS_options(item,2)(idx:idx) == ":") then
          call ESMF_UtilGetArg(ind+1, argvalue=optarg, rc=localrc)
          if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__)) &
            exit
        end if
        select case (trim(NEXUS_options(item,2)))
         case ("c:")
          ConfigFile = optarg
         case ("r:")
          ReGridFile = optarg
         case ("o:")
          OutputFile = optarg
         case ("d")
          debugLevel = 1
         case ("h")
          print "(a)", usage
          return
         case default
        end select
      end if
    end do
  end if

  call print_sep(char="=")
  print "(a)", description
  call print_sep()
  print "('ConfigFile = ', a)", trim(ConfigFile)
  print "('ReGridFile = ', a)", trim(ReGridFile)
  print "('debugLevel = ', i0)", debugLevel
  print "('OutputFile = ', a)", trim(OutputFile)
  call print_sep()

  ibuf(1) = localrc
  ibuf(2) = debugLevel
  call ESMF_VMBroadcast(vm, ibuf, size(ibuf), rootPet, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  localrc    = ibuf(1)
  debugLevel = ibuf(2)
  if (ESMF_LogFoundError(localrc, msg="Failure retrieving command-line arguments", &
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

  call init_cap(ConfigFile, ReGridFile, OutputFile, debugLevel, rc=rc)

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

  call ESMF_LogWrite("app FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

contains

  !> By default, 60 hyphens.
  subroutine print_sep(char, n)
    character(len=1), intent(in), optional :: char
    integer, intent(in), optional :: n

    character(len=1) :: char_
    integer :: n_
    character(len=:), allocatable :: sep
    integer i

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

    print "(a)", sep
  end subroutine print_sep

end program app
