program NEXUS_driver

  use ESMF
  use NEXUS_Methods_Mod

  implicit none

  character(len=*), parameter :: NEXUS_options(9,2) = reshape( &
    (/ &
      "-c           ", "c:           ", &
      "--config     ", "c:           ", & 
      "--config-file", "c:           ", &
      "-r           ", "r:           ", &
      "--regrid-to  ", "r:           ", &
      "-d           ", "d            ", &
      "--debug      ", "d            ", &
      "--wr         ", "wr           ", &
      "-o           ", "o:           ", &
      "--output     ", "o:           "  &
    /), (/ 9, 2 /), order=(/ 2, 1 /))


  integer :: localrc, rc
  integer :: localPet
  integer :: idx, ind, item
  integer :: debugLevel
  logical :: writeRestart
  integer :: ibuf(2)
  character(ESMF_MAXSTR) :: ConfigFile
  character(ESMF_MAXSTR) :: ReGridFile
  character(ESMF_MAXSTR) :: OutputFile
  character(ESMF_MAXSTR) :: optarg
  character(ESMF_MAXSTR) :: sbuf(3)
  type(ESMF_VM) :: vm

  ! ---------------------------------------------------------------------------
  !   Initialize ESMF.  Note that an output Log is created by default.
  ! ---------------------------------------------------------------------------

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_NOCALENDAR, rc=rc)
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("NEXUS Driver starts", ESMF_LOGMSG_INFO)

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
  writeRestart = .false.

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
          case ("wr")
            writeRestart = .true.
          case ("h")
          case default
        end select
      end if
    end do
  end if

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

  ! ----------------------------------------------------------------------------
  !   Initialize NEXUS
  ! ----------------------------------------------------------------------------
  call NEXUS_Initialize( ConfigFile, ReGridFile, OutputFile, debugLevel, writeRestart, rc=rc )
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! ----------------------------------------------------------------------------
  !   Run NEXUS
  ! ----------------------------------------------------------------------------
  call NEXUS_Run( rc=rc )
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

  ! ----------------------------------------------------------------------------
  !   Finalize NEXUS and clean up.
  ! ----------------------------------------------------------------------------

  call NEXUS_Finalize( rc=rc )
  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__,  &
    file=__FILE__)) &
    call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)


  call ESMF_LogWrite("NEXUS Driver ends", ESMF_LOGMSG_INFO)

  call ESMF_Finalize()
  
end program NEXUS_driver
