!> @brief Code that specializes generic NUOPC_Driver for NEXUS
module nexus_driver

  use ESMF
  use NUOPC
  use NUOPC_Driver, driverSS => SetServices
  use NUOPC_Connector, only: connectorSS => SetServices

  use nexus_cap, only: modelSS => SetServices
  use nexus_config_mod, only: nxs_read_full_config
  
  ! CDEPS data atmosphere component
  use cdeps_datm_comp, only: datm_SS => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
contains
  !-----------------------------------------------------------------------------

  !> @brief Sets services for the driver.
  !>
  !> @param driver The ESMF grid component.
  !> @param rc     Return code.
  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  !> @brief Specialization to set model services.
  !>
  !> @param driver The ESMF grid component.
  !> @param rc     Return code.
  subroutine SetModelServices(driver, rc)
    use nexus_cap, only: T_YY, T_MM, T_DD, T_H, T_M, T_S
    use nexus_config_mod, only: nxs_read_time_config

    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_GridComp)           :: child_nexus, child_datm
    type(ESMF_CplComp)            :: connector
    ! Standalone toggle from nexus.rc configuration
    logical :: standalone_mode
    character(len=255) :: hemco_config_file, grid_file, regrid_file
    integer :: config_rc
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: dt
    integer                       :: start_yy, start_mm, start_dd, start_h, start_m, start_s
    integer                       :: end_yy, end_mm, end_dd, end_h, end_m, end_s

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("NEXUS_DRIVER: Starting SetModelServices", ESMF_LOGMSG_INFO)
    
    ! Read standalone mode from nexus.rc configuration
    call nxs_read_full_config('nexus.rc', hemco_config_file, grid_file, standalone_mode, regrid_file, config_rc)
    if (config_rc /= 0) then
      call ESMF_LogWrite('NEXUS_DRIVER: Warning - could not read config, defaulting to standalone mode', ESMF_LOGMSG_WARNING)
      standalone_mode = .true.
    end if
    
    if (standalone_mode) then
      call ESMF_LogWrite('NEXUS_DRIVER: Standalone mode enabled (skipping external DATM + connector)', ESMF_LOGMSG_INFO)
    else
      call ESMF_LogWrite('NEXUS_DRIVER: Coupled mode (external DATM + connector) enabled', ESMF_LOGMSG_INFO)
    end if

    ! Read time configuration from nexus.rc to populate time arrays
    call ESMF_LogWrite("NEXUS_DRIVER: Reading time configuration", ESMF_LOGMSG_INFO)
    call nxs_read_time_config("nexus.rc", start_yy, start_mm, start_dd, &
                              start_h, start_m, start_s, &
                              end_yy, end_mm, end_dd, &
                              end_h, end_m, end_s, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return

    ! Populate global time arrays for NUOPC usage
    T_YY(1) = start_yy; T_MM(1) = start_mm; T_DD(1) = start_dd
    T_H(1) = start_h; T_M(1) = start_m; T_S(1) = start_s
    T_YY(2) = end_yy; T_MM(2) = end_mm; T_DD(2) = end_dd
    T_H(2) = end_h; T_M(2) = end_m; T_S(2) = end_s

    call ESMF_LogWrite("NEXUS_DRIVER: Time arrays populated", ESMF_LOGMSG_INFO)

    if (.not. standalone_mode) then
      ! Add CDEPS data atmosphere component for emission data provision
      call NUOPC_DriverAddComp(driver, "DATM", datm_SS, comp=child_datm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_CompAttributeSet(child_datm, name="Verbosity", value="low", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("NEXUS_DRIVER: Added CDEPS DATM component", ESMF_LOGMSG_INFO)
    end if

    ! Add NEXUS emission processing component
    call NUOPC_DriverAddComp(driver, "NEXUS", modelSS, comp=child_nexus, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child_nexus, name="Verbosity", value="low", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("NEXUS_DRIVER: Added NEXUS component", ESMF_LOGMSG_INFO)

    if (.not. standalone_mode) then
      ! Add connector from DATM to NEXUS (DATM exports data to NEXUS imports)
      call NUOPC_DriverAddComp(driver, srcCompLabel="DATM", dstCompLabel="NEXUS", &
                              compSetServicesRoutine=connectorSS, comp=connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("NEXUS_DRIVER: Added DATM->NEXUS connector", ESMF_LOGMSG_INFO)
    end if

    !
    ! Set the driver clock
    !

    call ESMF_LogWrite("NEXUS_DRIVER: Setting start/stop times", ESMF_LOGMSG_INFO)

    call ESMF_TimeSet(startTime, &
      yy=T_YY(1), mm=T_MM(1), dd=T_DD(1), &
      h=T_H(1), m=T_M(1), s=T_S(1), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, &
      yy=T_YY(2), mm=T_MM(2), dd=T_DD(2), &
      h=T_H(2), m=T_M(2), s=T_S(2), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Read timestep from config file
    call read_timestep("nexus.rc", dt, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Driver Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  !> @brief Reads the timestep from the configuration file.
  !>
  !> @param file The configuration file name.
  !> @param dt   The timestep in seconds.
  !> @param rc   Return code.
  subroutine read_timestep(file, dt, rc)
    character(len=*), intent(in) :: file
    integer, intent(out) :: dt
    integer, intent(out) :: rc

    integer :: unit, stat
    character(len=255) :: line, key, value

    rc = ESMF_SUCCESS
    dt = 3600 ! Default value

    open(newunit=unit, file=trim(file), status='old', iostat=stat)
    if (stat /= 0) then
      ! If file not found, use default and log warning
      call ESMF_LogWrite("Warning: Control file not found, using default timestep", ESMF_LOGMSG_WARNING, rc=rc)
      return
    end if

    do
      read(unit, '(a)', end=10) line
      ! Skip comments and empty lines
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse key-value pair
      key = trim(adjustl(line(1:index(line,':')-1)))
      value = trim(adjustl(line(index(line,':')+1:)))

      if (trim(key) == 'TIMESTEP') then
        read(value, *) dt
        exit
      end if
    end do
10  continue
    close(unit)

  end subroutine read_timestep

  !-----------------------------------------------------------------------------

end module nexus_driver
