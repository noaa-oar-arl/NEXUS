!> @brief Code that specializes generic NUOPC_Driver for NEXUS
module nexus_driver

  use ESMF
  use NUOPC
  use NUOPC_Driver, driverSS => SetServices

  use nexus_cap, only: modelSS => SetServices

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
    use nexus_cap, only: T_YY, T_MM, T_DD, T_H, T_M, T_S, HcoState

    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: dt

    rc = ESMF_SUCCESS

    ! SetServices for model component
    call NUOPC_DriverAddComp(driver, "NEXUS", modelSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="low", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !
    ! Set the driver clock
    !

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
