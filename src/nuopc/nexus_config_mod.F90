!> @brief NEXUS Configuration Management Module
!> @details This module provides configuration reading and validation utilities for NEXUS.
!> Extracted from cap.F90 for better modularity and maintainability.
!> @authors Barry Baker
!> @version 2.0
!> @date 2026-01-02

module nexus_config_mod

  use ESMF
  use HCO_Error_Mod, only: HCO_SUCCESS, HCO_MSG, HCO_ERROR

  implicit none
  private

  ! Public interfaces
  public :: nxs_read_time_config
  public :: nxs_read_config_file
  public :: nxs_read_full_config
  public :: parse_date
  public :: nxs_init
  public :: nxs_finalize
  public :: nxs_get_output_frequency
  public :: nxs_get_output_prefix

  ! Module variables for configuration state
  character(len=255), save :: ConfigFile_
  character(len=255), save :: ReGridFile_
  character(len=255), save :: OutputFile_
  character(len=255), save :: GridFile_
  integer, save :: debugLevel_
  integer, save :: outputFrequency_
  logical, save :: writeRestart_
  logical, save :: standaloneMode_

contains

  !> @brief Initialize NEXUS configuration
  !> @param[in] ConfigFile Configuration file name
  !> @param[in] ReGridFile Regridding file name
  !> @param[in] OutputFile Output file name
  !> @param[in] debugLevel Debug level (0-3)
  !> @param[in] outputFrequency Output frequency in seconds
  !> @param[in] writeRestart Whether to write restart files
  !> @param[out] rc Return code
  subroutine nxs_init(ConfigFile, ReGridFile, OutputFile, debugLevel, outputFrequency, writeRestart, rc)

    character(len=*), intent(in) :: ConfigFile, ReGridFile, OutputFile
    integer, intent(in) :: debugLevel, outputFrequency
    logical, intent(in) :: writeRestart
    integer, intent(out) :: rc

    rc = HCO_SUCCESS

    ! Store configuration parameters
    ConfigFile_ = trim(ConfigFile)
    ReGridFile_ = trim(ReGridFile)
    OutputFile_ = trim(OutputFile)
    debugLevel_ = debugLevel
    outputFrequency_ = outputFrequency
    writeRestart_ = writeRestart

    ! Validate configuration files exist
    call validate_config_files(rc)
    if ( rc /= HCO_SUCCESS ) then
       call HCO_ERROR('Configuration file validation failed', rc)
       return
    endif

    call HCO_MSG('NEXUS configuration initialized successfully')
    call HCO_MSG('  Config file: ' // trim(ConfigFile_))
    call HCO_MSG('  ReGrid file: ' // trim(ReGridFile_))
    call HCO_MSG('  Output file: ' // trim(OutputFile_))

    rc = HCO_SUCCESS

  end subroutine nxs_init

  !> @brief Finalize NEXUS configuration
  !> @param[out] rc Return code
  subroutine nxs_finalize( rc )

    integer, intent(out) :: rc

    rc = HCO_SUCCESS

    ! Clear configuration parameters
    ConfigFile_ = ''
    ReGridFile_ = ''
    OutputFile_ = ''
    debugLevel_ = 0
    outputFrequency_ = 0
    writeRestart_ = .false.

    call HCO_MSG('NEXUS configuration finalized')
    rc = HCO_SUCCESS

  end subroutine nxs_finalize

  !> @brief Read time configuration from nexus.rc control file
  !> @param[in] ConfigFile nexus.rc configuration file
  !> @param[out] start_yy Start year
  !> @param[out] start_mm Start month
  !> @param[out] start_dd Start day
  !> @param[out] start_h Start hour
  !> @param[out] start_m Start minute
  !> @param[out] start_s Start second
  !> @param[out] end_yy End year
  !> @param[out] end_mm End month
  !> @param[out] end_dd End day
  !> @param[out] end_h End hour
  !> @param[out] end_m End minute
  !> @param[out] end_s End second
  !> @param[out] rc Return code
  subroutine nxs_read_time_config(ConfigFile, start_yy, start_mm, start_dd, &
                                  start_h, start_m, start_s, &
                                  end_yy, end_mm, end_dd, &
                                  end_h, end_m, end_s, rc)

    character(len=*), intent(in) :: ConfigFile
    integer, intent(out) :: start_yy, start_mm, start_dd, start_h, start_m, start_s
    integer, intent(out) :: end_yy, end_mm, end_dd, end_h, end_m, end_s
    integer, intent(out) :: rc

    character(len=255) :: fullFilename
    character(len=255) :: line
    character(len=255) :: date_str, time_str
    integer :: unit, ios, colon_pos
    logical :: fileExists

    rc = HCO_SUCCESS

    ! Initialize outputs with defaults
    start_yy = 2023; start_mm = 1; start_dd = 1; start_h = 0; start_m = 0; start_s = 0
    end_yy = 2023; end_mm = 1; end_dd = 2; end_h = 0; end_m = 0; end_s = 0

    ! Use the filename directly (should be nexus.rc)
    fullFilename = trim(ConfigFile)

    ! Check if file exists
    inquire(file=trim(fullFilename), exist=fileExists)
    if ( .not. fileExists ) then
       call HCO_MSG('Warning: Config file not found: ' // trim(fullFilename))
       call HCO_MSG('Using default time configuration')
       rc = HCO_SUCCESS
       return
    endif

    ! Read time settings from config file
    open(newunit=unit, file=trim(fullFilename), status='old', action='read', iostat=ios)
    if ( ios /= 0 ) then
       call HCO_ERROR('Error opening config file: ' // trim(fullFilename), rc)
       return
    endif

    ! Parse time configuration from nexus.rc
    do while ( .true. )
       read(unit, '(A)', iostat=ios) line
       if ( ios /= 0 ) exit

       line = adjustl(trim(line))

       ! Skip comments and empty lines
       if ( len_trim(line) == 0 .or. line(1:1) == '#' .or. line(1:1) == '!' ) cycle

       ! Process START_TIME: YYYY-MM-DD HH:MM:SS
       if ( index(line, 'START_TIME:') > 0 ) then
          colon_pos = index(line, ':')
          if (colon_pos > 0) then
             ! Extract everything after "START_TIME:"
             line = adjustl(line(colon_pos+1:))
             ! Parse YYYY-MM-DD HH:MM:SS format
             call parse_date(line, start_yy, start_mm, start_dd, start_h, start_m, start_s)
             call HCO_MSG('Parsed START_TIME: ' // trim(line))
          endif

       ! Process END_TIME: YYYY-MM-DD HH:MM:SS
       else if ( index(line, 'END_TIME:') > 0 ) then
          colon_pos = index(line, ':')
          if (colon_pos > 0) then
             ! Extract everything after "END_TIME:"
             line = adjustl(line(colon_pos+1:))
             ! Parse YYYY-MM-DD HH:MM:SS format
             call parse_date(line, end_yy, end_mm, end_dd, end_h, end_m, end_s)
             call HCO_MSG('Parsed END_TIME: ' // trim(line))
          endif
       endif
    enddo

    close(unit)
    call HCO_MSG('Time configuration read successfully from: ' // trim(fullFilename))
    rc = HCO_SUCCESS

  end subroutine nxs_read_time_config

  !> @brief Read CONFIG_FILE from nexus.rc control file
  !> @param[in] ConfigFile nexus.rc configuration file name
  !> @param[out] config_file_name HEMCO configuration file name
  !> @param[out] rc Return code
  subroutine nxs_read_config_file(ConfigFile, config_file_name, rc)

    character(len=*), intent(in) :: ConfigFile
    character(len=*), intent(out) :: config_file_name
    integer, intent(out) :: rc

    ! Local variables
    character(len=512) :: line, fullFilename
    character(len=10) :: cwd
    integer :: unit, ios, colon_pos
    logical :: fileExists

    rc = HCO_SUCCESS
    config_file_name = 'HEMCO_Config.rc'  ! Default value

    ! Build full filename - check current directory first
    cwd = './'
    fullFilename = trim(cwd) // trim(ConfigFile)

    ! Check if file exists
    inquire(file=trim(fullFilename), exist=fileExists)
    if ( .not. fileExists ) then
       call HCO_MSG('Warning: Config file not found: ' // trim(fullFilename))
       call HCO_MSG('Using default CONFIG_FILE: ' // trim(config_file_name))
       rc = HCO_SUCCESS
       return
    endif

    ! Read config file to find CONFIG_FILE entry
    open(newunit=unit, file=trim(fullFilename), status='old', action='read', iostat=ios)
    if ( ios /= 0 ) then
       call HCO_ERROR('Error opening config file: ' // trim(fullFilename), rc)
       return
    endif

    ! Parse configuration from nexus.rc
    do while ( .true. )
       read(unit, '(A)', iostat=ios) line
       if ( ios /= 0 ) exit

       line = adjustl(trim(line))

       ! Skip comments and empty lines
       if ( len_trim(line) == 0 .or. line(1:1) == '#' .or. line(1:1) == '!' ) cycle

       ! Process CONFIG_FILE: filename
       if ( index(line, 'CONFIG_FILE:') > 0 ) then
          colon_pos = index(line, ':')
          if (colon_pos > 0) then
             ! Extract everything after "CONFIG_FILE:"
             config_file_name = adjustl(line(colon_pos+1:))
             call HCO_MSG('Found CONFIG_FILE: ' // trim(config_file_name))
             exit  ! Found what we need
          endif
       endif
    enddo

    close(unit)
    call HCO_MSG('Configuration file name read from: ' // trim(fullFilename))
    rc = HCO_SUCCESS

  end subroutine nxs_read_config_file

  !> @brief Parse date string into components
  !> @param[in] str Date string in format YYYY-MM-DD HH:MM:SS
  !> @param[out] yy Year
  !> @param[out] mm Month
  !> @param[out] dd Day
  !> @param[out] h Hour
  !> @param[out] m Minute
  !> @param[out] s Second
  subroutine parse_date(str, yy, mm, dd, h, m, s)

    character(len=*), intent(in) :: str
    integer, intent(out) :: yy, mm, dd, h, m, s

    character(len=255) :: work_str
    integer :: pos1, pos2

    ! Initialize outputs
    yy = 0; mm = 0; dd = 0; h = 0; m = 0; s = 0

    work_str = adjustl(trim(str))

    ! Parse YYYY-MM-DD format
    pos1 = index(work_str, '-')
    if ( pos1 > 0 ) then
       read(work_str(1:pos1-1), *) yy
       work_str = work_str(pos1+1:)

       pos2 = index(work_str, '-')
       if ( pos2 > 0 ) then
          read(work_str(1:pos2-1), *) mm
          work_str = work_str(pos2+1:)

          ! Check for space (start of time)
          pos1 = index(work_str, ' ')
          if ( pos1 > 0 ) then
             read(work_str(1:pos1-1), *) dd
             work_str = adjustl(work_str(pos1+1:))

             ! Parse HH:MM:SS format
             pos1 = index(work_str, ':')
             if ( pos1 > 0 ) then
                read(work_str(1:pos1-1), *) h
                work_str = work_str(pos1+1:)

                pos2 = index(work_str, ':')
                if ( pos2 > 0 ) then
                   read(work_str(1:pos2-1), *) m
                   read(work_str(pos2+1:), *) s
                endif
             endif
          else
             read(work_str, *) dd
          endif
       endif
    endif

  end subroutine parse_date

  !> @brief Validate that required configuration files exist
  !> @param[out] rc Return code
  subroutine validate_config_files(rc)

    integer, intent(out) :: rc

    character(len=255) :: fullFilename
    logical :: fileExists

    rc = HCO_SUCCESS

    ! Check main config file
    if ( len_trim(ConfigFile_) > 0 ) then
       fullFilename = trim(ConfigFile_)
       inquire(file=trim(fullFilename), exist=fileExists)
       if ( .not. fileExists ) then
          call HCO_MSG('Warning: Main config file not found: ' // trim(fullFilename))
       else
          call HCO_MSG('Found main config file: ' // trim(fullFilename))
       endif
    endif

    ! Check regrid file if specified
    if ( len_trim(ReGridFile_) > 0 ) then
       fullFilename = trim(ReGridFile_)
       inquire(file=trim(fullFilename), exist=fileExists)
       if ( .not. fileExists ) then
          call HCO_MSG('Warning: ReGrid file not found: ' // trim(fullFilename))
       else
          call HCO_MSG('Found ReGrid file: ' // trim(fullFilename))
       endif
    endif

    rc = HCO_SUCCESS

  end subroutine validate_config_files

  !> @brief Read comprehensive configuration from nexus.rc
  !> @details Reads all NEXUS configuration parameters including grid file and standalone mode
  !> @param[in] ConfigFile Configuration file path (usually 'nexus.rc')
  !> @param[out] hemco_config_file HEMCO configuration file name
  !> @param[out] grid_file Grid file path
  !> @param[out] standalone_mode Whether to run in standalone mode
  !> @param[out] regrid_file Regridding file path
  !> @param[out] rc Return code
  subroutine nxs_read_full_config(ConfigFile, hemco_config_file, grid_file, standalone_mode, regrid_file, rc)

    character(len=*), intent(in) :: ConfigFile
    character(len=*), intent(out) :: hemco_config_file, grid_file, regrid_file
    logical, intent(out) :: standalone_mode
    integer, intent(out) :: rc

    ! Local variables
    character(len=512) :: line, fullFilename
    character(len=10) :: cwd
    character(len=255) :: value_str
    integer :: unit, ios, colon_pos
    logical :: fileExists

    rc = HCO_SUCCESS

    ! Set defaults
    hemco_config_file = 'NEXUS_Config.rc'
    grid_file = 'HEMCO_sa_Grid.rc'  ! Default to test grid
    standalone_mode = .true.  ! Default to standalone
    regrid_file = ''

    ! Build full filename
    cwd = './'
    fullFilename = trim(cwd) // trim(ConfigFile)

    ! Check if file exists
    inquire(file=trim(fullFilename), exist=fileExists)
    if ( .not. fileExists ) then
       call HCO_MSG('Warning: Config file not found: ' // trim(fullFilename))
       call HCO_MSG('Using default configuration values')
       rc = HCO_SUCCESS
       return
    endif

    ! Read config file
    open(newunit=unit, file=trim(fullFilename), status='old', action='read', iostat=ios)
    if ( ios /= 0 ) then
       call HCO_ERROR('Error opening config file: ' // trim(fullFilename), rc)
       return
    endif

    ! Parse all configuration parameters
    do while ( .true. )
       read(unit, '(A)', iostat=ios) line
       if ( ios /= 0 ) exit

       line = adjustl(trim(line))

       ! Skip comments and empty lines
       if ( len_trim(line) == 0 .or. line(1:1) == '#' .or. line(1:1) == '!' ) cycle

       ! Find colon position
       colon_pos = index(line, ':')
       if ( colon_pos <= 0 ) cycle

       ! Extract value after colon
       value_str = adjustl(trim(line(colon_pos+1:)))

       ! Process different configuration parameters
       if ( index(line, 'CONFIG_FILE:') > 0 ) then
          hemco_config_file = value_str
          call HCO_MSG('CONFIG_FILE: ' // trim(hemco_config_file))
       else if ( index(line, 'GRID_FILE:') > 0 ) then
          grid_file = value_str
          call HCO_MSG('GRID_FILE: ' // trim(grid_file))
       else if ( index(line, 'STANDALONE_MODE:') > 0 ) then
          ! Parse boolean value
          if ( index(value_str, 'true') > 0 .or. index(value_str, 'TRUE') > 0 .or. &
               index(value_str, 'yes') > 0 .or. index(value_str, 'YES') > 0 .or. &
               trim(value_str) == '1' ) then
             standalone_mode = .true.
          else
             standalone_mode = .false.
          endif
          call HCO_MSG('STANDALONE_MODE: ' // trim(value_str))
       else if ( index(line, 'REGRID_FILE:') > 0 ) then
          regrid_file = value_str
          call HCO_MSG('REGRID_FILE: ' // trim(regrid_file))
       endif
    enddo

    close(unit)
    call HCO_MSG('Full configuration read from: ' // trim(fullFilename))
    rc = HCO_SUCCESS

  end subroutine nxs_read_full_config

  !> @brief Get the output frequency in seconds
  function nxs_get_output_frequency() result(frequency)
    integer :: frequency
    frequency = outputFrequency_
  end function nxs_get_output_frequency

  !> @brief Get the output file prefix
  function nxs_get_output_prefix() result(prefix)
    character(len=255) :: prefix
    prefix = trim(OutputFile_)
  end function nxs_get_output_prefix

end module nexus_config_mod