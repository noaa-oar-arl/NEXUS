!> @brief NEXUS Runtime Configuration Module
!> @details This module provides runtime configuration management for NEXUS,
!> including environment variable handling and default value management.
!> @authors Barry Baker
!> @version 2.0
!> @date 2026-01-02

module nexus_runtime_config_mod

  use HCO_Error_Mod, only: HCO_SUCCESS, HCO_MSG

  implicit none
  private

  ! Public interfaces
  public :: nexus_get_config_value
  public :: nexus_get_config_file
  public :: nexus_get_grid_size
  public :: nexus_get_grid_type
  public :: nexus_get_grid_file

  ! Configuration defaults - can be overridden by environment variables
  character(len=255), parameter :: DEFAULT_CONFIG_DIR = './'
  character(len=255), parameter :: DEFAULT_HEMCO_CONFIG = 'HEMCO_Config.rc'
  character(len=255), parameter :: DEFAULT_NEXUS_CONFIG = 'NEXUS_Config.rc'
  character(len=255), parameter :: DEFAULT_GRID_FILE = 'fix/grid_spec_C401.nc'
  integer, parameter :: DEFAULT_GRID_SIZE = 401
  character(len=255), parameter :: DEFAULT_GRID_TYPE = 'cubed_sphere'

contains

  !> @brief Get configuration value from environment or use default
  !> @param[in] env_var Environment variable name
  !> @param[in] default_val Default value if env var not set
  !> @param[out] value Result value
  subroutine nexus_get_config_value(env_var, default_val, value)

    character(len=*), intent(in) :: env_var
    character(len=*), intent(in) :: default_val
    character(len=*), intent(out) :: value

    call get_environment_variable(trim(env_var), value)
    if ( len_trim(value) == 0 ) then
       value = trim(default_val)
       call HCO_MSG('Using default for ' // trim(env_var) // ': ' // trim(value))
    else
       call HCO_MSG('Using environment ' // trim(env_var) // ': ' // trim(value))
    endif

  end subroutine nexus_get_config_value

  !> @brief Get configuration file path with directory resolution
  !> @param[in] env_var Environment variable for file name
  !> @param[in] default_file Default file name
  !> @param[out] full_path Full path to configuration file
  subroutine nexus_get_config_file(env_var, default_file, full_path)

    character(len=*), intent(in) :: env_var
    character(len=*), intent(in) :: default_file
    character(len=*), intent(out) :: full_path

    character(len=255) :: config_dir, file_name
    logical :: fileExists

    ! Get configuration file name
    call nexus_get_config_value(env_var, default_file, file_name)

    ! Check if it's an absolute path
    if ( file_name(1:1) == '/' ) then
       full_path = trim(file_name)
    else
       ! Get configuration directory
       call nexus_get_config_value('NEXUS_CONFIG_DIR', DEFAULT_CONFIG_DIR, config_dir)
       full_path = trim(config_dir) // '/' // trim(file_name)
    endif

    ! Verify file exists and warn if not
    inquire(file=trim(full_path), exist=fileExists)
    if ( .not. fileExists ) then
       call HCO_MSG('Warning: Configuration file not found: ' // trim(full_path))
    endif

  end subroutine nexus_get_config_file

  !> @brief Get grid size from environment or use default
  !> @returns Grid size integer
  function nexus_get_grid_size() result(grid_size)

    integer :: grid_size
    character(len=255) :: size_str
    integer :: ios

    call get_environment_variable('NEXUS_GRID_SIZE', size_str)
    if ( len_trim(size_str) > 0 ) then
       read(size_str, *, iostat=ios) grid_size
       if ( ios /= 0 ) then
          grid_size = DEFAULT_GRID_SIZE
          call HCO_MSG('Warning: Invalid NEXUS_GRID_SIZE, using default')
       else
          call HCO_MSG('Using grid size from environment: ' // trim(size_str))
       endif
    else
       grid_size = DEFAULT_GRID_SIZE
       call HCO_MSG('Using default grid size')
    endif

  end function nexus_get_grid_size

  !> @brief Get grid type from environment or use default
  !> @param[out] grid_type Grid type string
  subroutine nexus_get_grid_type(grid_type)

    character(len=*), intent(out) :: grid_type

    call nexus_get_config_value('NEXUS_GRID_TYPE', DEFAULT_GRID_TYPE, grid_type)

  end subroutine nexus_get_grid_type

  !> @brief Get grid file from environment or use production default
  !> @details Supports ESMF grid files (.nc), mosaic files, and HEMCO config (.rc) for testing
  !> Priority: NEXUS_GRID_FILE env var -> production grid -> test grid
  !> @param[out] grid_file Grid file path
  subroutine nexus_get_grid_file(grid_file)

    character(len=*), intent(out) :: grid_file
    logical :: file_exists

    ! Check for environment variable first
    call get_environment_variable('NEXUS_GRID_FILE', grid_file)
    
    if ( len_trim(grid_file) > 0 ) then
       ! User specified grid file
       inquire(file=trim(grid_file), exist=file_exists)
       if ( file_exists ) then
          call HCO_MSG('Using specified grid file: ' // trim(grid_file))
          return
       else
          call HCO_MSG('Warning: Specified grid file not found: ' // trim(grid_file))
       endif
    endif

    ! Try production grid file
    grid_file = DEFAULT_GRID_FILE
    inquire(file=trim(grid_file), exist=file_exists)
    if ( file_exists ) then
       call HCO_MSG('Using production grid file: ' // trim(grid_file))
       return
    endif

    ! Fall back to test grid file
    grid_file = 'HEMCO_sa_Grid.rc'
    inquire(file=trim(grid_file), exist=file_exists)
    if ( file_exists ) then
       call HCO_MSG('Using test grid file: ' // trim(grid_file))
    else
       call HCO_MSG('Warning: No grid file found, will use default grid creation')
       grid_file = ''  ! Empty indicates use default grid creation
    endif

  end subroutine nexus_get_grid_file

end module nexus_runtime_config_mod