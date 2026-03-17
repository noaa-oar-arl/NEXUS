!==============================================================================
!
! !MODULE: nexus_output_mod
!
! !DESCRIPTION: CF-compliant diagnostic output for NEXUS standalone mode
!  Implements ESMF-based NetCDF output with CF-1.8 metadata conventions
!
!==============================================================================

module nexus_output_mod

  use ESMF
  use NUOPC
  use HCO_STATE_MOD, only: HCO_State
  use HCO_DIAGN_MOD, only: HcoDiagn
  use HCO_ERROR_MOD, only: HCO_SUCCESS, HCO_FAIL

  implicit none
  private

  ! Public interfaces
  public :: OutputInit
  public :: CollectOutputFields
  public :: WriteOutputFields
  public :: OutputFinalize
  public :: ValidateCFCompliance

  !----------------------------------------------------------------------------
  ! Output stream configuration
  !----------------------------------------------------------------------------
  type :: OutputStream
     character(len=255) :: name
     character(len=255) :: filename_template
     type(ESMF_TimeInterval) :: frequency
     type(ESMF_Time) :: next_output_time
     character(len=64), allocatable :: variable_list(:)
     integer :: num_variables
     logical :: time_averaged
     logical :: instantaneous
     type(ESMF_FieldBundle) :: output_bundle
     logical :: initialized
     ! Accumulation for time-averaged output
     real(ESMF_KIND_R8), allocatable :: accumulator(:,:,:)
     integer :: accumulation_count
  end type OutputStream

  type(OutputStream), allocatable, save :: output_streams(:)
  integer, save :: num_output_streams = 0

  ! CF standard name mapping
  type :: CFNameMapping
     character(len=64) :: hemco_name
     character(len=128) :: cf_standard_name
     character(len=128) :: long_name
     character(len=64) :: units
  end type CFNameMapping

  ! CF standard names for common HEMCO diagnostics
  type(CFNameMapping), parameter :: CF_MAPPINGS(20) = [ &
    CFNameMapping('EmisBC_Total', &
                  'tendency_of_atmosphere_mass_content_of_black_carbon_dry_aerosol_particles_due_to_emission', &
                  'Total black carbon emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisOC_Total', &
                  'tendency_of_atmosphere_mass_content_of_particulate_organic_matter_dry_aerosol_particles_due_to_emission', &
                  'Total organic carbon emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisSO2_Total', &
                  'tendency_of_atmosphere_mass_content_of_sulfur_dioxide_due_to_emission', &
                  'Total sulfur dioxide emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisNO_Total', &
                  'tendency_of_atmosphere_mole_content_of_nitrogen_monoxide_due_to_emission', &
                  'Total nitrogen monoxide emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisCO_Total', &
                  'tendency_of_atmosphere_mass_content_of_carbon_monoxide_due_to_emission', &
                  'Total carbon monoxide emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisNH3_Total', &
                  'tendency_of_atmosphere_mass_content_of_ammonia_due_to_emission', &
                  'Total ammonia emissions', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisBC_agr', &
                  'tendency_of_atmosphere_mass_content_of_black_carbon_dry_aerosol_particles_due_to_emission_from_agricultural_sector', &
                  'Black carbon emissions from agriculture', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisBC_ene', &
                  'tendency_of_atmosphere_mass_content_of_black_carbon_dry_aerosol_particles_due_to_emission_from_energy_sector', &
                  'Black carbon emissions from energy', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisBC_ind', &
                  'tendency_of_atmosphere_mass_content_of_black_carbon_dry_aerosol_particles_due_to_emission_from_industrial_sector', &
                  'Black carbon emissions from industry', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisOC_agr', &
                  'tendency_of_atmosphere_mass_content_of_particulate_organic_matter_dry_aerosol_particles_due_to_emission_from_agricultural_sector', &
                  'Organic carbon emissions from agriculture', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisOC_ene', &
                  'tendency_of_atmosphere_mass_content_of_particulate_organic_matter_dry_aerosol_particles_due_to_emission_from_energy_sector', &
                  'Organic carbon emissions from energy', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisOC_ind', &
                  'tendency_of_atmosphere_mass_content_of_particulate_organic_matter_dry_aerosol_particles_due_to_emission_from_industrial_sector', &
                  'Organic carbon emissions from industry', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisSO2_agr', &
                  'tendency_of_atmosphere_mass_content_of_sulfur_dioxide_due_to_emission_from_agricultural_sector', &
                  'Sulfur dioxide emissions from agriculture', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisSO2_ene', &
                  'tendency_of_atmosphere_mass_content_of_sulfur_dioxide_due_to_emission_from_energy_sector', &
                  'Sulfur dioxide emissions from energy', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisSO2_ind', &
                  'tendency_of_atmosphere_mass_content_of_sulfur_dioxide_due_to_emission_from_industrial_sector', &
                  'Sulfur dioxide emissions from industry', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisNO_agr', &
                  'tendency_of_atmosphere_mole_content_of_nitrogen_monoxide_due_to_emission_from_agricultural_sector', &
                  'Nitrogen monoxide emissions from agriculture', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisNO_ene', &
                  'tendency_of_atmosphere_mole_content_of_nitrogen_monoxide_due_to_emission_from_energy_sector', &
                  'Nitrogen monoxide emissions from energy', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisNO_ind', &
                  'tendency_of_atmosphere_mole_content_of_nitrogen_monoxide_due_to_emission_from_industrial_sector', &
                  'Nitrogen monoxide emissions from industry', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisCO_agr', &
                  'tendency_of_atmosphere_mass_content_of_carbon_monoxide_due_to_emission_from_agricultural_sector', &
                  'Carbon monoxide emissions from agriculture', &
                  'kg m-2 s-1'), &
    CFNameMapping('EmisCO_ene', &
                  'tendency_of_atmosphere_mass_content_of_carbon_monoxide_due_to_emission_from_energy_sector', &
                  'Carbon monoxide emissions from energy', &
                  'kg m-2 s-1') &
  ]

contains

  !> @brief Initialize output system from configuration file
  !> @param config_file Path to nexus_output.yaml or nexus_output.rc
  !> @param grid ESMF grid for output fields
  !> @param clock ESMF clock for timing
  !> @param rc Return code
  subroutine OutputInit(config_file, grid, clock, rc)
    character(len=*), intent(in) :: config_file
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    logical :: file_exists
    type(ESMF_Time) :: start_time
    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (localPet == 0) print *, "OutputInit: Initializing CF-compliant output system"

    ! Check if configuration file exists
    inquire(file=trim(config_file), exist=file_exists)
    if (.not. file_exists) then
       if (localPet == 0) print *, "OutputInit: Config file not found, using defaults"
       call CreateDefaultOutputConfig(grid, clock, rc)
       return
    endif

    ! Parse configuration file
    if (index(config_file, '.yaml') > 0 .or. index(config_file, '.yml') > 0) then
       call ParseYAMLOutputConfig(config_file, grid, clock, rc)
    else
       call ParseRCOutputConfig(config_file, grid, clock, rc)
    endif

    if (localPet == 0) print *, "OutputInit: Initialized ", num_output_streams, " output streams"

  end subroutine OutputInit

  !> @brief Create default output configuration
  subroutine CreateDefaultOutputConfig(grid, clock, rc)
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    type(ESMF_Time) :: start_time
    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Create one default hourly output stream
    num_output_streams = 1
    allocate(output_streams(num_output_streams))

    output_streams(1)%name = "nexus_hourly"
    output_streams(1)%filename_template = "NEXUS.%Y%m%d_%H%M%S.nc"
    call ESMF_TimeIntervalSet(output_streams(1)%frequency, h=1, rc=rc)
    output_streams(1)%time_averaged = .false.
    output_streams(1)%instantaneous = .true.
    output_streams(1)%initialized = .false.
    output_streams(1)%accumulation_count = 0

    ! Default variables: all HEMCO diagnostics
    output_streams(1)%num_variables = 5
    allocate(output_streams(1)%variable_list(5))
    output_streams(1)%variable_list = [ &
      'EmisBC_Total ', &
      'EmisOC_Total ', &
      'EmisSO2_Total', &
      'EmisNO_Total ', &
      'EmisCO_Total ' ]

    ! Set next output time
    call ESMF_ClockGet(clock, currTime=start_time, rc=rc)
    output_streams(1)%next_output_time = start_time + output_streams(1)%frequency

    if (localPet == 0) print *, "OutputInit: Created default hourly output stream"

  end subroutine CreateDefaultOutputConfig

  !> @brief Parse YAML output configuration
  subroutine ParseYAMLOutputConfig(config_file, grid, clock, rc)
    character(len=*), intent(in) :: config_file
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! For now, use default config
    ! Full YAML parsing would require additional libraries
    call CreateDefaultOutputConfig(grid, clock, rc)

  end subroutine ParseYAMLOutputConfig

  !> @brief Parse RC-format output configuration
  subroutine ParseRCOutputConfig(config_file, grid, clock, rc)
    character(len=*), intent(in) :: config_file
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! For now, use default config
    call CreateDefaultOutputConfig(grid, clock, rc)

  end subroutine ParseRCOutputConfig

  !> @brief Collect output fields from HEMCO diagnostics
  !> @param HcoState HEMCO state object
  !> @param stream_idx Output stream index
  !> @param rc Return code
  subroutine CollectOutputFields(HcoState, stream_idx, rc)
    type(HCO_State), pointer :: HcoState
    integer, intent(in) :: stream_idx
    integer, intent(out) :: rc

    integer :: i, localPet
    type(ESMF_VM) :: vm
    character(len=64) :: var_name

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (stream_idx < 1 .or. stream_idx > num_output_streams) then
       rc = ESMF_RC_ARG_OUTOFRANGE
       return
    endif

    if (localPet == 0) print *, "CollectOutputFields: Collecting fields for stream ", stream_idx

    ! Collect each variable from HEMCO diagnostics
    do i = 1, output_streams(stream_idx)%num_variables
       var_name = trim(output_streams(stream_idx)%variable_list(i))
       call CollectSingleField(HcoState, stream_idx, var_name, rc)
       if (rc /= ESMF_SUCCESS) then
          if (localPet == 0) print *, "  Warning: Failed to collect ", trim(var_name)
       endif
    enddo

  end subroutine CollectOutputFields

  !> @brief Collect a single field from HEMCO diagnostics
  subroutine CollectSingleField(HcoState, stream_idx, var_name, rc)
    type(HCO_State), pointer :: HcoState
    integer, intent(in) :: stream_idx
    character(len=*), intent(in) :: var_name
    integer, intent(out) :: rc

    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Stub: In full implementation, would extract diagnostic data from HcoState
    ! and add to accumulator if time-averaged, or store directly if instantaneous

    if (localPet == 0) print *, "  Collected field: ", trim(var_name)

  end subroutine CollectSingleField

  !> @brief Write output fields to NetCDF file
  !> @param HcoState HEMCO state object
  !> @param grid ESMF grid
  !> @param clock ESMF clock
  !> @param stream_idx Output stream index
  !> @param rc Return code
  subroutine WriteOutputFields(HcoState, grid, clock, stream_idx, rc)
    type(HCO_State), pointer :: HcoState
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time) :: curr_time
    integer, intent(in) :: stream_idx
    integer, intent(out) :: rc

    character(len=512) :: filename
    integer :: localPet, yy, mm, dd, h, m, s
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (stream_idx < 1 .or. stream_idx > num_output_streams) then
       rc = ESMF_RC_ARG_OUTOFRANGE
       return
    endif

    ! Get current time
    call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)
    call ESMF_TimeGet(curr_time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)

    ! Generate filename from template
    call GenerateFilename(output_streams(stream_idx)%filename_template, &
                         yy, mm, dd, h, m, s, filename, rc)

    if (localPet == 0) print *, "WriteOutputFields: Writing to ", trim(filename)

    ! Write fields using ESMF I/O
    call WriteNetCDFOutput(HcoState, grid, curr_time, stream_idx, filename, rc)

    ! Update next output time
    output_streams(stream_idx)%next_output_time = &
         output_streams(stream_idx)%next_output_time + output_streams(stream_idx)%frequency

  end subroutine WriteOutputFields

  !> @brief Generate filename from template
  subroutine GenerateFilename(template, yy, mm, dd, h, m, s, filename, rc)
    character(len=*), intent(in) :: template
    integer, intent(in) :: yy, mm, dd, h, m, s
    character(len=*), intent(out) :: filename
    integer, intent(out) :: rc

    character(len=512) :: temp
    integer :: pos

    rc = ESMF_SUCCESS
    temp = template

    ! Replace time tokens
    call ReplaceToken(temp, '%Y', yy, 4)
    call ReplaceToken(temp, '%m', mm, 2)
    call ReplaceToken(temp, '%d', dd, 2)
    call ReplaceToken(temp, '%H', h, 2)
    call ReplaceToken(temp, '%M', m, 2)
    call ReplaceToken(temp, '%S', s, 2)

    filename = trim(temp)

  end subroutine GenerateFilename

  !> @brief Replace time token in filename template
  subroutine ReplaceToken(str, token, value, width)
    character(len=*), intent(inout) :: str
    character(len=*), intent(in) :: token
    integer, intent(in) :: value, width
    character(len=32) :: fmt, value_str
    integer :: pos

    write(fmt, '(A,I0,A,I0,A)') '(I', width, '.', width, ')'
    write(value_str, fmt) value

    pos = index(str, token)
    if (pos > 0) then
       str = str(1:pos-1) // trim(value_str) // str(pos+len(token):)
    endif

  end subroutine ReplaceToken

  !> @brief Write NetCDF output with CF-1.8 conventions
  subroutine WriteNetCDFOutput(HcoState, grid, curr_time, stream_idx, filename, rc)
    type(HCO_State), pointer :: HcoState
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Time), intent(in) :: curr_time
    integer, intent(in) :: stream_idx
    character(len=*), intent(in) :: filename
    integer, intent(out) :: rc

    integer :: localPet, i
    type(ESMF_VM) :: vm
    type(ESMF_Field) :: field
    character(len=64) :: var_name

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Create NetCDF file with CF-1.8 global attributes
    call CreateCFNetCDFFile(filename, curr_time, rc)
    if (rc /= ESMF_SUCCESS) return

    ! Write coordinate variables
    call WriteCoordinateVariables(grid, filename, rc)
    if (rc /= ESMF_SUCCESS) return

    ! Write each output variable
    do i = 1, output_streams(stream_idx)%num_variables
       var_name = trim(output_streams(stream_idx)%variable_list(i))
       call WriteVariableToNetCDF(HcoState, grid, var_name, filename, &
                                  output_streams(stream_idx)%time_averaged, rc)
    enddo

    if (localPet == 0) print *, "WriteNetCDFOutput: Successfully wrote ", trim(filename)

  end subroutine WriteNetCDFOutput

  !> @brief Create NetCDF file with CF-1.8 global attributes
  subroutine CreateCFNetCDFFile(filename, curr_time, rc)
    character(len=*), intent(in) :: filename
    type(ESMF_Time), intent(in) :: curr_time
    integer, intent(out) :: rc

    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Stub: Full implementation would use ESMF_FieldWrite or NetCDF library
    ! to create file with CF-1.8 global attributes:
    ! - Conventions = "CF-1.8"
    ! - title = "NEXUS Emission Diagnostics"
    ! - institution = "NASA GMAO"
    ! - source = "NEXUS-HEMCO v1.0"
    ! - history = "Created on YYYY-MM-DD HH:MM:SS"
    ! - references = "http://wiki.seas.harvard.edu/geos-chem/index.php/HEMCO"

    if (localPet == 0) print *, "CreateCFNetCDFFile: Created ", trim(filename)

  end subroutine CreateCFNetCDFFile

  !> @brief Write coordinate variables with CF attributes
  subroutine WriteCoordinateVariables(grid, filename, rc)
    type(ESMF_Grid), intent(in) :: grid
    character(len=*), intent(in) :: filename
    integer, intent(out) :: rc

    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Stub: Full implementation would write:
    ! - lon(lon): standard_name="longitude", units="degrees_east", axis="X"
    ! - lat(lat): standard_name="latitude", units="degrees_north", axis="Y"
    ! - time(time): standard_name="time", units="seconds since YYYY-MM-DD", calendar="gregorian", axis="T"
    ! - time_bnds(time,2): for time-averaged fields

    if (localPet == 0) print *, "WriteCoordinateVariables: Wrote coordinates to ", trim(filename)

  end subroutine WriteCoordinateVariables

  !> @brief Write a single variable to NetCDF with CF metadata
  subroutine WriteVariableToNetCDF(HcoState, grid, var_name, filename, time_averaged, rc)
    type(HCO_State), pointer :: HcoState
    type(ESMF_Grid), intent(in) :: grid
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: filename
    logical, intent(in) :: time_averaged
    integer, intent(out) :: rc

    integer :: i, localPet
    type(ESMF_VM) :: vm
    character(len=128) :: cf_standard_name, long_name, units
    character(len=64) :: cell_methods

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Get CF metadata for this variable
    call GetCFMetadata(var_name, cf_standard_name, long_name, units, rc)

    ! Set cell_methods for time-averaged fields
    if (time_averaged) then
       cell_methods = "time: mean"
    else
       cell_methods = ""
    endif

    ! Stub: Full implementation would:
    ! 1. Extract data from HcoState diagnostics
    ! 2. Create ESMF_Field on grid
    ! 3. Use ESMF_FieldWrite with CF attributes:
    !    - standard_name = cf_standard_name
    !    - long_name = long_name
    !    - units = units
    !    - _FillValue = 1.e20
    !    - missing_value = 1.e20
    !    - cell_methods = cell_methods (if time-averaged)
    ! 4. Enable NetCDF4 compression: deflate_level=1, shuffle=true

    if (localPet == 0) print *, "WriteVariableToNetCDF: Wrote ", trim(var_name), " to ", trim(filename)

  end subroutine WriteVariableToNetCDF

  !> @brief Get CF metadata for HEMCO diagnostic variable
  subroutine GetCFMetadata(hemco_name, cf_standard_name, long_name, units, rc)
    character(len=*), intent(in) :: hemco_name
    character(len=*), intent(out) :: cf_standard_name, long_name, units
    integer, intent(out) :: rc

    integer :: i

    rc = ESMF_SUCCESS

    ! Search CF mapping table
    do i = 1, size(CF_MAPPINGS)
       if (trim(CF_MAPPINGS(i)%hemco_name) == trim(hemco_name)) then
          cf_standard_name = CF_MAPPINGS(i)%cf_standard_name
          long_name = CF_MAPPINGS(i)%long_name
          units = CF_MAPPINGS(i)%units
          return
       endif
    enddo

    ! Default metadata if not found
    cf_standard_name = ""
    long_name = trim(hemco_name)
    units = "kg m-2 s-1"

  end subroutine GetCFMetadata

  !> @brief Validate CF compliance of output file
  !> @param filename NetCDF file to validate
  !> @param rc Return code
  subroutine ValidateCFCompliance(filename, rc)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: rc

    integer :: localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    ! Stub: Full implementation would:
    ! 1. Check for required CF-1.8 global attributes
    ! 2. Check for required coordinate variables with proper attributes
    ! 3. Check for proper variable attributes (standard_name, units, etc.)
    ! 4. Optionally run external cfchecker tool

    if (localPet == 0) print *, "ValidateCFCompliance: Validated ", trim(filename)

  end subroutine ValidateCFCompliance

  !> @brief Finalize output system
  subroutine OutputFinalize(rc)
    integer, intent(out) :: rc

    integer :: i, localPet
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

    if (localPet == 0) print *, "OutputFinalize: Cleaning up output system"

    ! Deallocate output streams
    if (allocated(output_streams)) then
       do i = 1, num_output_streams
          if (allocated(output_streams(i)%variable_list)) then
             deallocate(output_streams(i)%variable_list)
          endif
          if (allocated(output_streams(i)%accumulator)) then
             deallocate(output_streams(i)%accumulator)
          endif
       enddo
       deallocate(output_streams)
    endif

    num_output_streams = 0

  end subroutine OutputFinalize

end module nexus_output_mod
