# NEXUS Test Suite - Simple Tests
# Bug Condition, Preservation, and Integration Tests

# Find required packages
find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(ESMF REQUIRED)

# Set common include directories
set(TEST_INCLUDE_DIRS
  ${ESMF_F90COMPILEPATHS}
  ${CMAKE_BINARY_DIR}/src/external/HEMCO/mod
  ${CMAKE_BINARY_DIR}/mod
  ${CMAKE_BINARY_DIR}/src/nuopc
  ${CMAKE_SOURCE_DIR}/src/external/HEMCO/src/Core
  ${CMAKE_SOURCE_DIR}/src/external/HEMCO/src/Extensions
)

# Set common link libraries
set(TEST_LINK_LIBS
  NEXUS_Shared
  esmf
  MPI::MPI_Fortran
  ${HEMCO_LIBRARIES}
)

# Helper function to add a test executable
function(add_nexus_test TEST_NAME TEST_SOURCE)
  # Create executable
  add_executable(${TEST_NAME} ${TEST_SOURCE})

  # Set include directories
  target_include_directories(${TEST_NAME} PRIVATE ${TEST_INCLUDE_DIRS})

  # Link libraries
  target_link_libraries(${TEST_NAME} PRIVATE ${TEST_LINK_LIBS})

  # Add as CTest
  add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})

  # Set test properties
  set_tests_properties(${TEST_NAME} PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    TIMEOUT 300
  )

  # Add dependency on NEXUS_Shared so it's built first
  add_dependencies(${TEST_NAME} NEXUS_Shared)
endfunction()

# Phase 1: Bug Condition Tests (Task 7)
message(STATUS "Adding Bug Condition Tests...")
add_nexus_test(test_cdeps_init test_cdeps_init.F90)
add_nexus_test(test_clock_init test_clock_init.F90)
add_nexus_test(test_nuopc_phase test_nuopc_phase.F90)
add_nexus_test(test_standalone_driver test_standalone_driver.F90)
add_nexus_test(test_field_naming test_field_naming.F90)

# Phase 2: Preservation Tests (Task 8)
message(STATUS "Adding Preservation Tests...")
add_nexus_test(test_hemco_emission_preservation test_hemco_emission_preservation.F90)
add_nexus_test(test_hemco_diagnostics_preservation test_hemco_diagnostics_preservation.F90)
add_nexus_test(test_hemco_extensions_preservation test_hemco_extensions_preservation.F90)
add_nexus_test(test_hemco_config_grid_preservation test_hemco_config_grid_preservation.F90)
add_nexus_test(test_hemco_species_finalization_preservation test_hemco_species_finalization_preservation.F90)

# Phase 3: Integration Tests (Task 9)
message(STATUS "Adding Integration Tests...")
add_nexus_test(test_nuopc_full_init_sequence test_nuopc_full_init_sequence.F90)
add_nexus_test(test_cdeps_hemco_pipeline test_cdeps_hemco_pipeline.F90)
add_nexus_test(test_standalone_end_to_end test_standalone_end_to_end.F90)
add_nexus_test(test_multi_timestep_execution test_multi_timestep_execution.F90)
add_nexus_test(test_finalization_cleanup test_finalization_cleanup.F90)

# Create test labels for organized execution
set_tests_properties(
  test_cdeps_init
  test_clock_init
  test_nuopc_phase
  test_standalone_driver
  test_field_naming
  PROPERTIES LABELS "BugCondition;Task7"
)

set_tests_properties(
  test_hemco_emission_preservation
  test_hemco_diagnostics_preservation
  test_hemco_extensions_preservation
  test_hemco_config_grid_preservation
  test_hemco_species_finalization_preservation
  PROPERTIES LABELS "Preservation;Task8"
)

set_tests_properties(
  test_nuopc_full_init_sequence
  test_cdeps_hemco_pipeline
  test_standalone_end_to_end
  test_multi_timestep_execution
  test_finalization_cleanup
  PROPERTIES LABELS "Integration;Task9"
)

message(STATUS "Added 15 tests across 3 phases")
