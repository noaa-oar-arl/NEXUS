add_executable(nexus
  app.F90
)

add_library(NEXUS_Shared STATIC EXCLUDE_FROM_ALL
  cap.F90
  driver.F90
)

target_include_directories(NEXUS_Shared
  INTERFACE ${HEMCO_BINARY_DIR}/mod
)

find_package(MPI REQUIRED)
find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

target_link_libraries(NEXUS_Shared
  PUBLIC HCOI_Shared
  PUBLIC HCOI_Standalone
  PUBLIC esmf
)

target_link_libraries(nexus
  PUBLIC NEXUS_Shared
  PUBLIC esmf
  PUBLIC NetCDF::NetCDF_Fortran
)

set_target_properties(nexus PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# HEMCO sets a lot of flags already, and since they are set as INTERFACE options
# they are applied to NEXUS as well.
# https://github.com/geoschem/HEMCO/blob/main/CMakeLists.txt
# But we can add a few more flags.
if(UPPER_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  # Note HEMCO only support GNU and Intel
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Note `-Wpedantic` causes fatal warnings in HEMCO build
    set(EXTRA_FLAGS_NEXUS -Wconversion-extra -Wcharacter-truncation -Wpedantic)
  endif()
  target_compile_options(NEXUS_Shared PRIVATE ${EXTRA_FLAGS_NEXUS})
  target_compile_options(nexus PRIVATE ${EXTRA_FLAGS_NEXUS})
endif()

install(TARGETS nexus
  RUNTIME DESTINATION ${RUNDIR}
)
