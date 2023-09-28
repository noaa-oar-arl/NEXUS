add_executable(nexus2
  app.F90
)

add_library(NEXUS_Shared2 STATIC EXCLUDE_FROM_ALL
  cap.F90
  driver.F90
)

target_include_directories(NEXUS_Shared2
  INTERFACE ${HEMCO_BINARY_DIR}/mod
)

# find external libraries
find_package(MPI REQUIRED)
find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

target_link_libraries(NEXUS_Shared2
  PUBLIC HCOI_Shared
  PUBLIC HCOI_Standalone
  PUBLIC esmf
)

target_link_libraries(nexus2
  PUBLIC NEXUS_Shared2
  PUBLIC esmf
  PUBLIC NetCDF::NetCDF_Fortran
)

set_target_properties(nexus2 PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

install(TARGETS nexus2
  RUNTIME DESTINATION ${RUNDIR}
)
