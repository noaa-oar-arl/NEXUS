add_executable(nexus
  app.F90
)

add_library(NEXUS_Shared STATIC EXCLUDE_FROM_ALL
  cap.F90
  driver.F90
  nexus_io_mod.F90
)

# MPI is required if explicitly requested OR if parallel NetCDF is used
if(USE_MPI OR NetCDF_PARALLEL)
  find_package(MPI REQUIRED)
  if(USE_MPI)
    target_compile_definitions(NEXUS_Shared PRIVATE USE_MPI)
    target_compile_definitions(nexus PRIVATE USE_MPI)
  endif()
endif()

target_include_directories(NEXUS_Shared
  INTERFACE ${HEMCO_BINARY_DIR}/mod
  INTERFACE /scratch4/NCEPDEV/naqfc/Barry.Baker/models/ufs-weather-model/build/CDEPS-interface/mod
)

find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

# Add PIO support required by CDEPS - using CDEPS FindPIO module
if (DEFINED ENV{PIO_ROOT})
  message("PIO_ROOT is $ENV{PIO_ROOT}")
else()
  if (DEFINED PIO)
    set(PIO_PATH ${PIO})
  else()
    set(PIO_PATH $ENV{parallelio_ROOT})
  endif()
  find_package(PIO REQUIRED COMPONENT C Fortran PATH ${PIO_PATH})
endif()

if(NetCDF_PARALLEL)
  target_compile_definitions(NEXUS_Shared PRIVATE USE_PNETCDF)
  target_compile_definitions(nexus PRIVATE USE_PNETCDF)
endif()

target_compile_options(NEXUS_Shared PRIVATE
  "-I${ESMF_INCLUDE_DIRS}"
  "-I${NetCDF_Fortran_INCLUDE_DIRS}"
  "-I/scratch4/NCEPDEV/naqfc/Barry.Baker/models/ufs-weather-model/build/CDEPS-interface/mod"
  "-I${PIO_Fortran_INCLUDE_DIR}"
)

# Link to CDEPS directly by library file to avoid problematic target dependencies
# Add PIO libraries that CDEPS requires
target_link_libraries(NEXUS_Shared
  PUBLIC HCOI_Shared
  PUBLIC HCOI_NUOPC
  PUBLIC esmf
  PUBLIC /scratch4/NCEPDEV/naqfc/Barry.Baker/models/ufs-weather-model/build/CDEPS-interface/libcdeps.a
  PUBLIC ${PIO_C_LIBRARIES}
  PUBLIC ${PIO_Fortran_LIBRARIES}
)

target_link_libraries(nexus
  PUBLIC NEXUS_Shared
  PUBLIC esmf
  PUBLIC NetCDF::NetCDF_Fortran
  PUBLIC ${PIO_C_LIBRARIES}
  PUBLIC ${PIO_Fortran_LIBRARIES}
)



if(USE_MPI OR NetCDF_PARALLEL)
  target_link_libraries(NEXUS_Shared PUBLIC MPI::MPI_Fortran)
  target_link_libraries(nexus PUBLIC MPI::MPI_Fortran)
endif()

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
