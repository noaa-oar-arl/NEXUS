add_executable(nexus
  app.F90
)

add_library(NEXUS_Shared STATIC EXCLUDE_FROM_ALL
  nexus_types.F90
  nexus_species_mod.F90
  nexus_runtime_config_mod.F90
  nexus_grid_mod.F90
  nexus_config_mod.F90
  nexus_state_mod.F90
  nexus_field_advertisement_mod.F90
  nexus_initialize_mod.F90
  nexus_cdeps_inline_mod.F90
  nexus_output_mod.F90
  cap.F90
  driver.F90
  nexus_io_mod.F90
)

# MPI is always required for this system
find_package(MPI REQUIRED)
target_compile_definitions(NEXUS_Shared PRIVATE USE_MPI)
target_compile_definitions(nexus PRIVATE USE_MPI)

target_include_directories(NEXUS_Shared
  INTERFACE ${HEMCO_BINARY_DIR}/mod
)

find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)
find_package(PIO REQUIRED COMPONENTS Fortran C)

if(NetCDF_PARALLEL)
  target_compile_definitions(NEXUS_Shared PRIVATE USE_PNETCDF)
  target_compile_definitions(nexus PRIVATE USE_PNETCDF)
endif()

target_include_directories(NEXUS_Shared PRIVATE
  ${ESMF_F90COMPILEPATHS}
  ${NetCDF_Fortran_INCLUDE_DIRS}
  ${PIO_Fortran_INCLUDE_DIRS}
)

# Optional include dirs and defs for inline CDEPS
if(NEXUS_USE_INLINE_CDEPS)
  if(EXISTS "${CDEPS_INTERFACE_DIR}/mod")
    target_compile_options(NEXUS_Shared PRIVATE
      "-I${CDEPS_INTERFACE_DIR}/mod"
    )
  endif()
  target_compile_definitions(NEXUS_Shared PRIVATE NEXUS_USE_INLINE_CDEPS)
  target_compile_definitions(nexus PRIVATE NEXUS_USE_INLINE_CDEPS)
endif()

target_link_libraries(NEXUS_Shared
  PUBLIC HCOI_Shared
  PUBLIC HCOI_NUOPC
)

if(TARGET PIO::PIO_Fortran)
  target_link_libraries(NEXUS_Shared PUBLIC PIO::PIO_Fortran)
else()
  target_link_libraries(NEXUS_Shared PUBLIC ${PIO_Fortran_LIBRARIES} ${PIO_C_LIBRARIES})
endif()

# Link to CDEPS and ESMF when inline CDEPS is enabled
if(NEXUS_USE_INLINE_CDEPS)
  # When building CDEPS from source, link to the relevant library targets directly
  if(TARGET dshr)
     target_link_libraries(NEXUS_Shared PUBLIC dshr)
  endif()
  if(TARGET streams)
     target_link_libraries(NEXUS_Shared PUBLIC streams)
  endif()
  if(TARGET cdeps_share)
     target_link_libraries(NEXUS_Shared PUBLIC cdeps_share)
  endif()
  if(TARGET datm)
     target_link_libraries(NEXUS_Shared PUBLIC datm)
  endif()

  # Fallback for pre-built archive
  if(NOT TARGET dshr AND EXISTS "${CDEPS_INTERFACE_DIR}/libcdeps.a")
    target_link_libraries(NEXUS_Shared PUBLIC ${CDEPS_INTERFACE_DIR}/libcdeps.a)
  endif()

  # FoX libraries must be linked for CDEPS XML parsing
  # Find FoX libraries built in the same build
  if(TARGET FoX_dom)
    target_link_libraries(NEXUS_Shared PUBLIC FoX_dom FoX_sax FoX_wxml FoX_common FoX_utils FoX_fsys)
  endif()

  # Explicitly link PIO Fortran library for CDEPS
  target_link_libraries(NEXUS_Shared PUBLIC piof pioc)

  # ESMF must come after CDEPS in link order
  # ESMF automatically provides PIO linkage when built with external PIO support
  target_link_libraries(NEXUS_Shared PUBLIC esmf)
endif()

target_link_libraries(nexus
  PUBLIC NEXUS_Shared
  PUBLIC NetCDF::NetCDF_Fortran
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
