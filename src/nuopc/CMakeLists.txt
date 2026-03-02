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

# Add PIO support required by CDEPS when inline CDEPS is enabled
if(NEXUS_USE_INLINE_CDEPS)
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
endif()

if(NetCDF_PARALLEL)
  target_compile_definitions(NEXUS_Shared PRIVATE USE_PNETCDF)
  target_compile_definitions(nexus PRIVATE USE_PNETCDF)
endif()

target_compile_options(NEXUS_Shared PRIVATE
  "-I${ESMF_INCLUDE_DIRS}"
  "-I${NetCDF_Fortran_INCLUDE_DIRS}"
)

# Optional include dirs and defs for inline CDEPS
if(NEXUS_USE_INLINE_CDEPS)
  target_compile_options(NEXUS_Shared PRIVATE
    "-I${CDEPS_INTERFACE_DIR}/mod"
    "-I${PIO_Fortran_INCLUDE_DIR}"
  )
  target_compile_definitions(NEXUS_Shared PRIVATE NEXUS_USE_INLINE_CDEPS)
  target_compile_definitions(nexus PRIVATE NEXUS_USE_INLINE_CDEPS)
endif()

target_link_libraries(NEXUS_Shared
  PUBLIC HCOI_Shared
  PUBLIC HCOI_NUOPC
)

# Link to CDEPS and PIO only when inline CDEPS is enabled
if(NEXUS_USE_INLINE_CDEPS)
  # Prefer cdeps target if available, else link archive directly
  if(TARGET cdeps)
    target_link_libraries(NEXUS_Shared PUBLIC cdeps)
  elseif(EXISTS "${CDEPS_INTERFACE_DIR}/libcdeps.a")
    target_link_libraries(NEXUS_Shared PUBLIC ${CDEPS_INTERFACE_DIR}/libcdeps.a)
  else()
    message(FATAL_ERROR "Inline CDEPS enabled but libcdeps target or archive not found. Set CDEPS_INTERFACE_DIR correctly.")
  endif()
  # ESMF must come after CDEPS in link order
  target_link_libraries(NEXUS_Shared PUBLIC esmf)
  target_link_libraries(NEXUS_Shared PUBLIC ${PIO_C_LIBRARIES} ${PIO_Fortran_LIBRARIES})
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
