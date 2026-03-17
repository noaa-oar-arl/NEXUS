cmake_minimum_required(VERSION 3.13)

# Set the project name and version
project(NEXUS
  VERSION 0.2.0
  LANGUAGES Fortran)

# FC should be used to set compiler
if(NOT DEFINED ENV{FC})
  message(WARNING "FC is not set. You may want to use it to specify a specific compiler.")
else()
  if("$ENV{FC}" STREQUAL "")
    message(WARNING "FC is set to empty string. You may want to use it to specify a specific compiler.")
  else()
    message(STATUS "FC is set: '$ENV{FC}'")
  endif()
endif()

# Include local macros
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

option(USE_MPI "Build with MPI" ON)
option(NEXUS_USE_INLINE_CDEPS "Use inline CDEPS (link to prebuilt CDEPS-interface)" ON)
option(NEXUS_ENABLE_EXTERNAL_DATM "Enable external DATM component/connector build-time linkage" OFF)
set(CDEPS_INTERFACE_DIR "/scratch4/NCEPDEV/naqfc/Barry.Baker/models/ufs-weather-model/build/CDEPS-interface" CACHE PATH "Path to prebuilt CDEPS-interface (include 'mod' and libraries)")
# Find and configure MPI
if(USE_MPI)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  if(MPI_Fortran_FOUND)
    message(STATUS "MPI_Fortran_COMPILER: ${MPI_Fortran_COMPILER}")
    message(STATUS "MPI_Fortran_COMPILE_FLAGS: ${MPI_Fortran_COMPILE_FLAGS}")
    message(STATUS "MPI_Fortran_INCLUDE_PATH: ${MPI_Fortran_INCLUDE_PATH}")
    message(STATUS "MPI_Fortran_LINK_FLAGS: ${MPI_Fortran_LINK_FLAGS}")
    message(STATUS "MPI_Fortran_LIBRARIES: ${MPI_Fortran_LIBRARIES}")
  endif()
endif()

# Find ESMF early to support CDEPS configuration
find_package(ESMF REQUIRED)
if(ESMF_FOUND)
  message(STATUS "ESMF Found: ${ESMF_VERSION}")
  if(ESMF_PIO STREQUAL "external")
    message(STATUS "ESMF uses external PIO. Setting hints for PIO.")
    if(EXISTS "${ESMF_PIO_LIBPATH}")
       get_filename_component(PIO_ROOT_HINT "${ESMF_PIO_LIBPATH}" DIRECTORY)
       set(ENV{PIO} "${PIO_ROOT_HINT}")
       set(ENV{PIO_ROOT} "${PIO_ROOT_HINT}")
       set(ENV{parallelio_ROOT} "${PIO_ROOT_HINT}")
       set(PIO_ROOT "${PIO_ROOT_HINT}")
       set(PIO_PATH "${PIO_ROOT_HINT}")
       set(PIO_C_PATH "${PIO_ROOT_HINT}")
       set(PIO_Fortran_PATH "${PIO_ROOT_HINT}")
       message(STATUS "Setting PIO hint to ${PIO_ROOT_HINT} based on ESMF configuration")
    endif()
  endif()
  # Promote ESMF module paths globally to ensure consistent ESMF version usage across CDEPS, HEMCO, and NEXUS
  if(DEFINED ESMF_F90COMPILEPATHS)
    include_directories(${ESMF_F90COMPILEPATHS})
  endif()
endif()
# Control where the static and shared libraries are built
# so that on Windows we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

# Do not build HEMCO standalone executable
set(HEMCO_EXTERNAL_CONFIG ON CACHE BOOL "Build HEMCO as an external component")
set(NUOPC_ESMF ON CACHE BOOL "Enable NUOPC interface for HEMCO")

if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} UPPER_CMAKE_BUILD_TYPE)
endif()
message(STATUS "CMAKE_BUILD_TYPE: '${UPPER_CMAKE_BUILD_TYPE}'")

# Force double precision for HEMCO and others
add_compile_definitions(USE_REAL8)

if(UPPER_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  # HEMCO already sets `-ftrapuv` for Intel and similar options for GNU
  # https://github.com/geoschem/HEMCO/blob/main/CMakeLists.txt
  # and its options get applied to NEXUS as well.
  # Note HEMCO only support GNU and Intel.
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(EXTRA_FLAGS "-check all")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(EXTRA_FLAGS "-fcheck=all")
  endif()
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${EXTRA_FLAGS}")
  message(STATUS "CMAKE_Fortran_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  string(APPEND CMAKE_Fortran_FLAGS " -diag-disable=10448")
endif()

# Add project's subdirectories
add_subdirectory(src/external/CDEPS)

# Import external CDEPS (optional)
if(NEXUS_USE_INLINE_CDEPS)
  # Define macros required by CDEPS based on compiler
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    add_compile_definitions(CPRGNU)
    add_compile_definitions(HAVE_IEEE_ARITHMETIC)
  elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    add_compile_definitions(CPRINTEL)
    add_compile_definitions(HAVE_IEEE_ARITHMETIC)
  endif()
endif()

add_subdirectory(src)

# Add tests AFTER src so NEXUS_Shared is available
option(BUILD_TESTING "Build the testing tree" ON)
if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()
