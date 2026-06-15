cmake_minimum_required(VERSION 3.5)

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
message(STATUS "CMAKE_Fortran_COMPILER_ID: '${CMAKE_Fortran_COMPILER_ID}'")

# Include local macros
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Control where the static and shared libraries are built
# so that on Windows we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

# Do not build HEMCO standalone executable
set(HEMCO_EXTERNAL_CONFIG TRUE)
set(GCCLASSIC_WRAPPER TRUE)

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()
string(TOUPPER "${CMAKE_BUILD_TYPE}" UPPER_CMAKE_BUILD_TYPE)

set(GDB_FRIENDLY_DEFAULT OFF)
if(DEFINED ENV{GDB_FRIENDLY} AND NOT "$ENV{GDB_FRIENDLY}" STREQUAL "")
  set(GDB_FRIENDLY_DEFAULT "$ENV{GDB_FRIENDLY}")
endif()
option(GDB_FRIENDLY "Build with GDB-friendly settings by forcing Debug but skipping extra runtime checks" ${GDB_FRIENDLY_DEFAULT})

if(GDB_FRIENDLY AND NOT UPPER_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  message(STATUS "GDB_FRIENDLY is enabled; forcing CMAKE_BUILD_TYPE=Debug")
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
  set(UPPER_CMAKE_BUILD_TYPE DEBUG)
endif()

message(STATUS "CMAKE_BUILD_TYPE: '${UPPER_CMAKE_BUILD_TYPE}'")
message(STATUS "GDB_FRIENDLY: '${GDB_FRIENDLY}'")

if(UPPER_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  # HEMCO already sets `-ftrapuv` for Intel and similar options for GNU
  # https://github.com/geoschem/HEMCO/blob/main/CMakeLists.txt
  # and its options get applied to NEXUS as well.
  # Note HEMCO only support GNU and Intel (classic).
  # For GDB usage, we don't want check-all because it prevents GDB backtrace from working properly.
  if(GDB_FRIENDLY)
    message(STATUS "Skipping extra debug runtime-check flags because GDB_FRIENDLY is enabled")
  else()
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
      set(EXTRA_FLAGS "-check all")
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
      set(EXTRA_FLAGS "-fcheck=all")
    endif()
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${EXTRA_FLAGS}")
    message(STATUS "CMAKE_Fortran_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}")
  endif()
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  string(APPEND CMAKE_Fortran_FLAGS " -diag-disable=10448")
endif()

# Add project's subdirectories
add_subdirectory(HEMCO)
add_subdirectory(src)
