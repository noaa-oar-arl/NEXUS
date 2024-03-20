# check environment
cmake_minimum_required(VERSION 3.5)

# set compiler
if(NOT DEFINED ENV{CMAKE_Fortran_COMPILER})
  message(FATAL_ERROR "CMAKE_Fortran_COMPILER is not defined")
endif()

set(CMAKE_Fortran_COMPILER $ENV{CMAKE_Fortran_COMPILER})

# set the project name and version
project(NEXUS
        VERSION 0.1.0
        LANGUAGES Fortran)

# include local macros
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# control where the static and shared libraries are built so that on windows
# we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

# do not build HEMCO standalone
set(HEMCO_EXTERNAL_CONFIG TRUE)
set(GCCLASSIC_WRAPPER TRUE)

if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} UPPER_CMAKE_BUILD_TYPE)
endif()

if(UPPER_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -ftrapuv -check all")
endif()

# add project's subdirectories
add_subdirectory(HEMCO)
add_subdirectory(src)
