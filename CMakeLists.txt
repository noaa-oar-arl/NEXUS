# check environment
cmake_minimum_required(VERSION 3.5)

# set compiler
if(NOT DEFINED ENV{FC})
  message(FATAL_ERROR "CMAKE_Fortran_COMPILER is not defined")
endif()

set(CMAKE_Fortran_COMPILER $ENV{FC})
set(CMAKE_C_COMPILER $ENV{CC})
set(CMAKE_CXX_COMPILER $ENV{CXX})

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

# add project's subdirectories
add_subdirectory(HEMCO)
add_subdirectory(src)
add_subdirectory(src/nuopc)
