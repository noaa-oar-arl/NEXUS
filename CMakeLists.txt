cmake_minimum_required(VERSION 3.5)

# set the project name and version
project(NEXUS VERSION 0.1.0 LANGUAGES Fortran)

# specify the C++ standard
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED True)

# control where the static and shared libraries are built so that on windows
# we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

#set(CMAKE_FC_COMPILER mpiifort)
#set(FC  mpiifort)

set(HEMCO_EXTERNAL_CONFIG TRUE)

# option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# configure a header file to pass the version number only
# configure_file(TutorialConfig.h.in TutorialConfig.h)

# find external libraries
# find_library(ESMF esmf)
# add the MathFunctions library
add_subdirectory(HEMCO)
add_subdirectory(src)

# add the executable
#add_executable(nexus driver.F90)
# target_link_libraries(Tutorial PUBLIC MathFunctions)
