# - Try to find ncio
#
# This module defines
#  ncio_FOUND - system has ncio
#  ncio_INCLUDE_DIRS - the ncio include directory
#  ncio_LIBRARIES - the ncio library
#
# The following variables can be set to guide the search:
#  ncio_ROOT - The root directory of the ncio installation
#  NCIO_INC - The include directory of the ncio installation
#  NCIO_LIB - The library file of the ncio installation

# Check environment variables
if(NOT DEFINED ENV{NCIO_INC} AND NOT DEFINED ENV{ncio_ROOT})
    message(STATUS "NCIO_INC and ncio_ROOT environment variables are not defined.")
endif()

# Find include directory
find_path(ncio_INCLUDE_DIR
  NAMES module_ncio.mod ncio.mod
  HINTS
    $ENV{NCIO_INC}
    $ENV{ncio_ROOT}/include
    ${ncio_ROOT}/include
  PATH_SUFFIXES include
)

# Find library
# First check if NCIO_LIB is a direct path to the file
if(EXISTS "$ENV{NCIO_LIB}" AND NOT IS_DIRECTORY "$ENV{NCIO_LIB}")
    set(ncio_LIBRARY "$ENV{NCIO_LIB}")
else()
    # Extract directory from NCIO_LIB if it's a file path
    get_filename_component(NCIO_LIB_DIR "$ENV{NCIO_LIB}" DIRECTORY)

    find_library(ncio_LIBRARY
      NAMES ncio libncio.a
      HINTS
        ${NCIO_LIB_DIR}
        $ENV{NCIO_LIBDIR}
        $ENV{ncio_ROOT}/lib64
        $ENV{ncio_ROOT}/lib
        ${ncio_ROOT}/lib64
        ${ncio_ROOT}/lib
      PATH_SUFFIXES lib64 lib
    )
endif()

message(STATUS "ncio_INCLUDE_DIR: ${ncio_INCLUDE_DIR}")
message(STATUS "ncio_LIBRARY: ${ncio_LIBRARY}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ncio
  REQUIRED_VARS ncio_LIBRARY ncio_INCLUDE_DIR
)

if(ncio_FOUND AND NOT TARGET ncio)
  add_library(ncio UNKNOWN IMPORTED)
  set_target_properties(ncio PROPERTIES
    IMPORTED_LOCATION "${ncio_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${ncio_INCLUDE_DIR}"
  )
endif()

mark_as_advanced(ncio_INCLUDE_DIR ncio_LIBRARY)