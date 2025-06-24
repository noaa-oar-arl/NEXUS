#!/bin/bash

# build.sh
# 1 - determine host, load modules on supported hosts; proceed w/o otherwise
# 2 - configure; build; install

set -eu

echo "Start ... `date`"
dir_root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source $dir_root/ush/detect_machine.sh

# ==============================================================================
usage() {
  set +x
  echo
  echo "Usage: $0 -p <prefix> | -t <target> -h"
  echo
  echo "  -p  installation prefix <prefix>    DEFAULT: <none>"
  echo "  -t  target to build for <target>    DEFAULT: $MACHINE_ID"
  echo "  -c  additional CMake options        DEFAULT: <none>"
  echo "  -v  build with verbose output       DEFAULT: NO"
  echo "  -f  force a clean build             DEFAULT: NO"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

# ==============================================================================

# Defaults:
INSTALL_PREFIX="${dir_root}/install"
CMAKE_INSTALL_LIBDIR="lib"
CMAKE_OPTS=""
BUILD_TARGET="${MACHINE_ID:-'localhost'}"
BUILD_VERBOSE="NO"
CLEAN_BUILD="NO"
COMPILER="${COMPILER:-intel}"
WORKFLOW_BUILD=${WORKFLOW_BUILD:-"OFF"}

while getopts "wt:c:hvfa" opt; do
  case $opt in
    t)
      BUILD_TARGET=$OPTARG
      ;;
    c)
      CMAKE_OPTS=$OPTARG
      ;;
    v)
      BUILD_VERBOSE=YES
      ;;
    f)
      CLEAN_BUILD=YES
      ;;
    h|\?|:)
      usage
      ;;
  esac
done

case ${BUILD_TARGET} in
  hera | orion | hercules | wcoss2 | noaacloud | gaeac5 | gaeac6 | ursa )
    echo "Building NEXUS on $BUILD_TARGET"
    source $dir_root/ush/module-setup.sh
    module use $dir_root/modulefiles
    module load ufs_$BUILD_TARGET.$COMPILER
    module list
    ;;
  $(hostname))
    echo "Building NEXUS on $BUILD_TARGET"
    ;;
  *)
    echo "Building NEXUS on unknown target: $BUILD_TARGET"
    ;;
esac

BUILD_DIR=${BUILD_DIR:-$dir_root/build}
if [[ $CLEAN_BUILD == 'YES' ]]; then
  [[ -d ${BUILD_DIR} ]] && rm -rf ${BUILD_DIR}
fi
mkdir -p ${BUILD_DIR} && cd ${BUILD_DIR}

# Configure
echo "Configuring ... `date`"
set -x
cmake \
  ${CMAKE_OPTS:-} \
  $dir_root
set +x

# Build
echo "Building ... `date`"
set -x
make -j ${BUILD_JOBS:-8}
set +x

echo "Finish ... `date`"
exit 0
