# NEXUS

This is the NOAA Emission and Exchange Unified System (NEXUS)

## Development

Clone the repository and check out the submodules:
```
git clone -b develop --recurse-submodules https://github.com/noaa-oar-arl/NEXUS.git
```
or
```
git clone -b develop --recurse-submodules git@github.com:noaa-oar-arl/NEXUS.git
```
(Replace `noaa-oar-arl` with your fork if desired.)


### NOAA Hera

...

### GMU Hopper

...

### Ubuntu

Tested with Ubuntu 22.04.
Using GNU Fortran v12 available via `apt`.

Dependencies:
```bash
sudo apt install build-essential gfortran-12 libnetcdf-dev libnetcdff-dev liblapack-dev libopenblas-dev mpi-default-dev mpi-default-bin
# TODO: allow building NEXUS without MPI?
```

Build minimal ESMF:
```bash
v="8.3.1"  # ESMF
gcc="12"
ESMF_BASE=$HOME/esmf

export ESMF_DIR=${ESMF_BASE}/${v}-gcc-${gcc}
cd $ESMF_BASE
mkdir -p $ESMF_DIR
wget https://github.com/esmf-org/esmf/archive/refs/tags/v${v}.tar.gz
tar xzvf v${v}.tar.gz --directory=/tmp && mv /tmp/esmf-${v}/* $ESMF_DIR

export ESMF_LAPACK=netlib
export ESMF_COMPILER=gfortran
export ESMF_COMM=mpiuni  # MPI bypass

export ESMF_F90COMPILER=/usr/bin/gfortran-${gcc}
export ESMF_CCOMPILER=/usr/bin/gcc-${gcc}
export ESMF_CXXCOMPILER=/usr/bin/g++-${gcc}
export ESMF_F90LINKER=/usr/bin/ld
export ESMF_CLINKER=/usr/bin/ld
export ESMF_CXXLINKER=/usr/bin/ld

cd $ESMF_DIR
make

# Location of `esmf.mk` is needed for CMake to find the lib later
ESMFMKFILE=${ESMF_DIR}/lib/libO/Linux.gfortran.64.mpiuni.default/esmf.mk
```

Configure:
```bash
export CMAKE_Fortran_COMPILER=/usr/bin/gfortran-12
# TODO: allow `FC`
```
```
cmake -S . -B build
```
Build:
```
cmake --build build
```
If successful, the executable will be at `build/bin/nexus`.

To clean up, remove the build directory or use
```
cmake --build build --target clean
```
