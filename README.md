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


### Setup

#### NOAA Hera

Use the official setup.
```
module use ./modulefiles
module load hera.intel
```

Input data:
```
/scratch1/NCEPDEV/rstprod/nexus_emissions
```
```
/scratch1/RDARCH/rda-arl-gpu/Barry.Baker/emissions/nexus
```

#### GMU Hopper

##### hpc-stack feat. GCC v10 ([somewhat WIP](https://github.com/noaa-oar-arl/ufs-srweather-app/pull/6))

```
module reset
module load hpc-stack/1.2.0
module load netcdf-fortran/4.5.3-4p
```

Input data:
```
/groups/ESS3/ytang/RRFS-input/nexus_emissions
```

#### Ubuntu

Tested with Ubuntu 22.04.
Using GCC v12 available via `apt`.

Dependencies:
```bash
sudo apt install build-essential gfortran-12 libnetcdf-dev libnetcdff-dev liblapack-dev libopenblas-dev mpi-default-dev mpi-default-bin
```

Build ESMF and prepare for NEXUS build:
```bash
v="8.3.1"  # ESMF
gcc="12"
esmf_base=$HOME/esmf

export ESMF_DIR=${esmf_base}/${v}-gcc-${gcc}
cd $esmf_base
mkdir -p $ESMF_DIR
wget https://github.com/esmf-org/esmf/archive/refs/tags/v${v}.tar.gz
tar xzvf v${v}.tar.gz --directory=/tmp && mv /tmp/esmf-${v}/* $ESMF_DIR

export ESMF_COMPILER=gfortran
export ESMF_LAPACK=netlib
export ESMF_COMM=mpi
export ESMF_PIO=internal
export ESMF_NETCDF=nc-config

export OMPI_FC=gfortran-${gcc}
export OMPI_CC=gcc-${gcc}
export OMPI_CXX=g++-${gcc}
export ESMF_F90COMPILER=mpifort
export ESMF_CCOMPILER=mpicc
export ESMF_CXXCOMPILER=mpic++
export ESMF_F90LINKER=/usr/bin/ld
export ESMF_CLINKER=/usr/bin/ld
export ESMF_CXXLINKER=/usr/bin/ld

cd $ESMF_DIR
make lib

# Location of the corresponding `esmf.mk` is needed for CMake to find the lib later
# You can find these with, e.g., `find . -name 'esmf.mk' -exec realpath {} \;`,
# but it should be the following:
ESMFMKFILE=${ESMF_DIR}/lib/libO/Linux.gfortran.64.mpi.default/esmf.mk

# For NEXUS
export CMAKE_Fortran_COMPILER=$ESMF_F90COMPILER
```

To build NEXUS, at least these env vars should be set:
- `ESMFMKFILE`
- `OMPI_FC`
- `CMAKE_Fortran_COMPILER`

### Build

```
cmake -S . -B build
```
```
cmake --build build
```
If successful, the executable will be at `build/bin/nexus`.

To clean up, remove the build directory or use
```
cmake --build build --target clean
```
