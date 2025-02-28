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
(Replace `noaa-oar-arl/NEXUS` with your fork if desired.)

To install the pre-commit hooks, first [install `pre-commit`](https://pre-commit.com/#install),
e.g. to your Conda environment.
Then, run
```
pre-commit install --install-hooks
```

### Setup

#### Supported UFS machines

UFS spack-stack module files are included for Hera, WCOSS2, Orion, etc.
```
module use ./modulefiles
```

Then, on Hera, for example:
```
module load ufs_hera.intel
```

Hera input data:
```
/scratch1/NCEPDEV/rstprod/nexus_emissions
```
```
/scratch1/RDARCH/rda-arl-gpu/Barry.Baker/emissions/nexus
```

#### GMU Hopper

Custom modules.
```
. /groups/ESS3/zmoon/nexus/env5
```

Input data:
```
/groups/ESS3/ytang/RRFS-input/nexus_emissions
```

#### Ubuntu

See https://github.com/noaa-oar-arl/gha-esmf for examples of how to build ESMF,
or follow the steps to download and use a pre-built ESMF.

Remember to set `ESMFMKFILE` to point to the `esmf.mk` file of your ESMF build.

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

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
