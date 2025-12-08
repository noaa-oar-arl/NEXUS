# Building NEXUS

NEXUS uses a CMake-based build system and includes a helper script `build.sh` to simplify the process on supported platforms.

## Prerequisites

*   **CMake**: Version 3.12 or later.
*   **Fortran Compiler**: e.g., Intel Fortran (`ifort`) or GNU Fortran (`gfortran`).
*   **MPI**: An MPI implementation (e.g., Intel MPI, OpenMPI).
*   **NetCDF**: NetCDF-C and NetCDF-Fortran libraries.
*   **ESMF**: The Earth System Modeling Framework.

## Using `build.sh`

The easiest way to build NEXUS is using the `build.sh` script, which automatically detects the machine and loads the appropriate environment modules (on supported systems like Hera, Orion, etc.).

### Basic Usage

```bash
./build.sh
```

This will:
1.  Detect the machine.
2.  Load necessary modules (if on a supported platform).
3.  Create a `build` directory.
4.  Configure the project using CMake.
5.  Compile the code.

### Options

The `build.sh` script supports several options:

*   `-p <prefix>`: Install prefix (default: `install/` inside the repo).
*   `-t <target>`: Target machine to build for (default: auto-detected).
*   `-c <options>`: Additional CMake options.
*   `-v`: Verbose output.
*   `-f`: Force a clean build (removes the `build` directory first).

Example:
```bash
./build.sh -f -v
```

## Manual Build

If you prefer to build manually or are on an unsupported platform, follow these steps:

1.  **Set up the environment**: Ensure CMake, MPI, NetCDF, and ESMF are in your path. If using `modulefiles`, load the appropriate one:
    ```bash
    module use ./modulefiles
    module load <your_module_file>
    ```

2.  **Create a build directory**:
    ```bash
    mkdir build
    cd build
    ```

3.  **Configure with CMake**:
    ```bash
    cmake .. \
        -DHEMCO_EXTERNAL_CONFIG=ON \
        -DNUOPC_ESMF=ON \
        -DCMAKE_INSTALL_PREFIX=../install
    ```

4.  **Build**:
    ```bash
    make -j 8
    ```

5.  **Install** (optional):
    ```bash
    make install
    ```

## Submodules

NEXUS relies on the HEMCO submodule. Ensure you have cloned the repository recursively:

```bash
git clone --recurse-submodules https://github.com/noaa-oar-arl/NEXUS.git
```

If you already cloned without submodules, run:

```bash
git submodule update --init --recursive
```
