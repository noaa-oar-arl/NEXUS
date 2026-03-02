# System Architecture

NEXUS is built upon the **Earth System Modeling Framework (ESMF)** and the **National Unified Operational Prediction Capability (NUOPC)** layer. It is designed as a modular system to facilitate coupling with atmospheric models.

## Core Components

The architecture consists of the following key Fortran modules located in `src/nuopc/`:

### 1. Application Driver (`app.F90`)
The main entry point for the standalone NEXUS application.
*   Initializes ESMF.
*   Reads the `nexus.rc` control file.
*   Creates the NUOPC Driver.
*   Runs the driver loop.
*   Finalizes the system.

### 2. NUOPC Driver (`driver.F90`)
Specializes the generic `NUOPC_Driver`.
*   **Orchestration**: Manages the execution of child components (specifically the NEXUS Model).
*   **Time Management**: Controls the simulation clock and timesteps.
*   **Services**: Sets up the driver services and defines the run sequence.

### 3. NEXUS Cap (`cap.F90`)
The NUOPC "Cap" (Component) that wraps the HEMCO emissions model.
*   **Initialize**: Sets up HEMCO, grids, and states.
*   **Advertise**: Declares the fields (emissions) that NEXUS can provide to other components.
*   **Realize**: Allocates memory for these fields.
*   **Advance**: Stepping the model forward in time. This involves:
    *   Updating the HEMCO clock.
    *   Running the HEMCO core to compute emissions.
    *   Running HEMCO extensions.
    *   Updating diagnostic states.

### 4. NEXUS I/O (`nexus_io_mod.F90`)
Handles input and output operations.
*   **Input**: Reads external data (e.g., meteorological fields) required by HEMCO.
*   **Output**: Writes history files and diagnostics (NetCDF format).
*   **Configuration**: Reads I/O settings from `io.rc`.

## Data Flow

1.  **Input**: The `nexus_io_mod` reads meteorological data and populates the **Import State**.
2.  **Processing**: The `cap` passes this state to HEMCO. HEMCO calculates emissions based on its configuration (`HEMCO_Config.rc`) and the input data.
3.  **Output**: calculated emissions are stored in the **Export State**.
4.  **Distribution**: The `driver` or `nexus_io_mod` can then write these emissions to disk or pass them to another coupled component (e.g., an atmospheric model).

## Configuration Files

*   **`nexus.rc`**: Main control file (timesteps, debug levels, file paths).
*   **`io.rc`**: Configures input and output streams.
*   **`HEMCO_Config.rc`**: Detailed configuration for the HEMCO emissions component.
