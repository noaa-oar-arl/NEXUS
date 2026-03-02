# HEMCO Integration

NEXUS utilizes the **Harmonized Emissions Component (HEMCO)** for computing atmospheric emissions. HEMCO allows for flexible, user-defined emission inventories and scaling factors.

## Overview

HEMCO is integrated into NEXUS as a submodule located in the `HEMCO/` directory. NEXUS acts as a wrapper around HEMCO, utilizing the ESMF/NUOPC framework to drive the emissions calculations.

## Configuration

HEMCO is configured primarily through the `HEMCO_Config.rc` file (or as specified in `nexus.rc`). This file defines:

*   **Base Emissions**: The underlying emission inventories.
*   **Scale Factors**: Factors to scale emissions (e.g., temporal, spatial).
*   **Extensions**: Specialized modules for calculating emissions that depend on environmental variables (e.g., dust, lightning).

In the `nexus.rc` control file, you can specify the HEMCO configuration file:

```properties
# HEMCO configuration file
CONFIG_FILE: HEMCO_Config.rc
```

## Data Exchange

NEXUS interacts with HEMCO through the NUOPC "Cap" (`src/nuopc/cap.F90`). The cap handles:

1.  **Initialization**: Reading the configuration and setting up HEMCO states.
2.  **Run**: Advancing the HEMCO clock and computing emissions for the current timestep.
3.  **Data Flow**:
    *   **Import State**: Environmental data (meteorology) passed *to* HEMCO.
    *   **Export State**: Calculated emissions passed *from* HEMCO back to NEXUS (and potentially to other coupled components).

## More Information

For detailed information on HEMCO itself, please refer to:

*   [HEMCO Documentation (ReadTheDocs)](https://hemco.readthedocs.io/en/latest/)
*   [HEMCO Repository](https://github.com/geoschem/hemco)
