# NEXUS: NOAA Emission and Exchange Unified System

Welcome to the documentation for NEXUS, the NOAA Emission and Exchange Unified System.

## Overview

NEXUS is a hybrid project utilizing Fortran, C++, and Python, built upon the ESMF/NUOPC framework. Its primary purpose is to provide a unified system for calculating and exchanging emissions data for atmospheric modeling.

## Documentation Sections

*   **[Building NEXUS](building.md)**: Instructions on how to compile and install the system.
*   **[Architecture](architecture.md)**: An overview of the system's design and core components.
*   **[HEMCO Integration](hemco.md)**: Details on the underlying emissions component.
*   **[API Documentation](nexus/annotated.md)**: Auto-generated documentation for the Fortran source code.

## Getting Started

To get started with development, clone the repository recursively:

```bash
git clone --recurse-submodules https://github.com/noaa-oar-arl/NEXUS.git
```

Refer to the [Building](building.md) guide for compilation steps.
