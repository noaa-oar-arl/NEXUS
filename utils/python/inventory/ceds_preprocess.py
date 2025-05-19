#!/usr/bin/env python
# filepath: /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/models/nxs_config/utils/python/inventory/ceds_preprocess.py
"""
CEDS Preprocessor

This script preprocesses CEDS emission data files by converting sector-based emissions
into separate variables for each sector.

Example usage:
    python ceds_preprocess.py input_file.nc output_file.nc
"""

import sys
import xarray as xr
import numpy as np
import argparse
import logging
from datetime import datetime


def get_sector_mappings(sector_attrs):
    """
    Parse the sector IDs and names from the attributes and create mappings.

    Parameters
    ----------
    sector_attrs : dict
        Attributes of the sector coordinate containing sector definitions.
        Expected to have an 'ids' key with value in format '0: Agriculture; 1: Energy; ...'

    Returns
    -------
    dict
        Dictionary mapping sector indices (int) to short names (str).
        Examples: {0: 'agr', 1: 'ene', 2: 'ind', ...}

    Notes
    -----
    The function maps sector names to standardized short codes:
    - Agriculture -> 'agr'
    - Energy -> 'ene'
    - Industrial -> 'ind'
    - Transportation -> 'tra'
    - Residential, Commercial -> 'rco'
    - Solvents -> 'sol'
    - Waste -> 'was'
    - Shipping -> 'shp'
    - Others -> 's{idx:02d}' (e.g., 's09')

    Examples
    --------
    >>> attrs = {'ids': '0: Agriculture; 1: Energy; 2: Industrial'}
    >>> get_sector_mappings(attrs)
    {0: 'agr', 1: 'ene', 2: 'ind'}
    """
    # Parse the sector descriptions from the attribute
    if 'ids' in sector_attrs:
        sector_desc = sector_attrs['ids']
        # Split by semicolon and extract sector information
        sectors = sector_desc.split(';')
        sector_map = {}

        for sector in sectors:
            parts = sector.strip().split(':')
            if len(parts) != 2:
                continue

            idx = int(parts[0])
            name = parts[1].strip()

            # Create short name mapping
            if 'Agriculture' in name:
                short_name = 'agr'
            elif 'Energy' in name:
                short_name = 'ene'
            elif 'Industrial' in name:
                short_name = 'ind'
            elif 'Transportation' in name:
                short_name = 'tra'
            elif 'Residential, Commercial' in name:
                short_name = 'rco'
            elif 'Solvents' in name:
                short_name = 'sol'
            elif 'Waste' in name:
                short_name = 'was'
            elif 'Shipping' in name:
                short_name = 'shp'
            else:
                # Default fallback
                short_name = f"s{idx:02d}"

            sector_map[idx] = short_name

        return sector_map
    else:
        # If no sector IDs are found, use default indices as keys
        return {i: f"s{i:02d}" for i in range(8)}


def preprocess_ceds_file(input_file, output_file):
    """
    Preprocess CEDS file to split sector-based emissions into separate variables.

    This function transforms input CEDS emission files with variables containing a 'sector'
    dimension into separate variables for each sector. For example, a variable 'BC_em_anthro'
    with dimensions (time, sector, lat, lon) will be converted into multiple variables like
    'BC_agr', 'BC_ene', etc., each with dimensions (time, lat, lon).

    Parameters
    ----------
    input_file : str
        Path to input CEDS NetCDF file containing emission data with 'sector' dimension
    output_file : str
        Path to output processed NetCDF file where the results will be written

    Returns
    -------
    None
        The function writes results to the specified output file

    Notes
    -----
    The function preserves all global attributes and coordinates from the input file,
    except for the 'sector' coordinate which is not needed in the output.

    Each output variable maintains the attributes of the original variable, plus:
    - 'original_variable': name of source variable
    - 'sector_index': numeric index of the sector
    - 'long_name': modified to include sector information

    Examples
    --------
    >>> preprocess_ceds_file('BC-em-anthro_input4MIPs_emissions.nc', 'BC-em-anthro_HEMCO.nc')
    Processing BC-em-anthro_input4MIPs_emissions.nc to BC-em-anthro_HEMCO.nc
    Processing variable: BC_em_anthro
    Writing output to BC-em-anthro_HEMCO.nc
    Processing complete
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Processing {input_file} to {output_file}")

    # Open the dataset
    ds = xr.open_dataset(input_file, decode_cf=False, decode_times=False)

    # Create a new dataset for output
    ds_out = xr.Dataset()

    # Copy coordinates and global attributes
    for coord_name in ds.coords:
        if coord_name != 'sector':  # Skip the sector coordinate for output
            ds_out[coord_name] = ds[coord_name]

    # Copy global attributes
    ds_out.attrs.update(ds.attrs)

    # Find only the *_em_anthro variables that have a sector dimension
    emission_vars = []
    for var_name in ds.data_vars:
        if var_name.endswith('_em_anthro') and 'sector' in ds[var_name].dims:
            emission_vars.append(var_name)

    if not emission_vars:
        logger.warning("No _em_anthro variables with 'sector' dimension found in the file.")
        return

    # Process each emission variable
    for var_name in emission_vars:
        logger.info(f"Processing variable: {var_name}")
        var = ds[var_name]

        # Get the species name from the variable name
        species = var_name.split('_')[0]

        # Get sector mappings
        if 'sector' in ds.coords and hasattr(ds.sector, 'attrs'):
            sector_map = get_sector_mappings(ds.sector.attrs)
        else:
            # Default sector mapping if attributes are not available
            sector_map = {i: f"s{i:02d}" for i in range(len(ds.sector))}

        # Extract each sector and create new variables
        for sector_idx in ds.sector.values:
            sector_short = sector_map.get(int(sector_idx), f"s{int(sector_idx):02d}")
            new_var_name = f"{species}_{sector_short}"

            # Extract data for this sector
            sector_data = var.sel(sector=sector_idx).drop_vars('sector')

            # Add to output dataset
            ds_out[new_var_name] = sector_data

            # Copy variable attributes and add sector info
            ds_out[new_var_name].attrs.update(var.attrs)
            ds_out[new_var_name].attrs['original_variable'] = var_name
            ds_out[new_var_name].attrs['sector_index'] = int(sector_idx)

            if 'long_name' in var.attrs:
                base_long_name = var.attrs['long_name']
                ds_out[new_var_name].attrs['long_name'] = f"{base_long_name} - {sector_short}"

    # Copy dimension bounds if they exist (exclude sector bounds)
    for bound_var in ['lat_bnds', 'lon_bnds', 'time_bnds']:
        if bound_var in ds.data_vars and bound_var not in ds_out:
            ds_out[bound_var] = ds[bound_var]

    # Save processed dataset
    logger.info(f"Writing output to {output_file}")
    comp = dict(zlib=True, complevel=4, shuffle=True)
    encoding = {var: comp for var in ds_out.data_vars}
    ds_out.to_netcdf(output_file, encoding=encoding)
    logger.info("Processing complete")


def main():
    """
    Main entry point for the script when run from the command line.

    Parses command line arguments and calls the preprocess_ceds_file function
    with the specified input and output files.

    Parameters
    ----------
    None
        Arguments are parsed from the command line

    Returns
    -------
    None

    Examples
    --------
    From the command line:

    $ python ceds_preprocess.py input.nc output.nc
    """
    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )

    # Parse command line arguments
    parser = argparse.ArgumentParser(description="Preprocess CEDS emission files for HEMCO")
    parser.add_argument("input_file", help="Input CEDS emission file")
    parser.add_argument("output_file", help="Output file path")

    args = parser.parse_args()

    # Process file
    preprocess_ceds_file(args.input_file, args.output_file)


if __name__ == "__main__":
    main()