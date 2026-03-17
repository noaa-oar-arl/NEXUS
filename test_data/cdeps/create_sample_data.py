#!/usr/bin/env python3
"""
Create sample CDEPS data file for testing.
This is a minimal example - replace with real data in production.
"""

import netCDF4 as nc
import numpy as np
import os

def create_sample_cdeps_data(output_path):
    """Create a minimal CDEPS data file for testing."""
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    ds = nc.Dataset(output_path, 'w', format='NETCDF4')
    lat = ds.createDimension('lat', 180)
    lon = ds.createDimension('lon', 360)
    time = ds.createDimension('time', None)
    species = ds.createDimension('species', 5)
    lat_var = ds.createVariable('lat', 'f4', ('lat',))
    lon_var = ds.createVariable('lon', 'f4', ('lon',))
    time_var = ds.createVariable('time', 'f8', ('time',))
    species_var = ds.createVariable('species', 'i4', ('species',))
    lat_var[:] = np.linspace(-89.5, 89.5, 180)
    lon_var[:] = np.linspace(-179.5, 179.5, 360)
    time_var[:] = [0]
    species_var[:] = [1, 2, 3, 4, 5]
    emission = ds.createVariable('emission', 'f4', ('time', 'species', 'lat', 'lon'), fill_value=1e20)
    emission.units = 'kg/m2/s'
    emission.long_name = 'Sample emission data for testing'
    data = np.zeros((1, 5, 180, 360), dtype=np.float32)
    emission[:] = data
    ds.title = 'Sample CDEPS Data for NEXUS Testing'
    ds.institution = 'GEOS-Chem Support Group'
    ds.source = 'Sample data created for testing'
    ds.history = 'Created by create_sample_cdeps_data.py'
    ds.contact = 'geos-chem-support@g.harvard.edu'
    ds.close()
    print(f"Created sample CDEPS data file: {output_path}")

if __name__ == '__main__':
    import sys
    output = sys.argv[1] if len(sys.argv) > 1 else '/workspace/test_data/cdeps/emission_sample.nc'
    create_sample_cdeps_data(output)
