#!/usr/bin/env python3
"""
Create sample mesh file for CDEPS testing.
This is a minimal example - replace with real mesh data in production.
"""

import netCDF4 as nc
import numpy as np
import os

def create_sample_mesh(output_path):
    """Create a minimal mesh file for testing."""
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    ds = nc.Dataset(output_path, 'w', format='NETCDF4')
    nCells = ds.createDimension('nCells', 64800)
    nVertices = ds.createDimension('nVertices', 129600)
    nEdges = ds.createDimension('nEdges', 194400)
    vertexPerCell = ds.createDimension('vertexPerCell', 6)
    latCell = ds.createVariable('latCell', 'f8', ('nCells',))
    lonCell = ds.createVariable('lonCell', 'f8', ('nCells',))
    latVertex = ds.createVariable('latVertex', 'f8', ('nVertices',))
    lonVertex = ds.createVariable('lonVertex', 'f8', ('nVertices',))
    verticesPerCell = ds.createVariable('verticesPerCell', 'i4', ('nCells', 'vertexPerCell'))
    edgeVertices = ds.createVariable('edgeVertices', 'i4', ('nEdges', 2))
    cellsOnVertex = ds.createVariable('cellsOnVertex', 'i4', ('nVertices', 3))
    lat_vals = np.linspace(-89.5, 89.5, 180)
    lon_vals = np.linspace(-179.5, 179.5, 360)
    idx = 0
    for lat in lat_vals:
        for lon in lon_vals:
            latCell[idx] = np.radians(lat)
            lonCell[idx] = np.radians(lon)
            idx += 1
    ds.title = 'Sample Mesh for NEXUS Testing'
    ds.grid_type = 'unstructured'
    ds.conventions = 'UGRID'
    ds.close()
    print(f"Created sample mesh file: {output_path}")

if __name__ == '__main__':
    import sys
    output = sys.argv[1] if len(sys.argv) > 1 else '/workspace/test_data/mesh/test_mesh.nc'
    create_sample_mesh(output)
