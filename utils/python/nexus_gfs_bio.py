"""
Extract variables from GFS output and format for HEMCO
(i.e., as if MERRA-2)
"""

m2_ds_attrs = {
    "Title": "MERRA2 1-hour time-averaged parameters (A1), processed for GEOS-Chem input",
    "Contact": "GEOS-Chem Support Team (geos-chem-support@as.harvard.edu)",
    "References": "www.geos-chem.org; wiki.geos-chem.org",
    "Filename": "MERRA2_Glob",
    "History": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    "ProductionDateTime": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    "ModificationDateTime": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    "Format": "NetCDF-4",
    "SpatialCoverage": "global",
    "Conventions": "COARDS",
    "Version": "MERRA2",
    "VersionID": "5.12.4",
    "Nlayers": "72",
    "Start_Date": "20190101",
    "Start_Time": "00:00:00.0",
    "End_Date": "20190101",
    "End_Time": "23:59:59.99999",
    "Delta_Time": "010000",
    "Delta_Lon": "0.625",
    "Delta_Lat": "0.5",
}

m2_time_attrs = {
    "long_name": "time",
    "standard_name": "time",
    "delta_t": "0000-00-00 01:00:00",
    "begin_date": "20190101",
    "begin_time": "000000",
    "time_increment": "010000",
}

m2_lat_attrs = {
    "long_name": "latitude",
    "standard_name": "latitude",
    "units": "degrees_north",
}

m2_lon_attrs = {
    "long_name": "longitude",
    "standard_name": "longitude",
    "units": "degrees_east",
}

m2_T2M_attrs = {
    "long_name": "2-meter_air_temperature",
    "standard_name": "2-meter_air_temperature",
    "units": "K",
    "gamap_category": "GMAO-2D",
}

m2_GWETROOT_attrs = {
    "long_name": "root_zone_soil_wetness",
    "standard_name": "root_zone_soil_wetness",
    "units": "1",
    "gamap_category": "GMAO-2D",
}

m2_PARDF_attrs = {
    "long_name": "surface_downwelling_par_diffuse_flux",
    "standard_name": "surface_downwelling_par_diffuse_flux",
    "units": "W m-2",
    "gamap_category": "GMAO-2D",
}

m2_PARDR_attrs = {
    "long_name": "surface_downwelling_par_beam_flux",
    "standard_name": "surface_downwelling_par_beam_flux",
    "units": "W m-2",
    "gamap_category": "GMAO-2D",
}

# MERRA-2 data looks like this
# <xarray.Dataset>
# Dimensions:   (time: 24, lat: 361, lon: 576)
# Coordinates:
#   * time      (time) datetime64[ns] 2019-01-01T00:30:00 ... 2019-01-01T23:30:00
#   * lat       (lat) float32 -90.0 -89.5 -89.0 -88.5 ... 88.5 89.0 89.5 90.0
#   * lon       (lon) float32 -180.0 -179.4 -178.8 -178.1 ... 178.1 178.8 179.4

import netCDF4 as nc
import numpy as np

# MERRA-2 grid
# lat and lon are float32
# They are 1-D coord vars
lat = np.arange(-90, 90 + 0.5, 0.5, dtype=np.float32)
lon = np.arange(-180, 180, 0.625, dtype=np.float32)
