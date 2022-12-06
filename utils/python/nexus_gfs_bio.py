"""
Extract variables from GFS output and format for HEMCO
(i.e., as if MERRA-2)
"""
from pathlib import Path

HERE = Path(__file__).parent

# MERRA-2 data looks like this:
# netcdf MERRA2.20190101.A1.05x0625 {
# dimensions:
#         time = 24 ;
#         lat = 361 ;
#         lon = 576 ;
# variables:
#         int time(time) ;
#                 time:calendar = "gregorian" ;
#                 time:long_name = "time" ;
#                 time:standard_name = "time" ;
#                 time:units = "minutes since 2019-01-01 00:00:00.0" ;
#                 time:delta_t = "0000-00-00 01:00:00" ;
#                 time:begin_date = "20190101" ;
#                 time:begin_time = "000000" ;
#                 time:time_increment = "010000" ;
#         float lat(lat) ;
#                 lat:long_name = "latitude" ;
#                 lat:standard_name = "latitude" ;
#                 lat:units = "degrees_north" ;
#         float lon(lon) ;
#                 lon:long_name = "longitude" ;
#                 lon:standard_name = "longitude" ;
#                 lon:units = "degrees_east" ;
#
# In xarray:
# <xarray.Dataset>
# Dimensions:   (time: 24, lat: 361, lon: 576)
# Coordinates:
#   * time      (time) datetime64[ns] 2019-01-01T00:30:00 ... 2019-01-01T23:30:00
#   * lat       (lat) float32 -90.0 -89.5 -89.0 -88.5 ... 88.5 89.0 89.5 90.0
#   * lon       (lon) float32 -180.0 -179.4 -178.8 -178.1 ... 178.1 178.8 179.4
# ...

m2_ds_attrs = {
    # "Title": "MERRA2 1-hour time-averaged parameters (A1), processed for GEOS-Chem input",
    # "Contact": "GEOS-Chem Support Team (geos-chem-support@as.harvard.edu)",
    # "References": "www.geos-chem.org; wiki.geos-chem.org",
    # "Filename": "MERRA2_Glob",
    # "History": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    # "ProductionDateTime": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    # "ModificationDateTime": "File generated on: 2019/03/02 17:56:33 GMT-0400",
    "Format": "NetCDF-4",
    "SpatialCoverage": "global",
    "Conventions": "COARDS",
    # "Version": "MERRA2",
    # "VersionID": "5.12.4",
    # "Nlayers": "72",
    # "Start_Date": "20190101",
    # "Start_Time": "00:00:00.0",
    # "End_Date": "20190101",
    # "End_Time": "23:59:59.99999",
    # "Delta_Time": "010000",
    "Delta_Lon": "0.625",
    "Delta_Lat": "0.5",
}

m2_time_attrs = {
    "long_name": "time",
    "standard_name": "time",
    # "delta_t": "0000-00-00 01:00:00",
    # "begin_date": "20190101",
    # "begin_time": "000000",
    # "time_increment": "010000",
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

m2_data_var_info = {
    "T2M": {
        "gfs_name": "tmp2m",
        "attrs": {
            "long_name": "2-meter_air_temperature",
            "standard_name": "2-meter_air_temperature",
            "units": "K",
            "gamap_category": "GMAO-2D",
        },
    },
    "GWETROOT": {
        "gfs_name": "soilw4",
        "attrs": {
            "long_name": "root_zone_soil_wetness",
            "standard_name": "root_zone_soil_wetness",
            "units": "1",
            "gamap_category": "GMAO-2D",
        },
    },
    "PARDF": {
        "gfs_name": "vddsf_ave",
        "attrs": {
            "long_name": "surface_downwelling_par_diffuse_flux",
            "standard_name": "surface_downwelling_par_diffuse_flux",
            "units": "W m-2",
            "gamap_category": "GMAO-2D",
        },
    },
    "PARDR": {
        "gfs_name": "vbdsf_ave",
        "attrs": {
            "long_name": "surface_downwelling_par_beam_flux",
            "standard_name": "surface_downwelling_par_beam_flux",
            "units": "W m-2",
            "gamap_category": "GMAO-2D",
        },
    },
}

m2_data_var_old_to_new = {d["gfs_name"]: k for k, d in m2_data_var_info.items()}

import datetime

import netCDF4 as nc
import numpy as np
from scipy.interpolate import RectSphereBivariateSpline, interp1d

#
# Define the new grid (MERRA-2)
#

# lat and lon are float32 in the MERRA-2 files
# They are 1-D coord vars
lat_m2_deg = np.arange(-90, 90 + 0.5, 0.5, dtype=np.float32)
lon_m2_deg = np.arange(-180, 180, 0.625, dtype=np.float32)

lat_m2 = np.deg2rad(lat_m2_deg)
lon_m2 = np.deg2rad(lon_m2_deg)

colat_m2_deg = 90 - lat_m2_deg
colat_m2 = np.deg2rad(colat_m2_deg, dtype=np.float64)

lat_m2 = np.deg2rad(lat_m2_deg)
lon_m2 = np.deg2rad(lon_m2_deg)
assert (np.diff(colat_m2) < 0).all(), "not ascending"
lon_m2_mesh, colat_m2_mesh = np.meshgrid(lon_m2, colat_m2)

# For the lon mesh, [-180, 180) -> [0, 2pi)
# Otherwise we don't get W hemi properly
lon_m2_mesh[lon_m2_mesh < 0] += 2 * np.pi
assert (lon_m2_mesh >= 0).all() and (lon_m2_mesh < 2 * np.pi).all()


DIR = Path("/scratch1/RDARCH/rda-arl-gpu/Barry.Baker/tmp")

o_fp = Path("./t.nc")
files = sorted(DIR.glob("gfs.t00z.sfcf???.nc"))[:2]  # TESTING
assert len(files) >= 2, "need at least 2 for time interp and time calcs"

#
# Create and initialize new dataset
#

ds_new = nc.Dataset(o_fp, "w", format="NETCDF4")
ds_new.title = "Biogenic inputs from GFS for NEXUS/HEMCO"
for k, v in m2_ds_attrs.items():
    ds_new.setncattr(k, v)

ntime_gfs = len(files)  # e.g. 25 (0:3:72)
ntime_m2 = (ntime_gfs - 1) * 3  # e.g. 72 (0.5:1:71.5)
time_dim = ds_new.createDimension("time", ntime_m2)
time = ds_new.createVariable("time", np.int32, ("time",))
for k, v in m2_time_attrs.items():
    setattr(time, k, v)

lat_dim = ds_new.createDimension("lat", lat_m2_deg.size)
lat = ds_new.createVariable("lat", np.float32, ("lat",))
lat[:] = lat_m2_deg
for k, v in m2_lat_attrs.items():
    setattr(lat, k, v)

lon_dim = ds_new.createDimension("lon", lon_m2_deg.size)
lon = ds_new.createVariable("lon", np.float32, ("lon",))
lon[:] = lon_m2_deg
for k, v in m2_lon_attrs.items():
    setattr(lon, k, v)

for vn, d in m2_data_var_info.items():
    var = ds_new.createVariable(vn, np.float32, ("time", "lat", "lon"))
    var[:] = 0
    for k, v in d["attrs"].items():
        setattr(var, k, v)

#
# Get old grid info from first GFS file
#

# The GFS files are 3-hourly and look like this:
# netcdf gfs.t00z.sfcf030 {
# dimensions:
#         grid_xt = 3072 ;
#         grid_yt = 1536 ;
#         time = 1 ;
# variables:
#         double grid_xt(grid_xt) ;
#                 grid_xt:cartesian_axis = "X" ;
#                 grid_xt:long_name = "T-cell longitude" ;
#                 grid_xt:units = "degrees_E" ;
#         double lon(grid_yt, grid_xt) ;
#                 lon:long_name = "T-cell longitude" ;
#                 lon:units = "degrees_E" ;
#         double grid_yt(grid_yt) ;
#                 grid_yt:cartesian_axis = "Y" ;
#                 grid_yt:long_name = "T-cell latiitude" ;
#                 grid_yt:units = "degrees_N" ;
#         double lat(grid_yt, grid_xt) ;
#                 lat:long_name = "T-cell latitude" ;
#                 lat:units = "degrees_N" ;
#         double time(time) ;
#                 time:long_name = "time" ;
#                 time:units = "hours since 2022-11-30 00:00:00" ;
#                 time:cartesian_axis = "T" ;
#                 time:calendar_type = "JULIAN" ;
#                 time:calendar = "JULIAN" ;
# ...

ds = nc.Dataset(files[0], "r")

lat_gfs_deg = ds["grid_yt"][:]
lon_gfs_deg = ds["grid_xt"][:]

# grid_yt starts at N pole, so we are already ascending once convert to colat
colat_gfs_deg = 90 - lat_gfs_deg
colat_gfs = np.deg2rad(colat_gfs_deg)
assert (np.diff(colat_gfs) > 0).all(), "already ascending"
assert colat_gfs.min() > 0 and colat_gfs.max() < np.pi

# grid_xt is [0, 360), so we already meet the conditions
lon_gfs = np.deg2rad(lon_gfs_deg)
assert -np.pi <= lon_gfs[0] < np.pi and lon_gfs[-1] <= lon_gfs[0] + 2 * np.pi

ds.close()

#
# Interpolate to new grid
#

gfs_times = []
for i, fp in enumerate(files):

    ds = nc.Dataset(fp, "r")

    # Get time
    assert ds.dimensions["time"].size == 1
    t = nc.num2date(ds["time"][0], units=ds["time"].units, calendar=ds["time"].calendar)
    print(f"{fp.as_posix()} ({t})")
    gfs_times.append(t)

    for vn_old, vn_new in m2_data_var_old_to_new.items():
        print(f"{vn_old} -> {vn_new}")
        data = ds[vn_old][:].squeeze()  # squeeze singleton time
        # TODO: deal with `.missing_value`/`_FillValue`? (both are set)

        f = RectSphereBivariateSpline(u=colat_gfs, v=lon_gfs, r=data)

        # NOTE: we get `ValueError: requested theta out of bounds.` here
        # for the S pole (colat 180) unless we convert it to float64
        # (I guess there is a bounds check against float64 pi)
        data_new = f.ev(colat_m2_mesh.ravel(), lon_m2_mesh.ravel()).reshape(
            (lon_m2.size, lat_m2.size)
        )

        ds_new[vn_new][i, :, :] = data_new

#
# Set time values and attrs
#

gfs_times = np.array(gfs_times)

# Define output time based on the GFS times
hh = np.timedelta64(30, "m")
h = np.timedelta64(1, "h")
m2_times = np.arange(gfs_times[0] + hh, gfs_times[-1], h)
assert m2_times.size == ntime_m2

# Intermediate calculations for the time attributes
m2_times_dt = m2_times.astype(datetime.datetime)
t0 = m2_times_dt[0]
t0_floored = t0.replace(hour=0, minute=0, second=0, microsecond=0)
calendar = "gregorian"
units = f"minutes since {t0_floored}.0"
delta_t = m2_times_dt[1] - m2_times_dt[0]
assert (np.diff(m2_times_dt) == delta_t).all()
assert delta_t.days == 0
delta_t_h, rem = divmod(delta_t.seconds, 3600)
delta_t_m, delta_t_s = divmod(rem, 60)

# Assign time values and attributes
time[:] = nc.date2num(m2_times, calendar=calendar, units=units)
time.calendar = calendar
time.units = units
time.delta_t = f"0000-00-00 {delta_t_h:02d}:{delta_t_m:02d}:{delta_t_s:02d}"
time.begin_date = t0_floored.strftime(r"%Y%m%d")
time.begin_time = t0_floored.strftime(r"%H%M%S")
time.time_increment = f"{delta_t_h:02d}{delta_t_m:02d}{delta_t_s:02d}"

#
# Time interpolation of data vars
#

x = gfs_times.astype(float)
x_new = m2_times.astype(float)
assert x[0] < x_new[0] < x_new[-1] < x[-1], "fully contains"

for vn in m2_data_var_info:
    f = interp1d(x, ds_new[vn][:ntime_gfs], kind="linear", axis=0, copy=False, assume_sorted=True)
    tmp = f(x_new)
    ds_new[vn][:] = tmp


ds_new.close()

# TESTING
import matplotlib.pyplot as plt, xarray as xr

ds = xr.open_dataset(o_fp, mask_and_scale=False)
ds.T2M.isel(time=0).plot()
plt.show()
