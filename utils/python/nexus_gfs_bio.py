#!/usr/bin/env python
"""
Extract variables from GFS output and format for HEMCO
(i.e., as if MERRA-2)
"""
from pathlib import Path

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

M2_DS_ATTRS = {
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

M2_TIME_ATTRS = {
    "long_name": "time",
    "standard_name": "time",
    # "delta_t": "0000-00-00 01:00:00",
    # "begin_date": "20190101",
    # "begin_time": "000000",
    # "time_increment": "010000",
}

M2_LAT_ATTRS = {
    "long_name": "latitude",
    "standard_name": "latitude",
    "units": "degrees_north",
}

M2_LON_ATTRS = {
    "long_name": "longitude",
    "standard_name": "longitude",
    "units": "degrees_east",
}

M2_DATA_VAR_INFO = {
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

M2_DATA_VAR_OLD_TO_NEW = {d["gfs_name"]: k for k, d in M2_DATA_VAR_INFO.items()}

# https://github.com/NCAR/ccpp-physics/blob/c348f3e363f066c2c513b0449690859d3104bac8/physics/set_soilveg.f#L258
DRYSMC = [
    None,  # for vtype 0
    0.010, 0.025, 0.010, 0.010, 0.010, 0.010,
    0.010, 0.010, 0.010, 0.010, 0.010, 0.010,
    0.010, 0.010, 0.010, 0.010, 0.010, 0.010,
    0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
]

# https://github.com/NCAR/ccpp-physics/blob/c348f3e363f066c2c513b0449690859d3104bac8/physics/set_soilveg.f#L270
MAXSMC = [
    None,  # for vtype 0
    0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
    0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
    0.464, 0.421, 0.200, 0.421, 0.457, 0.200,
    0.395, 0.000, 0.000, 0.000, 0.000, 0.000,
    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
]


def main(i_fps, o_fp):
    """
    Parameters
    ----------
    i_fps : list of Path
        GFS files to be loaded. Can be a single path with a glob expression file name.
    o_fp : Path, optional
        Desired path of output file.
    """
    import datetime

    import netCDF4 as nc
    import numpy as np
    from scipy.interpolate import interp1d

    if len(i_fps) == 1:
        maybe_glob = i_fps[0]
        if "?" in maybe_glob.name or "*" in maybe_glob.name:
            # Expand
            files = sorted(maybe_glob.parent.glob(maybe_glob.name))
        else:
            # Not glob, single file
            files = [maybe_glob]
    else:
        files = sorted(i_fps)
    assert len(files) >= 2, "need at least 2 for time interp and time calcs"

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
    assert (np.diff(lon_gfs) > 0).all(), "already ascending"
    assert -np.pi <= lon_gfs[0] < np.pi and lon_gfs[-1] <= lon_gfs[0] + 2 * np.pi

    # Soil type (so we can set non-soil points to 0 instead of 1)
    sotyp = ds["sotyp"][:].squeeze()

    # Veg type (needed for normalizing the soil water wrt. min/max)
    vtype = ds["vtype"][:].squeeze().astype(int)
    unique_vtypes = sorted(np.unique(vtype))

    # t0
    t0 = nc.num2date(ds["time"][0], units=ds["time"].units, calendar=ds["time"].calendar)

    ds.close()

    #
    # Get GFS file times and grid
    #

    gfs_times = []
    for i, fp in enumerate(files):
        ds = nc.Dataset(fp, "r")

        # Get time
        assert ds.dimensions["time"].size == 1
        t_num = ds["time"][0]
        t = nc.num2date(t_num, units=ds["time"].units, calendar=ds["time"].calendar)
        gfs_times.append(t_num)
        print(t_num, t, fp)

        # Get grid
        if i == 0:
            # TODO: should move this up and here check that these are same for all files
            gfs_lon_1d = ds["grid_xt"][:]
            gfs_lat_1d = ds["grid_yt"][:]
            gfs_time_units = ds["time"].units
            gfs_time_calendar = ds["time"].calendar
            gfs_time_dtype = ds["time"].dtype

        ds.close()

    assert (np.diff(gfs_lon_1d) > 0).all(), "already ascending"
    lat_needs_flip = gfs_lat_1d[0] > gfs_lat_1d[-1]
    if lat_needs_flip:
        print("Will flip lat")

    #
    # Create and initialize new dataset
    #

    if o_fp is None:
        o_fp = Path.cwd() / t0.strftime(r"gfs-bio_%Y%m%d.nc")
    ds_new = nc.Dataset(o_fp, "w", format="NETCDF4")
    ds_new.title = "Biogenic inputs from GFS for NEXUS/HEMCO"
    ds_new.history = "NOAA GFS data reformatted to fit the COARDS conventions and be used in NEXUS/HEMCO"
    for k, v in M2_DS_ATTRS.items():
        ds_new.setncattr(k, v)

    ntime_gfs = len(files)  # e.g. 25 (0:3:72)
    # ntime_m2 = int((gfs_times[-1] - gfs_times[0]).total_seconds() / 3600)  # e.g. 72 (0.5:1:71.5)
    # ntime_m2 = int(gfs_times[-1] - gfs_times[0])
    # ntime_m2 = len(gfs_times)
    ntime_m2 = int(gfs_times[-1] - gfs_times[0] + 1)
    ds_new.createDimension("time", ntime_m2)
    time = ds_new.createVariable("time", gfs_time_dtype, ("time",))
    for k, v in M2_TIME_ATTRS.items():
        setattr(time, k, v)
    time.axis = "T"

    lat_dim = ds_new.createDimension("lat", gfs_lat_1d.size)
    lat = ds_new.createVariable("lat", gfs_lat_1d.dtype, ("lat",))
    lat[:] = gfs_lat_1d[::-1] if lat_needs_flip else gfs_lat_1d
    for k, v in M2_LAT_ATTRS.items():
        setattr(lat, k, v)
    lat.axis = "Y"

    lon_dim = ds_new.createDimension("lon", gfs_lon_1d.size)
    lon = ds_new.createVariable("lon", gfs_lon_1d.dtype, ("lon",))
    lon[:] = gfs_lon_1d
    for k, v in M2_LON_ATTRS.items():
        setattr(lon, k, v)
    lon.axis = "X"

    ds_new_pre = {}
    for vn, d in M2_DATA_VAR_INFO.items():
        var = ds_new.createVariable(vn, np.float32, ("time", "lat", "lon"))
        var[:] = 0
        for k, v in d["attrs"].items():
            setattr(var, k, v)

        ds_new_pre[vn] = np.empty((ntime_gfs, lat_dim.size, lon_dim.size), dtype=np.float32)

    #
    # Load variables
    #

    print("Loading variables")
    for i, (fp, t) in enumerate(zip(files, gfs_times)):

        print(f"{fp.as_posix()} ({t})")

        ds = nc.Dataset(fp, "r")

        for vn_old, vn_new in M2_DATA_VAR_OLD_TO_NEW.items():
            print(f"{vn_old} -> {vn_new}")
            data = ds[vn_old][:].squeeze()  # squeeze singleton time
            # TODO: deal with `.missing_value`/`_FillValue`? (both are set)

            if vn_old == "soilw4":
                # Set non-soil to 0 (from 1)
                # 0: Water
                # 16: Antarctica
                data[(sotyp == 0) | (sotyp == 16)] = 0

                # Normalize soil moisture based on min/max by vtype data
                for vt in unique_vtypes:
                    if vt == 0 or vt > 19:
                        continue
                    max_vt = MAXSMC[vt]
                    min_vt = DRYSMC[vt]
                    assert 0 < min_vt < max_vt < 1, f"{vt} ({min_vt}, {max_vt})"
                    is_vt = vtype == vt
                    data[is_vt] = (data[is_vt] - min_vt) / (max_vt - min_vt)

            if vn_old == "soilw4":
                data_new = np.clip(data, 0, 1)
            else:
                data_new = np.clip(data, 0, None)  # no negatives

            ds_new_pre[vn_new][i, :, :] = data_new[::-1, :] if lat_needs_flip else data_new

    #
    # Set time values and attrs
    #

    # gfs_times = np.array(gfs_times, dtype="datetime64[us]")

    # # Define output time based on the GFS times
    # hh = np.timedelta64(30, "m")
    # h = np.timedelta64(1, "h")
    # m2_times = np.arange(gfs_times[0] + hh, gfs_times[-1], h)
    # assert m2_times.size == ntime_m2

    # # Intermediate calculations for the time attributes
    # m2_times_dt = m2_times.astype(datetime.datetime)
    # t0 = m2_times_dt[0]
    # t0_floored = t0.replace(hour=0, minute=0, second=0, microsecond=0)
    # calendar = "gregorian"
    # units = f"minutes since {t0_floored}.0"
    # delta_t = m2_times_dt[1] - m2_times_dt[0] if ntime_m2 > 1 else t0 - t0_floored
    # assert (np.diff(m2_times_dt) == delta_t).all()
    # assert delta_t.days == 0
    # delta_t_h, rem = divmod(delta_t.seconds, 3600)
    # delta_t_m, delta_t_s = divmod(rem, 60)

    # # Assign time values and attributes
    # time[:] = nc.date2num(m2_times_dt, calendar=calendar, units=units)
    # time.calendar = calendar
    # time.units = units
    # time.delta_t = f"0000-00-00 {delta_t_h:02d}:{delta_t_m:02d}:{delta_t_s:02d}"
    # time.begin_date = t0_floored.strftime(r"%Y%m%d")
    # time.begin_time = t0_floored.strftime(r"%H%M%S")
    # time.time_increment = f"{delta_t_h:02d}{delta_t_m:02d}{delta_t_s:02d}"

    # gfs_times = np.array(gfs_times, dtype=gfs_time_dtype)
    # assert (np.floor(gfs_times) == gfs_times).all(), "hourly on the hour"
    # assert gfs_time_units.startswith("hours since ")
    # h = 1
    # hh = 0.5
    # m2_times = np.arange(gfs_times[0] + hh, gfs_times[-1], h)
    # assert m2_times.size == ntime_m2

    # time[:] = nc.date2num(m2_times_dt, calendar=gfs_time_calendar, units=gfs_time_units)
    # time[:] = m2_times
    # time[:] = gfs_times
    # time.calendar = gfs_time_calendar
    # time.units = gfs_time_units

    #
    # Time interpolation of data vars
    #

    # x = gfs_times #.astype(float)
    # x_new = m2_times #.astype(float)
    # assert x[0] < x_new[0] <= x_new[-1] < x[-1], "fully contains"

    gfs_times = np.array(gfs_times, dtype=gfs_time_dtype)
    gfs_is_hourly = (np.diff(gfs_times) == 1).all()
    assert (np.floor(gfs_times) == gfs_times).all(), "on the hour"
    assert gfs_time_units.startswith("hours since ")

    m2_times = np.arange(gfs_times[0], gfs_times[-1] + 1, 1, dtype=gfs_time_dtype)
    assert m2_times.size == ntime_m2

    time[:] = m2_times
    time.calendar = gfs_time_calendar
    time.units = gfs_time_units

    x = gfs_times
    x_new = m2_times
    assert x[-1] == x_new[-1]
    with np.printoptions(precision=14):
        print(x)
        print(x_new)
        inds = np.searchsorted(x, x_new)
        print(inds)
        print(inds.clip(1, len(x) - 1).astype(int))

    print("Time interp")
    if gfs_is_hourly:
        assert (gfs_times == m2_times).all()
        print("(but the GFS input is already hourly, so we won't actually do time interp, just load variables)")
    for vn in M2_DATA_VAR_INFO:
        print(vn)
        if gfs_is_hourly:
            tmp = ds_new_pre[vn]
        else:
            f = interp1d(x, ds_new_pre[vn], kind="linear", axis=0, copy=False, assume_sorted=True)
            tmp = f(x_new)
        # tmp = ds_new_pre[vn][:ntime_m2]
        # tmp = ds_new_pre[vn]
        ds_new[vn][:] = tmp
        # ds_new[vn][:-1] = tmp
        # ds_new[vn][-1] = tmp[-1]


    print(f"Writing out new dataset to {o_fp.as_posix()}")
    ds_new.close()
    print("Done")

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(
        description="Extract and format bio inputs for NEXUS from GFS data",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-i",
        "--input",
        type=Path,
        nargs="+",
        help=(
            "paths to the GFS files to be loaded. "
            "Can be an expanded or unexpanded glob expression. "
            "If unexpanded, the wildcards must be in the file name part of the path."
        ),
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="output file path. Defaults to gfs-bio_YYYYMMDD.nc if not specified.",
        required=False,
    )

    args = parser.parse_args(argv)

    return {
        "i_fps": args.input,
        "o_fp": args.output,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
