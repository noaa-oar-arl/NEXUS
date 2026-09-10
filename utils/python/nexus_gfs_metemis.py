#!/usr/bin/env python
"""
Extract variables from GFS output and format for HEMCO
(i.e., as if MERRA-2).

This script is based on `nexus_gfs_bio.py`,
see it for more details about the dataset/grid differences.
"""

from pathlib import Path

M2_DS_ATTRS = {
    "Format": "NetCDF-4",
    "SpatialCoverage": "global",
    "Conventions": "COARDS",
    "Delta_Lon": "0.625",
    "Delta_Lat": "0.5",
}

M2_TIME_ATTRS = {
    "long_name": "time",
    "standard_name": "time",
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
    "QV2M": {
        "gfs_name": "spfh2m",
        "attrs": {
            "long_name": "2-meter_specific_humidity",
            "standard_name": "2-meter_specific_humidity",
            "units": "kg/kg",
            "gamap_category": "GMAO-2D",
        },
    },
    "PRECTOT": {
        "gfs_name": "prate_ave",
        "attrs": {
            "long_name": "total_precipitation_rate",
            "standard_name": "total_precipitation_rate",
            "units": "kg/m2/s",
            "gamap_category": "GMAO-2D",
        },
    },
    "FRSNO": {
        "gfs_name": "snowc_ave",
        "attrs": {
            "long_name": "fractional snow cover",
            "standard_name": "fractional_area_of_land_snowcover",
            "units": "1",
            "gamap_category": "GMAO-2D",
        },
    },
    "U10M": {
        "gfs_name": "ugrd10m",
        "attrs": {
            "long_name": "10-meter_eastward_wind",
            "standard_name": "10-meter_eastward_wind",
            "units": "m/s",
            "gamap_category": "GMAO-2D",
        },
    },
    "V10M": {
        "gfs_name": "vgrd10m",
        "attrs": {
            "long_name": "10-meter_northward_wind",
            "standard_name": "10-meter_northward_wind",
            "units": "m/s",
            "gamap_category": "GMAO-2D",
        },
    },
}

M2_DATA_VAR_OLD_TO_NEW = {d["gfs_name"]: k for k, d in M2_DATA_VAR_INFO.items()}


def main(i_fps, o_fp):
    """
    Parameters
    ----------
    i_fps : list of Path
        GFS files to be loaded. Can be a single path with a glob expression file name.
    o_fp : Path, optional
        Desired path of output file.
    """
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

    # t0
    t0 = nc.num2date(ds["time"][0], units=ds["time"].units, calendar=ds["time"].calendar)

    ds.close()

    #
    # Get GFS file times and grid
    #

    print("Reading times")
    gfs_times = []
    for i, fp in enumerate(files):
        ds = nc.Dataset(fp, "r")

        # Get time
        assert ds.dimensions["time"].size == 1
        t_num = ds["time"][0]
        t = nc.num2date(t_num, units=ds["time"].units, calendar=ds["time"].calendar)
        gfs_times.append(t)
        print(t, fp)

        # Get grid
        if i == 0:
            # TODO: should move this up and here check that these are same for all files
            gfs_lon_1d = ds["grid_xt"][:]
            gfs_lat_1d = ds["grid_yt"][:]
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
        o_fp = Path.cwd() / t0.strftime(r"gfs-metemis_%Y%m%d.nc")
    ds_new = nc.Dataset(o_fp, "w", format="NETCDF4")
    ds_new.title = "MetEmis inputs from GFS for NEXUS/HEMCO"
    ds_new.history = (
        "NOAA GFS data reformatted to fit the COARDS conventions and be used in NEXUS/HEMCO"
    )
    for k, v in M2_DS_ATTRS.items():
        ds_new.setncattr(k, v)

    ntime_gfs = len(files)  # e.g. 25 (0:3:72)
    ntime_m2 = int((gfs_times[-1] - gfs_times[0]).total_seconds() / 3600) + 1  # e.g. 73 (0:1:72)
    # NOTE: ^ assumes GFS times are on the hour
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

            if vn_old == "snowc_ave":
                # Convert from % to fraction
                data_new = np.clip(data / 100, 0, 1)
            else:
                data_new = np.clip(data, 0, None)  # no negatives

            ds_new_pre[vn_new][i, :, :] = data_new[::-1, :] if lat_needs_flip else data_new

    #
    # Time interpolation of data vars and set times
    #

    gfs_time_units = gfs_times[0].strftime(r"hours since %Y-%m-%d %H:%M:%S")
    gfs_times_num = nc.date2num(gfs_times, units=gfs_time_units, calendar=gfs_time_calendar)
    gfs_is_hourly = (np.diff(gfs_times_num) == 1).all()
    assert (np.floor(gfs_times_num) == gfs_times_num).all(), "on the hour"

    m2_times_num = np.arange(gfs_times_num[0], gfs_times_num[-1] + 1, 1, dtype=gfs_times_num.dtype)
    assert m2_times_num.size == ntime_m2

    time[:] = m2_times_num
    time.calendar = gfs_time_calendar
    time.units = gfs_time_units

    x = gfs_times_num
    x_new = m2_times_num

    print("Time interp")
    if gfs_is_hourly:
        assert (gfs_times_num == m2_times_num).all()
        print(
            "(but the GFS input is already hourly, so we won't actually do time interp, "
            "just load variables)"
        )
    else:
        print(gfs_times_num, gfs_time_units)
        print("->", m2_times_num)
    for vn in M2_DATA_VAR_INFO:
        print(vn)
        if gfs_is_hourly:
            tmp = ds_new_pre[vn]
        else:
            f = interp1d(x, ds_new_pre[vn], kind="linear", axis=0, copy=False, assume_sorted=True)
            tmp = np.clip(f(x_new), 0, None)
        ds_new[vn][:] = tmp

    print(f"Writing out new dataset to {o_fp.as_posix()}")
    ds_new.close()
    print("Done")

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(
        description="Extract and format MetEmis inputs for NEXUS from GFS data",
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
        help="output file path. Defaults to gfs-metemis_YYYYMMDD.nc if not specified.",
        required=False,
    )

    args = parser.parse_args(argv)

    return {
        "i_fps": args.input,
        "o_fp": args.output,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
