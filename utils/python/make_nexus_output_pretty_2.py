#!/usr/bin/env python
"""
Make NEXUS output pretty
"""
from pathlib import Path

DEFAULT_GRID_FILE_PATH = Path("./grid_spec.nc")
DEFAULT_TIME_FILE_PATH = Path("./HEMCO_sa_Time.rc")


def get_hemco_dates(time_file=DEFAULT_TIME_FILE_PATH):
    import datetime as dt

    print(time_file.as_posix())

    def parse_dt_line(line):
        _, s_date, s_time = line.split()
        return dt.datetime.strptime(f"{s_date} {s_time}", r"%Y-%m-%d %H:%M:%S")

    start = end = ts_emis = None
    with open(time_file, "r") as f:
        for line in f:
            line = line.strip()
            if line.startswith("START:"):
                start = parse_dt_line(line)
            elif line.startswith("END:"):
                end = parse_dt_line(line)
            elif line.startswith("TS_EMIS:"):  # time step (s)
                _, s_ts = line.split()
                ts_emis = float(s_ts)

    start_base = start.replace(hour=0, minute=0, second=0)
    assert ts_emis == 3600
    start_offset = (start - start_base).total_seconds() / 3600
    dates = [
        start_base + dt.timedelta(hours=h) + dt.timedelta(hours=start_offset)
        for h in range(int((end - start).total_seconds() / 3600) + 1)
    ]

    assert dates[0] == start, f"{dates[0]} should be same as {start}"
    assert dates[-1] == end, f"{dates[-1]} should be same as {end}"
    assert (
        dates[1] - dates[0]
    ).total_seconds() == ts_emis, f"dt {dates[1] - dates[0]} should equal {ts_emis} s"

    return dates, start_base


def main(s_fp, g_fp, t_fp, o_fp):
    import netCDF4 as nc
    import numpy as np

    dates, date_base = get_hemco_dates(t_fp)

    # Open source and grid datasets
    ds_s = nc.Dataset(s_fp, "r")
    ds_g = nc.Dataset(g_fp, "r")

    # Create new dataset
    ds = nc.Dataset(o_fp, "w", format="NETCDF4")
    x_dim = ds.createDimension("x", ds_g["grid_xt"].size)
    y_dim = ds.createDimension("y", ds_g["grid_yt"].size)
    time_dim = ds.createDimension("time", None)
    ds.title = "NEXUS Generated Emission Data"

    # Add coordinates
    lat = ds.createVariable("latitude", np.float32, ("y", "x"))
    lat.long_name = "latitude"
    lat.units = "degree_north"
    lat[:] = ds_g["grid_latt"][:]
    lon = ds.createVariable("longitude", np.float32, ("y", "x"))
    lon.long_name = "longitude"
    lon.units = "degree_east"
    lon[:] = ds_g["grid_lont"][:]
    time = ds.createVariable("time", np.float64, ("time",))
    time.units = date_base.strftime(r"hours since %Y-%m-%d")
    time.long_name = "time"
    time[:] = nc.date2num(dates, time.units)

    # Add other variables
    tmp = np.full((time.size, y_dim.size, x_dim.size), np.nan, dtype=np.float32)
    for vn in ds_s.variables:
        ds.createVariable(vn, np.float32, ("time", "y", "x"), zlib=True, complevel=1)
        ds[vn].units = "kg m-2 s-1"
        ds[vn].long_name = vn
        tmp[:-1, :, :] = ds_s[vn][:].filled(np.nan)
        ds[vn][:] = np.nan_to_num(tmp, posinf=0, neginf=0, nan=0)
        # NOTE: this makes all values 0 at the last time in the dataset

    ds.close()

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(
        description="Make the NEXUS output pretty",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-s",
        "--src",
        type=Path,
        help="input ('ugly') NEXUS file path",
        required=True,
    )
    parser.add_argument(
        "-g",
        "--grid",
        type=Path,
        default=DEFAULT_GRID_FILE_PATH,
        help="grid file path",
    )
    parser.add_argument(
        "-t",
        "--hemco-time",
        type=Path,
        default=DEFAULT_TIME_FILE_PATH,
        help="HEMCO time file path",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="output file path",
        required=True,
    )

    args = parser.parse_args(argv)

    return {
        "s_fp": args.src,
        "g_fp": args.grid,
        "t_fp": args.hemco_time,
        "o_fp": args.output,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
