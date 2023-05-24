#!/usr/bin/env python
"""
Combine output files from NEXUS split jobs into single netCDF file
after make-pretty has been run on each split job output file.
"""


def main(ifp, ofp):
    """
    Parameters
    ----------
    ifp, ofp
        Input and output file path.
    """
    from collections import defaultdict

    import netCDF4 as nc4
    from glob import glob

    files = glob(ifp)
    files.sort()
    print(files)

    # We can't use nc.MFDataset since the files are NETCDF4 non-classic
    # There may be duplicate times in the splits, let's figure out what they are.

    time2files = defaultdict(list)
    for f in files:
        print(f)
        ds = nc4.Dataset(f)
        times_num = ds["time"][:]
        times_dt = nc4.num2date(times_num, units=ds["time"].units)
        # ^ by default these are `cftime.DatetimeGregorian`s
        print(times_num)
        print(times_dt)

        for i, t in enumerate(times_dt):
            time2files[t].append((f, i))

        ds.close()

    for t, files in time2files.items():
        if len(files) > 1:
            print(f"{t:%Y-%m-%d %H:%M} appears more than once")
            for f, i in files:
                print(f"- {f} time {i}")

    # # Open all files
    # src = nc4.MFDataset(files, aggdim="time")

    # # Now make new netcdf file
    # dst = nc4.Dataset(ofp, "w", format="NETCDF4")

    # # First create dimensions
    # for name, dimension in src.dimensions.items():
    #     dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)

    # # Now copy variables
    # for name, variable in src.variables.items():
    #     dst.createVariable(name, variable.dtype, variable.dimensions)
    #     dst[name][:] = src[name][:]

    # dst.close()
    # src.close()

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description="Combine outputs from NEXUS split jobs which have had make-pretty applied.")
    parser.add_argument("INPUT", type=str, help="Input directory.")
    parser.add_argument("OUTPUT", type=str, help="Output file path.")

    args = parser.parse_args(argv)
    return {
        "ifp": args.INPUT,
        "ofp": args.OUTPUT,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
