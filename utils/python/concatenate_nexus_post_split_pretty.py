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
    from glob import glob

    import netCDF4 as nc4
    import numpy as np

    files = glob(ifp)
    files.sort()
    print(files)

    # NOTE: We can't use nc.MFDataset since the files are NETCDF4 non-classic

    #
    # There may be duplicate times in the splits, let's figure out what they are.
    #

    ifp2ds = {}
    time2files = defaultdict(list)
    for f in files:
        print(f)
        ds = nc4.Dataset(f)
        times_num = ds["time"][:]
        times_dt = nc4.num2date(times_num, units=ds["time"].units)
        # ^ by default these are `cftime.DatetimeGregorian`s
        #   the pretty files currently don't have calendar set
        print(len(times_dt), "times")
        print(f"first: {times_dt[0]:%Y-%m-%d %H:%M}, last: {times_dt[-1]:%Y-%m-%d %H:%M}")

        for i, t in enumerate(times_dt):
            time2files[t].append((f, i))

        ifp2ds[f] = ds

    time = []  # times for new file
    time2files_unique = {}
    for t, locs in sorted(time2files.items()):
        if len(locs) > 1:
            print(f"{t:%Y-%m-%d %H:%M} appears more than once")
            for f, i in locs:
                print(f"- {f} time {i}")
        time2files_unique[t] = locs[-1]  # take last one (later simulation)
        time.append(t)

    #
    # Create new file skeleton based on first src file
    #

    print("Creating new file skeleton based on first src file")
    src0 = ifp2ds[files[0]]

    time = np.array(time)
    ntime = time.size

    dst = nc4.Dataset(ofp, "w", format="NETCDF4")
    dst.title = "NEXUS output for AQM"
    dst.history = f"Combined results from {len(files)} splits."

    # dims
    for name, dimension in src0.dimensions.items():
        dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)
    assert dst.dimensions["time"].isunlimited()

    # coords
    for name in ["time", "latitude", "longitude"]:
        dst.createVariable(name, src0[name].dtype, src0[name].dimensions)
        dst[name].setncatts({key: getattr(src0[name], key) for key in src0[name].ncattrs()})
        if name == "time":
            dst[name][:] = nc4.date2num(time, units=dst[name].units)
        else:
            dst[name][:] = src0[name][:]
    assert dst.variables["time"].size == ntime

    # variables
    for name, variable in src0.variables.items():
        if name in ["time", "latitude", "longitude"]:
            continue
        dst.createVariable(name, variable.dtype, variable.dimensions)
        dst[name].setncatts({key: getattr(variable, key) for key in variable.ncattrs()})

    #
    # Add the actual data from the split files
    #

    print("Adding data from split files")
    for i, (t, (f, i_f)) in enumerate(sorted(time2files_unique.items())):
        print(f"{i+1}/{ntime} {t:%Y-%m-%d %H:%M}")
        if f not in ifp2ds:
            ifp2ds[f] = nc4.Dataset(f)
        src = ifp2ds[f]
        for name in src.variables:
            if name in ["time", "latitude", "longitude"]:
                continue
            dst[name][i] = src[name][i_f]

    for src in ifp2ds.values():
        src.close()
    dst.close()

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
