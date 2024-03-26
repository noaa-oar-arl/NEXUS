#!/usr/bin/env python
"""
Combine output files from NEXUS split jobs into single netCDF file
after make-pretty has been run on each split job output file.
"""


def dt_fmt(dt):
    """Convert (cftime) datetime to string."""
    Y = dt.year
    m = dt.month
    d = dt.day
    H = dt.hour
    M = dt.minute
    return f"{Y:04d}-{m:02d}-{d:02d} {H:02d}:{M:02d}"


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
        print(f"first: {dt_fmt(times_dt[0])}, last: {dt_fmt(times_dt[-1])}")

        for i, t in enumerate(times_dt):
            time2files[t].append((f, i))

        ifp2ds[f] = ds

    time = []  # times for new file
    time2files_unique = {}
    for t, locs in sorted(time2files.items()):
        if len(locs) > 1:
            print(f"{dt_fmt(t)} appears more than once")
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

    dt = np.diff(time)
    dt_min = dt.min()
    if not (dt == dt_min).all():
        print(f"WARNING: there are one or more time gaps (min dt is {dt_min})")
        inds = np.where(dt != dt_min)[0]
        for i in inds:
            print(
                f"- time {i} ({dt_fmt(time[i])}) to {i+1} ({dt_fmt(time[i+1])}) has dt {dt[i]}"
            )

    dst = nc4.Dataset(ofp, "w", format="NETCDF4")
    dst.title = "NEXUS output for AQM"
    dst.history = f"Combined results from {len(files)} splits."

    # dims
    for name, dimension in src0.dimensions.items():
        dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)
    assert dst.dimensions["time"].isunlimited()

    # coords
    coord_names = ["time", "latitude", "longitude"]
    for name in coord_names:
        dst.createVariable(name, src0[name].dtype, src0[name].dimensions)
        dst[name].setncatts({key: getattr(src0[name], key) for key in src0[name].ncattrs()})
        if name == "time":
            dst[name][:] = nc4.date2num(time, units=dst[name].units)
        else:
            dst[name][:] = src0[name][:]
    assert dst.variables["time"].size == ntime

    # variables
    for name, variable in src0.variables.items():
        if name in coord_names:
            continue
        dst.createVariable(name, variable.dtype, variable.dimensions)
        dst[name].setncatts({key: getattr(variable, key) for key in variable.ncattrs()})

    #
    # Add the actual data from the split files
    #

    # Combine locs into slices
    last_f = None
    slices = []
    for i, (t, (f, i_f)) in enumerate(sorted(time2files_unique.items())):
        if i == 0:
            # Start first slice
            a = i
            a_f = i_f

        if last_f is not None and f != last_f:
            # New file, end slice
            s = slice(a, i)
            slices.append((s, (last_f, slice(a_f, a_f + s.stop - s.start))))
            # Start next slice
            a = i
            a_f = i_f

        last_f = f

    # End last slice
    s = slice(a, i + 1)
    slices.append((s, (last_f, slice(a_f, a_f + s.stop - s.start))))

    print("Adding data from split files")
    for s_dst, (f, s_src) in slices:
        print(f"time slice {s_src.start}:{s_src.stop} in {f} -> {s_dst.start}:{s_dst.stop} in dst")
        src = ifp2ds[f]
        for name in src.variables:
            if name in coord_names:
                continue
            dst[name][s_dst] = src[name][s_src]

    for src in ifp2ds.values():
        src.close()
    dst.close()

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(
        description="Combine outputs from NEXUS split jobs which have had make-pretty applied.",
    )
    parser.add_argument(
        "INPUT",
        type=str,
        help="Quoted input file path glob for multiple files OR single file path.",
    )
    parser.add_argument(
        "OUTPUT",
        type=str,
        help="Output file path.",
    )

    args = parser.parse_args(argv)
    return {
        "ifp": args.INPUT,
        "ofp": args.OUTPUT,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
