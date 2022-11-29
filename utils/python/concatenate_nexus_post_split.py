#!/usr/bin/env python
"""
Combine output files from NEXUS split jobs into single netCDF file.
"""


def main(ifp, ofp):
    """
    Parameters
    ----------
    ifp, ofp
        Input and output file path.
    """
    import netCDF4 as nc4
    from glob import glob

    files = glob(ifp)
    files.sort()
    print(files)

    # Open all files
    src = nc4.MFDataset(files, aggdim="time")

    # Now make new netcdf file
    dst = nc4.Dataset(ofp, "w", format="NETCDF4")

    # First create dimensions
    for name, dimension in src.dimensions.items():
        dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)

    # Now copy variables
    for name, variable in src.variables.items():
        dst.createVariable(name, variable.dtype, variable.dimensions)
        dst[name][:] = src[name][:]

    dst.close()
    src.close()

    return 0


def parse_args(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description="Combine outputs from NEXUS split jobs.")
    parser.add_argument("INPUT", type=str, help="Input directory.")
    parser.add_argument("OUTPUT", type=str, help="Output file path.")

    args = parser.parse_args(argv)
    return {
        "ifp": args.INPUT,
        "ofp": args.OUTPUT,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
