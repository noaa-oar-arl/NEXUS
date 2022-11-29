#!/usr/bin/env python
"""
Concatenate all species from nexus split jobs 
"""
def main(ifp, ofp, *, compress=True):
    """
    Parameters
    ----------
    ifp, ofp : Path
        Input and output file path.
    """
#    import xarray as xr
    import netCDF4 as nc4
    from glob import glob
    files = glob(ifp)
    files.sort()
    print(files)

    #open all files
    src = nc4.MFDataset(files, aggdim='time')

    # now make new netcdf file
    dst = nc4.Dataset(ofp,'w',format="NETCDF4")

    #first create dimensions
    for name, dimension in src.dimensions.items():
        dst.createDimension(name, (len(dimension) if not dimension.isunlimited() else None))

    # now copy variables
    for name, variable in src.variables.items():
        x = dst.createVariable(name, variable.dtype, variable.dimensions)
        dst[name][:] = src[name][:]

    dst.close()
    src.close()
        
    return 0




def parse_args(argv=None):
    import argparse
    from pathlib import Path
    from glob import glob

    parser = argparse.ArgumentParser(
        description="Combine outputs from NEXUS split jobs."
    )
    parser.add_argument("INPUT", type=str, help="Input directory.")
    parser.add_argument("OUTPUT", type=str, help="Output file path.")
    parser.add_argument("--compress", action="store_true", help="Whether to apply compression for the variables.")
    parser.add_argument("--no-compress", action="store_false", dest="compress")
    parser.set_defaults(compress=True)

    
    args = parser.parse_args(argv)
    return {
        "ifp": args.INPUT,
        "ofp": args.OUTPUT,
        "compress": args.compress,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
