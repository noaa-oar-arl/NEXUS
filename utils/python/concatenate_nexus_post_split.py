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
    import xarray as xr
    from glob import glob
    files = glob(ifp)
    files.sort()
    print(files)

    dsets = []
    for fi in files:
        dsets.append(xr.open_dataset(fi))
    ds = xr.concat(dsets, dim='time')
    
    ds.to_netcdf(ofp)

    return 0


def parse_args(argv=None):
    import argparse
    from pathlib import Path
    from glob import glob

    parser = argparse.ArgumentParser(
        description="Combine outputs from NEXUS split jobs."
    )
    parser.add_argument("INPUT", type=str, help="Input file path.")
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
