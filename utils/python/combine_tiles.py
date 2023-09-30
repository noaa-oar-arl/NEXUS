#!/usr/bin/env python

__author__  = 'Barry Baker'
__email__   = 'barry.baker@noaa.gov'
__license__ = 'GPL'

'''
Simple utility to combine the FV3 6 tiles to one mosaic file.
'''

import xarray as xr
from glob import glob
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from numpy import sort

def write_ncf(dset,outfile):
    print('Output File:', outfile)
    encoding = {}
    for v in dset.data_vars:
        encoding[v] = dict(zlib=True, complevel=4)
    if 'latitude' in dset:
        encoding['latitude'] = dict(zlib=True, complevel=4)
        encoding['longitude'] = dict(zlib=True, complevel=4)
    #else:
    #    encoding["lat"] = dict(zlib=True, complevel=4)
    #    encoding["lon"] = dict(zlib=True, complevel=4)
    if 'lat_b' in dset:
        encoding["lat_b"] = dict(zlib=True, complevel=4)
        encoding["lon_b"] = dict(zlib=True, complevel=4)
    dset.load().to_netcdf(outfile, encoding=encoding)
    
if __name__ == '__main__':

    parser = ArgumentParser(description='Combine 6 tiles into a single file script', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-f', '--files', help='input filename string - example: "GFS_20180101_tile?.nc"', type=str, required=True)
    args = parser.parse_args()
    
    fstring = args.files
    print('===========================')
    print('PROGRAM STARTING')
    print('===========================\n')

    # glob all files
    files = sort(glob(files))
    
    # combine all files 
    print('Reading Files \n')
    q = xr.open_mfdataset(files,concat_dim='nf',combine='nested',decode_cf=False)
    
    # format 
    print('Formatting \n')
    qq = q.rename({'x':'Xdim','y':'Ydim','longitude':'lons','latitude':'lats'})
    qqq = qq.transpose('time','nf','Ydim','Xdim')

    # create output name 
    output = files[0].replace('tile1','')

    write_ncf(qqq,output)

    print('\n=================')
    print('PROGRAM FINISHED')
    print('=================')

