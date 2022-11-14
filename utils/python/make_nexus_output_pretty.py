#!/usr/bin/env python

__author__ = 'Barry Baker'
__email__ = 'barry.baker@noaa.gov'
__license__ = 'GPL'

import netCDF4 as nc
import numpy as np
from netCDF4 import date2num,num2date
import datetime as dt
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter 

def get_hemco_dates(time_file='HEMCO_sa_Time.rc'):
    print(time_file)
    with open(time_file, 'r') as fi:
        lines = fi.readlines()
    for L in lines:
        if L.startswith('START'):
            start_base = dt.datetime.strptime(L.split()[1],'%Y-%m-%d')
            start = dt.datetime.strptime(L, 'START:   %Y-%m-%d %H:00:00\n')
        if L.startswith('END'):
            end = dt.datetime.strptime(L, 'END:     %Y-%m-%d %H:00:00\n')
        if L.startswith('TS_EMIS'):
            ts_emis = float(L.split()[1].strip('\n'))
    dates_offset = (start - start_base).total_seconds() / 3600
    dates = [start + dt.timedelta(hours=h) + dt.timedelta(hours=dates_offset) for h in range(int((end - start).total_seconds() / 3600) + 1 )]
    return dates,start_base

def create_ds_dict(dset):
    d = {}
    for var in dset.variables.values():
        d[var.name] = dset[var.name][:]
    return d


if __name__ == '__main__':

    parser = ArgumentParser(description='Make the NEXUS output pretty', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-s', '--src', help='input nexus file', type=str, required=True)
    parser.add_argument('-g', '--grid', help='input grid file', required=False)
    parser.add_argument('-t', '--read-hemco-time', help='Read HEMCO time file', default=True, required=False)
    parser.add_argument('-o', '--output', help='output file name', default=None, required=False)
    args = parser.parse_args()
    
    # get the hemco dates 
    dates,date_base = get_hemco_dates(time_file=args.read_hemco_time)
    
    # open the nexus output 
    ds = nc.Dataset(args.src)
    
    # open the grid file 
    grid_ds = nc.Dataset(args.grid)

    # create dataset dictionaries 
    ds_dict = create_ds_dict(ds)
    grid_dict = create_ds_dict(grid_ds)
    
    # open file
    ncfile = nc.Dataset(args.output,mode='w',format='NETCDF4')

    #create dims
    x_dim = ncfile.createDimension('x',grid_ds['grid_xt'].shape[0])
    y_dim = ncfile.createDimension('y',grid_ds['grid_yt'].shape[0])
    time_dim = ncfile.createDimension('time',None)
    
    # add attributes 
    ncfile.title='NEXUS Generated Emission Data'
    
    # create variables - lat and lon and time
    lat = ncfile.createVariable('latitude',np.float32,('y','x',))
    lat.long_name = 'latitude'
    lat.units = 'degrees_north'
    lon = ncfile.createVariable('longitude',np.float32,('y','x',))
    lon.long_name='longitude'
    lon.units = 'degrees_east'
    
    time = ncfile.createVariable('time',np.float64,('time',))
    time.units = date_base.strftime('hours since %Y-%m-%d')
    time.long_name = 'time'
    
    # create all other variables 
    nc_var_dict = {} 
    for var in ds.variables.values():
        nc_var_dict[var.name] = ncfile.createVariable(var.name, np.float32, ('time','y','x',), zlib=True)
        nc_var_dict[var.name].units = 'kg m-2 s-1'
        nc_var_dict[var.name].long_name = var.name

    #populate variables 
    
    # # first with lat and lon and time 
    lat[:] = grid_dict['grid_latt'][:]
    lon[:] = grid_dict['grid_lont'][:]
    
    # # now the other variables 
    for var in nc_var_dict.keys():
        nc_var_dict[var][:] = np.nan_to_num(ds_dict[var][:].filled(np.nan), posinf=0.0, neginf=0.0, nan=0.0)
        
    # # fill time variable
    times = date2num(dates, time.units)
    time[:] = times
    
    ncfile.close()



