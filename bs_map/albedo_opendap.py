#!/usr/bin/env python
from joblib import Parallel, delayed
import xarray as xr
from numpy import exp
import pandas as pd
import global_land_mask as glm
import monet as m

def _get_file_url(collection,product,date):
    import pandas as pd
    year = date.strftime('%Y')
    doy = date.strftime('%j')
    base = 'https://ladsweb.modaps.eosdis.nasa.gov/opendap/allData'
    url = '{}/{}/{}/{}/{}/contents.html'.format(base,collection,product,year,doy)
    c = pd.read_html(url)[0].dropna()
    output_url = '{}/{}/{}/{}/{}/{}'.format(base,collection,product,year,doy,c.Name.values[0])
    return output_url

def _read_variable(url,variable):
    try:
        var = xr.open_dataset(url)[variable]
    except:
        print('First attempt failed.  Trying again...')
        var = xr.open_dataset(url)[variable]
    return var


def calc_wn(albedo,nbar):
    wn = (1-albedo.where(albedo >=0)) / nbar
    wn.name = 'shadow'
    wn.attrs['long_name'] = 'Shadow'
    wn.attrs['description'] = 'Chappel and Webb 2016, eq. 28'
    wn.attrs['units'] = 'None'
    return wn

def calc_wns(wn,a=.0001,b = .1, minimum=0,maximum = 35):
    wns = (a - b)* ( wn - maximum) / (minimum - maximum) + b
    wns.name = 'wns'
    wns.attrs['long_name'] = 'Normalized BRDF Albedo'
    wns.attrs['description'] = 'Chappel and Webb 2016, eq. 8'
    wns.attrs['units'] = 'None'
    wns.attrs['_FillValue'] = -999
    wns.attrs['valid_range'] = [0,1]
    return wns.where(wns > 0).where(wns <=1)

def calc_usuf(wns):
    usuf = 0.0311 * exp(-wns**1.131/ 0.016) + 0.007
    usuf.name = 'usuf'
    usuf.attrs['long_name'] = 'Surface Friction to Free Stream Velocity'
    usuf.attrs['description'] = 'Chappel and Webb 2016, eq. 24'
    usuf.attrs['units'] = 'None'
    usuf.attrs['_FillValue'] = -999
    usuf.attrs['valid_min'] = 0
    return usuf.where(usuf > 0).fillna(-999)

def calc_rt(wns):
    rt = 0.9516 * exp(-wns**1.1869/ 0.0039) + 0.048
    rt.name = 'drag_partition'
    rt.attrs['long_name'] = 'Albedo Derived Drag Partition'
    rt.attrs['description'] = 'Chappel and Webb 2016, eq. 20'
    rt.attrs['units'] = 'None'
    rt.attrs['valid_range'] = [1e-3,1]
    rt.attrs['_FillValue'] = 1e-4
    return rt.fillna(1e-4)

def calc_utuf(wns):
    utuf = 0.0497 * ( 1 - exp(-wns**1.326/ 0.00027) ) + 0.038
    utuf.name = 'utuf'
    utuf.attrs['long_name'] = 'Total Friction to Free Stream Velocity'
    utuf.attrs['description'] = 'Chappel and Webb 2016, eq. 19'
    utuf.attrs['units'] = 'None'
    utuf.attrs['_FillValue'] = -999
    utuf.attrs['valid_min'] = 0.
    return utuf.fillna(-999)

def calc_z0(wns,d=10):
    lndz0 =  0.41 / (( 0.0497 * (1 - exp(-1 * (wns ** 1.326 / 0.0027))) + 0.038))
    z0 = exp(-lndz0) * 10
    z0.name = 'Z0'
    z0.attrs['long_name'] = 'Surface Roughness'
    z0.attrs['units'] = 'm'
    z0.attrs['_FillValue'] = -999
    z0.attrs['valid_min'] = 0.
    return z0.fillna(-999)

def find_island(wns):
    x = wns.rename({'Latitude':'lat','Longitude':'lon'})
    ff = m.dataset_to_monet(x.to_dataset())
    island = glm.is_land(ff.latitude.values,ff.longitude.values)
    return island

def calc_bsmap(wns, snow=None, wns_max=0.04, wnsi_max=500, name='BSM'):
    if snow is None:
        w = wns.where(wns < wns_max)
    else:
        w = wns.where(wns < wns_max).where(snow < 1)
    wnsi = (1/w).fillna(0)
    wnsin = ((wnsi - wnsi.min())/(wnsi_max + wnsi.min())).fillna(0)
    wnsin.name = name
    if name != 'BSM':
        wnsin.attrs['long_name'] = 'Baker-Schepanski Sediment Supply Map - Gap Filled'
    else:
        wnsin.attrs['long_name'] = 'Baker-Schepanski Sediment Supply Map'
    wnsin.attrs['units'] = 'None'
    wnsin.attrs['_FillValue'] = 0.
    wnsin.attrs['valid_range'] = [0,1]
    return wnsin.where(wnsin <= 1).where(wnsin > 0)
    
def driver(date,output=None):
    # get the needed urls
    print('Collecting urls...')
    albedo_url = _get_file_url(6,'MCD43C3',date)
    nbar_url = _get_file_url(6,'MCD43C4',date)

    # get the albedo
    print('Reading Albedo....')
    albedo = _read_variable(albedo_url, 'Albedo_BSA_Band1')
    # get the snow
    print('Reading Snow...')
    snow = _read_variable(albedo_url, 'Percent_Snow')
    # get the nbar
    print('Reading NBAR...')
    nbar = _read_variable(nbar_url, 'Nadir_Reflectance_Band1')

    print('Calculating Parameters...')
    wn = calc_wn(albedo,nbar)
    wns = calc_wns(wn)
    island = find_island(wns)
    wns_mean = xr.open_dataset('wns_gap_filled-mean_2002-2019.nc').wns.rename({'lat':'Latitude','lon':'Longitude'})
    #print(wns)
    #print(wns_mean)
    wnsgf = wns.where(wns > 0).where(island)
    wnsgf = wnsgf.where(snow ==0).fillna(wns_mean)
    #wnsgf = wns.where(wns > 0).where(island).where(snow < 1).fillna(wns_mean)
    usuf = calc_usuf(wnsgf)
    utuf = calc_utuf(wnsgf)
    #   z0 = calc_z0(wns)
    rt = calc_rt(wnsgf)
    bsm = calc_bsmap(wns,snow=snow)
    bsm_L2 = calc_bsmap(wnsgf)
    wns = wns.fillna(-999)
    
    # create BS BINS
    bs_map_low = bsm_L2.where(bsm_L2 > 0).where(bsm_L2 <= .2).fillna(-999)
    bs_map_med = bsm_L2.where(bsm_L2 > 0.2).where(bsm_L2 <= 0.6).fillna(-999)
    bs_map_hig = bsm_L2.where(bsm_L2 > 0.6).where(bsm_L2 <= 1).fillna(-999)
    print('Creating Dataset...')
    das = dict(shadow=wn,
               wns=wns,
               drag=rt,
               usuf=usuf,
               utuf=utuf,
               bsm_L2=bsm_L2,
               snow=snow,
               bsm=bsm,
               bsm_L2_1=bs_map_low,
               bsm_L2_2=bs_map_med,
               bsm_L2_3=bs_map_hig)
    ds = xr.Dataset(das)

    ds['time'] = date

    ds = ds.expand_dims('time')#.set_coords('time')
#    print(ds['time'])
    #ds['time'] = date
    ds = ds.rename({'Latitude':'lat','Longitude':'lon'})
    ds = ds.set_coords(['time'])
#    print(ds)
    if output is None:
        return ds
    else:
        write_ncf(ds,output)
        return ds

def write_ncf(dset,output_name):
    print('Writing:', output_name)
    comp = dict(zlib=True,complevel=5)
    encoding = {}
    for i in dset.data_vars.keys():
        encoding[i] = comp 
#    encoding = {dset.name:comp}
#    final['time'] = date
#    if 'time' not in dset.dims:
#        final = dset.expand_dims('time')
#    else:
#        final = dset
#    final = final.set_coords('time')
    dset.attrs['github_url'] = 'https://github.com/bbakernoaa/dust_map'
    dset.attrs['contact'] = 'barry.baker@noaa.gov, kerstin.schepanski@tropos.de'
    dset.attrs['date_created'] = pd.to_datetime('today').strftime('%Y-%m-%d')
#    final.attrs['transform'] = (0.004, 0.0, -179.9999999971294, 0.0, -0.004, 69.99791666038062)
#    final.attrs['crs'] = '+init=epsg:4326'
#    final.attrs['res'] = (0.004, 0.004)
    dset.to_netcdf(output_name,encoding=encoding)


def multi_day_loop(dates,concat=False):
    if concat:
        dsets = []
        for i in dates:
            print('Processing:',i)
            try:
                dsets.append(driver(i))
            except:
                dsets.append(driver(i))
        ds = xr.concat(dsets,dim='time')
        write_ncf(ds,dates[0].strftime('BS_MAP_L2_%Y%m%d.nc'))
    else:
        for i in dates:
            name = i.strftime('BS_MAP_L2_%Y%m%d.nc')
            driver(i,output=name)

def copy_attrs(ds1,ds2): 
    for i in ds1.attrs: 
        ds2.attrs[i] = ds1.attrs[i] 
    for i in ds1.data_vars: 
        for j in ds1[i].attrs: 
            ds2[i].attrs[j] = ds1[i].attrs[j] 
    return ds2
            
def multi_day_average_loop(dates, nprocs=5):
    dsets = []
    #for i in dates:
    #    print('Processing:',i)
    #    try:
    #        dsets.append(driver(i))   
    #    except:
    #        dsets.append(driver(i))
    dsets = Parallel(n_jobs=nprocs,verbose=10)(delayed(driver)(i) for i in dates)
    print('Concatenating Arrays...')
    ds = xr.concat(dsets,dim='time')
    print('Averaging Arrays...')
    ds2 = copy_attrs(ds,ds.mean(dim='time'))
    ds2['time'] = dates[0]
    ds2 = ds2.expand_dims('time')
    name = dates[0].strftime('BS_MAP_AVERAGE_L2_%Y%m%d.nc')
    
    write_ncf(ds2,name)
    
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(
        description='Create the Baker-Schepanski Sediment Supply Map',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-s',
                        '--start_date',
                        type=str,
                        default=None,
                        required=True,
                        help="Inital Day for processing.  Format: YYYY-MM-DD"
                        )
    parser.add_argument('-e',
			'--end_date',
			type=str,
			default=None,
			required=False,
                        help="Final Day for processing.  It will process from start_date to end_date. Format YYYY-MM-DD"
                        )
    parser.add_argument('-a',
			'--average',
			type=bool,
			default=False,
			help="Average Results from start day to end day"
			)
    parser.add_argument('-c',
                        '--concatenate',
                        type=bool,
                        default=False,
                        help='Concatenate all dates over time'
                        )
    parser.add_argument('-p',
                        '--processors',
                        type=int,
                        default=5,
                        help='Number of parallel jobs'
                        )
    args = parser.parse_args()

    sdate = args.start_date
    edate = args.end_date

    if edate is None:
        dates = pd.date_range(start=sdate,end=sdate)
    else:
        dates = pd.date_range(start=sdate,end=edate)

    dsets = []
    if args.average:
        multi_day_average_loop(dates, nprocs=args.processors)
    else:
        multi_day_loop(dates,concat=args.concatenate)
        
