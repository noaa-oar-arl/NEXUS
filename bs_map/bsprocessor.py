import os
from glob import glob

import numpy as np
import rasterio as rio
import xarray as xr
from rasterio.transform import from_origin
import time
from joblib import Parallel, delayed
from nasa_utils import get_modis_files_to_download, get_nasa_data, SessionWithHeaderRedirection, create_netrc, get_nsidc_files_to_download
from utility import merge_tile_data, warp_to_wgs84, write_array_tif
import gc
path = '.'

# def _create_doy(year, doy):
#     import numpy as np
#     day4_list = np.arange(1, 366, 8)
#     try:
#         doy = int(day4_list[day4_list < doy][-1])
#     except:
#         doy = int(day4_list[0])
#     return doy


def driver(year, doy, user, passwd, sat='MOLA'):

    region=None

    if sat is 'MOLA' or sat is 'MOTA':
        s = 'MOTA'
        p1 = 'MCD43A1.006'
        p2 = 'MCD43A3.006'
        ext='.hdf'
    else:
        s = 'VIIRS'
        p1 = 'VNP43IA1.001'
        p2 = 'VNP43IA3.001'
        ext='.h5'

    #ESTABLISH NASA PASSWORD AND USER
    session = SessionWithHeaderRedirection(user,passwd)
    #make sure user has .netrc file in home directory
    create_netrc(user,passwd)

    mcd43a1_path = os.path.join(os.getcwd(), '{}_{}_{}{}.tif'.format(
        s, p1, year, str(doy).zfill(3)))
    mcd43a3_path = os.path.join(os.getcwd(), '{}_{}_{}{}.tif'.format(
        s, p2, year, str(doy).zfill(3)))
    mod10a2_path = os.path.join(os.getcwd(), '{}_{}_{}{}.tif'.format('MOSA','MOD10A2.006',year,str(doy).zfill(3)))

    if not os.path.exists(mcd43a1_path.replace('.tif','.nc')) and not os.path.exists(mcd43a3_path.replace('.tif','.nc')):
        output_path = './'
        print('###################################################')
        print('Downloading {} {} Tiles for: {}'.format(s,p1,doy))
        print("###################################################")
        for i in range(4):
            fns, basenames_a1 = get_modis_files_to_download(year, doy, None, output_path, ext,sat=s,product=p1)
            if len(fns) > 0:
                Parallel(n_jobs=5,verbose=10)(delayed(download)(user,passwd,fn) for fn in fns)
            else:
                print('{} {} is all downloaded ......'.format(s,p1))
                break
#        basenames_a1 = glob('VNP43IA1*.h5')
        print('###################################################')
        print('Downloading {} {} Tiles for: {}'.format(s,p2,doy))
        print("###################################################")
        for i in range(4):
            fns, basenames_a2 = get_modis_files_to_download(year, doy, None, output_path, ext, sat=s,product=p2)
            if len(fns) > 0:
                Parallel(n_jobs=5,verbose=10)(delayed(download)(user,passwd,fn) for fn in fns)
            else:
                print('{} {} is all downloaded ......'.format(s,p2))
                break
 #       basenames_a2 = glob('VNP43IA3*.h5')
        print('###################################################')
        print('Downloading {} {} Tiles for: {}'.format(s,p2,doy))
        print("###################################################")
        for i in range(4):
            fns, basenames_a3 = get_nsidc_files_to_download(year, doy, user,passwd, ext='hdf', sat='MOST',product='MOD10A2.006')
            if len(fns) > 0:
                Parallel(n_jobs=5,verbose=10)(delayed(download)(user,passwd,fn) for fn in fns)
            else:
                print('{} {} is all downloaded ......'.format(s,p2))
                break


        #check for tiff tiles already created
        print('################################################')
        print('          Writing {} to TIFs'.format(p1))
        print('################################################')
        tiffnames1 = [fname.replace(ext,'.tif') for fname in basenames_a1]
#        print(tiffnames1,basenames_a1)
        if s == 'VIIRS':
            Parallel(n_jobs=24,verbose=10)(delayed(_convert_hdf_tif_vnp43ia1)(hdf,tif) for hdf,tif in zip(basenames_a1,tiffnames1))
        else:
            Parallel(n_jobs=24,verbose=10)(delayed(_convert_hdf_tif_mcd43a1)(hdf,tif) for hdf,tif in zip(basenames_a1,tiffnames1))
        gc.collect()
        print('################################################')
        print('          Writing {} to TIFs'.format(p2))
        print('################################################')
        tiffnames3 = [fname.replace(ext,'.tif') for fname in basenames_a2]
        if s == 'VIIRS':
            Parallel(n_jobs=24,verbose=10)(delayed(_convert_hdf_tif_vnp43ia3)(hdf,tif) for hdf,tif in zip(basenames_a2,tiffnames3))
        else:
            Parallel(n_jobs=24,verbose=10)(delayed(_convert_hdf_tif_mcd43a3)(hdf,tif) for hdf,tif in zip(basenames_a2,tiffnames3))
        gc.collect()
        print('################################################')
        print('          Writing MOD10A2 to TIFs')
        print('################################################')
        tiffnamesnow = [fname.replace('.hdf','.tif') for fname in basenames_a3]
        Parallel(n_jobs=24,verbose=10)(delayed(_convert_hdf_tif_mod10a2)(hdf,tif) for hdf,tif in zip(basenames_a3,tiffnamesnow))
        gc.collect()
        print('################################################')
        print('          WARPING {} TIF Files'.format(p1))
        print('################################################')
        tiffwarp1 = [fname.replace('.tif','_warped.tif') for fname in tiffnames1]
        Parallel(n_jobs=12,verbose=10)(delayed(warp_to_wgs84)(fname) for fname in tiffnames1)

        print('################################################')
        print('          WARPING {} TIF Files'.format(p2))
        print('################################################')
        tiffwarp3 = [fname.replace('.tif','_warped.tif') for fname in tiffnames3]
        Parallel(n_jobs=12,verbose=10)(delayed(warp_to_wgs84)(fname) for fname in tiffnames3)
        gc.collect()
        print('################################################')
        print('          WARPING MOD10A2 TIF Files')
        print('################################################')
        tiffwarpsnow = [fname.replace('.tif','_warped.tif') for fname in tiffnamesnow]
        Parallel(n_jobs=12,verbose=10)(delayed(warp_to_wgs84)(fname) for fname in tiffnamesnow)
        gc.collect()
        print('################################################')
        print('        Merging {} Warped TIFS to File'.format(p1))
        print('################################################')
        pp = p1.split('.')[0] + '.A'
        tiffm1 = [fname.replace('.tif','_warped.tif') for fname in tiffnames1]
        tiffm1 = glob('{}{}{}.*_warped.tif'.format(pp,year, str(doy).zfill(3)))
        merge_tile_data(tiffm1, mcd43a1_path)
        gc.collect()
        print('################################################')
        print('        Merging {} Warped TIFS to File'.format(p2))
        print('################################################')
        pp = p2.split('.')[0] + '.A'
        tiffm3 = [fname.replace('.tif','_warped.tif') for fname in tiffnames3]
        tiffm3 = glob('{}{}{}.*_warped.tif'.format(pp,year, str(doy).zfill(3)))
        merge_tile_data(tiffm3, mcd43a3_path)
        gc.collect()
        print('################################################')
        print('        Merging MOD10A2 Warped TIFS to File')
        print('################################################')
        pp = 'MOD10A2.A'
        tiffmsnow = [fname.replace('.tif','_warped.tif') for fname in tiffnamesnow]
        tiffmsnow = glob('{}{}{}.*_warped.tif'.format(pp,year, str(doy).zfill(3)))
        merge_tile_data(tiffmsnow, mod10a2_path)
        gc.collect()
        print('################################################')
        print('        Cleaning Up Files....')
        print('################################################')
#        files = glob('{}.A{}{}.*.tif'.format(year, str(doy).zfill(3)))
        Parallel(n_jobs=12,verbose=10)(delayed(os.remove)(fname) for fname in files)
#        files =	glob('{}.A{}{}.*.tif'.format(p2.plyear, str(doy).zfill(3)))
        Parallel(n_jobs=12,verbose=10)(delayed(os.remove)(fname) for fname in files)

        # Print Cleaning up HDF files
        print('################################################')
        print('          Cleaning Up RAW HDF Files')
        print('################################################')
        #        Parallel(n_jobs=10,verbose=10)(delayed(os.remove)(fname) for fname in basenames_a1)
        #        Parallel(n_jobs=10,verbose=10)(delayed(os.remove)(fname) for fname in basenames_a2)

        # write to netcdf
        nbar = xr.open_rasterio(mcd43a1_path)
        albedo = xr.open_rasterio(mcd43a3_path)
        wn = calc_wn(albedo, nbar)
        wns = calc_wns(wn)

        snow = xr.open_rasterio(mod10a2_path)
        bs_map = calc_bsmap(wns, snow)

        bs_map_low = bs_map.where(bs_map > 0).where(bs_map <= .2).fillna(-999)
        bs_map_med = bs_map.where(bs_map > 0.2).where(bs_map <= 0.6).fillna(-999)
        bs_map_hig = bs_map.where(bs_map > 0.6).where(bs_map <= 1).fillna(-999)

        das = dict(shadow=wn,
                   wns=wns,
                   #               drag=rt,
                   #               usuf=usuf,
                   #               utuf=utuf,
                   #               z0=z0,
                   snow=snow,
                   bs_map=bs_map,
                   bs_map1=bs_map_low,
                   bs_map2=bs_map_med,
                   bs_map3=bs_map_hig)
        ds = xr.Dataset(das)

        ds['time'] = date
        ds = ds.expand_dims('time').set_coords('time')
        write_ncf(ds,mcd43a3_path.replace('.tif','.nc')

    return mcd43a3_path.replace('.tif','.nc')


def write_ncf(dset,output_name):
    print('Writing:', output_name)
    comp = dict(zlib=True,complevel=5)
    encoding = {}
    for i in dset.data_vars.keys():
        encoding[i] = comp
    dset.attrs['github_url'] = 'https://github.com/bbakernoaa/dust_map'
    dset.attrs['contact'] = 'barry.baker@noaa.gov, kerstin.schepanski@tropos.de'
    dset.attrs['date_created'] = pd.to_datetime('today').strftime('%Y-%m-%d')
    dset.to_netcdf(output_name,encoding=encoding)

def calc_bsmap(wns, snow, wns_max=0.04, wnsi_max=500):
    w = wns.where(wns < wns_max).where(snow <1)
    wnsi = (1/w).fillna(0)
    wnsin = ((wnsi - wnsi.min())/(wnsi_max + wnsi.min())).fillna(0)
    wnsin.name = 'BS_MAP'
    wnsin.attrs['long_name'] = 'Baker-Schepanski Sediment Supply Map'
    wnsin.attrs['units'] = 'None'
    wnsin.attrs['_FillValue'] = -999
    wnsin.attrs['valid_range'] = [0,1]
    return wnsin.where(wnsin <= 1).where(wnsin > 0)

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

def download(user,passwd,fn):
    print("==========downloading:{}==========".format(fn))
    for attempt in range(1):  # keep retrying
        try:
            a = get_nasa_data('barrybaker', 'Tallguy1', fn)
            if a is None:
                print("attempt:{}".format(attempt))
                break
            else:
                if attempt == 10:
                    print("too many attempts! for {}".format(fn))
                    break
        except:
            time.sleep(90)
            print("attempt:{}".format(attempt+1))


def _convert_hdf_tiff(basename):
    # from utility import
    hdf_file_name = basename.split(os.sep)[-1]
    tif_file_name = hdf_file_name[:-3] + "tif"
    # print(fn)
    convert_hdf_tif_modis(hdf_file_name, tif_file_name)

def _modis_hv_extent(fname):
    tile = fname.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917

def calc_omega_ns(mcd43a3,mcd43a1):
    wn = mcd43a3 / (mcd43a1 + 1e-3)
    wn = wn.where(xr.ufuncs.isfinite(wn))
    a = 0
    b = 1
    wnmax = 35.
    wnmin = 0.
    wns = (a-b)*(wn - wnmax) / (wnmin - wnmax) + b
    return wns

def _convert_hdf_tiff(basename):
    # from utility import
    hdf_file_name = basename.split(os.sep)[-1]
    tif_file_name = hdf_file_name[:-3] + "tif"
    # print(fn)
    convert_hdf_tif_modis(hdf_file_name, tif_file_name)

def _modis_hv_extent(fname):
    tile = fname.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917

def _convert_hdf_tif_vnp43ia1(hdf_file_name,tif_file_name):
    crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
    tile = hdf_file_name.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917
    transform = from_origin(ulx - res / 2, uly - res / 2, res, res)
    s = rio.open(hdf_file_name)
    if len(s.subdatasets) < 1:
        iso = s.read().squeeze()
    else:
        band = [i for i in s.subdatasets if 'BRDF_Albedo_Parameters_I1' in i][0]
        iso = rio.open(band).read()[:,:,0].squeeze()
    write_array_tif(iso, crs, transform, tif_file_name)

def _convert_hdf_tif_vnp43ia3(hdf_file_name,tif_file_name):
    crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
    tile = hdf_file_name.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917
    transform = from_origin(ulx - res / 2, uly - res / 2, res, res)
#    print(hdf_file_name)
    s =	rio.open(hdf_file_name)
#    print(s,hdf_file_name)
#    if len(s.subdatasets) < 1:
#        albedo = s.read().squeeze()
#    else:
#        print(s.subdatasets)
    band = [i for i in s.subdatasets if 'Albedo_BSA_I1' in i][0]
    bandqc = [i for i in s.subdatasets if 'BRDF_Albedo_Band_Mandatory_Quality_I1' in i][0]
    albedo = xr.open_rasterio(band).squeeze().where(xr.open_rasterio(bandqc) ==0).squeeze().data
    write_array_tif(albedo, crs, transform, tif_file_name)


def _convert_hdf_tif_mcd43a1(hdf_file_name, tif_file_name):
    crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
    tile = hdf_file_name.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917
    transform = from_origin(ulx - res / 2, uly - res / 2, res, res)
    f = xr.open_dataset(hdf_file_name).rename({'YDim:MOD_Grid_BRDF':'y','XDim:MOD_Grid_BRDF':'x','Num_Parameters:MOD_Grid_BRDF':'n'})
    iso = f.BRDF_Albedo_Parameters_Band1[:,:,0]
    #iso = iso.where(f.BRDF_Albedo_Band_Mandatory_Quality_Band1 == 0)
#    print(tif_file_name)
    write_array_tif(iso.data, crs, transform, tif_file_name)
#    print(tif_file_name)
    return tif_file_name

def _convert_hdf_tif_mcd43a3(hdf_file_name, tif_file_name):
    crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
    tile = hdf_file_name.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917
    transform = from_origin(ulx - res / 2, uly - res / 2, res, res)
    f = xr.open_dataset(hdf_file_name).rename({'YDim:MOD_Grid_BRDF':'y','XDim:MOD_Grid_BRDF':'x'})
    albedo = f.Albedo_BSA_Band1 # .where(f.BRDF_Albedo_Band_Mandatory_Quality_Band1 == 0)
    write_array_tif(albedo.data, crs, transform, tif_file_name)

def _convert_hdf_tif_mod10a2(hdf_file_name, tif_file_name):
    crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
    tile = hdf_file_name.split(".")[2]
    h = int(tile[1:3])
    v = int(tile[4:])
    ulx = -20015109.354 + h * 1111950.5196666666
    uly = -10007554.677 + (18 - v) * 1111950.5196666666
    res = 463.312716527917
    transform = from_origin(ulx - res / 2, uly - res / 2, res, res)
    f = xr.open_dataset(hdf_file_name)
    c = f.Maximum_Snow_Extent.where(f.Maximum_Snow_Extent ==25) #clear of snow
    write_array_tif(c.data, crs, transform, tif_file_name)
