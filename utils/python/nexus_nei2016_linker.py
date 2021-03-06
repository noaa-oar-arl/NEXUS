#!/usr/bin/env python

__author__  = 'Barry Baker'
__email__   = 'barry.baker@noaa.gov'
__license__ = 'GPL'

'''
Simple utility to link the appropriate date for the workflow. 
Looks for the nearest day of the week compared to 2016 date of the 
NEI2016 dataset and links it.
'''

import os
from glob import glob
import sys
import subprocess
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


def get_date_yyyymmdd(date=None):
    return datetime.strptime(date,'%Y%m%d')

def get_nei2016_files(src_dir=None,current_month='08',sector='airport'):
#    fullpath = "{}/NEI2016v1/v2020-07/{}/NEI2016v1_0.1x0.1_????????_{}.nc".format(src_dir,current_month,sector)
 #   print(fullpath)
    files = glob("{}/NEI2016v1/v2020-07/{}/NEI2016v1_0.1x0.1_????????_{}.nc".format(src_dir,current_month,sector))
    return sorted(files)

def get_nei2016_dates(files):
    dates = [datetime.strptime(os.path.basename(fname).split('_')[2], '%Y%m%d') for fname in files]
    return dates

def get_day_of_week(dates):
    days_of_week = [date.isoweekday() for date in dates]
    return days_of_week

def get_closest_file(target_date,dates,files):
    # get the day of the week
    dow = get_day_of_week(dates)
    tiwd = target_date.isoweekday() # target date day of the week (ie sunday monday tuesday ....) 
    if len(files) > 7:
        print('ALL DAYS HERE')
        #daily files available for the entire month
        if target_date.day <= 7: 
            # if it is the first week only check the first week of the dates for the appropriate day
            days_of_week = dow[:7]
            files_of_week = files[:7]
            index,_ = min(enumerate(days_of_week), key=lambda x: abs(x[1]-d.isoweekday()))
            return files[index]
        else: 
            return find_day_in_iso_week(d, dates,files)
    elif len(files) == 7:
        print('ONLY ONE WEEK')
        #daily files are availbale for a single month
        index = dow.index(target_date.isoweekday())
        return files[index]
    elif len(files) == 4:
        print('ONLY 4 DAYS')
        # a week day and friday sat and sunday are available
        if tiwd == 1:
            return files[0]
        elif tiwd == 6:
            return files[-2]
        elif tiwd == 7:
            return files[-1]
        else:
            return files[1]
    else:
        print('SINGLE_FILE')
        # only a single file for the entire month
        return files[0]


def link_file(src_file,target_file):
    if os.path.exists(target_file) and os.path.islink(target_file):
        print('File already exists/or linked:',target_file)
    else:
        os.symlink(src_file, target_file)

def find_day_in_iso_week(target_date, dates, files):
    iso_week = [ da.isocalendar()[1] for da in dates ]
    iso_week_max = max(iso_week)
    iso_week_min = min(iso_week)
    dow = [ da.isoweekday() for da in dates ]
    dweek = d.isocalendar()[1]
#    print(dweek in iso_week, d.isocalendar()[1])
    if (dweek in iso_week):
        indexs = [ index for index,val in enumerate(iso_week) if val == dweek ]
#        print(indexs)
        days_of_week = [ dow[i]    for i in indexs]
        dates_of_week = [dates[i] for i in indexs]
        files_of_week = [files[i] for i in indexs]
#        print(days_of_week)
#        print(dates_of_week)
    if target_date.isoweekday() in days_of_week:
#        print('here')
        index = days_of_week.index(target_date.isoweekday())
        return files_of_week[index]
    else:
#        print('here2')
        if (dweek + 1 > iso_week_max) & (dweek > iso_week_min):
            dweek -= 1
        else:
            dweek += 1
        indexs = [ index for index,val in enumerate(iso_week) if val == dweek ]
        days_of_week = [ dow[i]    for i in indexs]
        dates_of_week = [dates[i] for i in indexs]
        files_of_week =    [files[i] for i    in indexs]
        if target_date.isoweekday() in days_of_week:
            index = days_of_week.index(target_date.isoweekday())
            return files_of_week[index]

def create_target_name(workdir,fname,month,target_date):
    basename = 'NEI2016v1/v2020-07/{}/{}'.format(month,os.path.basename(fname))
    datestr = basename.split('_')[2]
    newname = basename.replace(datestr, target_date.strftime('%Y%m%d'))
    struct_name = '{}/{}'.format(workdir,newname)
    return struct_name


if __name__ == '__main__':

     parser = ArgumentParser(description='Modify the start and end date of the NEXUS config script', formatter_class=ArgumentDefaultsHelpFormatter)
     parser.add_argument('-s', '--src_dir', help='Source Directory to Emission files', type=str, required=True)
     parser.add_argument('-d', '--date', help='date for file: format %Y-%m-%d', required=True)
     parser.add_argument('-w', '--work_dir', help='work directory in the workflow', required=False)
     args = parser.parse_args()

     src_dir = args.src_dir
     d = datetime.strptime(args.date, '%Y%m%d')
     work_dir = args.work_dir
     
     month = d.strftime('%m')

     all_files = glob('{}/NEI2016v1/v2020-07/{}/*.nc'.format(src_dir,month))
     sectors = sorted(list(set([ os.path.basename(i)[27:][:-3] for i in all_files])))
     for i in sectors:
         print(i,month,src_dir)
         if (i == 'ptfire') | (i == 'ptagfire'):
             pass
         else:
             files = get_nei2016_files(src_dir=src_dir, current_month=month, sector=i)
             dates = get_nei2016_dates(files)
             fname = get_closest_file(d,dates,files)
             target_name = create_target_name(work_dir,fname,month,d)
             print(fname,target_name)
             link_file(fname, target_name)
