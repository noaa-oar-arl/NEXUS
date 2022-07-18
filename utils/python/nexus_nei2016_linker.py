#!/usr/bin/env python

__author__ = 'Barry Baker'
__email__ = 'barry.baker@noaa.gov'
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
    return datetime.strptime(date, '%Y%m%d')


def get_nei2016_files(src_dir=None, current_month='08', sector='airport', version='v2020-07'):
    month = int(current_month)
    files = []
    for m in [-1, 0, +1]:
        files.extend(glob("{}/NEI2016v1/{}/{}/NEI2016v1_0.1x0.1_????????_{}.nc".format(src_dir, version, "%2.2i" % (month + m), sector)))
        print("{}/NEI2016v1/{}/{}/NEI2016v1_0.1x0.1_????????_{}.nc".format(src_dir, version, "%2.2i" % (month + m), sector))
    return sorted(files)


def get_nei2016_dates(files):
    dates = [datetime.strptime(os.path.basename(fname).split('_')[2], '%Y%m%d') for fname in files]
    return dates


def get_day_of_week(dates):
    days_of_week = [date.isoweekday() for date in dates]
    return days_of_week


def get_month(dates):
    return [date.month for date in dates]


def get_files_in_month(files, dates, target_date):
    months = get_month(dates)
    target_month = target_date.month
    files_in_month = [f for f, m in zip(files, months) if m == target_month]
    iwd_in_month = [d.isoweekday() for d, m in zip(dates, months) if m == target_month]
    return files_in_month, iwd_in_month


def get_num_files_per_month(files, dates):
    months = get_month(dates)
    unique_months = list(set(months))
    return min([months.count(um) for um in unique_months])


def find_closest_index(lst, val):
    return min(range(len(lst)), key=lambda i: abs(lst[i] - val))


def find_indexes(lst, val):
    return [index for index, item in enumerate(lst) if item == val]


def find_day_in_iso_week(target_date, dates, files):
    iso_week = [da.isocalendar()[1] for da in dates]
    iso_week_max = max(iso_week)
    iso_week_min = min(iso_week)
    dow = [da.isoweekday() for da in dates]
    dweek = target_date.isocalendar()[1]
    days_of_week = []
    if dweek in iso_week:
        indexs = find_indexes(iso_week, dweek)
        days_of_week = [dow[i] for i in indexs]
        dates_of_week = [dates[i] for i in indexs]
        files_of_week = [files[i] for i in indexs]
    if target_date.isoweekday() in days_of_week:
        index = days_of_week.index(target_date.isoweekday())
        return files_of_week[index]
    else:
        if (dweek + 1 > iso_week_max) & (dweek > iso_week_min):
            dweek -= 1
        else:
            dweek += 1
        indexs = find_indexes(iso_week, dweek)
        if len(indexs) == 0:
            # return closest available week
            indexs = [find_closest_index(iso_week, dweek)]
        days_of_week = [dow[i] for i in indexs]
        dates_of_week = [dates[i] for i in indexs]
        files_of_week = [files[i] for i in indexs]
        if target_date.isoweekday() in days_of_week:
            index = days_of_week.index(target_date.isoweekday())
            return files_of_week[index]
        else:
            # return closest day of the week
            index = find_closest_index(days_of_week, target_date.isoweekday())
            return files_of_week[index]


def get_closest_file(target_date, dates, files):
    # get the day of the week
    # dow = get_day_of_week(dates)

    nfiles_per_month = get_num_files_per_month(files, dates)
    if nfiles_per_month > 7:
        print('ALL DAYS HERE')
        # daily files available for the entire month
        # if target_date.day <= 7:
        #     # if it is the first week only check the first week of the dates for the appropriate day
        #     days_of_week = dow[:7]
        #     files_of_week = files[:7]
        #     index = find_closest_index(days_of_week, d.isoweekday())
        #     return files[index]
        # else:
        #     return find_day_in_iso_week(d, dates,files)
        return find_day_in_iso_week(target_date, dates, files)
    elif nfiles_per_month == 7:
        print('ONLY ONE WEEK')
        # daily files are availbale for a single month
        files_in_month, iwd_in_month = get_files_in_month(files, dates, target_date)
        index = iwd_in_month.index(target_date.isoweekday())
        return files_in_month[index]
    elif nfiles_per_month == 4:
        print('ONLY 4 DAYS')
        # a week day and friday sat and sunday are available
        # should return the files in the current month regardless
        tiwd = target_date.isoweekday()  # target date day of the week (ie sunday monday tuesday ....)
        files_in_month, iwd_in_month = get_files_in_month(files, dates, target_date)
        if tiwd == 1:
            index = iwd_in_month.index(tiwd)
            return files_in_month[index]
        elif tiwd == 6:
            index = iwd_in_month.index(tiwd)
            return files_in_month[index]
        elif tiwd == 7:
            index = iwd_in_month.index(tiwd)
            return files_in_month[index]
        else:
            index = [iwd_in_month.index(item) for item in iwd_in_month if item not in [1, 6, 7]][0]
            return files_in_month[index]
    else:
        print('SINGLE_FILE')
        # only a single file for the entire month
        target_month = target_date.month
        file_month = get_month(dates)
        index = find_closest_index(file_month, target_month)
        return files[index]


def link_file(src_file, target_file):
    if os.path.exists(target_file) and os.path.islink(target_file):
        print('File already exists/or linked:', target_file)
    else:
        os.symlink(src_file, target_file)


def create_target_name(workdir, fname, month, target_date,version='v2020-07'):
    basename = 'NEI2016v1/{}/{}/{}'.format(version, month, os.path.basename(fname))
    datestr = basename.split('_')[2]
    newname = basename.replace(datestr, target_date.strftime('%Y%m%d'))
    struct_name = '{}/{}'.format(workdir, newname)
    # make sure directories exist
    os.makedirs(workdir, exist_ok=True)
    os.makedirs(os.path.join(workdir,'NEI2016v1'), exist_ok=True)
    os.makedirs(os.path.join(workdir,'NEI2016v1',version), exist_ok=True)
    os.makedirs(os.path.join(workdir,'NEI2016v1',version,month), exist_ok=True)
    return struct_name


def get_hemco_simulation_time(file_path):
    from datetime import datetime
    from datetime import timedelta

    with open(file_path, 'r') as reader:
        lines = reader.readlines()
    for L in lines:
        if L.startswith('START'):
            start_base = datetime.strptime(L.split()[1],'%Y-%m-%d')
            start_time = datetime.strptime(L, 'START:   %Y-%m-%d %H:00:00\n')
        if L.startswith('END'):
            end_time = datetime.strptime(L, 'END:     %Y-%m-%d %H:00:00\n')
        if L.startswith('TS_EMIS'):
            ts_emis = float(L.split()[1].strip('\n'))
        # skip the first three comment lines
    dates = []
    currtime = start_time
    print(currtime, end_time)
    while currtime <= end_time:
        print(currtime)
        dates.append(currtime)
        currtime = currtime + timedelta(days=1)
    return dates


if __name__ == '__main__':

    parser = ArgumentParser(description='Modify the start and end date of the NEXUS config script', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-s', '--src_dir', help='Source Directory to Emission files', type=str, required=True)
    parser.add_argument('-d', '--date', help='date for file: format %Y-%m-%d', required=False)
    parser.add_argument('-w', '--work_dir', help='work directory in the workflow', required=False)
    parser.add_argument('-t', '--read_hemco_time', help='Read HEMCO time file', default=True, required=False)
    parser.add_argument('-tf', '--time_file_path', help='Location of the HEMCO Time File', default=None, required=False)
    parser.add_argument('-v', '--nei_version', help='NEI VERSION', default='v2020-07', required=False)
    args = parser.parse_args()

    src_dir = args.src_dir
    work_dir = args.work_dir
    version = args.nei_version
    d = datetime.strptime(args.date, '%Y%m%d')

    # ensure directory exists
    if args.read_hemco_time:
        if args.time_file_path is None:
            hemco_time_file = os.path.join(args.work_dir, '../HEMCO_sa_Time.rc')
        else:
            hemco_time_file = args.time_file_path
        dates = get_hemco_simulation_time(hemco_time_file)

    for d in dates:
        month = d.strftime('%m')
        
        all_files = glob('{}/NEI2016v1/{}/{}/*.nc'.format(src_dir, version, month))
        sectors = sorted(list(set([os.path.basename(i)[27:][:-3] for i in all_files])))
        print(sectors)
        for i in sectors:
            print(i, month, src_dir)
            if (i == 'ptfire') | (i == 'ptagfire'):
                pass
            else:
                files = get_nei2016_files(src_dir=src_dir, current_month=month, sector=i,version=version)
                dates = get_nei2016_dates(files)
                fname = get_closest_file(d, dates, files)
                target_name = create_target_name(work_dir, fname, month, d, version=version)
                print(fname, target_name)
                link_file(fname, target_name)
