#!/usr/bin/env python

__author__  = 'Barry Baker'
__email__   = 'barry.baker@noaa.gov'
__license__ = 'GPL'

'''
Simple utility to modify the HEMCO_sa_Time.rc file.
'''

import os
from glob import glob
import sys
import subprocess
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


def fix_time_string(input_str,date_str, cycle, start=False):
    s = input_str.split()
    if start:
        s[1] = '  ' + date_str
    else:
        s[1] = '    ' + date_str
    #s[1] = date_str
    s[2] = ':'.join(map(str, [cycle,'00','00']))
    return ' '.join(map(str, s)) + '\n'

if __name__ == '__main__':

    parser = ArgumentParser(description='Modify the start and end date of the NEXUS config script', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-f', '--files', help='input nemsio file name', type=str, required=True)
    parser.add_argument('-s', '--start_date', help='starting date: format %Y-%m-%d', required=True)
    parser.add_argument('-e', '--end_date', help='ending date: format %Y-%m-%d', required=True)
    parser.add_argument('-c', '--cycle', help='cycle: format %H',default = '00', required=False)
    parser.add_argument('-t', '--time_step', help='time step in seconds', default='3600', required=False)
    args = parser.parse_args()

    finput = args.files
    sdate = args.start_date
    edate = args.end_date
    cycle = args.cycle
    tstep = args.time_step

    cycle_str = ':'.join(map(str, [cycle,'00','00']))

    with open(finput,'r') as f:
        lines = f.readlines()
        start_date_line = lines[2]
        end_date_line = lines[3]
        time_step_line = lines[4]
        lines[2] = fix_time_string(start_date_line, sdate,cycle, start=True)
        lines[3] = fix_time_string(end_date_line, edate,cycle)
        lines[4] = 'TS_EMIS: ' + tstep
        
    with open(finput,'w') as writer:
        writer.writelines(lines)
