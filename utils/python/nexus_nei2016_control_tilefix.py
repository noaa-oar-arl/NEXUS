#!/usr/bin/env python

__author__  = 'Barry Baker'
__email__   = 'barry.baker@noaa.gov'
__license__ = 'GPL'

'''
Simple utility to modify the NEXUS_Config.rc ROOT emission path.
'''

import os
from glob import glob
import sys
import subprocess
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from datetime import datetime


if __name__ == '__main__':

    parser = ArgumentParser(description='Modify the start and end date of the NEXUS config script', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-f', '--files', help='input nemsio file name', type=str, required=True)
    parser.add_argument('-d', '--date', help='Date of model run. Format: %Y%m%d     example: 20190810', default='./', required=True)
    
    args = parser.parse_args()

    finput = args.files
    d = datetime.strptime(args.date,'%Y%m%d')

    with open(finput,'r') as f:
        lines = f.readlines()
        for index,line in enumerate(lines):
            if not line.startswith('#'):
                if '$ROOT/' in line:
                    if 'NEI2016' in line:
                        if '2016$MM$DD' in line:
                            line = line.replace('2016$MM$DD', d.strftime('%Y$MM$DD'))
                            lines[index] = line
        f.close()
    
    with open(finput, 'w') as writer:
        writer.writelines(lines)
