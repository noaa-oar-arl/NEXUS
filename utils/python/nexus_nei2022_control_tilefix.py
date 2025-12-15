#!/usr/bin/env python
"""
Simple utility to modify the NEXUS_Config.rc ROOT emission path.
"""

__author__ = "Barry Baker"
__email__ = "barry.baker@noaa.gov"
__license__ = "GPL"


def get_start_time(fname):
    """Function to read the start date from the HEMCO_sa_Time.rc"""
    from datetime import datetime

    with open(fname) as f:
        lines = f.readlines()
    for line in lines:
        if "START" in line:
            date = datetime.strptime(line, "START:   %Y-%m-%d %H:00:00\n")
            break
    else:
        raise ValueError("Could not find the start date/time in the HEMCO_sa_Time.rc")

    return date


if __name__ == "__main__":
    from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

    parser = ArgumentParser(
        description="Modify the start and end date of the NEXUS config script",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-f",
        "--files",
        help="input NEXUS_Config.rc file name",
        type=str,
        default="NEXUS_Config.rc",
        required=False,
    )
    parser.add_argument(
        "-t",
        "--time_file",
        help="HEMCO Time File: HEMCO_sa_Time.rc",
        default="HEMCO_sa_Time.rc",
        required=False,
    )

    args = parser.parse_args()

    # get input NEXUS_Config.rc
    finput = args.files
    d = get_start_time(args.time_file)

    with open(finput) as f:
        lines = f.readlines()
        for index, line in enumerate(lines):
            if not line.startswith("#"):
                if "$ROOT/" in line:
                    if "NEI2019" in line:
                        if "2019$MM$DD" in line:
                            line = line.replace("2019$MM$DD", d.strftime("%Y$MM$DD"))
                            lines[index] = line
        f.close()

    with open(finput, "w") as writer:
        writer.writelines(lines)
