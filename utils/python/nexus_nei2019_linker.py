#!/usr/bin/env python
"""
Simple utility to link the appropriate NEI2019 date for the workflow.
"""

import os
from datetime import datetime, timedelta
from glob import glob


def get_hemco_simulation_time(file_path):
    with open(file_path) as reader:
        lines = reader.readlines()
    for L in lines:
        if L.startswith("START"):
            start_base = datetime.strptime(L.split()[1], "%Y-%m-%d")  # noqa: F841
            start_time = datetime.strptime(L, "START:   %Y-%m-%d %H:00:00\n")
        if L.startswith("END"):
            end_time = datetime.strptime(L, "END:     %Y-%m-%d %H:00:00\n")
        if L.startswith("TS_EMIS"):
            ts_emis = float(L.split()[1].strip("\n"))  # noqa: F841
        # skip the first three comment lines
    dates = []
    currtime = start_time
    print(currtime, end_time)
    while currtime <= end_time:
        print(currtime)
        dates.append(currtime)
        currtime = currtime + timedelta(days=1)
    return dates


def get_file_map(src_dir, version):
    """Mapping of month and day-of-week to the date and combined file path."""
    files = [
        fp
        for fp in glob(f"{src_dir}/NEMO/NEI2019/{version}/??/NEI2019*_all.nc")
        if not os.path.islink(fp)
    ]
    file_map = {}
    for fp in files:
        sd = os.path.basename(fp).split("_")[-2]
        d = datetime.strptime(sd, r"%Y%m%d")
        key = (d.month, d.isoweekday())
        if key in file_map:
            print(f"warning: possible duplicate files for key {key}: {file_map[key]}, {fp}")
        file_map[key] = (d, fp)

    # The files are Mon, Tue, Sat, Sun
    # Use Tue for Wed--Fri
    for mo in range(1, 13):
        tue = file_map.get((mo, 2))
        for iwd in [3, 4, 5]:  # Wed, Thu, Fri
            key = (mo, iwd)
            if key in file_map:
                print(f"warning: unexpected existing file for key {key}: {file_map[key]}")
            file_map[key] = tue

    return file_map


def link_file(src_file, tgt_file):
    if os.path.exists(tgt_file) and os.path.islink(tgt_file):
        print("target already linked:", tgt_file)
    else:
        os.symlink(src_file, tgt_file)


if __name__ == "__main__":
    from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

    parser = ArgumentParser(
        description="Link NEI2019 files to the work directory",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-s",
        "--src_dir",
        "--src-dir",
        help=(
            "Source Directory to Emission files "
            "e.g., /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/emissions/nexus on Hera."
        ),
        type=str,
        required=True,
    )
    parser.add_argument(
        "-w",
        "--work_dir",
        "--work-dir",
        help="work directory in the workflow",
        required=True,
    )
    parser.add_argument(
        "-d",
        "--date",
        help=r"date for file: format YYYYMMDD or YYYY-MM-DD",
        required=False,
    )
    parser.add_argument(
        "-t",
        "--read_hemco_time",
        "--read-hemco-time",
        help="Read HEMCO time file",
        action="store_true",
        default=True,
        required=False,
    )
    parser.add_argument(
        "--no_read_hemco_time",
        "--no-read-hemco-time",
        action="store_false",
        dest="read_hemco_time",
    )
    parser.add_argument(
        "-tf",
        "--time_file_path",
        "--time-file-path",
        help="Location of the HEMCO Time File",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-v",
        "--nei_version",
        "--nei-version",
        help="NEI VERSION",
        default="v2023-03",
        required=False,
    )
    args = parser.parse_args()

    src_dir = args.src_dir.rstrip("/")
    work_dir = args.work_dir.rstrip("/")
    version = args.nei_version

    if not os.path.isdir(work_dir):
        print(f"error: work dir setting {work_dir!r} does not exist or is not a directory")
        raise SystemExit(2)

    if args.read_hemco_time:
        if args.time_file_path is None:
            hemco_time_file = os.path.join(args.work_dir, "../HEMCO_sa_Time.rc")
        else:
            hemco_time_file = args.time_file_path
        dates = get_hemco_simulation_time(hemco_time_file)
    elif args.date is not None:
        d = datetime.strptime(args.date.replace("-", ""), r"%Y%m%d")
        dates = [d]
    else:
        print("error: date info not specified")
        raise SystemExit(2)

    file_map = get_file_map(src_dir, version)
    print("file map size:", len(file_map))
    if not file_map:
        print(f"error: no files found in {src_dir} for version {version!r}")
        raise SystemExit(1)

    print("src dir:", src_dir)
    print("work dir:", work_dir)
    for d in dates:
        mo = d.month
        iwd = d.isoweekday()

        print(f"date: {d.strftime('%Y-%m-%d')}, month: {mo}, isoweekday: {iwd}")
        src = file_map.get((mo, iwd))
        if src is None:
            print(f"error: no file found for month {mo}, iwd {iwd}")
            raise SystemExit(1)
        src_d, src_fp = src

        # Form target file path, with same relative directory structure but updated date in file name
        tgt_fn = os.path.basename(src_fp).replace(src_d.strftime(r"%Y%m%d"), d.strftime(r"%Y%m%d"))
        tgt_fp = os.path.join(
            work_dir,
            os.path.dirname(os.path.relpath(src_fp, src_dir)),
            tgt_fn,
        )
        os.makedirs(os.path.dirname(tgt_fp), exist_ok=True)

        print("linking", src_fp, "to", tgt_fp)
        link_file(src_fp, tgt_fp)
