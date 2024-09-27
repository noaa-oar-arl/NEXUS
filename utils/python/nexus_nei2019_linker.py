import os
from datetime import datetime, timedelta
from glob import glob


if __name__ == "__main__":
    from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

    parser = ArgumentParser(
        description="Link NEI2019 files to the work directory",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-s",
        "--src_dir",
        help=(
            "Source Directory to Emission files "
            "e.g., /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/emissions/nexus/NEMO on Hera."
        ),
        type=str,
        required=True,
    )
    parser.add_argument(
        "-d",
        "--date",
        help=r"date for file: format %Y-%m-%d",
        required=False,
    )
    parser.add_argument(
        "-w",
        "--work_dir",
        help="work directory in the workflow",
        required=True,
    )
    parser.add_argument(
        "-t",
        "--read_hemco_time",
        help="Read HEMCO time file",
        default=True,
        required=False,
    )
    parser.add_argument(
        "-tf",
        "--time_file_path",
        help="Location of the HEMCO Time File",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-v",
        "--nei_version",
        help="NEI VERSION",
        default="v2023-03",
        required=False,
    )
    args = parser.parse_args()

    src_dir = args.src_dir
    work_dir = args.work_dir
    version = args.nei_version
    d = datetime.strptime(args.date, r"%Y-%m-%d")

    # ensure directory exists
    if args.read_hemco_time:
        if args.time_file_path is None:
            hemco_time_file = os.path.join(args.work_dir, "../HEMCO_sa_Time.rc")
        else:
            hemco_time_file = args.time_file_path
        dates = get_hemco_simulation_time(hemco_time_file)
    else:
        dates = [d]

    for d in dates:
        month = d.strftime("%m")

        all_files = glob(f"{src_dir}/NEI2019/{version}/{month}/*.nc")

        print(d, month)
        files = get_nei2019_files(src_dir=src_dir, current_month=month, version=version)
        dates = get_nei2019_dates(files)
        fname = get_closest_file(d, dates, files)
        target_name = create_target_name(work_dir, fname, month, d, version=version)
        print(fname, target_name)
        link_file(fname, target_name)
