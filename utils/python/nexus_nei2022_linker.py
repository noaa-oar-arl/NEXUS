#!/usr/bin/env python
"""
Simple utility to link the appropriate NEI2019 date for the workflow.
"""

import logging
import os
import sys
from datetime import datetime, timedelta
from glob import glob


def setup_logger(log_level=logging.INFO):
    """Configure logging for the application.

    Sets up a logger that outputs to the terminal with formatting.

    Parameters
    ----------
    log_level : int, optional
        The logging level to use, by default logging.INFO

    Returns
    -------
    logging.Logger
        Configured logger instance
    """
    logger = logging.getLogger("nexus_nei2019_linker")
    logger.setLevel(log_level)

    # Create console handler with formatting
    handler = logging.StreamHandler()
    formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)

    return logger


logger = setup_logger()


def get_hemco_simulation_time(file_path):
    """Extract simulation dates from HEMCO configuration file.

    Parameters
    ----------
    file_path : str
        Path to the HEMCO configuration file

    Returns
    -------
    list of datetime.datetime
        List of dates in the simulation period

    Raises
    ------
    FileNotFoundError
        If the HEMCO time file doesn't exist
    ValueError
        If required time information cannot be extracted
    Exception
        For any other errors during processing
    """
    if not os.path.exists(file_path):
        logger.error(f"HEMCO time file not found: {file_path}")
        raise FileNotFoundError(f"Cannot find HEMCO time file: {file_path}")

    try:
        with open(file_path) as reader:
            lines = reader.readlines()

        start_time = None
        end_time = None

        for L in lines:
            if L.startswith("START"):
                start_base = datetime.strptime(L.split()[1], "%Y-%m-%d")  # noqa: F841
                start_time = datetime.strptime(L, "START:   %Y-%m-%d %H:00:00\n")
            if L.startswith("END"):
                end_time = datetime.strptime(L, "END:     %Y-%m-%d %H:00:00\n")
            if L.startswith("TS_EMIS"):
                ts_emis = float(L.split()[1].strip("\n"))  # noqa: F841

        if start_time is None or end_time is None:
            logger.error(f"Failed to parse START or END times in {file_path}")
            raise ValueError(f"Could not extract required time information from {file_path}")

        dates = []
        currtime = start_time
        logger.info(f"Simulation period: {currtime} to {end_time}")

        while currtime <= end_time:
            logger.debug(f"Adding date: {currtime}")
            dates.append(currtime)
            currtime = currtime + timedelta(days=1)

        if not dates:
            logger.warning("No dates found in the simulation period")

        return dates

    except Exception as e:
        logger.error(f"Error processing HEMCO time file: {e}")
        raise


def get_file_map(src_dir, version):
    """Create a mapping of month and day-of-week to NEI2019 file paths.

    Maps each month and day-of-week to the appropriate NEI2019 data file.
    For days without data, fills in using a prioritized approach.

    Parameters
    ----------
    src_dir : str
        Source directory containing NEI2019 data files
    version : str
        Version of NEI2019 data (e.g., "v2023-03")

    Returns
    -------
    dict
        Dictionary mapping (month, isoweekday) tuples to (date, filepath) tuples

    Raises
    ------
    FileNotFoundError
        If no NEI2019 files are found
    Exception
        For any other errors during processing
    """
    try:
        search_pattern = f"{src_dir}/NEMO/NEI2019/{version}/??/NEI2019*_all.nc"
        logger.info(f"Searching for files with pattern: {search_pattern}")

        files = [fp for fp in glob(search_pattern) if not os.path.islink(fp)]

        if not files:
            logger.error(f"No files found matching pattern: {search_pattern}")
            raise FileNotFoundError(f"No NEI2019 files found in {src_dir} for version {version}")

        logger.info(f"Found {len(files)} NEI2019 files")

        file_map = {}
        for fp in files:
            try:
                sd = os.path.basename(fp).split("_")[-2]
                d = datetime.strptime(sd, r"%Y%m%d")
                key = (d.month, d.isoweekday())
                if key in file_map:
                    logger.warning(f"Duplicate files for key {key}: {file_map[key][1]}, {fp}")
                file_map[key] = (d, fp)
            except (ValueError, IndexError) as e:
                logger.warning(f"Could not process filename {fp}: {str(e)}")

        # Create a more robust mapping by filling in missing days
        all_months = {month for month, _ in file_map.keys()}
        logger.info(f"Found data for {len(all_months)} months")

        for mo in all_months:
            # Check which day types are available for this month
            available_days = {iwd for (month, iwd) in file_map if month == mo}
            logger.debug(f"Month {mo} has data for days: {available_days}")

            # First handle weekdays (1-5)
            weekday_map = {}
            for iwd in range(1, 6):  # Mon-Fri
                if iwd in available_days:
                    weekday_map[iwd] = file_map[(mo, iwd)]

            # If we have some weekdays but are missing some, fill them in
            if weekday_map and len(weekday_map) < 5:
                # Priority: Tue > Mon > Wed > Thu > Fri
                priority_order = [2, 1, 3, 4, 5]
                fill_source = None
                for day in priority_order:
                    if day in weekday_map:
                        fill_source = day
                        break

                if fill_source:
                    for iwd in range(1, 6):
                        if iwd not in weekday_map:
                            file_map[(mo, iwd)] = weekday_map[fill_source]
                            logger.info(f"Using day {fill_source} data for month {mo}, day {iwd}")

            # Handle weekend days (6-7)
            weekend_map = {}
            for iwd in range(6, 8):  # Sat-Sun
                if iwd in available_days:
                    weekend_map[iwd] = file_map[(mo, iwd)]

            # If we have one weekend day but not both, use the available one
            if len(weekend_map) == 1:
                available_weekend = list(weekend_map.keys())[0]
                missing_weekend = 13 - available_weekend  # 13-6=7, 13-7=6
                file_map[(mo, missing_weekend)] = weekend_map[available_weekend]
                logger.info(
                    f"Using day {available_weekend} data for month {mo}, day {missing_weekend}"
                )

            # If we have no weekend days but have weekdays, use a weekday
            elif len(weekend_map) == 0 and weekday_map:
                # Use Tuesday (or first available weekday) for weekend
                fill_day = next((d for d in [2, 1, 3, 4, 5] if d in weekday_map), None)
                if fill_day:
                    for iwd in [6, 7]:
                        file_map[(mo, iwd)] = weekday_map[fill_day]
                        logger.info(f"Using weekday {fill_day} data for month {mo}, weekend {iwd}")

        logger.info(f"Created file mapping with {len(file_map)} entries")
        return file_map

    except Exception as e:
        logger.error(f"Error building file map: {e}")
        raise


def link_file(src_file, tgt_file):
    """Create a symbolic link, with error handling.

    Creates a symbolic link from source to target file with proper error handling.
    If the target already exists, it will be removed before creating the new link.

    Parameters
    ----------
    src_file : str
        Source file path to link from
    tgt_file : str
        Target file path to link to

    Raises
    ------
    FileNotFoundError
        If the source file does not exist
    Exception
        For any errors during link creation
    """
    try:
        if not os.path.exists(src_file):
            logger.error(f"Source file does not exist: {src_file}")
            raise FileNotFoundError(f"Source file not found: {src_file}")

        if os.path.exists(tgt_file):
            if os.path.islink(tgt_file):
                current_link = os.readlink(tgt_file)
                if current_link == src_file:
                    logger.info(f"Link already exists and points to correct source: {tgt_file}")
                    return
                else:
                    logger.warning(
                        f"Target link exists but points to different source: {tgt_file} -> {current_link}"
                    )
                    os.remove(tgt_file)
            else:
                logger.warning(f"Target exists but is not a link, removing: {tgt_file}")
                os.remove(tgt_file)

        # Use os.path.abspath to ensure we have absolute paths for both source and target
        src_abs = os.path.abspath(src_file)

        # Create the symlink
        os.symlink(src_abs, tgt_file)
        logger.info(f"Created link: {tgt_file} -> {src_abs}")

    except FileExistsError:
        # This can happen if the file is created between our check and actual symlink creation
        logger.warning(f"File appeared during linking, removing and retrying: {tgt_file}")
        try:
            os.remove(tgt_file)
            os.symlink(os.path.abspath(src_file), tgt_file)
            logger.info(f"Created link on second attempt: {tgt_file} -> {src_file}")
        except Exception as e:
            logger.error(f"Failed to create link on second attempt: {e}")
            raise
    except Exception as e:
        logger.error(f"Failed to create link {tgt_file}: {e}")
        raise


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
    parser.add_argument(
        "--debug",
        help="Enable debug logging",
        action="store_true",
        default=False,
    )
    args = parser.parse_args()

    # Configure logging level based on arguments
    if args.debug:
        logger = setup_logger(logging.DEBUG)
        logger.debug("Debug logging enabled")

    try:
        src_dir = args.src_dir.rstrip("/")
        work_dir = args.work_dir.rstrip("/")
        version = args.nei_version

        logger.info(
            f"Starting NEI2019 linker with src_dir={src_dir}, work_dir={work_dir}, version={version}"
        )

        if not os.path.isdir(src_dir):
            logger.error(f"Source directory does not exist: {src_dir}")
            sys.exit(2)

        if not os.path.isdir(work_dir):
            logger.error(f"Work directory does not exist: {work_dir}")
            sys.exit(2)

        # Get dates for processing
        if args.read_hemco_time:
            if args.time_file_path is None:
                hemco_time_file = os.path.join(args.work_dir, "../HEMCO_sa_Time.rc")
            else:
                hemco_time_file = args.time_file_path
            logger.info(f"Reading simulation time from: {hemco_time_file}")
            dates = get_hemco_simulation_time(hemco_time_file)
        elif args.date is not None:
            try:
                d = datetime.strptime(args.date.replace("-", ""), r"%Y%m%d")
                dates = [d]
                logger.info(f"Using single date: {d.strftime('%Y-%m-%d')}")
            except ValueError:
                logger.error(f"Invalid date format: {args.date}")
                sys.exit(2)
        else:
            logger.error("No date information provided. Use --date or --read-hemco-time")
            sys.exit(2)

        # Get file mapping
        file_map = get_file_map(src_dir, version)
        logger.info(f"File map contains {len(file_map)} entries")

        if not file_map:
            logger.error(f"No files found or mapped in {src_dir} for version {version}")
            sys.exit(1)

        # Process each date
        for d in dates:
            mo = d.month
            iwd = d.isoweekday()

            logger.info(
                f"Processing date: {d.strftime('%Y-%m-%d')}, month: {mo}, isoweekday: {iwd}"
            )

            if (mo, iwd) not in file_map:
                logger.error(f"No source file found for month {mo}, day {iwd}")
                sys.exit(1)

            src_d, src_fp = file_map[(mo, iwd)]

            # Form target file path, maintaining the full relative path structure
            src_rel_dir = os.path.dirname(os.path.relpath(src_fp, src_dir))
            tgt_fn = os.path.basename(src_fp).replace(
                src_d.strftime(r"%Y%m%d"), d.strftime(r"%Y%m%d")
            )
            tgt_fp = os.path.join(work_dir, src_rel_dir, tgt_fn)

            try:
                # Create the directory structure if it doesn't exist
                target_dir = os.path.dirname(tgt_fp)
                if not os.path.exists(target_dir):
                    logger.debug(f"Creating directory structure: {target_dir}")
                    os.makedirs(target_dir, exist_ok=True)

                logger.info(f"Linking {src_fp} to {tgt_fp}")
                link_file(src_fp, tgt_fp)
            except Exception as e:
                logger.error(f"Failed to create link for date {d.strftime('%Y-%m-%d')}: {e}")
                sys.exit(1)

        logger.info("NEI2019 linking completed successfully")

    except Exception as e:
        logger.error(f"An error occurred during execution: {e}")
        logger.debug("Error details:", exc_info=True)
        sys.exit(1)
