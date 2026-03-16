#!/usr/bin/env python
"""
Simple utility to link the appropriate NEI2022 date for the workflow.
"""

import logging
import os
import re
from collections import Counter
from datetime import datetime, timedelta
from glob import glob

HOLIDAY_MD = {
    #   : "0101 0102 XXXX XXXX XXXX XXXX 0704 0705 XXXX XXXX XXXX XXXX XXXX 1224 1225 1226".split(),
    2000: "0101 0102 0421 0422 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2001: "0101 0102 0413 0414 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2002: "0101 0102 0329 0330 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2003: "0101 0102 0418 0419 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2004: "0101 0102 0409 0410 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2005: "0101 0102 0325 0326 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2006: "0101 0102 0414 0415 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2007: "0101 0102 0406 0407 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2008: "0101 0102 0321 0322 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2009: "0101 0102 0410 0411 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2010: "0101 0102 0402 0403 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2011: "0101 0102 0422 0423 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2012: "0101 0102 0406 0407 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2013: "0101 0102 0329 0330 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2014: "0101 0102 0418 0419 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2015: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2016: "0101 0102 0325 0326 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2017: "0101 0102 0414 0415 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2018: "0101 0102 0330 0331 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2019: "0101 0102 0419 0420 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2020: "0101 0102 0410 0411 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2021: "0101 0102 0402 0403 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2022: "0101 0102 0415 0416 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2023: "0101 0102 0407 0408 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2024: "0101 0102 0329 0330 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2025: "0101 0102 0418 0419 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2026: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2027: "0101 0102 0326 0327 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2028: "0101 0102 0414 0415 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2029: "0101 0102 0330 0331 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2030: "0101 0102 0419 0420 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2031: "0101 0102 0411 0412 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2032: "0101 0102 0326 0327 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2033: "0101 0102 0415 0416 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2034: "0101 0102 0407 0408 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2035: "0101 0102 0323 0324 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2036: "0101 0102 0411 0412 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2037: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2038: "0101 0102 0423 0424 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2039: "0101 0102 0408 0409 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2040: "0101 0102 0330 0331 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2041: "0101 0102 0419 0420 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2042: "0101 0102 0404 0405 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2043: "0101 0102 0327 0328 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2044: "0101 0102 0415 0416 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2045: "0101 0102 0407 0408 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2046: "0101 0102 0323 0324 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2047: "0101 0102 0412 0413 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2048: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2049: "0101 0102 0416 0417 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2050: "0101 0102 0408 0409 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
}
# Holidays + day after:
# - New Year's Day
# - Good Friday
# - Memorial Day
# - Independence Day
# - Labor Day
# Holidays + day before and after:
# - Thanksgiving
# - Xmas

# Location of MetEmis files for sector under base
# Note currently there is only one MetEmis setup
# (don't need to support multiple versions)
METEMIS_SUBDIR = {
    "onroad": "METEMIS/onroad/2021",
    "livestock": "METEMIS/livestock/2022",
    "rwc": "METEMIS/rwc/2022",
    "afdust": "METEMIS/afdust/2022",
}

METEMIS_NUMS = {
    "onroad": list(range(10)),
    "livestock": list(range(12)),
    "rwc": None,
    "afdust": None,
}


def is_holiday(date):
    """Is this a date that we treat as a holiday?"""
    md = date.strftime(r"%m%d")
    return md in HOLIDAY_MD[date.year]


def dayofyear(date):
    """Day-of-year (int)."""
    return int(date.strftime(r"%j"))


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
    logger = logging.getLogger("nexus_nei2022_linker")
    logger.setLevel(log_level)

    # Clear existing handlers to avoid duplicates
    logger.handlers.clear()

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
        Path to the HEMCO configuration file.

    Returns
    -------
    list of datetime.datetime
        List of dates in the simulation period.

    Raises
    ------
    FileNotFoundError
        If the HEMCO time file doesn't exist.
    ValueError
        If required time information cannot be extracted.
        If the START time is not before the END time.
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"HEMCO time file path does not exist: {file_path}")

    with open(file_path) as reader:
        lines = reader.readlines()

    start_time = None
    end_time = None

    for line in lines:
        line = line.strip()
        if line.startswith("START"):
            try:
                # Note strptime single space also matches multiple spaces
                start_time = datetime.strptime(line, r"START: %Y-%m-%d %H:%M:%S")
            except ValueError as e:
                raise ValueError(f"Invalid START line in {file_path}: {line.strip()}") from e
        elif line.startswith("END"):
            try:
                end_time = datetime.strptime(line, r"END: %Y-%m-%d %H:%M:%S")
            except ValueError as e:
                raise ValueError(f"Invalid END line in {file_path}: {line.strip()}") from e

    if start_time is None or end_time is None:
        raise ValueError(f"Could not extract START and END times from {file_path}")

    if start_time >= end_time:
        raise ValueError(f"START time {start_time} must be before END time {end_time}")

    logger.info(f"Simulation period: {start_time} to {end_time}")

    dates = []
    if start_time.date() == end_time.date():
        logger.debug(f"Single-day simulation, adding date: {start_time}")
        dates.append(start_time)
    else:
        curr_time = start_time
        while curr_time.date() < end_time.date():
            logger.debug(f"Adding date: {curr_time}")
            dates.append(curr_time)
            curr_time = curr_time + timedelta(days=1)
        logger.debug(f"Adding date: {end_time}")
        dates.append(end_time)

    if not dates:
        logger.warning("No dates found in the simulation period")

    return dates


def link_file(src_file, tgt_file):
    """Create a symbolic link.

    If the target already exists, it will be removed before creating the new link.

    Parameters
    ----------
    src_file : str
        Source file path.
    tgt_file : str
        Target file path (the link).

    Raises
    ------
    FileNotFoundError
        If the source file does not exist.
    """
    if not os.path.exists(src_file):
        raise FileNotFoundError(f"Source file does not exist: {src_file}")

    src_abs = os.path.abspath(src_file)

    if os.path.lexists(tgt_file):
        if os.path.islink(tgt_file):
            current_link = os.readlink(tgt_file)
            current_abs = os.path.abspath(os.path.join(os.path.dirname(tgt_file), current_link))
            if current_abs == src_abs:
                logger.info(f"Link already exists and points to correct source: {tgt_file}")
                return
            logger.info(f"Replacing existing link: {tgt_file} -> {current_link}")
        else:
            logger.info(f"Replacing existing file: {tgt_file}")
        os.remove(tgt_file)

    os.symlink(src_abs, tgt_file)
    logger.info(f"Created link: {tgt_file} -> {src_abs}")


class FileMatcher:
    def __init__(self, fps):
        self.fps = sorted(fps)

    def dates(self):
        """Parse dates (:class:`datetime.date`) from the file paths."""
        if not self.fps:
            raise ValueError("No files provided for classification")

        dates = []
        for fp in self.fps:
            m = re.search(r"[0-9]{8}", os.path.basename(fp).replace("-", ""))
            if m is None:
                raise ValueError(f"Could not find date in file name: {fp}")
            dt = datetime.strptime(m.group(), r"%Y%m%d")
            dates.append(dt.date())

        return dates

    def classify(self):
        """Classify the source file organization type.

        - 1dpy: one representative day per year
          (assumed if only one file)
        - 1dpm: one representative day per month
        - 4dpm: 4 days per month, no holidays
          (representative Mon, Tue, Sat, Sun)
        - 4dpmh: 4 days per month, with holidays
        - 7dpm: 7 days per month, no holidays
          (representative full week)
        - 7dpmh: 7 days per month, with holidays
        - daily: every day
        """
        dates = self.dates()

        m_dates = {m: [] for m in range(1, 13)}
        for date in dates:
            m_dates[date.month].append(date)
        m_counts = {m: len(md) for m, md in m_dates.items()}
        dow_counts = Counter(date.isoweekday() for date in dates)
        n_unique_dow = len(dow_counts)

        if len(dates) == 1:
            return "1dpy"

        if all(n == 1 for n in m_counts.values()):
            assert n_unique_dow == 1
            return "1dpm"
        elif all(n == 4 for n in m_counts.values()):
            assert n_unique_dow == 4
            return "4dpm"
        elif all(n == 7 for n in m_counts.values()):
            assert n_unique_dow == 7
            return "7dpm"

        if n_unique_dow in {5, 6}:
            assert all(4 <= n <= 6 for n in m_counts.values())
            return "4dpmh"
        elif n_unique_dow == 7:
            if any(n >= 28 for n in m_counts.values()):
                return "daily"
            else:
                assert all(7 <= n <= 10 for n in m_counts.values())
                return "7dpmh"
        else:
            raise ValueError(
                "Unexpected file organization type. "
                f"Month counts: {m_counts}, day of week counts: {dow_counts}."
            )

    def closest(self, date):
        """Match `date` to the most applicable source file."""

        src_dates = self.dates()
        org = self.classify()

        year_counts = Counter(d.year for d in src_dates)
        max_year_count = max(year_counts.values())
        if len(year_counts) > 1:
            s_year_counts = ", ".join(f"{y} ({n})" for y, n in year_counts.items())
            logger.info(f"Pruning source dates to a single source year from {s_year_counts}")
            year_counts = Counter({y: n for y, n in year_counts.items() if 12 * n < max_year_count})
        unique_years = sorted(year_counts)
        if len(unique_years) > 1:
            s_year_counts = ", ".join(f"{y} ({n})" for y, n in year_counts.items())
            s_dates = ", ".join(str(d) for d in src_dates)
            raise ValueError(f"Files span multiple years ({s_year_counts}): {s_dates}")
        src_year = unique_years[0]

        # Filter to target month
        tgt_m = date.month
        tgt_md = date.strftime(r"%m%d")
        tgt_dow = date.isoweekday()
        tgt_doy = dayofyear(date)
        src_dates_m = []
        fps_m = []
        for d, fp in zip(src_dates, self.fps):
            if d.month == tgt_m:
                src_dates_m.append(d)
                fps_m.append(fp)

        # Filter out holidays
        src_dates_m_nh = []
        fps_m_nh = []
        for d, fp in zip(src_dates_m, fps_m):
            if not is_holiday(d):
                src_dates_m_nh.append(d)
                fps_m_nh.append(fp)

        # Assess whether target is a holiday
        tgt_is_holiday = is_holiday(date)
        i_holiday = None
        if tgt_is_holiday:
            i_holiday = HOLIDAY_MD[date.year].index(tgt_md)

        # Match
        if org == "1dpy":
            # One file, use it
            return self.fps[0]

        elif org == "1dpm":
            # One file in this month, use it
            return fps_m[0]

        elif org == "7dpm" or (org == "7dpmh" and not tgt_is_holiday):
            # Representative week, non-holiday
            src_iwds = [d.isoweekday() for d in src_dates_m_nh]
            assert src_iwds == list(range(1, 8))
            i = src_iwds.index(tgt_dow)
            return fps_m_nh[i]

        elif org == "4dpm" or (org == "4dpmh" and not tgt_is_holiday):
            # Representative 4 days, non-holiday
            src_iwds = [d.isoweekday() for d in src_dates_m_nh]
            assert src_iwds == [1, 2, 6, 7]
            if tgt_dow in {6, 7}:  # weekend
                i = src_iwds.index(tgt_dow)
                return fps_m_nh[i]
            else:  # Use Mon for Mon or Fri
                if tgt_dow in {1, 5}:
                    i = src_iwds.index(1)
                else:  # and Tue for Tue--Thu
                    i = src_iwds.index(2)
                return fps_m_nh[i]

        elif org in {"4dpmh", "7dpmh", "daily"} and tgt_is_holiday:
            # Holiday
            # It could be in a different month
            assert i_holiday is not None
            tgt_md = HOLIDAY_MD[src_year][i_holiday]
            src_date = datetime.strptime(f"{src_year}{tgt_md}", r"%Y%m%d").date()
            i = src_dates.index(src_date)
            return self.fps[i]

        elif org == "daily" and not tgt_is_holiday:
            # Find the closest matching day-of-week that isn't holiday
            # It could be in a different month
            cands = [d for d in src_dates if d.isoweekday() == tgt_dow and not is_holiday(d)]
            cands.sort(key=lambda d: abs(dayofyear(d) - tgt_doy))
            best = cands[0]
            i = src_dates.index(best)
            return self.fps[i]

        else:
            raise AssertionError(f"Unexpected file organization type: {org}")


if __name__ == "__main__":
    from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

    metemis_all_sectors = list(METEMIS_SUBDIR)
    metemis_arg_choices = metemis_all_sectors + ["all", "none"]

    parser = ArgumentParser(
        description="Link NEI 2022 files to the work directory",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-s",
        "--src-dir",
        "--src_dir",
        help=(
            "emissions base source directory, "
            "e.g., /gpfs/f6/bil-fire3/world-shared/Emissions/nexus on Gaea C6"
        ),
        type=str,
        required=True,
    )
    parser.add_argument(
        "-w",
        "--work-dir",
        "--work_dir",
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
        "--read-hemco-time",
        "--read_hemco_time",
        help="read HEMCO time file",
        action="store_true",
        default=True,
        required=False,
    )
    parser.add_argument(
        "--no-read-hemco-time",
        "--no_read_hemco_time",
        action="store_false",
        dest="read_hemco_time",
    )
    parser.add_argument(
        "-tf",
        "--time-file-path",
        "--time_file_path",
        help="location of the HEMCO time file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-v",
        "--nei-version",
        "--nei_version",
        help="NEI version",
        default="v2026-03",
        required=False,
    )
    parser.add_argument(
        "-m",
        "--met-emis",
        "--met_emis",
        help="MetEmis sector(s) to use (default: all)",
        nargs="+",
        choices=metemis_arg_choices,
        default="all",
        required=False,
    )
    parser.add_argument(
        "--debug",
        help="enable debug logging",
        action="store_true",
        default=False,
    )
    args = parser.parse_args()

    # Configure logging level based on arguments
    if args.debug:
        logger = setup_logger(logging.DEBUG)
        logger.debug("Debug logging enabled")

    src_dir = args.src_dir.rstrip("/")
    work_dir = args.work_dir.rstrip("/")
    version = args.nei_version

    metemis_sectors = args.met_emis
    if isinstance(metemis_sectors, str):
        metemis_sectors = [metemis_sectors]
    metemis_sectors = sorted(set(metemis_sectors), key=lambda sec: metemis_arg_choices.index(sec))
    if "all" in metemis_sectors:
        metemis_sectors = metemis_all_sectors
    elif "none" in metemis_sectors:
        metemis_sectors = []

    logger.info(
        f"Starting NEI2022 linker with src_dir={src_dir}, work_dir={work_dir}, version={version}, "
        f"met_emis={metemis_sectors}"
    )

    # Validate directories
    if not os.path.isdir(src_dir):
        logger.error(f"Source directory does not exist: {src_dir}")
        raise SystemExit(2)
    if not os.path.isdir(work_dir):
        logger.error(f"Work directory does not exist: {work_dir}")
        raise SystemExit(2)

    # Get target dates for processing
    if args.read_hemco_time:
        if args.time_file_path is None:
            hemco_time_file = os.path.join(args.work_dir, "../HEMCO_sa_Time.rc")
        else:
            hemco_time_file = args.time_file_path
        logger.info(f"Reading simulation time from: {hemco_time_file}")
        try:
            dates = get_hemco_simulation_time(hemco_time_file)
        except (FileNotFoundError, ValueError) as e:
            logger.error(f"Failed to read HEMCO time file: {e}")
            raise SystemExit(2)
    elif args.date is not None:
        try:
            d = datetime.strptime(args.date.replace("-", ""), r"%Y%m%d")
            dates = [d]
            logger.info(f"Using single date: {d.strftime(r'%Y-%m-%d')}")
        except ValueError as e:
            logger.error(f"Invalid date format '{args.date}': {e}")
            raise SystemExit(2)
    else:
        logger.error("No date information provided. Use --date or --read-hemco-time")
        raise SystemExit(2)

    todo = []

    # Identify NEI sectors
    search_pattern = f"{src_dir}/NEI2022v1/{version}/*"
    logger.info(f"Searching for sectors: {search_pattern}")
    sector_dirs = sorted([p for p in glob(search_pattern) if os.path.isdir(p)])
    if not sector_dirs:
        logger.error("No sectors found")
        raise SystemExit(2)
    logger.info(f"Found {len(sector_dirs)} NEI sectors")
    for sector_dir in sector_dirs:
        name = os.path.basename(sector_dir)
        todo.append(
            (
                name,
                f"{sector_dir}/*.nc",
                False,
            )
        )

    # MetEmis files
    for sector in metemis_sectors:
        metemis_dir = f"{src_dir}/{METEMIS_SUBDIR[sector]}"
        if not os.path.isdir(metemis_dir):
            logger.error(f"MetEmis directory does not exist for sector '{sector}': {metemis_dir}")
            raise SystemExit(2)
        # Some MetEmis sectors have multiple files per day (bins),
        # which we want to handle separately
        nums = METEMIS_NUMS.get(sector)
        if nums is not None:
            for num in nums:
                name = f"{sector}_{num}"
                search_pattern = f"{metemis_dir}/*_{num}_*.nc"
                todo.append((name, search_pattern, True))
        else:
            todo.append((sector, f"{metemis_dir}/*.nc", True))

    for sector, search_pattern, is_metemis in todo:
        sector_dir_rel = os.path.dirname(search_pattern).replace(src_dir, "$ROOT")
        logger.info(f"Sector: {sector} ({sector_dir_rel})")

        if (
            not is_metemis
            and any(sec in sector for sec in metemis_sectors)
            and not any(sec_part in sector for sec_part in ["canada", "mexico"])
        ):
            # We skip sector if MetEmis is doing it, but it only includes CONUS,
            # so we always include the Canada/Mexico files if they exist
            logger.info(f"Skipping {sector} in favor of MetEmis")
            continue

        files = sorted(glob(search_pattern))
        if not files:
            logger.error(f"No files found matching: {search_pattern}")
            raise SystemExit(1)
        logger.info(f"Found {len(files)} source files")
        try:
            matcher = FileMatcher(files)
            org = matcher.classify()
            logger.info(f"Detected file organization: {org}")
        except ValueError as e:
            logger.error(f"Failed to analyze source files: {e}")
            raise SystemExit(1)

        # Process each target date
        for d in dates:
            logger.info(f"Processing date: {d.strftime(r'%Y-%m-%d')}")

            try:
                src_fp = matcher.closest(d.date())
            except (ValueError, AssertionError) as e:
                logger.error(
                    f"Failed to find matching source file for {d.strftime(r'%Y-%m-%d')}: {e}"
                )
                raise SystemExit(1)

            # Form target file path, maintaining the full relative path structure
            m = re.search(r"[0-9\-]{8,}", os.path.basename(src_fp))
            if m is None:
                logger.error(f"Could not extract date from source filename: {src_fp}")
                raise SystemExit(1)
            src_date_str = m.group()
            tgt_date_str = d.strftime(r"%Y%m%d")
            src_rel_dir = os.path.dirname(os.path.relpath(src_fp, src_dir))
            tgt_fn = os.path.basename(src_fp).replace(src_date_str, tgt_date_str)
            tgt_fp = os.path.join(work_dir, src_rel_dir, tgt_fn)

            # Currently MetEmis are METEMIS/sector/YYYY/ but we want METEMIS/YYYY/
            if is_metemis:
                base_sector = re.sub(r"_[0-9]+$", "", sector)
                tgt_fp = tgt_fp.replace(f"{base_sector}/", "")

            # Create directory structure if needed
            target_dir = os.path.dirname(tgt_fp)
            if not os.path.exists(target_dir):
                logger.debug(f"Creating directory: {target_dir}")
                os.makedirs(target_dir, exist_ok=True)

            # Create the symlink
            try:
                logger.info(f"Linking {os.path.basename(src_fp)} -> {os.path.basename(tgt_fp)}")
                link_file(src_fp, tgt_fp)
            except (FileNotFoundError, OSError) as e:
                logger.error(f"Failed to create link for {d.strftime(r'%Y-%m-%d')}: {e}")
                raise SystemExit(1)

    logger.info("NEI2022 linking completed successfully")
