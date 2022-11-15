#!/usr/bin/env python
"""
Make NEXUS output pretty
"""
from pathlib import Path

from mpi4py import MPI

comm = MPI.COMM_WORLD
np = comm.Get_size()
rank = comm.Get_rank()

print(f"rank {rank} of {np}")

DEFAULT_TIME_FILE_PATH = Path("./HEMCO_sa_Time.rc")


def get_hemco_dates(time_file=DEFAULT_TIME_FILE_PATH):
    import datetime as dt

    def parse_dt_line(line):
        _, s_date, s_time = line.split()
        return dt.datetime.strptime(f"{s_date} {s_time}", r"%Y-%m-%d %H:%M:%S")

    print(time_file.as_posix())
    start = end = ts_emis = None
    with open(time_file, "r") as f:
        for line in f:
            line = line.strip()
            if line.startswith("START:"):
                start = parse_dt_line(line)
            elif line.startswith("END:"):
                end = parse_dt_line(line)
            elif line.startswith("TS_EMIS:"):  # time step
                _, s_ts = line.split()
                ts_emis = float(s_ts)

    start_base = start.replace(hour=0, minute=0, second=0)
    assert ts_emis == 3600
    start_offset = (start - start_base).total_seconds() / 3600
    dates = [
        start + dt.timedelta(hours=h) + dt.timedelta(hours=start_offset)
        for h in range(int((end - start).total_seconds() / 3600) + 1)
    ]

    assert dates[0] == start
    assert dates[-1] == end
    assert (dates[1] - dates[0]).total_seconds() / 3600 == ts_emis

    return dates, start_base
