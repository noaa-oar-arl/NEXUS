#!/usr/bin/env python
"""
Collect data from runs done with run_config.py
"""

import argparse
import datetime
import itertools
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
TMP_BASE_DIR_DEFAULT = REPO / "tmp"

parser = argparse.ArgumentParser(description=__doc__)

parser.add_argument(
    "base_dirs",
    metavar="DIRS",
    type=Path,
    default=[TMP_BASE_DIR_DEFAULT],
    nargs="*",
    help=(
        "directories to look for run directories in "
        f"(default: just {TMP_BASE_DIR_DEFAULT.as_posix()})"
    ),
)

parser.add_argument(
    "-o",
    dest="output_path",
    type=Path,
    default=HERE / "data.ndjson",
    help="output file path (default: %(default)s)",
)

args = parser.parse_args()

for d in args.base_dirs:
    if not d.is_dir():
        print(f"error: input {d.as_posix()!r} is not a directory")
        raise SystemExit(2)

rows = []
for d in itertools.chain.from_iterable(base_dir.glob("*") for base_dir in args.base_dirs):
    print(d)

    # Load settings info
    with open(d / "settings.json") as f:
        data = json.load(f)
    data["case_dir"] = d.as_posix()

    # Load Slurm stdout
    (slurm_stdout_p,) = d.glob("slurm-*.out")
    slurm_stdout = slurm_stdout_p.read_text()

    # Get run time from the slurm output
    tic = toc = None
    for line in slurm_stdout.splitlines():
        if line.startswith("tic=="):
            tic = datetime.datetime.fromisoformat(line.split("==")[1])
        elif line.startswith("toc=="):
            toc = datetime.datetime.fromisoformat(line.split("==")[1])
    data["run_time"] = (toc - tic).total_seconds()

    # Get mem info from the slurm output
    lines = slurm_stdout.splitlines()
    for i, line in enumerate(lines):
        if line.startswith("Peak memory usage summary:"):
            break
    else:
        raise AssertionError("Peak memory usage summary not found")
    for line in lines[i + 1 : i + 4]:
        a, b = line.split("=", 1)
        data[f"mem_{a.strip()}"] = b.strip()

    # Load Slurm stderr
    (slurm_stderr_p,) = d.glob("slurm-*.err")
    slurm_stderr = slurm_stderr_p.read_text()

    # Check for error
    data["success"] = "srun: error: " not in slurm_stderr

    rows.append(data)

print(f"Collected {len(rows)} data records")

p = args.output_path
if p.exists():
    while True:
        r = input(f"Overwrite {p}? [y/n]: ")
        if r == "n":
            raise SystemExit
        elif r == "y":
            break

with open(p, "w") as f:
    for data in rows:
        json.dump(data, f)
        f.write("\n")
