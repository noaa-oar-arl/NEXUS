#!/usr/bin/env python
"""
Disable a certain MetEmis sector.
"""
from pathlib import Path

SECTOR_SWITCHES = {
    "onroad": "ME Onroad",
    "livestock": "ME Livestock",
    "rwc": "ME RWC",
    "afdust": "ME AFD",
}


def disable_metemis(config_path, *, sector=None, dry_run=False):
    if sector is None:  # whole extension
        pre = "115"
        tgt = "MetEmis"
        val = "off"
    else:
        pre = "-->"
        tgt = SECTOR_SWITCHES[sector]
        val = "false"

    with open(config_path) as f:
        lines = f.readlines()

    for i, line in enumerate(lines):
        if line.lstrip().startswith("#") or not line.strip():
            continue
        if pre in line and tgt in line:
            lhs, rhs = line.split(":")
            assert lhs.lstrip().startswith(pre)
            val0 = rhs.split()[0].strip()
            print(f"found line ({i+1}):\n{line}")
            print(f"'{val0}' -> '{val}'")
            lines[i] = line.replace(val0, val)
            break
    else:
        raise AssertionError(f"setting line not found. Should look like: '{pre} {tgt} : ...'")

    if not dry_run:
        with open(config_path, "w") as f:
            f.writelines(lines)


if __name__ == "__main__":
    from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

    sector_options = list(SECTOR_SWITCHES)

    parser = ArgumentParser(
        description="Disable MetEmis",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        metavar="CONFIG",
        dest="config_path",
        type=Path,
    )
    parser.add_argument(
        "-s",
        "--sector",
        help="MetEmis sector to disable. If not specified, disables all of MetEmis.",
        choices=sector_options,
        type=str,
        default=None,
    )
    parser.add_argument(
        "--except",
        help="leave this sector on",
        dest="except_sector",
        choices=sector_options,
        type=str,
        default=None,
    )
    parser.add_argument(
        "--dry-run",
        help="print the changes that would be made, without modifying the file",
        action="store_true",
    )

    args = parser.parse_args()

    if args.sector is not None and args.except_sector is not None:
        parser.error("can't use both --sector and --except")

    kwargs = {
        "config_path": args.config_path,
        "dry_run": args.dry_run,
    }
    if args.except_sector is not None:
        to_disable = [s for s in sector_options if s != args.except_sector]
        for sector in to_disable:
            disable_metemis(sector=sector, **kwargs)
    else:
        disable_metemis(sector=args.sector, **kwargs)
