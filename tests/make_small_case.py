"""
Extract a small spatial subset from larger inputs.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path
from socket import gethostname
from textwrap import indent

import numpy as np
import pandas as pd
import xarray as xr

HERE = Path(__file__).parent
OUT_BASE = HERE / "cases"


# Short reference to the focus location and emissions configuration
case_id = "ncwcp_anthro_pm"

# Focus location
latc, lonc = 38.9721, -76.9245

# FV3 grid tile
fv3_res = "C384"
fv3_tile = 5

# Input case directory
in_base = Path("~/downloads/gocart-nexus").expanduser()
assert in_base.is_dir()
print("input case directory:", in_base.as_posix())

# Grid settings
nx = ny = 3  # HEMCO grid
dx = dy = 0.1
ne = 1  # thickness of input data halo (cells)
assert nx % 2 == 1 and ny % 2 == 1, "nx and ny must be odd"

# Make case directory
case_dir = OUT_BASE / case_id
print("full case ID:", case_id)
if case_dir.is_dir():
    print(f"removing existing case directory {case_dir.as_posix()}")
    shutil.rmtree(case_dir)
case_dir.mkdir()

# Store settings
settings = {
    "case_id": case_id,
    "source": f"{gethostname()}:{in_base.as_posix()}",
    "focus": (latc, lonc),
    "nx": nx,
    "ny": ny,
    "dx": dx,
    "dy": dy,
    "ne": ne,
    "fv3_res": fv3_res,
    "fv3_tile": fv3_tile,
}
with open(case_dir / "case.json", "w") as f:
    json.dump(settings, f, indent=2)
    f.write("\n")

# Round center point to nearest grid point center, assuming [0, dx, dx + 1, ...] are edges
xc_i, yc_i = lonc, latc
rx = 1 / dx
ry = 1 / dy
yc_u = round(yc_i * ry - dy / 2) / ry + dy / 2
yc_d = round(yc_i * ry + dy / 2) / ry - dy / 2
xc_u = round(xc_i * rx - dx / 2) / rx + dx / 2
xc_d = round(xc_i * rx + dx / 2) / rx - dx / 2
yc, _ = sorted([yc_u, yc_d], key=lambda y: abs(yc_i - y))
xc, _ = sorted([xc_u, xc_d], key=lambda x: abs(xc_i - x))
yc = round(yc, 3)
xc = round(xc, 3)
xc = (xc + 180) % 360 - 180  # ensure [-180, 180)
fmt = "9.4f"
print(f"input focus   (y, x): {yc_i:{fmt}} {xc_i:{fmt}}")
print(f"rounded focus (y, x): {yc:{fmt}} {xc:{fmt}}")

# Rectangular grid (centers)
y = np.arange(
    yc - (ny - 1 + ne * 2) / 2 * dy,
    yc + (ny - 1 + ne * 2) / 2 * dy + dy,
    dy,
)[: ny + ne * 2]
x = np.arange(
    xc - (nx - 1 + ne * 2) / 2 * dx,
    xc + (nx - 1 + ne * 2) / 2 * dx + dx,
    dx,
)[: nx + ne * 2]
y = np.round(y, 3)
x = np.round(x, 3)
assert y.size == ny + ne * 2 and x.size == nx + ne * 2
x_360 = np.mod(x, 360)
print("lat (y):", y)
print("lon (x):", x)
print("lon (x) 360:", x_360)

# We need these config files
# Only the grid file, which is used to set the HEMCO grid, should need to be adjusted
# - NEXUS_Config.rc
# - HEMCO_sa_Diagn.rc
# - HEMCO_sa_Grid.rc
# - HEMCO_sa_Spec.rc
# - HEMCO_sa_Time.rc

configs = {
    "main config": "NEXUS_Config.rc",
    "diagnostic definitions": "HEMCO_sa_Diagn.rc",
    # "grid definition": "HEMCO_sa_Grid.rc",
    "species definitions": "HEMCO_sa_Spec.rc",
    "simulation time settings": "HEMCO_sa_Time.rc",
}

for desc, fn in configs.items():
    p_in = in_base / fn
    if p_in.is_file():
        print(f"copying {fn!r} ({desc})")
        shutil.copy(p_in, case_dir / fn)
    else:
        rc_files = sorted(in_base.glob("*.rc"))
        s_files = "\n".join(f"- {p.name}" for p in rc_files)
        raise RuntimeError(
            f"{in_base.as_posix()} is missing the {fn!r} file ({desc}). "
            f"The directory has these .rc files:\n{s_files}"
        )

# Create HEMCO grid spec
# The MIN/MAX correspond to outer grid cell edges
# HEMCO computes the spacing as (MAX - MIN) / N
# Example (global):
# > cat HEMCO_sa_Grid.rc
# # Emission grid specifications:
# XMIN: -180.0
# XMAX:  180.0
# YMIN: -90.0
# YMAX:  90.0
# NX: 3600
# NY: 1800
# NZ: 1

xmin = x[ne] - dx / 2
xmax = x[-1 - ne] + dx / 2
ymin = y[ne] - dy / 2
ymax = y[-1 - ne] + dy / 2

fmt = ">8.3f"

s_hemco_grid_spec = f"""\
# Emission grid specifications:
XMIN: {xmin:{fmt}}
XMAX: {xmax:{fmt}}
YMIN: {ymin:{fmt}}
YMAX: {ymax:{fmt}}
NX: {nx}
NY: {ny}
NZ: 1
"""
fn = "HEMCO_sa_Grid.rc"
desc = "grid definition"
sep = "-" * 31
print(f"writing {fn!r} ({desc}):")
print(sep)
print(s_hemco_grid_spec, end="")
print(sep)
with open(case_dir / fn, "w") as f:
    f.write(s_hemco_grid_spec)

# From the the HEMCO config, learn the base input directory
with open(in_base / configs["main config"]) as f:
    for line in f:
        if line.startswith("ROOT"):
            root = line.split(":")[-1].strip()
            break
    else:
        raise AssertionError("ROOT setting not found")
in_data = in_base / root
assert in_data.is_dir()
out_data = case_dir / in_data.name
out_data.mkdir()

# Check for FV3 grid spec file
grid_spec_fn = f"{fv3_res}_grid_spec.tile{fv3_tile}.nc"
grid_spec_p = in_base / grid_spec_fn
if not grid_spec_p.is_file():
    raise FileNotFoundError(f"missing grid spec file: {grid_spec_p.as_posix()}")

# Select input data and write to case directory
lat = y
lon = x
lon_360 = x_360
for p in list(in_data.glob("*.nc")) + [grid_spec_p]:
    if "grid_spec" in p.name and p != grid_spec_p:
        continue

    print(p.relative_to(in_base).as_posix())

    ds = xr.open_dataset(p, decode_times=False)
    sel = None

    if "grid_spec" in p.name:  # for regridding to UFS
        # edges: grid_lat, grid_lon (dims: grid_x, grid_y)
        # centers: grid_latt, grid_lont (dims: grid_xt, grid_yt)
        # lon in [0, 360)

        assert ds.grid_lont.dims == ds.grid_latt.dims == ("grid_yt", "grid_xt")
        lon_gs = ds.grid_lont.values
        lat_gs = ds.grid_latt.values
        box = (
            (lon_gs >= lon_360[0])
            & (lon_gs <= lon_360[-1])
            & (lat_gs >= lat[0])
            & (lat_gs <= lat[-1])
        )
        y_inds, x_inds = box.nonzero()
        if not y_inds.size > 0:
            print("- ERROR: UFS grid spec appears not to overlap target grid.")
            parts = [
                "lons:",
                pd.cut(lon_gs.ravel(), bins=np.arange(0, 360 + 20, 20), right=False)
                .value_counts()
                .to_string(),
                "lats:",
                pd.cut(lat_gs.ravel(), bins=np.arange(-90, 90 + 20, 20), right=False)
                .value_counts()
                .to_string(),
            ]
            print(indent("\n".join(parts), "  "))
            raise SystemExit(2)
        ix1, ix2 = x_inds.min(), x_inds.max()
        if not ix2 - ix1 > 1:
            ix1 -= 1
            ix2 += 1
        iy1, iy2 = y_inds.min(), y_inds.max()
        if not iy2 - iy1 > 1:
            iy1 -= 1
            iy2 += 1

        sel = ds.isel(
            grid_xt=slice(ix1, ix2 + 1),
            grid_yt=slice(iy1, iy2 + 1),
            grid_x=slice(ix1, ix2 + 2),
            grid_y=slice(iy1, iy2 + 2),
        )
        print("- lon:", sel.grid_lon.values)
        print("- lat:", sel.grid_lont.values)
    elif "HTAP" in p.name and dx == dy == 0.1:
        # Exact selection
        sel = ds.sel(lat=lat, lon=lon_360)
        print("- lat exact:", sel.lat.values)
        print("- lon exact:", sel.lon.values)
    else:
        # Note that nearest for low-res data will include duplicates
        assert ds.lon.max() < 180
        sel = ds.sel(lat=lat, lon=lon, method="nearest")
        print("- lat nearest:", sel.lat.values)
        print("- lon nearest:", sel.lon.values)

    if p == grid_spec_p:
        p_out = case_dir / "grid_spec.nc"
    else:
        p_out = out_data / p.name

        # Replace lat/lon
        lat_attrs = sel.lat.attrs.copy()
        lon_attrs = sel.lon.attrs.copy()
        sel = sel.assign_coords(lat=lat, lon=lon)
        sel["lat"].attrs.update(lat_attrs)
        sel["lon"].attrs.update(lon_attrs)

    encoding = {
        k: {"zlib": True, "complevel": 3} for k in sel.data_vars if k not in {"UTC_OFFSET", "time"}
    }
    sel.to_netcdf(p_out, encoding=encoding)
