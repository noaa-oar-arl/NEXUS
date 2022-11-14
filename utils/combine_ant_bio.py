#!/usr/bin/env python
"""
Combine/reassign HEMCO species in order to get:
CMAQv5.2.1 CB6 Emission Species w/biogenics
"""

SPECIES = [
    "AACD",
    "ACET",
    "ACROLEIN",
    "ALD2",
    "ALD2_PRIMARY",
    "ALDX",
    "APIN",
    "BENZ",
    "BUTADIENE13",
    "CH4",
    "CH4_INV",
    "CL2",
    "CO",
    "CO2_INV",
    "ETH",
    "ETHA",
    "ETHY",
    "ETOH",
    "FACD",
    "FORM",
    "FORM_PRIMARY",
    "HCL",
    "HONO",
    "IOLE",
    "ISOP",
    "KET",
    "MEOH",
    "NAPH",
    "NH3",
    "NH3_FERT",
    "NO",
    "NO2",
    "OLE",
    "PAL",
    "PAR",
    "PCA",
    "PCL",
    "PEC",
    "PFE",
    "PH2O",
    "PK",
    "PMC",
    "PMG",
    "PMN",
    "PMOTHR",
    "PNA",
    "PNCOM",
    "PNH4",
    "PNO3",
    "POC",
    "PRPA",
    "PSI",
    "PSO4",
    "PTI",
    "SESQ",
    "SO2",
    "SOAALK",
    "SULF",
    "TERP",
    "TOL",
    "UNK",
    "UNR",
    "VOC_INV",
    "XYLMN",
]


def main(ifp, ofp, *, compress=True):
    """
    Parameters
    ----------
    ifp, ofp : Path
        Input and output file path.
    """
    import numpy as np
    from netCDF4 import Dataset

    ds = Dataset(ifp, "r")
    ds_new = Dataset(ofp, "w")

    ds_new.createDimension("time", None)
    ds_new.createVariable("time", ds["time"].dtype, ("time",))
    ds_new["time"][:] = ds["time"][:]
    ds_new["time"].units = ds["time"].units
    ds_new.createDimension("y", ds.dimensions["y"].size)
    ds_new.createDimension("x", ds.dimensions["x"].size)

    em_units = "kg m-2 s-1"
    for vn in ds.variables:
        if vn not in {"time", "latitude", "longitude"} and ds[vn].units.strip() != em_units:
            print(f"warning: expected {vn} to have units {em_units!r} but it has {ds[vn].units!r}")

    for spc in SPECIES:

        kwargs = dict(fill_value=9.96920997e+36)
        if compress:
            kwargs.update(zlib=True, complevel=1)
        ds_new.createVariable(spc, np.float32, ("time", "y", "x"), **kwargs)
        ds_new[spc].units = em_units

        # 1. Use HEMCO MEGANv2.1 instantaneous diagnostic for some bio-only species
        if spc == "AACD":
            ds_new[spc][:] = ds["InvMEGAN_AAXX"][:]
        elif spc == "FACD":
            ds_new[spc][:] = ds["InvMEGAN_FAXX"][:]
        elif spc == "APIN":
            ds_new[spc][:] = ds["InvMEGAN_APIN"][:]
        elif spc == "SESQ":
            # Sesquiterpene is also only biogenic
            ds_new[spc][:] = ds["SESQ_bio"][:]

        # 2. The following species need to combine HEMCO anthropogenic and MEGANv2.1 biogenic species
        elif spc in {"ACET", "ALD2", "ETH", "ETOH", "ISOP", "MEOH", "OLE"}:
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds[f"{spc}_bio"][:]
        elif spc == "TERP":
            # Lumped terpene should combine all HEMCO MEGANv2.1 terpenoids
            ds_new[spc][:] = (
                ds[f"{spc}_ant"][:] + ds["MTPA_bio"][:] + ds["MTPO_bio"][:] + ds["LIMO_bio"][:]
            )

        # 3. Following species are approximately calculated from other BVOCs
        # (estimated from NACC-CMAQv5.3.1/BEISv3.6.1 summer simulation) and then combined with anthropogenic
        elif spc == "IOLE":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["OLE_bio"][:] * 0.967963
        elif spc == "PAR":
            ds_new[spc][:] = (
                ds[f"{spc}_ant"][:]
                + (ds["MTPA_bio"][:] + ds["MTPO_bio"][:] + ds["LIMO_bio"][:]) * 0.576825
            )
        elif spc == "ETHA":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ETH_bio"][:] * 0.160406
        elif spc == "ALDX":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ALD2_bio"][:] * 0.166038
        elif spc == "FORM":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ALD2_bio"][:] * 0.914909
        elif spc == "FORM_PRIMARY":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ALD2_bio"][:] * 0.914909
        elif spc == "ALD2_PRIMARY":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ALD2_bio"][:] * 0.670921
        elif spc == "KET":
            ds_new[spc][:] = ds[f"{spc}_ant"][:] + ds["ACET_bio"][:] * 0.0339559
        elif spc == "CO":
            ds_new[spc][:] = (
                ds[f"{spc}_ant"][:]
                + (ds["MTPA_bio"][:] + ds["MTPO_bio"][:] + ds["LIMO_bio"][:]) * 0.4666
            )

        # 4. The remainder of species are just anthropogenic from HEMCO
        else:
            ds_new[spc][:] = ds[f"{spc}_ant"][:]

    ds.close()

    return 0


def parse_args(argv=None):
    import argparse
    from pathlib import Path

    parser = argparse.ArgumentParser(
        description="Combine/reassign NEXUS emissions outputs for CMAQ."
    )
    parser.add_argument("INPUT", type=Path, help="Input file path.")
    parser.add_argument("OUTPUT", type=Path, help="Output file path.")
    parser.add_argument("--compress", action="store_true", help="Whether to apply compression for the variables.")
    parser.add_argument("--no-compress", action="store_false", dest="compress")
    parser.set_defaults(compress=True)

    args = parser.parse_args(argv)

    return {
        "ifp": args.INPUT,
        "ofp": args.OUTPUT,
        "compress": args.compress,
    }


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
