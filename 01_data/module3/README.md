# Module 5 — Input Data

This folder contains the large raster files required by Module 5 (Digital
Soil Mapping and Modeling). These files are **not tracked by Git** due to
their size and must be downloaded separately.

## Quick Start

### If the `.tif` files are missing:

**Option A (recommended):** Run the R download script:

```r
source("01_data/module5/download_module5_data.R")
```

**Option B (Windows only):** Run the PowerShell script:

```powershell
cd SoilFER-Training-Resources\01_data\module5
.\download_module5_data.ps1
```

**Option C:** Download manually from Zenodo (see `DATA_SOURCES.md`).

### If the `.tif` files are already present:

You are ready to run the Module 5 workflow. Proceed to:
- `02_scripts/module5/modelling_&_mapping.R` — the main modelling script
- `SoilFER-Training-Manual/05-Digital-soil-mapping-and-modeling.Rmd` — the chapter

## Contents

| File | Size | Description |
|------|------|-------------|
| `Env_Cov_250m_KANSAS.tif` | ~203 MB | Environmental covariates at 250 m resolution |
| `HighRes_Cov_100m_KANSAS.tif` | ~552 MB | Environmental covariates at 100 m resolution |
| `Cropland_Mask_KANSAS.tif` | ~24 MB | Binary cropland mask for Kansas |
| `checksums_module5.csv` | <1 KB | SHA256 checksums for integrity verification |
| `download_module5_data.R` | <5 KB | R download + verification script |
| `download_module5_data.ps1` | <3 KB | PowerShell download + verification script |
| `DATA_SOURCES.md` | <3 KB | Full documentation of data sources, DOIs, and licenses |
| `README.md` | this file | |

## Additional Input Data

The modelling script also requires `KSSL_DSM_0-30.csv`, which is produced
by Module 1. It is expected at:

```
SoilFER-Training-Resources/03_outputs/module1/KSSL_DSM_0-30.csv
```

If this file is missing, run the Module 1 workflow first.

## Updating Data Files

See `DATA_SOURCES.md` for instructions on updating the Zenodo release and
regenerating checksums.
