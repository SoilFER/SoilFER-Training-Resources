# Module 5 — Data Sources

## Overview

Module 5 (Digital Soil Mapping and Modeling) requires three large GeoTIFF
raster files that are too large to store in the Git repository. These files
are distributed via Zenodo as a versioned data release.

## Required Files

| File | Size | Description |
|------|------|-------------|
| `Env_Cov_250m_KANSAS.tif` | ~203 MB | Environmental covariates at 250 m resolution (multi-band) |
| `HighRes_Cov_100m_KANSAS.tif` | ~552 MB | Environmental covariates at 100 m resolution (multi-band) |
| `Cropland_Mask_KANSAS.tif` | ~24 MB | Binary cropland mask for the Kansas study area |

**Total download size:** ~779 MB

## Data Release

| Field | Value |
|-------|-------|
| **Repository** | Zenodo |
| **DOI** | `10.5281/zenodo.18548350` |
| **Version** | 1.0 |
| **License** | CC-BY-4.0 |
| **Release date** | TBD |

> **Note:** The DOI and download URLs above are placeholders. Once the
> Zenodo deposit is published, replace `PLACEHOLDER` in this file, in
> `checksums_module5.csv`, and in the download scripts with the actual
> Zenodo record number.

## How to Download

### Option A — R script (recommended, cross-platform)

```r
source("download_module5_data.R")
```

The script will:
1. Download each file from Zenodo into this folder (`01_data/module5/`)
2. Verify SHA256 checksums against `checksums_module5.csv`
3. Report success or failure for each file

### Option B — PowerShell (Windows only)

```powershell
.\download_module5_data.ps1
```

### Option C — Manual download

1. Go to <https://zenodo.org/records/18548350>
2. Download the three `.tif` files
3. Place them in this folder: `SoilFER-Training-Resources/01_data/module5/`
4. Verify checksums (optional):
   ```r
   tools::md5sum("Env_Cov_250m_KANSAS.tif")  # use digest::digest() for SHA256
   ```

## Checksum Verification

SHA256 checksums are stored in `checksums_module5.csv` (same folder as this
file). Both download scripts verify checksums automatically. To verify
manually in R:

```r
library(digest)
digest("Env_Cov_250m_KANSAS.tif", algo = "sha256", file = TRUE)
```

## Additional Input Data

The modelling script also requires `KSSL_DSM_0-30.csv`, which is produced
by Module 1 and expected at:

```
SoilFER-Training-Resources/03_outputs/module1/KSSL_DSM_0-30.csv
```

This file is small enough to be tracked in Git. If it is missing, run the
Module 1 workflow first.

## Updating the Data Release

When updating the raster files:

1. Upload new versions to Zenodo (create a new version of the existing deposit)
2. Recompute SHA256 checksums:
   ```r
   library(digest)
   digest("Env_Cov_250m_KANSAS.tif", algo = "sha256", file = TRUE)
   ```
3. Update `checksums_module5.csv` with new hashes, sizes, URLs, and version
4. Update the DOI/record number in this file and in the download scripts
5. Commit changes to the repository
