# Module 2 - Three-Stage Sampling Design Scripts

## 📋 Overview

This folder contains **R scripts** for designing a three-stage probabilistic sampling scheme for soil surveys using **Covariate Space Coverage (CSC)** methodology.

**⚠️ IMPORTANT: Script Execution Order**

Scripts **MUST** be run in this sequence:

1. `gee_soilfer_env_covariates.txt` (Google Earth Engine): Generates environmental rasters
2. `1_climate_soil_covariates.R` (R): Processes climate data and creates Newhall regimes
3. `sampling_design_csc_soilfer.R` (R): Main sampling design (run 3 times for cropland/grassland/forest)
4. `sampling_design_csc_soilfer.R` (R): Merge results (Sections 24-34 only)

**Total time**: ~30-50 hours (mostly automated)

---

## 📁 Folder Structure

```
03_scripts/module2/
│
├── shapes/                                # Example shapefiles (see 01_data/module2)
├── sampling_design_csc_soilfer.R          # MAIN SCRIPT
├── climate_soil_covariates.R              # Soil climate processing
├── opt_sample.R                           # Sample size optimization (DON'T RUN IT)
├── gee_soilfer_env_covariates.txt         # GEE code template
└── README.md                              # This file
```

---

## 📜 Script Descriptions

### 1. `gee_soilfer_env_covariates.txt` - GEE EXPORT SCRIPT

**Purpose**: Download environmental covariates from Google Earth Engine

**What it does**:
- Exports CHELSA climate variables (bio1-bio19, PET, wind speed)
- Exports MODIS vegetation indices (NDVI, FPAR, LST by quarter)
- Exports Sentinel-2 spectral bands and indices (NDVI, EVI, NDBSI)
- Exports terrain data (SRTM elevation, slope, aspect, TPI, TWI)
- Exports land cover (ESA WorldCover, cropland mask)
- Exports geomorphology (Geomorphon landforms)
- Exports TerraClimate time series (1981-2023)

**Inputs**: Study area boundary coordinates

**Outputs** (to Google Drive `GEE_Exports/` folder):
- `Environmental_Covariates_250m_[AREA].tif` (PSU-level, ~80 bands)
- `HighRes_Covariates_100m_[AREA].tif` (SSU-level, ~30 bands)
- `Cropland_Mask_[AREA].tif` (20m binary mask)
- `TerraClimate_AvgTemp_1981_2023_[AREA].tif` (monthly temperature)
- `TerraClimate_Precip_1981_2023_[AREA].tif` (monthly precipitation)
- `TerraClimate_PET_1981_2023_[AREA].tif` (monthly PET)
- `Geomorphon_Landforms_[AREA].tif` (90m landforms)
- `DEM_[AREA].tif` and `Slope_[AREA].tif` (100m terrain)
- `ESA_WorldCover_2021_[AREA].tif` (10m land cover)

**Runtime**: 30 minutes - 3 hours (depends on area size and GEE queue)

**How to use**:
1. Open [Google Earth Engine Code Editor](https://code.earthengine.google.com/)
2. Create folder in Google Drive: `GEE_Exports`
3. Copy/paste code from this file
4. **Section 1**: Define your region (3 options provided)
   ```javascript
   // Option 1: UN country borders
   var ISO = ['KEN'];  // ISO country code
   
   // Option 2: Custom shapefile
   // Upload asset to GEE first
   
   // Option 3: Rectangle coordinates (easiest!)
   var region = ee.Geometry.Rectangle({
     coords: [minLon, minLat, maxLon, maxLat]
   });
   ```
5. Update `countryName` variable (line 56)
6. Run script (click ▶️ Run button)
7. Go to **Tasks** tab → Click **Run** on each export
8. Wait for exports to complete (monitor in Tasks tab)
9. Download from Google Drive → place in `rasters/GEE_Exports/`

**Important notes**:
- Exports may be tiled if area is large (will be merged in Step 2)
- Keep default resolutions (specified in lines 66-73)
- TerraClimate is low resolution (~4km) - this is normal

---

### 2. `1_climate_soil_covariates.R` - CLIMATE PROCESSING SCRIPT

**Purpose**: Process raw climate data and generate Newhall soil-climate regimes

**What it does**:
1. Processes TerraClimate monthly data (1981-2023)
2. Calculates long-term monthly averages for temperature and precipitation
3. Combines climate data with DEM and available water capacity (AWC)
4. Runs **jNSM Newhall simulation model** (calculates soil temperature and moisture regimes)
5. Merges tiled GEE exports (if data was exported in multiple tiles)
6. Exports final climate and Newhall rasters

**Inputs**:
- TerraClimate data from GEE (temperature, precipitation, PET)
- DEM from GEE
- Available Water Capacity (AWC) raster
  - Download from [SoilGrids](https://soilgrids.org/)
  - Variable: `sol_awc_0-200cm`
  - Place in `rasters/` folder as `awc_isric_250m.tif`
- Study area boundary shapefile

**Outputs**:
- `newhall.tif` - Soil climate regimes (temperature, moisture, water balance)
- `climate_vars.tif` - Monthly temperature and precipitation (24 layers)
- `[Dataset]_merged.tif` - Merged GEE tiles (if applicable)

**Runtime**: 6-8 hours (Newhall model is intensive)

**Configuration** (Section 1):
```r
raster.path <- "rasters/"
shp.path <- "shapes/"
epsg <- "EPSG:3857"  # Your target CRS
```

**Requirements**:
```r
# Install jNSMR package first!
install.packages("jNSMR")

# Other packages
packages <- c("sp", "terra", "sf", "jNSMR", "dplyr")
```

**How to run**:
```r
# In RStudio
source("03_scripts/module2/1_climate_soil_covariates.R")
# Or run line-by-line to monitor progress
```

**What happens**:
```
Step 1: Process temperature (lines 62-116)
  - Loads monthly data from 1981-2023
  - Calculates average for each month across all years
  - Exports: Temp_Stack_01-22_TC.tif

Step 2: Process precipitation (lines 118-167)
  - Same as temperature
  - Exports: Prec_Stack_01-22_TC.tif

Step 3: Merge climate data (lines 169-198)
  - Combines temperature + precipitation
  - Renames layers (tJan, pFeb, etc.)
  - Exports: climate_vars.tif

Step 4: Load spatial data (lines 200-250)
  - Loads DEM, ROI boundary
  - Loads AWC from SoilGrids
  - Resamples all to match

Step 5: Prepare Newhall inputs (lines 252-297)
  - Combines climate + elevation + AWC
  - Adds longitude/latitude
  - Exports: climate_newhall_vars.tif

Step 6: Run Newhall model (lines 299-313) ⏱️ SLOW!
  - Computes soil temperature regime
  - Computes soil moisture regime
  - Calculates water balance metrics
  - Uses 4 CPU cores (6-7 hours)

Step 7: Plot and export results (lines 315-402)
  - Creates diagnostic plots
  - Exports: newhall.tif

Step 8: Merge tiled exports (lines 404-467)
  - Spatially stitches tiles together
  - Exports: *_merged.tif files
```

**Troubleshooting**:

**Error: "Can't find awc_isric_250m.tif"**
```r
# Download AWC from SoilGrids:
# https://files.isric.org/soilgrids/latest/data/
# Variable: sol_awc.0-200cm_mean.tif
# Rename to: awc_isric_250m.tif
```

**Error: "jNSMR package not found"**
```r
install.packages("jNSMR")
# If that fails:
remotes::install_github("ncss-tech/jNSMR")
```

**Newhall too slow**
```r
# Option 1: Use fewer cores
newhall_results <- jNSMR::newhall_batch(newhall, cores = 2)

# Option 2: Aggregate raster (reduce resolution)
newhall <- terra::aggregate(newhall, fact = 2)  # 2x lower resolution
```

---

### 3. `sampling_design_csc_soilfer.R` ⭐ MAIN SCRIPT

**Purpose**: Complete three-stage sampling design workflow

**What it does**:
- Selects Primary Sampling Units (PSUs) using environmental clustering
- Generates Secondary Sampling Units (SSUs) within each PSU
- Creates Tertiary Sampling Units (TSUs) as point sampling locations
- Generates target and replacement sites for field work

**Inputs**:
- Study area boundary (shapefile)
- Environmental covariates (rasters from GEE)
- Land use mask (binary raster)
- Optional: legacy data, protected areas, geology

**Outputs**:
- `PSUs_target.shp` - Selected 2x2 km sampling areas
- `TSUs_target.shp` - Actual field sampling points (GPS coordinates)
- `PSUs_replacements.shp` - Backup PSUs
- `TSUs_replacements.shp` - Backup sampling points
- Various analysis rasters and visualizations

**Runtime**: 4-12 hours (depends on study area size)

**Key Sections**:
```
1-3:   Setup and functions
4-9:   Data loading and preprocessing
10-11: PSU grid generation
12:    Optimal sample size calculation (6-24 hours!)
13:    PSU selection using CSC
14:    Load high-resolution covariates
15:    Generate SSUs and TSUs (main loop)
16-18: Data structuring
19-23: Generate replacement PSUs/TSUs
24-34: PART 2 - Merge land uses and create IDs
```

---

### 4. `opt_sample.R`

**Purpose**: Calculate optimal number of sampling units

**What it does**:
- Tests different sample sizes (50, 75, 100, ..., up to 3000)
- Measures environmental representativeness using Kullback-Leibler divergence
- Identifies minimum sample size needed for 95% coverage

**Usage**: Called automatically by main script (Section 12)

**Standalone usage**:
```r
source("opt_sample.R")

# Prepare covariate data
covs <- data.frame(scale(your_environmental_data))

# Run optimization
result <- opt_sample(
  alg = "fcs",
  s_min = 50,
  s_max = 3000,
  s_step = 25,
  s_reps = 4,
  covs = covs,
  cpus = 4,
  conf = 0.95
)

optimal_n <- result$optimal_sites[1,2]
```

**Output**: Optimal sample size (e.g., 412 PSUs)

**Note**: Very computationally intensive! Results are cached for reuse.

---

### 5. `climate_soil_covariates.R` (DEPRECATED - Use script #2 instead)

**Purpose**: Legacy script for Newhall processing

**Note**: This script has been replaced by `1_climate_soil_covariates.R` (Script #2 above), which includes additional functionality for merging tiled GEE exports. Use the new script instead.

---

### 6. `gee_soilfer_env_covariates.txt` (Covered in Script #1)

---

## 🚀 Quick Start Guide

### Complete Workflow Overview

```
┌─────────────────────────────────────────────────────────────┐
│  STEP 1: Generate Rasters from Google Earth Engine (GEE)    │
│  Script: gee_soilfer_env_covariates.txt                     │
│  Runtime: 30 min - 3 hours                                  │
│  Output: Environmental covariates → Google Drive            │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  STEP 2: Process Climate Data & Generate Newhall            │
│  Script: 1_climate_soil_covariates.R                        │
│  Runtime: 6-8 hours                                         │
│  Output: newhall.tif, merged tiles                          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  STEP 3: Generate Sampling Design (run 3× for each LULC)    │
│  Script: sampling_design_csc_soilfer.R                      │
│  Runtime: 4-12 hours per land use                           │
│  Output: PSUs and TSUs for field sampling                   │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  STEP 4: Merge All Land Uses                                │
│  Script: sampling_design_csc_soilfer.R (Sections 24-34)     │
│  Runtime: 5-10 minutes                                      │
│  Output: Unified dataset with country-wide IDs              │
└─────────────────────────────────────────────────────────────┘
```

---

### Step 1: Generate Environmental Covariates (GEE)

**Time**: 30 minutes - 3 hours

1. **Sign up** for Google Earth Engine (free): https://earthengine.google.com/

2. **Create folder** in Google Drive named `GEE_Exports`

3. **Open** [GEE Code Editor](https://code.earthengine.google.com/)

4. **Copy/paste** code from `gee_soilfer_env_covariates.txt`

5. **Edit Section 1** - Define your region (choose one option):
   ```javascript
   // OPTION 1: UN country borders by ISO code
   var ISO = ['KEN'];  // Kenya
   var aoi = ee.FeatureCollection('projects/digital-soil-mapping-gsp-fao/assets/UN_BORDERS/BNDA_CTY')
     .filter(ee.Filter.inList('ISO3CD', ISO));
   var region = aoi.geometry();
   
   // OPTION 2: Custom shapefile (upload to GEE assets first)
   // var shapefile = ee.FeatureCollection('projects/your-project/assets/your_shapefile');
   // var region = shapefile.geometry().bounds();
   
   // OPTION 3: Rectangle coordinates [west, south, east, north]
   var region = ee.Geometry.Rectangle({
     coords: [-102.05, 36.993, -94.58, 40.00],  // Kansas example
     geodesic: false
   });
   ```

6. **Update country name** (line 56):
   ```javascript
   var countryName = 'KENYA';  // Used in export filenames
   ```

7. **Run the script**: Click ▶️ **Run** button at top

8. **Start exports**: Go to **Tasks** tab (right panel) → Click **Run** on each export

9. **Monitor progress**: Tasks will show "Running..." then "Completed"

10. **Download from Google Drive**:
    - Open Google Drive → `GEE_Exports` folder
    - Download all `.tif` files
    - Place in `01_data/module2/rasters/GEE_Exports/`

**Expected files** (9 exports):
- ✅ `Environmental_Covariates_250m_AREA.tif`
- ✅ `HighRes_Covariates_100m_AREA*.tif` (may be multiple tiles)
- ✅ `Cropland_Mask_AREA*.tif` (may be multiple tiles)
- ✅ `TerraClimate_AvgTemp_1981_2023_AREA.tif`
- ✅ `TerraClimate_Precip_1981_2023_AREA.tif`
- ✅ `TerraClimate_PET_1981_2023_AREA.tif`
- ✅ `Geomorphon_Landforms_AREA.tif`
- ✅ `DEM_AREA.tif`
- ✅ `Slope_AREA.tif`
- ✅ `ESA_WorldCover_2021_AREA*.tif` (may be multiple tiles)

**Note**: Large areas export in tiles (e.g., `file-0000000000-0000000000.tif`). This is normal - Step 2 will merge them.

---

### Step 2: Process Climate Data

**Time**: 6-8 hours

**Prerequisites**:
1. All GEE exports downloaded
2. AWC raster from SoilGrids (download once, reuse for all projects):
   - Go to: https://files.isric.org/soilgrids/latest/data/
   - Download: `sol_awc.0-200cm_mean.tif`
   - Rename to: `awc_isric_250m.tif`
   - Place in: `01_data/module2/rasters/`

**Run the script**:

```r
# Open RStudio
source("03_scripts/module2/1_climate_soil_covariates.R")

# Or run section by section to monitor progress
```

**What happens** (console output):
```
Temperature data dimensions: 516 months (43 years)
Processing temperature month: 1
Processing temperature month: 13
...
Processing precipitation month: 1
...
Reprojecting DEM data to EPSG:3857
Found 3 tile(s) for Cropland_Mask_KANSAS
Mosaicking tiles spatially...
SUCCESS: Merged Cropland_Mask_KANSAS

Running Newhall model... (6-7 hours)
[Progress bar will appear]
```

**Outputs** (in `rasters/`):
- ✅ `newhall.tif` - Soil climate regimes
- ✅ `climate_vars.tif` - Monthly climate data
- ✅ `Cropland_Mask_AREA_merged.tif` - Merged tiles
- ✅ `HighRes_Covariates_100m_AREA_merged.tif` - Merged tiles
- ✅ `ESA_WorldCover_2021_AREA_merged.tif` - Merged land cover

**Create additional land use masks**:
```r
# After script completes, create grassland and forest masks
library(terra)

# Load ESA WorldCover
esa <- rast("01_data/module2/rasters/ESA_WorldCover_2021_AREA_merged.tif")

# ESA WorldCover classes:
# 10 = Tree cover (forest)
# 30 = Grassland
# 40 = Cropland

# Create masks
cropland <- esa == 40
grassland <- esa == 30
forest <- esa == 10

# Export (20m resolution maintained)
writeRaster(cropland, "01_data/module2/rasters/Cropland_Mask_AREA_merged.tif", overwrite=TRUE)
writeRaster(grassland, "01_data/module2/rasters/Grassland_Mask_AREA_merged.tif", overwrite=TRUE)
writeRaster(forest, "01_data/module2/rasters/Forest_Mask_AREA_merged.tif", overwrite=TRUE)
```

---

### Step 3: Generate Sampling Design (Run 3 Times)

**Time**: 4-12 hours per land use type

**Prerequisites**: Steps 1-2 completed

**Configure script** (`sampling_design_csc_soilfer.R` Section 2):

```r
## 2 - DEFINE VARIABLES AND PARAMETERS

# Update these for your study
ISO.code <- "KANSAS"          # Change to your area code
landuse <- "cropland"         # "cropland", "grassland", or "forest"
epsg <- "EPSG:26714"          # Your UTM zone (e.g., "EPSG:32615")

# Sampling design
psu_size <- 2000              # PSU: 2 km × 2 km
ssu_size <- 100               # SSU: 100 m × 100 m
num_primary_ssus <- 4         # Target SSUs per PSU
num_alternative_ssus <- 4     # Replacement SSUs per PSU
number_TSUs <- 3              # Points per SSU

# Quality control
percent_crop <- 20            # Min % land use in PSU
iterations <- 10              # Clustering iterations
```

**Update file paths** in Sections 4-8:
```r
# Section 4: Study area
country_boundaries <- file.path(paste0(shp.path, "roi_YOUR_AREA_epsg_4326.shp"))

# Section 8: Land use
landuse_file <- file.path(paste0(raster.path, "Cropland_Mask_YOUR_AREA.tif"))
```

### Step 3: Run for Each Land Use

**You must run the script THREE times** (once per land use type):

#### Run 1: Cropland
```r
landuse <- "cropland"
landuse_file <- "Cropland_Mask_AREA.tif"
n.psu <- round(optimal_N_KLD * 0.85, 0)  # 85% of samples
# Run entire script (Sections 1-23)
```

#### Run 2: Grassland
```r
landuse <- "grassland"
landuse_file <- "Grassland_Mask_AREA.tif"
n.psu <- round(optimal_N_KLD * 0.10, 0)  # 10% of samples
# Run entire script (Sections 1-23)
```

#### Run 3: Forest
```r
landuse <- "forest"
landuse_file <- "Forest_Mask_AREA.tif"
n.psu <- round(optimal_N_KLD * 0.05, 0)  # 5% of samples
# Run entire script (Sections 1-23)
```

### Step 4: Merge Results

After all three runs complete:

```r
# Open NEW R session
source("sampling_design_csc_soilfer.R")

# Run only Sections 24-34 (PART 2: Merge Land Uses)
# This creates unified IDs and summary statistics
```

---

## ⚙️ Running Options

### Option A: Full Script (Recommended for First Run)

```r
# In RStudio
source("sampling_design_csc_soilfer.R")
# Or press Ctrl+Shift+Enter (Windows) / Cmd+Shift+Return (Mac)
```

**Runtime**: 4-12 hours depending on:
- Study area size
- Number of PSUs
- CPU cores available

### Option B: Section-by-Section (For Learning/Debugging)

```r
# Run each section sequentially with Ctrl+Enter
# Inspect outputs between sections:

plot(cov.dat[[1]])              # View rasters
print(head(target.PSUs))        # View tables
summary(all_tsus)               # Check results
```

### Option C: Resume from Checkpoint (After Crashes)

If script crashes or needs modification:

```r
# Section 7: Resume from saved PCA
cov.dat <- rast(paste0(results.path,"PCA_projected.tif"))

# Section 12: Skip optimization (if already calculated)
optimal_N_KLD <- readRDS(paste0(results.path,"../optimal_N_KLD.RDS"))

# Continue from any section as needed
```

---

## 📊 Understanding the Outputs

### Target PSUs (`results/[landuse]/PSUs_target.shp`)

| Field | Description |
|-------|-------------|
| `ID` | Original grid cell ID |
| `Replace_ID` | Cluster ID for finding replacements |
| `lulc` | Land use code (C/G/F) |

**Use case**: Overview map of sampling areas

---

### Target TSUs (`results/[landuse]/TSUs_target.shp`) ⭐ FIELD DATASET

| Field | Description | Example |
|-------|-------------|---------|
| `PSU_ID` | Primary sampling unit ID | `1` |
| `SSU_ID` | Secondary sampling unit (1-4=target, 5-8=replacement) | `1` |
| `SSU_Type` | Target or Replacement SSU | `Target` |
| `SSU_Repl` | Which target SSU this replaces (if replacement) | `1` |
| `TSU_ID` | Point ID within SSU | `1` |
| `TSU_Type` | Target or Alternative point | `Target` |
| `site_id` | **Unique site identifier** | `KANSAS0001-1-1C` |
| `lulc` | Land use code | `C` |
| `Replace_ID` | Backup PSU cluster ID | `85` |
| `geometry` | GPS coordinates | `POINT (...)` |

**Use case**: Load onto GPS for field navigation

---

### Merged Dataset (`results/all/all_tsus_target.shp`) ⭐⭐ MASTER FIELD DATASET

Same structure as above, but combines all land uses with country-wide unique IDs.

**Site ID Format**: `[COUNTRY][####]-[#]-[#][L]`
- `KANSAS0001-1-1C` = Kansas, PSU 1, SSU 1, TSU 1, Cropland
- `KANSAS0412-3-2G` = Kansas, PSU 412, SSU 3, TSU 2, Grassland

**Filter for field work**:
```r
# In QGIS or R, show only primary target sites:
field_sites <- all_tsus_target %>%
  filter(SSU_Type == "Target" & TSU_Type == "Target")
# Result: 4 sites per PSU (one per target SSU)
```

---

## 🐛 Troubleshooting

### Common Errors

#### "replacement has length zero"
**Cause**: Legacy points outside PSU grid

**Fix**:
```r
# Option 1: Remove legacy data
rm(legacy)  # Add before Section 13

# Option 2: Filter legacy to PSU extent
legacy <- st_filter(legacy, psu_grid)
```

---

#### "Mismatch in SSU and covariate rows"
**Cause**: MULTIPOLYGON geometry issues

**Fix**: Already handled in script (Section 15, geometry union step)

---

#### "PSU X has only Y usable SSUs"
**Cause**: Insufficient crop pixels

**Fix**:
```r
# Option 1: Lower crop threshold
percent_crop <- 15  # Section 2

# Option 2: Reduce pixel buffer
min_crop_pixels <- number_TSUs + 2  # Section 15 (line ~580)
```

---

### Performance Issues

#### Script too slow
```r
# Reduce iterations
iterations <- 5  # Section 2 (default: 10)

# Use coarser optimization steps
by.n <- 50  # Section 12 (default: 25)

# Reduce max sample size
final.n <- 1000  # Section 12 (default: 3000)
```

#### Out of memory
```r
# Add garbage collection after data loading
gc()
terra::tmpFiles(remove=TRUE)

# Reduce covariate resolution (re-download from GEE at 500m instead of 250m)
```

#### Section 12 taking days
```r
# Option 1: Skip and use predetermined value
optimal_N_KLD <- 200  # Your chosen sample size
saveRDS(optimal_N_KLD, paste0(results.path,"../optimal_N_KLD.RDS"))

# Option 2: Run on cluster/server with more CPUs
cpus <- 8  # Section 12 (default: 4)
```

---

## 📚 Methodology

This implementation follows:

- **Minasny & McBratney (2006)** - Conditioned Latin Hypercube sampling
- **Brus (2019)** - Digital soil mapping sampling tutorial
- **Walvoort et al. (2010)** - Spatial coverage sampling by k-means
- **Saurette et al. (2023)** - Divergence metrics for determining optimal training sample size in digital soil mapping

### Key Concepts

**Covariate Space Coverage (CSC)**:
- Ensures samples span full environmental gradient
- Uses k-means clustering in multi-dimensional covariate space
- Selects sites nearest to cluster centers

**Three-Stage Design**:
1. **PSUs** (2×2 km): Spatially distributed primary units
2. **SSUs** (100×100 m): Environmental clusters within PSUs
3. **TSUs** (points): Random samples within SSUs

**Why this approach?**:
- Balances spatial coverage with environmental diversity
- Reduces travel costs (clustered sampling)
- Provides replacements for inaccessible sites
- Incorporates legacy data seamlessly

---

## 💡 Advanced Usage

### Custom PSU/SSU Sizes

```r
# Section 2: Modify sizes
psu_size <- 3000   # 3×3 km PSUs
ssu_size <- 200    # 200×200 m SSUs (4 hectares)

# IMPORTANT: Ensure covariate resolution matches!
# PSU covariates should be ~psu_size/10 (e.g., 300m for 3km PSUs)
```

### Different Number of SSUs

```r
# Section 2: Change SSU configuration
num_primary_ssus <- 6        # 6 target SSUs
num_alternative_ssus <- 6    # 6 replacements

# Script will auto-adjust to 12 SSUs per PSU
```

### Skip Optimization

```r
# Section 12: Comment out optimization code
# opt_N_fcs <- opt_sample(...)

# Manually set sample size
optimal_N_KLD <- 300  # Your budget allows 300 PSUs
saveRDS(optimal_N_KLD, paste0(results.path,"../optimal_N_KLD.RDS"))
```

### Multiple Countries

Run script separately for each country, then merge:

```r
# After all countries complete:
country1 <- sf::st_read("country1/all/all_tsus_target.shp")
country2 <- sf::st_read("country2/all/all_tsus_target.shp")

# Renumber PSU_IDs to avoid conflicts
country2$PSU_ID <- country2$PSU_ID + max(country1$PSU_ID)

# Combine
global_samples <- rbind(country1, country2)
```

---

## 📖 Script Documentation

### Important Functions

#### `CSIS()` - Constrained k-means
```r
CSIS(fixed, nsup, nstarts, mygrd)
# fixed: Legacy data (data.frame with units column)
# nsup: Number of new samples to select
# nstarts: Random starts for optimization
# mygrd: Scaled covariate grid
```

#### `kmeans_with_progress()` - Standard k-means
```r
kmeans_with_progress(data, centers, iter.max, nstart)
# Wrapper around base::kmeans() with progress reporting
```

#### `generate_tsu_points_within_ssu()` - TSU sampling
```r
generate_tsu_points_within_ssu(ssu, number_TSUs, index, ssu_type, crops)
# ssu: Single SSU polygon (sf object)
# number_TSUs: How many points to generate
# index: SSU identifier
# ssu_type: "Target" or "Replacement"
# crops: Crop mask raster for constrained sampling
```

### Main Loop Structure (Section 15)

```r
for (psu_id in 1:nrow(target.PSUs)) {
  # 1. Generate 100m SSU grid
  # 2. Calculate crop percentage per SSU
  # 3. Filter SSUs by crop pixel count
  # 4. Extract 100m covariates
  # 5. Prepare data (scaling, remove NAs)
  # 6. Cluster SSUs using k-means
  # 7. Select target + replacement SSUs
  # 8. Generate TSU points
  # 9. Verify TSU generation success
  # 10. Store results
}
```

This loop processes each PSU independently, allowing for:
- Parallel processing (future enhancement)
- Resume after crashes (track `psu_id`)
- Graceful failure handling (skip problematic PSUs)

---

## ✅ Pre-Run Checklist

### Data Preparation
- [ ] Study area boundary in `01_data/module2/shapes/`
- [ ] Environmental covariates in `rasters/GEE_Exports/`
- [ ] Land use masks created (20m resolution)
- [ ] All spatial data in EPSG:4326
- [ ] File paths updated in script

### Script Configuration
- [ ] `ISO.code` updated (Section 2)
- [ ] `epsg` set to correct UTM zone (Section 2)
- [ ] `landuse` variable set (Section 2)
- [ ] File paths match your data (Sections 4-8)
- [ ] Legacy data removed if not available (Section 4)

### System Requirements
- [ ] R version ≥ 4.0
- [ ] All packages installed (see main README)
- [ ] At least 8GB RAM available
- [ ] 50GB free disk space for temp files

---

## 🔄 Workflow Summary

```
COMPLETE WORKFLOW (Total time: ~30-50 hours)

┌────────────────────────────────────────────────────────────────┐
│ STEP 1: Generate Rasters from GEE                              │
│ ────────────────────────────────────────────────────────────── │
│ Script: gee_soilfer_env_covariates.txt                         │
│ Time: 30 min - 3 hours                                         │
│ Action:                                                        │
│   1. Edit region definition in GEE code                        │
│   2. Run script in GEE Code Editor                             │
│   3. Start all exports from Tasks tab                          │
│   4. Download from Google Drive → place in GEE_Exports/        │
│                                                                │
│ Output: 9 raster files (covariates, climate, masks)            │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ STEP 2: Process Climate & Generate Newhall                     │
│ ────────────────────────────────────────────────────────────── │
│ Script: 1_climate_soil_covariates.R                            │
│ Time: 6-8 hours                                                │
│ Action:                                                        │
│   1. Download AWC raster from SoilGrids                        │
│   2. Update file paths in script                               │
│   3. Run entire script                                         │
│   4. Create additional land use masks (grassland, forest)      │
│                                                                │
│ Output: newhall.tif, climate_vars.tif, merged tiles            │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ STEP 3A: Run Sampling Design for CROPLAND                      │
│ ────────────────────────────────────────────────────────────── │
│ Script: sampling_design_csc_soilfer.R                          │
│ Time: 4-12 hours                                               │
│ Config:                                                        │
│   landuse <- "cropland"                                        │
│   landuse_file <- "Cropland_Mask_AREA_merged.tif"              │
│   n.psu <- round(optimal_N_KLD * 0.85, 0)                      │
│                                                                │
│ Run: Sections 1-23                                             │
│ Output: results/cropland/PSUs_target.shp, TSUs_target.shp      │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ STEP 3B: Run Sampling Design for GRASSLAND                     │
│ ────────────────────────────────────────────────────────────── │
│ Script: sampling_design_csc_soilfer.R                          │
│ Time: 4-12 hours                                               │
│ Config:                                                        │
│   landuse <- "grassland"                                       │
│   landuse_file <- "Grassland_Mask_AREA_merged.tif"             │
│   n.psu <- round(optimal_N_KLD * 0.10, 0)                      │
│                                                                │
│ Run: Sections 1-23                                             │
│ Output: results/grassland/PSUs_target.shp, TSUs_target.shp     │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ STEP 3C: Run Sampling Design for FOREST                        │
│ ────────────────────────────────────────────────────────────── │
│ Script: sampling_design_csc_soilfer.R                          │
│ Time: 4-12 hours                                               │
│ Config:                                                        │
│   landuse <- "forest"                                          │
│   landuse_file <- "Forest_Mask_AREA_merged.tif"                │
│   n.psu <- round(optimal_N_KLD * 0.05, 0)                      │
│                                                                │
│ Run: Sections 1-23                                             │
│ Output: results/forest/PSUs_target.shp, TSUs_target.shp        │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ STEP 4: Merge All Land Uses                                    │
│ ────────────────────────────────────────────────────────────── │
│ Script: sampling_design_csc_soilfer.R (Sections 24-34 ONLY)    │
│ Time: 5-10 minutes                                             │
│ Action:                                                        │
│   1. Open NEW R session                                        │
│   2. Source script and run ONLY Sections 24-34                 │
│   3. Creates unified IDs across all land uses                  │
│                                                                │
│ Output: results/all/                                           │
│   ├─ all_psus_target.shp                                       │
│   ├─ all_tsus_target.shp ⭐ MAIN FIELD DATASET                 │
│   ├─ all_psus_replacements.shp                                 │
│   ├─ all_tsus_replacements.shp                                 │
│   ├─ final_site_distribution.png                               │
│   └─ final_site_distribution_province.png                      │
└────────────────────────────────────────────────────────────────┘
                              ↓
                    ✅ READY FOR FIELD WORK!

TOTAL SAMPLING SITES:
- Cropland: ~350 PSUs × 4 target SSUs = ~1,400 sites
- Grassland: ~40 PSUs × 4 target SSUs = ~160 sites  
- Forest: ~20 PSUs × 4 target SSUs = ~80 sites
─────────────────────────────────────────────────────
TOTAL: ~410 PSUs with ~1,640 field sampling locations
```

---

## 📞 Support

### Getting Help
1. Check console error messages
2. Review this README and troubleshooting section
3. Inspect intermediate outputs: `plot(raster)`, `print(head(data))`
4. Test with small area first (subset study region)

### Common Questions

**Q: How long should Section 12 take?**  
A: 2-24 hours depending on area. Result is cached for reuse.

**Q: Can I run sections out of order?**  
A: No - sections depend on previous outputs. Run sequentially.

**Q: What if some PSUs fail?**  
A: Normal! Script tracks `skipped_psus`. Typically 5-15% are skipped.

**Q: Can I modify PSU/SSU sizes?**  
A: Yes, but ensure covariate resolutions match new sizes.

---

## 📄 File Exports

All output shapefiles use **WGS84 (EPSG:4326)** for GPS compatibility.

**To convert to UTM for GIS analysis**:
```r
# In R
library(sf)
data <- st_read("all_tsus_target.shp")
data_utm <- st_transform(data, crs = "EPSG:32615")  # Your UTM zone
st_write(data_utm, "all_tsus_target_utm.shp")
```

**To export to GPX for GPS**:
```
# In QGIS
Right-click layer → Export → Save Features As... → GPX
Use site_id as waypoint name
```

---

## 🎓 Learning Path

### Beginner
1. Run with provided Kansas example data
2. Inspect outputs at each section
3. Understand PSU selection (Section 13)
4. Learn TSU generation (Section 15)

### Intermediate
1. Prepare data for your study area
2. Modify sampling parameters
3. Customize exclusion criteria
4. Integrate additional covariates

### Advanced
1. Modify CSC algorithm
2. Implement parallel processing
3. Add custom QC checks
4. Integrate with field data collection apps

