# Three-Stage Sampling Design for Soil Survey

## 📋 Overview

This folder contains the **provided spatial data** for Module 2. These files serve as examples and templates for your own sampling design projects.

**📦 What's Provided**
- ✅ Shapefiles (Kansas, USA study area):
  - Region of interest boundary
  - Soil legacy data points
  - Ecoregions
  - Protected areas

**🔨 What You Must Generate:**
  - All raster files (using provided scripts in `03_scripts/module2`)
  - Environmental covariates from Google Earth Engine
  - Climate data processing (Newhall model)
  - Land use masks

**⚠️ Data Generation Workflow:**

```
Step 1: Run GEE script → Download 9 rasters from Google Drive
Step 2: Run climate processing script → Generate newhall.tif + merge tiles
Step 3: Create land use masks → 3 binary rasters (cropland, grassland, forest)
```

See `03_scripts/module2/README.md` for complete instructions.

---

## 📁 Folder Structure

```
01_data/module2/
│
├── shapes/                                    # Vector data (PROVIDED)
│   ├── Metadata_WDPA_protected_areas_in_English/  # Metadata folder
│   ├── ecoregions_kansas_epsg_4326.*         # Ecoregions/geology
│   ├── metadata_administrative_boundaries.txt # Metadata file
│   ├── metadata_ecoregions.txt               # Metadata file
│   ├── protected_areas_epsg_4326.*           # Protected areas to exclude
│   ├── roi_kansas_us_epsg_4326.*             # Study area boundary
│   └── soil_legacy_data_kansas_epsg_4326.*   # Existing soil samples
│
├── rasters/                                   # Raster data (YOU CREATE)
│   ├── GEE_Exports/                          # From Google Earth Engine
│   │   ├── Environmental_Covariates_250m_KANSAS.tif
│   │   ├── HighRes_Covariates_100m_KANSAS_merged.tif
│   │   └── Geomorphon_Landforms_KANSAS.tif
│   ├── Cropland_Mask_KANSAS_merged.tif
│   ├── newhall.tif
│   └── slope_mask_epsg_4326.tif
│
└── results/                                   # Outputs (SCRIPT GENERATED)
    ├── cropland/
    ├── grassland/
    ├── forest/
    └── all/

```

---

## 📦 Provided Shapefiles

All shapefiles in the `shapes/` folder are **provided as examples** using Kansas, USA as the study area.

### 1. Study Area Boundary (REQUIRED)
**Files**: `roi_kansas_us_epsg_4326.*`

- **Description**: Polygon defining the study region
- **CRS**: EPSG:4326 (WGS84 Geographic)
- **Purpose**: Defines the sampling universe
- **Usage**: The script will clip all data to this boundary and reproject to your target CRS

**For your project**: Replace with your own study area polygon

---

### 2. Soil Legacy Data (OPTIONAL)
**Files**: `soil_legacy_data_kansas_epsg_4326.*`

- **Description**: Point locations of existing soil observations
- **CRS**: EPSG:4326 (WGS84 Geographic)
- **Purpose**: Ensures new samples complement existing data
- **Usage**: Script uses constrained clustering to integrate legacy points

**For your project**:
- Include if you have existing soil data
- Delete or comment out legacy code if not available

---

### 3. Ecoregions or Geology (OPTIONAL)
**Files**: `ecoregions_kansas_epsg_4326.*`

- **Description**: Polygon shapefile of ecological regions
- **CRS**: EPSG:4326 (WGS84 Geographic)
- **Field**: `US_L3NAME` (ecoregion names)
- **Purpose**: Adds environmental stratification to sampling design
- **Usage**: Converted to dummy variables for PSU selection

**For your project**:
- Use geology, ecoregions, or soil associations
- Ensure polygon layer has classification field
- Update field name in script (Section 5): `geo.classes <- "YOUR_FIELD_NAME"`

---

### 4. Protected Areas (OPTIONAL)
**Files**: `protected_areas_epsg_4326.*`

- **Description**: Polygons of areas to EXCLUDE from sampling
- **CRS**: EPSG:4326 (WGS84 Geographic)
- **Purpose**: Removes inaccessible areas (national parks, reserves, military zones)
- **Usage**: Script creates "non-protected area" mask for sampling

**For your project**:
- Download from [World Database on Protected Areas (WDPA)](https://www.protectedplanet.net/)
- Or create custom exclusion zones (urban areas, water bodies, etc.)
- Delete if no exclusions needed

---

## 🗂️ Folders You Need to Create

The following folders are **NOT provided** - you must create them and populate with your data:

### 1. `rasters/` Folder

**You will generate these rasters** using the provided scripts in two steps:

---

#### **STEP 1: Generate Rasters from Google Earth Engine**

Use the provided script: `03_scripts/module2/gee_soilfer_env_covariates.txt`

**What it generates** (automatically exported to your Google Drive):

1. **Environmental_Covariates_250m_[AREA].tif** (PSU-level)
   - CHELSA climate variables (bio1, bio12, bio5, etc.)
   - MODIS vegetation indices (NDVI, FPAR, LST by quarter)
   - OpenLandMap terrain derivatives
   - **Resolution**: 250m
   - **Use**: PSU selection via environmental clustering

2. **HighRes_Covariates_100m_[AREA].tif** (SSU-level)
   - SRTM terrain (elevation, slope, aspect, TPI, TWI)
   - Sentinel-2 spectral bands (B2-B8, B11, B12)
   - Sentinel-2 indices (NDVI, EVI, NDBSI, NBRplus)
   - **Resolution**: 100m
   - **Use**: SSU clustering within PSUs

3. **Cropland_Mask_[AREA].tif** (Land use mask)
   - Binary mask from ESA WorldCover (1=cropland, 0=other)
   - **Resolution**: 20m
   - **Use**: Defines sampling universe

4. **TerraClimate Data** (1981-2023)
   - `TerraClimate_AvgTemp_1981_2023_[AREA].tif` (monthly temperature)
   - `TerraClimate_Precip_1981_2023_[AREA].tif` (monthly precipitation)
   - `TerraClimate_PET_1981_2023_[AREA].tif` (potential evapotranspiration)
   - **Resolution**: ~4km
   - **Use**: Input for Newhall soil-climate model

5. **Geomorphon_Landforms_[AREA].tif**
   - Landform classification (10 classes: flat, peak, ridge, etc.)
   - **Resolution**: 90m

6. **DEM_[AREA].tif** and **Slope_[AREA].tif**
   - SRTM elevation and slope
   - **Resolution**: 100m

7. **ESA_WorldCover_2021_[AREA].tif**
   - Full land cover classification
   - **Resolution**: 10m

**How to run**:
1. Open [Google Earth Engine Code Editor](https://code.earthengine.google.com/)
2. Copy/paste code from `gee_soilfer_env_covariates.txt`
3. Edit Section 1 (define your region)
4. Run script → exports start automatically
5. Download from Google Drive → place in `rasters/GEE_Exports/`

---

#### **STEP 2: Process Climate Data and Generate Newhall**

Use the provided script: `03_scripts/module2/1_climate_soil_covariates.R`

**What it does**:
1. Processes TerraClimate temperature and precipitation
2. Calculates monthly averages across all years (1981-2023)
3. Combines with DEM and available water capacity (AWC) data
4. Runs **jNSM Newhall model** to compute soil-climate regimes
5. Merges tiled GEE exports (if data was exported in multiple tiles)

**What it generates**:

1. **newhall.tif** (Soil climate regimes)
   - Temperature regime (e.g., mesic, thermic, frigid)
   - Moisture regime (e.g., udic, ustic, xeric)
   - Water balance metrics
   - Days dry/moist statistics
   - **Use**: Environmental covariate for PSU selection

2. **climate_vars.tif** (Processed climate)
   - Monthly temperature (12 layers)
   - Monthly precipitation (12 layers)
   - **Use**: Intermediate file for Newhall

3. **Merged tiles** (if GEE exported multiple tiles):
   - `Cropland_Mask_[AREA]_merged.tif`
   - `HighRes_Covariates_100m_[AREA]_merged.tif`
   - `ESA_WorldCover_2021_[AREA]_merged.tif`

**Requirements**:
- TerraClimate data from GEE (Step 1)
- DEM from GEE (Step 1)
- Available Water Capacity (AWC) raster
  - Download from [SoilGrids](https://soilgrids.org/) (0-200cm depth)
  - Place in `rasters/` folder

**Runtime**: 6-8 hours (Newhall model is computationally intensive)

---

#### **Additional Land Use Masks**

For grassland and forest sampling, create additional masks:

**Option A: Using ESA WorldCover** (recommended)
```r
# In R (after running climate script)
library(terra)

# Load ESA WorldCover
esa <- rast("rasters/ESA_WorldCover_2021_AREA_merged.tif")

# Create masks
cropland <- esa == 40  # Class 40 = cropland
grassland <- esa == 30  # Class 30 = grassland
forest <- esa == 10     # Class 10 = tree cover

# Export
writeRaster(cropland, "rasters/Cropland_Mask_AREA_merged.tif")
writeRaster(grassland, "rasters/Grassland_Mask_AREA_merged.tif")
writeRaster(forest, "rasters/Forest_Mask_AREA_merged.tif")
```

**Option B: Using QGIS Raster Calculator**
```
# For grassland
("ESA_WorldCover@1" = 30) * 1

# For forest
("ESA_WorldCover@1" = 10) * 1
```

---

#### **Optional: Slope Mask**

Create accessibility mask to exclude steep areas:

**In QGIS**:
```
("Slope@1" <= 30) * 1
```
Save as: `rasters/slope_mask_epsg_4326.tif`

**Threshold guidelines**:
*Evaluate the Slope variation in your region of interest*
- Gentle terrain: 0-15% (agricultural machinery)
- Moderate terrain: 15-30% (hand tools possible)
- Steep terrain: >30% (exclude from sampling)

---

### 2. `results/` Folder

**Auto-created by scripts** - do not create manually

The sampling design script will automatically create:

```
results/
├── cropland/
│   ├── PSUs_target.shp              # Selected PSUs
│   ├── TSUs_target.shp              # Sampling points
│   ├── PSUs_replacements.shp        # Backup PSUs
│   ├── TSUs_replacements.shp        # Backup points
│   ├── PCA_projected.tif            # Principal components
│   └── clusters.tif                 # Environmental clusters
│
├── grassland/
│   └── (same structure)
│
├── forest/
│   └── (same structure)
│
└── all/
    ├── all_psus_target.shp          # Merged PSUs (all land uses)
    ├── all_tsus_target.shp          # **FIELD DATASET** (all sites)
    ├── all_psus_replacements.shp
    ├── all_tsus_replacements.shp
    ├── final_site_distribution.png
    └── final_site_distribution_province.png
```

---

## 🔄 Adapting for Your Study Area

### Step 1: Replace Boundary
1. Export your study area as shapefile in EPSG:4326
2. Replace `roi_kansas_us_epsg_4326.shp` with your file
3. Update filename in script (Section 4)

### Step 2: Update Coordinate System
Determine your appropriate UTM zone:
- Find your study area's UTM zone [here](https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-)
- Update in script Section 2: `epsg <- "EPSG:32615"` (example for UTM 15N)

### Step 3: Prepare Environmental Covariates
1. Sign up for [Google Earth Engine](https://earthengine.google.com/)
2. Use the provided GEE script template (`03_scripts/module2/gee_soilfer_env_covariates.txt`)
3. Modify study area boundary in GEE code
4. Export to Google Drive
5. Download and place in `rasters/GEE_Exports/`

### Step 4: Create Land Use Masks
1. Download land cover data (ESA WorldCover recommended)
2. Open in QGIS
3. Use Raster Calculator to create binary masks
4. Save as 20m resolution GeoTIFF in `rasters/`

### Step 5: Add Optional Data
- Include legacy data if available
- Add protected areas from WDPA
- Create slope mask if terrain is steep

---

## 📊 Data Format Requirements

### Shapefiles
- **CRS**: EPSG:4326 (will be reprojected by script)
- **Geometry**: Valid (no self-intersections)
- **Encoding**: UTF-8

### Rasters
- **Format**: GeoTIFF (.tif)
- **NoData**: Properly defined (typically -9999 or NA)
- **CRS**: EPSG:4326 or same as study area
- **Compression**: None or LZW (for compatibility)

---

## 📖 Metadata Files

Included text files provide detailed information about data sources:

- `metadata_administrative_boundaries.txt`: Info on ROI shapefile
- `metadata_ecoregions.txt`: Info on ecoregion classification
- `Metadata_WDPA_protected_areas_in_English/`: Protected areas documentation

---

## ⚠️ Important Notes

1. **File Naming Convention**:
   - Keep consistent naming
   - Use underscores (not spaces)
   - Include area name: `roi_[yourarea]_epsg_4326.shp`
   - Match filenames in script

3. **CRS Consistency**: 
   - Start with EPSG:4326 (lat/lon)
   - Script handles reprojection to UTM
   - Don't mix projected and geographic CRS

4. **Resolution Guidelines**:
   - Land use mask: 20m (for TSU accuracy)
   - SSU covariates: 100m
   - PSU covariates: 250m-2km

5. **File Sizes**:
   - Large rasters (>1GB) may cause memory issues
   - Consider tiling or reducing extent for testing
   - Use cloud-optimized GeoTIFF format

---

## 🆘 Troubleshooting

**Problem**: "Cannot read shapefile"
- **Solution**: Check file permissions, encoding, and geometry validity in QGIS

**Problem**: "CRS mismatch errors"
- **Solution**: Reproject all data to EPSG:4326 before running script

**Problem**: "Out of memory"
- **Solution**: Reduce raster resolution or study area extent

**Problem**: "Empty raster after masking"
- **Solution**: Verify study area overlaps with covariate data

---

## 📚 Data Sources

### Recommended Global Datasets

**Boundaries**:
- [GADM](https://gadm.org/) - Administrative boundaries
- [Natural Earth](https://www.naturalearthdata.com/) - Country boundaries

**Land Cover**:
- [ESA WorldCover](https://worldcover.org/) - 10m resolution, 2020-2021
- [MODIS Land Cover](https://lpdaac.usgs.gov/products/mcd12q1v006/) - 500m resolution
- [Copernicus Global Land Cover](https://land.copernicus.eu/global/) - 100m resolution

**Climate**:
- [WorldClim](https://www.worldclim.org/) - Bioclimatic variables
- [CHELSA](https://chelsa-climate.org/) - High-resolution climate

**Terrain**:
- [SRTM 30m](https://earthexplorer.usgs.gov/) - Global DEM
- [ALOS World 3D](https://www.eorc.jaxa.jp/ALOS/en/aw3d30/) - 30m DEM

**Soil**:
- [SoilGrids](https://soilgrids.org/) - 250m global soil properties
- [OpenLandMap](https://openlandmap.org/) - Various soil layers

**Protected Areas**:
- [WDPA](https://www.protectedplanet.net/) - World Database on Protected Areas

---

## ✅ Checklist Before Running Scripts

- [ ] `shapes/` folder contains study area boundary
- [ ] `rasters/GEE_Exports/` folder created and populated
- [ ] Land use masks created (cropland, grassland, forest)
- [ ] All shapefiles in EPSG:4326
- [ ] All rasters have proper NoData values
- [ ] File names match those specified in script
- [ ] Legacy data prepared (or legacy code commented out)
- [ ] Protected areas added (or exclusion code commented out)

---

## 📞 Support

For questions about data preparation:
- Review the main project README in `training_material/`
- Check script comments in `03_scripts/module2/`
- Validate data in QGIS before running scripts

