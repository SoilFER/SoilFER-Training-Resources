###############################################################################
# SoilFER Online Training Programme — Module 1
# SESSION 4: Spatial Analysis + Preparation of Covariates for Sampling Design
#            and Digital Soil Mapping (1.5 hours)
# Source: Module1-B.txt (Sections: Spatial Analysis, Vector data with sf,
#                                   Raster data with terra, Newhall NSM)
###############################################################################
#
# LEARNING OBJECTIVES
# -------------------
# By the end of this session, participants will be able to:
#   1. Understand the difference between vector and raster spatial data
#   2. Install and load {sf} and {terra} packages
#   3. Create spatial point objects from CSV data
#   4. Import, inspect, and export shapefiles and spatial objects
#   5. Set and reproject coordinate reference systems (CRS)
#   6. Perform geometry operations: buffer, clip, dissolve, spatial join
#   7. Visualize spatial data with base R and {ggplot2}
#   8. Import, inspect, and visualize raster data
#   9. Perform raster operations: mosaic, crop, mask, resample, algebra
#  10. Extract raster covariate values at soil sample locations
#  11. Run the Newhall Simulation Model (NSM) to derive soil-climate covariates
#
# PREREQUISITE
# ------------
# Session 3 outputs must be available. Specifically:
#   - KSSL_DSM_0-30.csv (or .xlsx) in 03_outputs/module1/
#   - Raster files in 01_data/module2/rasters/ (downloaded from Google Drive)
#   - Shapefile Tiger_2020_Counties.shp in 01_data/module1/shapes
#
# TIMING GUIDE (approximate)
# ---------------------------
#  0:00 – 0:30  Vector data: sf object creation, CRS, geometry operations,
#               buffering, clipping, dissolving, spatial join, visualization
#  0:30 – 1:10  Raster data: read/inspect/plot rasters, mosaic, crop, mask,
#               resample, combine stacks, raster algebra, extract at points
#  1:10 – 1:30  Newhall Simulation Model (NSM): climate averaging,
#               input stack preparation, running NSM, exporting results
###############################################################################


# =============================================================================
# PART 1 — SPATIAL PACKAGES: INSTALLATION AND LOADING
# =============================================================================

  install.packages("sf")
  install.packages("terra")

  library(sf)
  library(terra)

  # Define the folder to store the results of the exercise
  output_dir <-"03_outputs/module1/"
  
  # Define the relative path to the folder with the downloaded covariates
  rasters_dir <-"../GEE_Exports/"

# =============================================================================
# PART 2 — WORKING WITH VECTOR DATA USING {sf}
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1  Creating Spatial Vector Objects from CSV Files
# -----------------------------------------------------------------------------
# Convert the DSM-ready table (with lon/lat) to an sf POINT object.

# Import the previously standardized dataset for 0-30 cm depth 
kdata <- read.csv (paste0(output_dir,"KSSL_DSM_0-30.csv"))
# Convert to sf: create POINT geometry from lon/lat, set CRS = WGS84 (EPSG:4326)
soil_sf <- st_as_sf(kdata, coords = c("lon", "lat"), crs = 4326)
# Print object geometry
soil_sf$geometry
# Print CRS details
st_crs(soil_sf)

# -----------------------------------------------------------------------------
# 2.2  Importing Shapefiles
# -----------------------------------------------------------------------------

# Read the previously created shapefile containing soil profile points
soil_sf <- st_read(paste0(output_dir,"soil_profiles.shp"))
# Inspect the first rows
head(soil_sf)
# Quick visualization
plot(soil_sf["pH"], pch = 16, cex = 0.6, key.pos = 1)  # key.pos controls legend position

# -----------------------------------------------------------------------------
# 2.3  Exporting sf Objects
# -----------------------------------------------------------------------------

# Export as shapefile
st_write(soil_sf, paste0(output_dir,"soil_profiles.shp"), delete_layer = TRUE) # overwrites shp

# -----------------------------------------------------------------------------
# 2.4  Setting and Reprojecting CRS for Vector Data
# -----------------------------------------------------------------------------
# Always confirm the CRS of spatial data before operations.
# st_crs()      → inspect CRS
# st_set_crs()  → assign a missing CRS (no coordinate transformation)
# st_transform() → reproject to a different CRS (changes coordinate values)

sf_3857 <- st_transform(soil_sf, 3857)

# Preview of the projected object
sf_3857

# -----------------------------------------------------------------------------
# 2.5  Inspecting and Manipulating {sf} Objects
# -----------------------------------------------------------------------------

# Check structure
str(soil_sf)
# Summary of attributes + geometry
summary(soil_sf)
# Extract the geometry column
st_geometry(soil_sf)
# Check the geometry type. Use head to avoid long printing
head(st_geometry_type(soil_sf))
# Check the CRS:
st_crs(soil_sf)
# Check the spatial extent of the object:
st_bbox(soil_sf)

# Subset sf rows and columns using the pipping (%>%) operator with {dplyr}
alkaline <- soil_sf %>%
  filter(pH > 7.4) %>%
  select(ProfID, pH) %>%
  mutate(alkaline = TRUE)
# Preview alkaline data
alkaline


# =============================================================================
# PART 3 — GEOMETRY OPERATIONS
# =============================================================================
# NOTE: Distance and area-based operations require a Projected CRS (metres).
#       Always ensure both layers share the same CRS before operations.

# -----------------------------------------------------------------------------
# 3.1  Buffering Plots (Influence Area)
# -----------------------------------------------------------------------------

# Original KSSL data is in WGS84 EPSG:4326  geographic coordinates(lon/lat)
# Transform the subset of alkaline soils to NAD83 / UTM 14N (EPSG: 26914)
alkaline_utm <- st_transform(alkaline, crs = 26914)  # UTM 14N, distance in metres

# Create a 1 km buffer around each plot
plots_buffer_1k <- st_buffer(alkaline_utm, dist = 1000)

# Plot buffers (first 3 plots) and then overlay the same points
plot(st_geometry(plots_buffer_1k[1:3, ]), 
     col = rgb(0, 0, 1, 0.3), border = "blue",
     main = "Plot of first 3 alkaline soils with 1 km buffers")
# Overlay the original sample locations
plot(st_geometry(alkaline_utm[1:3, ]), 
     add = TRUE, pch = 16, col = "red")

# -----------------------------------------------------------------------------
# 3.2  Clip Soil Data to Defined Boundaries (Example 1: Riley County)
# -----------------------------------------------------------------------------

# Read administrative boundaries (example file)
admin <- st_read("01_data/module1/shapes/Tiger_2020_Counties.shp")
# Ensure both layers share the same CRS
admin <- st_transform(admin, st_crs(soil_sf))
# Example: select one unit (replace with your column/value)
study_area <- admin[admin$NAME == "Riley", ]
# Keep only points inside the study area
soil_clip <- st_intersection(soil_sf, study_area)
# Plot result
plot(st_geometry(study_area), col = NA, border = "black", main="Sampled soils in Riley County", cex.main = 1 )
plot(st_geometry(soil_clip), add = TRUE, pch = 16, col = "red", cex = 0.6)

# -----------------------------------------------------------------------------
# 3.3  Dissolve Polygons to Create a Single Study Region Boundary (Example 2)
# -----------------------------------------------------------------------------

# Dissolve all counties into one Kansas boundary
admin_dissolved <- st_union(admin)
# Plot comparison: before vs after dissolve
# Adjust graphics settings to 1 row x 2 columns layout with tight margins
op <- par(
  mfrow = c(1, 2),
  mar = c(0.5, 0.5, 0.1, 0.5),  # very small top margin
  xaxs = "i", yaxs = "i"
)
plot(st_geometry(admin[1]),
     col = NA, border = "black",
     axes = FALSE, asp = 1)
title("Before dissolve", line = -0.6, cex.main = 0.95)
plot(st_geometry(admin_dissolved),
     col = NA, border = "black",
     axes = FALSE, asp = 1)
title("After dissolve", line = -0.6, cex.main = 0.95)
# restore graphics settings
par(op)

# -----------------------------------------------------------------------------
# 3.4  Spatial Relationships and Joins
# -----------------------------------------------------------------------------

# Ensure both layers share the same CRS
alkaline <- st_transform(alkaline, st_crs(admin))
# Join admin attributes to points (points inherit polygon attributes)
alkaline_with_admin <- st_join(alkaline, admin, join = st_within)
# Preview result
head(alkaline_with_admin)

# -----------------------------------------------------------------------------
# 3.5  Visualizing {sf} Objects with {ggplot2}
# -----------------------------------------------------------------------------

library(ggplot2)
ggplot(data = soil_sf) +
  geom_sf(aes(color = pH), size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Soil sampling sites – pH",
    color = "pH"
  )


# =============================================================================
# PART 4 — WORKING WITH RASTER DATA USING {terra}
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1  Importing and Exporting Raster Data
# -----------------------------------------------------------------------------

library(terra)
# Read a single-layer raster (e.g., cropland mask or DEM)
crops <- rast(paste0(rasters_dir,"Cropland_Mask_KANSAS.tif"))
# Read a multi-layer raster (e.g., environmental covariates stack)
covs <- rast(paste0(rasters_dir,"Environmental_Covariates_250m_KANSAS.tif"))
# Export a raster as GeoTIFF
writeRaster(crops, "03_outputs/module1/Cropland_Mask_KANSAS.tif", overwrite = TRUE) # overwrites tif

# -----------------------------------------------------------------------------
# 4.2  Inspecting and Exploring SpatRaster Objects
# -----------------------------------------------------------------------------

# Basic metadata (prints summary to console)
covs
# Number of layers
nlyr(covs)
# Spatial properties
ext(covs)
res(covs)
crs(covs)
# Layer names (multi-layer rasters)
names(covs)
# Value ranges for the first 4 layers
global(covs[[1:4]], fun = "range", na.rm = TRUE)
# Histograms of the first 4 layers
# Adjust graphics settings to 2 row x 2 columns layout with tight margins
op <- par(
  mfrow = c(2, 2),
  mar  = c(3.5, 3.5, 2.2, 1.2),  # more space around each panel
  oma  = c(1, 1, 1, 1),          # extra space around the whole figure
  mgp  = c(2.2, 0.7, 0)          # axis title/labels spacing
)

for (i in 1:4) {
  hist(covs[[i]],
       main = paste("Histogram of", names(covs)[i]),
       xlab = names(covs)[i])
}
# restore previous graphics settings
par(op)

# -----------------------------------------------------------------------------
# 4.3  Raster Visualization
# -----------------------------------------------------------------------------

# Plot single-layer crops raster mask
plot(crops, main="Cropland Mask")

# Plot a multi-layer raster (e.g., environmental covariates stack)
plot(covs)

# Plot Elevation and Temperature from the covs stack
# Adjust graphics settings to 1 row x 2 columns layout with tight margins
op <- par(
  mfrow = c(1, 2),
  mar = c(0.5, 0.5, 0.5, 0.5),
  xaxs = "i", yaxs = "i"
)
plot(covs[["dtm_elevation_250m"]], main = "Elevation (DEM)", cex.main = .8)
plot(covs[["bio1"]], main = "Annual Mean Temperature", cex.main = .8)
par(op) # restore previous graphics settings

# -----------------------------------------------------------------------------
# 4.4  Setting and Reprojecting CRS for Raster Data
# -----------------------------------------------------------------------------

# Inspect CRS
crs(covs)
# Reproject raster (example: to EPSG:3857)
covs_3857 <- project(covs, "EPSG:3857")


# =============================================================================
# PART 5 — SPATIAL OPERATIONS ON RASTERS
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1  Merge Rasters (Mosaic)
# -----------------------------------------------------------------------------
# Join adjacent raster tiles into a single continuous layer.

# Read input rasters (adjacent tiles)
r1 <- rast(paste0(rasters_dir,"Cropland_Mask_KANSAS-0000000000-0000000000.tif"))
r2 <- rast(paste0(rasters_dir,"Cropland_Mask_KANSAS-0000000000-0000023296.tif"))
# Mosaic into a single raster
m <- mosaic(r1, r2, fun = "max")
# Cast to integer to optimize file size (not valid for non-integer continuous data)
m <- as.int(m)
# Write output with compression to reduce file size
writeRaster(m, "03_outputs/module1/Cropland_Mask_KANSAS.tif", overwrite = TRUE,
  wopt = list(datatype = "INT1U",  # Byte (0–255); NA retained as NoData
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
  )
)

# -----------------------------------------------------------------------------
# 5.2  Crop and Mask to Study Area
# -----------------------------------------------------------------------------
# crop() reduces raster to bounding box of a polygon.
# mask() sets values outside the polygon to NA.
# Use both together: crop() first, then mask().

# Crop and mask covariates to the extent of Riley
# Align CRS of both datasets
admin <- st_transform(admin, crs(covs))
# Crop covariates to the extent of the county
covs_crop <- crop(covs, admin[admin$NAME=="Riley",])
# Mask covariates
covs_mask <- mask(covs_crop, admin[admin$NAME=="Riley",])

# Plot covariate (bio1 = temperature) in Riley County
# Adjust graphics settings to 1 row x 2 columns layout with tight margins
op <- par(
  mfrow = c(1, 2),
  mar = c(0.5, 0.5, 0.5, 0.5),
  xaxs = "i", yaxs = "i"
)
plot(covs_crop[["bio1"]], main= "Crop", cex.main = 1)
plot(covs_mask[["bio1"]], main= "Mask", cex.main = 1)
# restore previous graphics settings
par(op) 

# -----------------------------------------------------------------------------
# 5.3  Resampling and Alignment
# -----------------------------------------------------------------------------
# resample() transfers values from one SpatRaster to the grid of another.
# Use method = "near" for categorical rasters; "bilinear" for continuous.

# Align raster of crops to the grid of covariates
crops_aligned <- resample(crops, covs, method = "near")
# Spatial properties of aligned crops and covariates are equal
ext(crops_aligned)
ext(covs)
res(covs)
res(crops_aligned)

# -----------------------------------------------------------------------------
# 5.4  Combine and Remove Rasters in a Raster Stack
# -----------------------------------------------------------------------------

# Set a clear layer name for the aligned cropland mask
names(crops_aligned) <- "crops"
# Add the cropland layer to the covariate stack
covs <- c(covs,crops_aligned)
# Check layer names
names(covs)

# Remove "crops" from the stack
covs <- covs[[names(covs) != "crops"]]
# Check layer names
names(covs)

# -----------------------------------------------------------------------------
# 5.5  Raster Algebra
# -----------------------------------------------------------------------------
# Apply cell-by-cell mathematical or logical operations on rasters.
# Rasters must share the same CRS, origin, resolution, and extent.

# Example: classify a continuous covariate into two classes
high_temperature <- covs[[1]] > 12
# Adjust graphics settings to 1 row x 2 columns layout with tight margins
op <- par(
  mfrow = c(1, 2),
  mar = c(0.5, 0.5, 0.5, 0.5),
  xaxs = "i", yaxs = "i"
)
plot(covs[["bio1"]], main= "Annual Mean Temperature", cex.main = .8)
plot(high_temperature, main= "High Temperature (>12ºC)", cex.main = .8)
par(op) # restore previous graphics settings

# -----------------------------------------------------------------------------
# 5.6  Merging GEE-Exported Tiles (Practical Example)
# -----------------------------------------------------------------------------
# Google Earth Engine exports large rasters as tiles.
# Merge Crop mask rasters (two adjacent tiles)
# Read input rasters
r1 <- rast(paste0(rasters_dir,"Cropland_Mask_KANSAS-0000000000-0000000000.tif"))
r2 <- rast(paste0(rasters_dir,"Cropland_Mask_KANSAS-0000000000-0000023296.tif"))
# Mosaic into a single raster
m <- mosaic(r1, r2, fun = "max")
# Cast to integer to optimize file size
# This ensures the output is written as integer values (1 and NA),
m <- as.int(m)
# Write output with compression to reduce file size
writeRaster(m, "03_outputs/module1/Cropland_Mask_KANSAS.tif", overwrite = TRUE,
  wopt = list(datatype = "INT1U",  # Byte (0–255); NA retained as NoData
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
  )
)

# Merge ESA rasters (two adjacent tiles)
# Read input rasters
r1 <- rast(paste0(rasters_dir,"ESA_WorldCover_2021_KANSAS-0000000000-0000000000.tif"))
r2 <- rast(paste0(rasters_dir,"ESA_WorldCover_2021_KANSAS-0000000000-0000065536.tif"))
# Mosaic into a single raster
m <- mosaic(r1, r2, fun = "max")
# Cast to integer to optimize file size
# This ensures the output is written as integer values
m <- as.int(m)
# Write output with compression to reduce file size
writeRaster(m, "03_outputs/module1/ESA_WorldCover_2021_KANSAS.tif", overwrite = TRUE,
  wopt = list(datatype = "INT1U",  # Byte (0–255); NA retained as NoData
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
  )
)

# -----------------------------------------------------------------------------
# 5.7  Combining Environmental (Raster) and Soil (Vector) Information
# -----------------------------------------------------------------------------
# Extract raster values at sample locations to link soil observations
# (points) with environmental covariates (rasters) for DSM calibration.

# Extract covariate values at point locations (soils_sf)
cov_values <- extract(covs, soil_sf)
cov_values

# cov_values includes an ID column in the first column
soil_sf <- cbind(soil_sf, cov_values[ , -1, drop = FALSE])
soil_sf

# =============================================================================
# PART 6 — NEWHALL SIMULATION MODEL (NSM) FOR SOIL-CLIMATE COVARIATES
# =============================================================================
# The NSM estimates soil moisture and temperature regimes from climate and
# site characteristics. Outputs are useful as environmental covariates in
# DSM and sampling design workflows.
#
# Required inputs: monthly precipitation (pJan…pDec), monthly temperature
#   (tJan…tDec), elevation (DEM), available water capacity (AWC), lon/lat.

# -----------------------------------------------------------------------------
# 6.1  Environment Setup
# -----------------------------------------------------------------------------

# Packages
library(terra)
library(sf)
library(dplyr)
library(jNSMR)

# Target CRS for the workflow (use a projected CRS for analysis)
epsg <- "EPSG:3857"   # Web Mercator (meters). Replace if needed.
agg.factor <- 1       # Optional aggregation to speed up Newhall calculations (1 = no aggregation)

# -----------------------------------------------------------------------------
# 6.2  Prepare Monthly Average Rasters of Climatic Data
# -----------------------------------------------------------------------------
# TerraClimate files contain large time series (1981–2020). Monthly
# temperature and precipitation averages must be calculated as independent
# raster files. Temperature is stored scaled (×10) and must be adjusted.

# Monthly temperature time series (multi-year, monthly layers)
tmp <- rast(paste0(rasters_dir,"TerraClimate_AvgTemp_1981_2023_KANSAS.tif"))

# Number of years in the stack (assuming complete years)
nyears <- nlyr(tmp) / 12

# Monthly means across all years
temp_list <- vector("list", 12)
tmp_ref <- tmp[[1]]  # reference layer for initializing
for(i in 1:12){
  var_sum <- tmp_ref * 0
  k <- i
  for(j in 1:nyears){
    var_sum <- var_sum + tmp[[k]]
    k <- k + 12
  }
  temp_list[[i]] <- var_sum / nyears
}
# Convert to stack
Temp_Stack <- rast(temp_list) * 0.1  # convert scaled values to °C (×0.1)
# Write to disk
writeRaster(Temp_Stack, "03_outputs/module1/Temp_Stack_monthlyMean.tif", overwrite = TRUE)


# Monthly precipitation time series (multi-year, monthly layers)
prec_mm <- rast(paste0(rasters_dir,"TerraClimate_Precip_1981_2023_KANSAS.tif"))
# Number of years in the stack (assuming complete years
nyears <- nlyr(prec_mm) / 12
# Monthly means across all years
prec_list <- vector("list", 12)
pre_ref <- prec_mm[[1]] # reference layer for initializing
for(i in 1:12){
  var_sum <- pre_ref * 0
  k <- i
  for(j in 1:nyears){
    var_sum <- var_sum + prec_mm[[k]]
    k <- k + 12
  }
  prec_list[[i]] <- var_sum / nyears
}
# Convert to stack
Prec_Stack <- rast(prec_list)
# Write to disk
writeRaster(Prec_Stack, "03_outputs/module1/Prec_Stack_monthlyMean.tif", overwrite = TRUE)

# -----------------------------------------------------------------------------
# 6.3  Prepare Monthly Average Stack of Climatic Data
# -----------------------------------------------------------------------------
# NSM expects monthly layers with specific, predefined names.

# Load outputs (or reuse objects from above)
Prec_Stack <- rast("03_outputs/module1/Prec_Stack_monthlyMean.tif")
Temp_Stack <- rast("03_outputs/module1/Temp_Stack_monthlyMean.tif")

names(Prec_Stack) <- c("pJan","pFeb","pMar","pApr","pMay","pJun","pJul","pAug","pSep","pOct","pNov","pDec")
names(Temp_Stack) <- c("tJan","tFeb","tMar","tApr","tMay","tJun","tJul","tAug","tSep","tOct","tNov","tDec")

combined_stack <- c(Prec_Stack, Temp_Stack)
writeRaster(combined_stack, "03_outputs/module1/climate_vars.tif", overwrite = TRUE)

# -----------------------------------------------------------------------------
# 6.4  Prepare Site Descriptors (DEM, AWC, Lon/Lat)
# -----------------------------------------------------------------------------

# DEM
# Read raster
elev <- rast(paste0(rasters_dir,"/DEM_KANSAS.tif"))
# Align CRS to the common EPSG
if(crs(elev) != epsg) elev <- project(elev, epsg, method = "near")

# AWC (0–200 cm, in mm)
# Read raster
awc <- rast(paste0(rasters_dir,"awc_KANSAS.tif"))
# Align CRS to the common EPSG
if(crs(awc) != epsg) awc <- project(awc, epsg, method = "near")
# Align geometries with the DEM raster
awc <- resample(awc, elev)
# Define name
names(awc) <- "awc"

# Temperature and Precipitation
# Read climate stack
newhall <- rast("03_outputs/module1/climate_vars.tif")
# Align CRS to the common EPSG
if(crs(newhall) != epsg) newhall <- project(newhall, epsg, method = "near")
# Align geometries with the DEM raster
newhall <- resample(newhall, elev)

# Longitude and Latitude 
# calculate lon/lat from the climate stack
newhall$lonDD <- init(newhall[[1]], "x")
newhall$latDD <- init(newhall[[1]], "y")

# Mask climate stack to DEM valid area and add DEM/AWC to the stack
newhall <- mask(newhall, elev)
newhall$awc  <- awc
newhall$elev <- elev

# Save full NSM input stack
writeRaster(newhall, "03_outputs/module1/climate_newhall_vars.tif", overwrite = TRUE)

# -----------------------------------------------------------------------------
# 6.5  Calculate NSM Outputs
# -----------------------------------------------------------------------------
# NOTE: This calculation can take hours at high resolution.
# Optional aggregation (agg.factor > 1) reduces runtime.

# Read the full Newhall input stack
newhall <- rast("03_outputs/module1/climate_newhall_vars.tif")

# Optional: aggregate to speed up (e.g., fact = 2, 3, ...)
# newhall <- aggregate(newhall, fact = agg.factor)

system.time({
  newhall_results <- jNSMR::newhall_batch(newhall, cores = 4)
})

# -----------------------------------------------------------------------------
# 6.6  Inspect and Export NSM Results
# -----------------------------------------------------------------------------

# Simple validity mask
mask_valid <- newhall_results$annualRainfall * 0 + 1

# Apply mask and export
results <- mask(newhall_results, mask_valid)
writeRaster(results, "03_outputs/module1/newhall.tif", overwrite = TRUE)

# Optional compact integer exports (×10 preserves 1 decimal place)
results_climate_int <- app(newhall,  function(x) as.integer(x * 10))
writeRaster(results_climate_int, "03_outputs/module1/climate_intx10.tif", overwrite = TRUE)

results_newhall_int <- app(results, function(x) as.integer(x * 10))
writeRaster(results_newhall_int, "03_outputs/module1/newhall_intx10.tif", overwrite = TRUE)


###############################################################################
# END OF SESSION 4 — END OF MODULE 1
#
# The raster stack 'newhall_results' contains NSM outputs (soil moisture and
# temperature regime classes and indices) ready to use as covariates in:
#   - Module 2: Sampling Design (covariate space coverage)
#   - Module 4: Digital Soil Mapping (environmental predictors)
#
# Summary of Module 1 sessions:
#   Session 1 — R fundamentals (objects, structures, functions, tidyverse)
#   Session 2 — KSSL data import; site/coord/depth cleaning (Part 1)
#   Session 3 — Lab validation; duplicates; depth harmonization (Part 2)
#   Session 4 — Spatial analysis with sf/terra; covariate preparation; NSM
###############################################################################
