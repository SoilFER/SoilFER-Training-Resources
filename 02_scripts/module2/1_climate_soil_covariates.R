################################################################################
# Climate Data Processing and Soil-Climate Environmental Covariates
################################################################################
# 
# PURPOSE:
# This script processes raw temperature and precipitation data from Google Earth
# Engine (GEE), calculates monthly averages, and generates soil-climate 
# environmental covariates using the Newhall model.
#
# WORKFLOW:
# 1. Process raw temperature and precipitation data from GEE
# 2. Calculate monthly averages across all years
# 3. Combine climate data with elevation and soil water capacity
# 4. Run Newhall soil-climate model
# 5. Generate and export results
#
# REQUIREMENTS:
# - Temperature and precipitation data from GEE (see soilfer_env_covariates script)
# - Digital elevation model (DEM)
# - Available water capacity (AWC) data (0-200 cm depth)
# - Region of interest (ROI) shapefile
#
# AUTHORS: Luis Rodriguez-Lado, PhD 
#          Wanderson de Sousa Mendes, PhD
# DATE: 29 October 2025
################################################################################

## 0 - ENVIRONMENT SETUP =======================================================

# Set working directory to source file location
# Note: This requires running the script in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")  # Move up to main project folder
getwd()       # Display current working directory

# List of required packages
packages <- c("sp",         # Spatial data classes and methods
              "terra",      # Raster data manipulation
              "sf",         # Simple features for vector data
              "jNSMR",      # Newhall Simulation Model in R
              "dplyr")      # Data manipulation

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)  # Clean up environment

## 1 - USER-DEFINED VARIABLES ==================================================

# Define file paths (modify these according to your directory structure)
raster.path <- "rasters/"           # Path to raster data
shp.path <- "shapes/"               # Path to shapefiles


# Define coordinate reference system
# EPSG:3857 is Web Mercator projection (units in meters)
# Verify on https://epsg.io/
epsg <- "EPSG:3857"

# Aggregation factor for upscaling rasters (if needed)
# Set to 1 for no aggregation, >1 to reduce resolution
agg.factor <- 1

## 2 - PROCESS TEMPERATURE DATA ================================================
# Temperature data should be downloaded from GEE script
# File format: TerraClimate_AvgTemp_1981_2023_KANSAS.tif (monthly data for multiple years)

# Load raw temperature data (scaled by 10, needs conversion to Celsius)
tmp <- terra::rast(paste0(raster.path, "GEE_Exports/TerraClimate_AvgTemp_1981_2023_KANSAS.tif"))

# Get reference layer (first month) for initialization
tmp_Jan_1 <- tmp[[1]]
cat("Temperature data dimensions:", dim(tmp), "\n")
cat("Total months in dataset:", dim(tmp)[3], "\n")
cat("Number of years:", dim(tmp)[3]/12, "\n")

# Create empty list to store monthly averages
temp_list <- list()

# Calculate average temperature for each month across all years
# Loop through 12 months
for (i in 1:12) { 
  
  # Initialize sum raster with zeros
  var_sum <- tmp_Jan_1 * 0
  
  # Start with month i
  k <- i
  
  # Loop through all years
  for (j in 1:(dim(tmp)[3]/12)) {
    cat("Processing temperature month:", k, "\n")
    
    # Add current month to sum
    var_sum <- var_sum + tmp[[k]]
    
    # Move to same month next year
    k <- k + 12
  }
  
  # Calculate average by dividing sum by number of years
  var_avg <- var_sum / (dim(tmp)[3]/12)
  
  # Store in list
  temp_list[[i]] <- var_avg
}

# Create raster stack from list
Temp_Stack <- terra::rast(temp_list)

# Convert from scaled values to degrees Celsius (data is scaled by 10)
Temp_Stack <- Temp_Stack * 0.1

# Save intermediate temperature result
terra::writeRaster(Temp_Stack, 
                   filename = paste0(raster.path, 'Temp_Stack_01-22_TC.tif'),
                   overwrite = TRUE)

## 3 - PROCESS PRECIPITATION DATA ==============================================
# Precipitation data should be downloaded from GEE 
# File format: TerraClimate_Precip_1981_2023_KANSAS.tif (monthly data for multiple years)

# Load raw precipitation data (mm)
prec_mm <- terra::rast(paste0(raster.path, "/GEE_Exports/TerraClimate_Precip_1981_2023_KANSAS.tif"))

# Get reference layer (first month) for initialization
pre_Jan_1 <- prec_mm[[1]]
cat("Precipitation data dimensions:", dim(prec_mm), "\n")

# Create empty list to store monthly averages
prec_list <- list()

# Calculate average precipitation for each month across all years
# Loop through 12 months
for (i in 1:12) { 
  
  # Initialize sum raster with zeros
  var_sum <- pre_Jan_1 * 0
  
  # Start with month i
  k <- i
  
  # Loop through all years
  for (j in 1:(dim(prec_mm)[3]/12)) {
    cat("Processing precipitation month:", k, "\n")
    
    # Add current month to sum
    var_sum <- var_sum + prec_mm[[k]]
    
    # Move to same month next year
    k <- k + 12
  }
  
  # Calculate average by dividing sum by number of years
  var_avg <- var_sum / (dim(prec_mm)[3]/12)
  
  # Store in list
  prec_list[[i]] <- var_avg
}

# Create raster stack from list
Prec_Stack <- terra::rast(prec_list)

# Save intermediate precipitation result
terra::writeRaster(Prec_Stack, 
                   filename = paste0(raster.path, 'Prec_Stack_01-22_TC.tif'),
                   overwrite = TRUE)


## 4 - MERGE CLIMATE DATA ======================================================
# Combine temperature and precipitation into single climate variables file

# Define layer names following Newhall model conventions
# p = precipitation (mm), t = temperature (°C)
prec_colnames <- c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", 
                   "pJul", "pAug", "pSep", "pOct", "pNov", "pDec")
temp_colnames <- c("tJan", "tFeb", "tMar", "tApr", "tMay", "tJun", 
                   "tJul", "tAug", "tSep", "tOct", "tNov", "tDec")

# Rename precipitation layers
names(Prec_Stack) <- prec_colnames

# Rename temperature layers
names(Temp_Stack) <- temp_colnames

# Combine into single raster stack (24 layers total)
combined_stack <- c(Prec_Stack, Temp_Stack)

print(names(combined_stack))

# Export merged climate data (primary input for Newhall model)
terra::writeRaster(combined_stack, 
                   filename = paste0(raster.path, 'climate_vars.tif'),
                   overwrite = TRUE)

# Clean up temporary variables
rm(tmp, prec_mm, temp_list, prec_list, tmp_Jan_1, pre_Jan_1, 
   Temp_Stack, Prec_Stack, prec_colnames, temp_colnames)
gc()  # Garbage collection to free memory

## 5 - IMPORT ADDITIONAL SPATIAL DATA ==========================================

# Load region of interest (ROI) boundary shapefile
# Make sure the shapefile is uploaded in the shapes folder
country_boundaries <- sf::st_read(file.path(paste0(shp.path, "roi_kansas_us_epsg_4326.shp")), 
                                  quiet = TRUE)

# Check and reproject if necessary to match target CRS
if(sf::st_crs(country_boundaries) != epsg) {
  cat("Reprojecting country boundaries to", epsg, "\n")
  country_boundaries <- country_boundaries %>%
    sf::st_as_sf() %>% 
    sf::st_transform(crs = epsg)
}

# Load digital elevation model (DEM)
# This file should contain elevation in meters
# The DEM was downloaded using GEE script
# Move this file to the raster folder
elev <- list.files(raster.path, 
                   pattern = "DEM_KANSAS.tif$",  
                   recursive = TRUE, 
                   full.names = TRUE)
elev <- terra::rast(elev)
#elev <- elev$dtm_elevation_250m

# Reproject elevation to match target CRS if needed
if(terra::crs(elev) != epsg) {
  cat("Reprojecting DEM data to", epsg, "\n")
  elev <- terra::project(elev, epsg, method = "near")
}

# Load available water capacity (AWC) data
# AWC represents the soil's ability to hold water (0-200 cm depth)
# Data source: Hengl and Gupta (https://zenodo.org/records/2629149)
# This folder should be downloaded in the raster folder.
awc <- terra::rast(paste0(raster.path, "sol_available.water.capacity_usda.mm_m_250m_0..200cm_1950..2017_v0.1.tif"))

# Reproject AWC to match target CRS if needed (12 min)
if(terra::crs(awc) != epsg) {
  cat("Reprojecting AWC data to", epsg, "\n")
  awc <- terra::project(awc, epsg, method = "near")
}

# Mask to the AOI geometry (sets outside cells to NA) (12 min)
awc <- terra::crop(awc, country_boundaries)
names(awc) <- "awc"
plot(awc)

# Resample AWC to match elevation grid
awc <- terra::resample(awc, elev)

## 6 - PREPARE NEWHALL MODEL INPUTS ============================================

# Load the climate variables created in previous steps
newhall <- terra::rast(paste0(raster.path, "climate_vars.tif"))

# Reproject climate data if needed
if(terra::crs(newhall) != epsg) {
  cat("Reprojecting climate data to", epsg, "\n")
  newhall <- terra::project(newhall, epsg, method = "near")
}

# Resample to match elevation grid (ensures all layers align)
newhall <- terra::resample(newhall, elev)

# Add longitude and latitude as covariates
# These are required for accurate Newhall calculations
newhall$lonDD <- terra::init(newhall[[1]], 'x')  # Longitude in decimal degrees
newhall$latDD <- terra::init(newhall[[1]], 'y')  # Latitude in decimal degrees
names(newhall)

# Mask climate data to elevation extent (remove NoData areas)
newhall <- terra::mask(newhall, elev)

# Add AWC and elevation to the stack
newhall$awc <- awc
newhall$elev <- elev

# Visual check of some climate layers
terra::plot(newhall["tJan"], main = "January Temperature (°C)")
terra::plot(newhall["pJan"], main = "January Precipitation (mm)")

# Display summary statistics
print(summary(newhall))

# Optional: Crop to administrative boundary
# Uncomment the following line if you want to limit analysis to specific region
# newhall <- terra::crop(newhall, country_boundaries, mask = TRUE)

# Save complete Newhall input dataset
terra::writeRaster(newhall, 
                   paste0(raster.path, "climate_newhall_vars.tif"), 
                   overwrite = TRUE)

# Clean up
rm(elev, awc)
gc()

## 7 - COMPUTE NEWHALL SOIL-CLIMATE MODEL ======================================

# Optional: Aggregate raster to reduce computation time
# Uncomment if processing large datasets
# newhall <- terra::aggregate(newhall, fact = agg.factor)

# Run Newhall batch processing
# This calculates soil temperature and moisture regimes
# cores = 4 enables parallel processing (adjust based on your CPU) - 6-7 hrs
system.time({
  newhall_results <- jNSMR::newhall_batch(newhall, cores = 4)
})

# Create mask from results (used to remove invalid areas)
mask <- newhall_results$annualRainfall - newhall_results$annualRainfall + 1

## 8 - PLOT NEWHALL MODEL RESULTS ==============================================

# Water balance plots
terra::plot(newhall_results$annualWaterBalance,
            cex.main = 0.9, 
            main = "Annual Water Balance (P-PET) [mm]")

terra::plot(newhall_results$summerWaterBalance,
            cex.main = 0.9, 
            main = "Summer Water Balance [mm]")

# Regime classifications
terra::plot(newhall_results$temperatureRegime, 
            main = "Temperature Regime")

terra::plot(newhall_results$moistureRegime, 
            main = "Moisture Regime")

# Temporal metrics
terra::plot(newhall_results$numCumulativeDaysDry, 
            col = grDevices::terrain.colors(20),
            cex.main = 0.75, 
            main = "Number of Cumulative Days Dry")

terra::plot(newhall_results$numCumulativeDaysDryOver5C, 
            col = grDevices::terrain.colors(20),
            cex.main = 0.75, 
            main = "Number of Cumulative Days Dry over 5°C")

terra::plot(newhall_results$numConsecutiveDaysMoistInSomePartsOver8C, 
            col = rev(grDevices::terrain.colors(20)),
            cex.main = 0.75, 
            main = "Number of Consecutive Days Moist\nin some parts over 8°C")

terra::plot(newhall_results$dryDaysAfterSummerSolstice, 
            col = grDevices::terrain.colors(20),
            cex.main = 0.75, 
            main = "Number of Dry Days After Summer Solstice")

terra::plot(newhall_results$moistDaysAfterWinterSolstice,
            col = rev(grDevices::terrain.colors(50)),
            cex.main = 0.75, 
            main = "Number of Moist Days After Winter Solstice")

# Regime subdivisions (if calculated)
if("regimeSubdivision1" %in% names(newhall_results)) {
  terra::plot(newhall_results$regimeSubdivision1,
              cex.main = 0.75, 
              main = "Regime Subdivision 1")
}

if("regimeSubdivision2" %in% names(newhall_results)) {
  terra::plot(newhall_results$regimeSubdivision2,
              cex.main = 0.75, 
              main = "Regime Subdivision 2")
}

## 9 - EXPORT RESULTS ==========================================================

# Apply mask to results
results <- terra::mask(newhall_results, mask)

# Export main Newhall results
terra::writeRaster(results, 
                   paste0(raster.path, "newhall.tif"), 
                   overwrite = TRUE)


# Export climate input as integer (multiplied by 10 to preserve 1 decimal place)
# This reduces file size while maintaining sufficient precision
results_climate_int <- terra::app(newhall, function(x) { as.integer(x * 10) })
names(results_climate_int) <- names(newhall)

terra::writeRaster(results_climate_int,
                   paste0(raster.path, "climate_intx10.tif"), 
                   overwrite = TRUE)

# Export Newhall results as integer (multiplied by 10)
results_newhall_int <- terra::app(results, function(x) { as.integer(x * 10) })
names(results_newhall_int) <- names(results)

terra::writeRaster(results_newhall_int,
                   paste0(raster.path, "newhall_intx10.tif"), 
                   overwrite = TRUE)

# Final cleanup
rm(results_climate_int, results_newhall_int, mask, newhall, newhall_results)
gc()

## 10 - MERGE TILED GEE EXPORTS ================================================

# Function to merge tiles for a given dataset (SPATIAL merge, not stacking)
merge_gee_tiles <- function(base_name, input_path, output_path) {
  
  cat("\n========================================\n")
  cat("Processing:", base_name, "\n")
  cat("========================================\n")
  
  # Find all tiles for this dataset
  tile_files <- list.files(input_path,
                           pattern = paste0("^", base_name, ".*\\.tif$"),
                           full.names = TRUE)
  
  if(length(tile_files) == 0) {
    cat("WARNING: No tiles found for", base_name, "\n")
    return(NULL)
  }
  
  cat("Found", length(tile_files), "tile(s)\n")
  print(basename(tile_files))
  
  # If only one tile, just copy it
  if(length(tile_files) == 1) {
    cat("Single tile - copying directly\n")
    merged <- terra::rast(tile_files[1])
  } else {
    # Load all tiles
    cat("Loading tiles...\n")
    tile_list <- lapply(tile_files, terra::rast)
    
    # SPATIAL MERGE using mosaic (stitches tiles together)
    cat("Mosaicking tiles spatially...\n")
    merged <- do.call(terra::mosaic, tile_list)
  }
  
  # Create output filename
  output_file <- paste0(output_path, base_name, "_merged.tif")
  
  # Export merged raster
  cat("Exporting to:", output_file, "\n")
  terra::writeRaster(merged, 
                     filename = output_file,
                     overwrite = TRUE)
  
  cat("SUCCESS: Merged", base_name, "\n\n")
  return(merged)
}

# List of datasets to merge
datasets_to_merge <- c(
  "ESA_WorldCover_2021_KANSAS",
  "Cropland_Mask_KANSAS",
  "HighRes_Covariates_100m_KANSAS"
)

# Merge EACH dataset separately (not stacked together)
for(dataset in datasets_to_merge) {
  merge_gee_tiles(
    base_name = dataset,
    input_path = paste0(raster.path, "/GEE_Exports/"),
    output_path = raster.path
  )
}


################################################################################
# END OF SCRIPT
################################################################################
