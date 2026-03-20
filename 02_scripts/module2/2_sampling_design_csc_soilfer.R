################################################################################
# MULTI-STAGE SAMPLING DESIGN FOR SOIL SAMPLING - Covariate Space Coverage
# 
# This script creates a three-stage sampling design:
# - Primary Sampling Units (PSUs): 2x2 km grids
# - Secondary Sampling Units (SSUs): 100x100 m cells within PSUs
# - Tertiary Sampling Units (TSUs): Point locations within SSUs
#
# Author: Luis Rodriguez-Lado, PhD 
#         Wanderson de Sousa Mendes, PhD
# Date: 06 November 2025
# Version: 2.0
################################################################################

## 1 - SET ENVIRONMENT AND LOAD LIBRARIES ======================================
# Purpose: Load all required packages and set working directory

# Set working directory to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")  # Move up to main project folder
getwd()  # Verify location

# List of required packages
packages <- c("sp","terra","raster","sf", "sgsR","entropy", "tripack","tibble",
              "manipulate","dplyr","synoptReg", "doSNOW","Rfast","fields", 
              "ggplot2", "rassta", "snowfall", "maps", "tidyterra",
              "data.table", "RColorBrewer", "stringr")

# Load all packages silently
invisible(lapply(packages, library, character.only = TRUE))
rm(packages)

## 2 - DEFINE VARIABLES AND PARAMETERS =========================================
# Purpose: Set all study-specific parameters in one place for easy modification

# Country identification
ISO.code <- "KANSAS"  # 3-letter country code or full-name for file naming

# Land use types to analyze (run script separately for each)
landuse <- "cropland"  # Options: "cropland", "grassland", "forest"

# File paths
raster.path <- "rasters/"
shp.path <- "shapes/"
other.path <- "other/"
landuse_dir <- paste0("results/", landuse, "/")

# Create directories if they don't exist
dir.create(other.path, showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)
if (!file.exists(landuse_dir)) dir.create(landuse_dir)
results.path <- landuse_dir

# Coordinate reference system
epsg <- "EPSG:26714"  # NAD27 UTM Zone 14N - ADJUST FOR YOUR STUDY AREA

# Sampling unit sizes (in meters)
psu_size <- 2000   # PSU: 2 km x 2 km
ssu_size <- 100    # SSU: 100 m x 100 m (1 hectare)

# Number of SSUs per PSU
num_primary_ssus <- 4        # Target SSUs (one per cluster)
num_alternative_ssus <- 4    # Replacement SSUs (one per cluster)

# Number of TSUs per SSU
number_TSUs <- 3  # Point samples within each SSU

# Optimization parameters
iterations <- 10  # K-means clustering iterations

# Minimum crop coverage for PSU selection (percentage)
percent_crop <- 20  # PSUs must have >20% crop coverage

## 3 - DEFINE CUSTOM FUNCTIONS =================================================
# Purpose: Create reusable functions for sampling design

# Covariate Space Coverage with Legacy Data
## This function performs constrained k-means clustering
CSIS <- function(fixed, nsup, nstarts, mygrd) {
  # Args:
  #   fixed: Data frame of fixed legacy points
  #   nsup: Number of supplementary points to select
  #   nstarts: Number of random starts for optimization
  #   mygrd: Grid of all potential sampling locations
  
  n_fix <- nrow(fixed)
  p <- ncol(mygrd)
  units <- fixed$units
  mygrd_minfx <- mygrd[-units, ]
  MSSSD_cur <- NA
  
  for (s in 1:nstarts) {
    units <- sample(nrow(mygrd_minfx), nsup)
    centers_sup <- mygrd_minfx[units, ]
    centers <- rbind(fixed[, names(mygrd)], centers_sup)
    
    repeat {
      D <- rdist(x1 = centers, x2 = mygrd)
      cluster <- apply(X = D, MARGIN = 2, FUN = which.min) %>% as.factor(.)
      centers_cur <- centers
      
      for (i in 1:p) {
        centers[, i] <- tapply(mygrd[, i], INDEX = cluster, FUN = mean)
      }
      
      # Restore fixed centers (legacy data points don't move)
      centers[1:n_fix, ] <- centers_cur[1:n_fix, ]
      
      # Check convergence
      sumd <- diag(rdist(x1 = centers, x2 = centers_cur)) %>% sum(.)
      if (sumd < 1E-12) {
        D <- rdist(x1 = centers, x2 = mygrd)
        Dmin <- apply(X = D, MARGIN = 2, FUN = min)
        MSSSD <- mean(Dmin^2)
        
        if (s == 1 | MSSSD < MSSSD_cur) {
          centers_best <- centers
          clusters_best <- cluster
          MSSSD_cur <- MSSSD
        }
        break
      }
    }
    print(paste0(s," out of ",nstarts))
  }
  list(centers = centers_best, cluster = clusters_best)
}

# K-means with Progress Reporting
## This function performs k-means clustering if legacy data is not available (progress reporting)
kmeans_with_progress <- function(data, centers, iter.max = 10000, nstart = 100) {
  # Provides visual feedback during long k-means operations
  best_result <- NULL
  best_totss <- Inf
  
  cat("Running k-means clustering with", nstart, "random starts...\n")
  
  for (s in 1:nstart) {
    result <- kmeans(data, centers = centers, iter.max = iter.max, nstart = 1)
    
    if (result$tot.withinss < best_totss) {
      best_result <- result
      best_totss <- result$tot.withinss
    }
    
    print(paste0(s, " out of ", nstart))
  }
  
  cat("Best tot.withinss:", best_totss, "\n")
  return(best_result)
}

# Generate TSU Points Within SSU
## This function creates random point samples within each SSU polygon
generate_tsu_points_within_ssu <- function(ssu, number_TSUs, index, ssu_type, crops) {
  # Args:
  #   ssu: Single SSU polygon (sf object)
  #   number_TSUs: Number of points to generate
  #   index: SSU identifier
  #   ssu_type: "Target" or "Replacement"
  #   crops: Crop mask raster (20m resolution)
  
  ssu_vect <- vect(ssu)
  
  # Validate geometry
  if (is.null(ssu_vect) || nrow(ssu_vect) == 0 || is.na(ext(ssu_vect))) {
    warning(paste("SSU", index, "has invalid geometry. Skipping TSU generation."))
    return(NULL)
  }
  
  # Clip crop raster to SSU
  clipped_lu <- try(crop(crops, ssu_vect), silent = TRUE)
  if (inherits(clipped_lu, "try-error") || is.null(clipped_lu)) {
    warning(paste("SSU", index, "could not crop land use raster. Skipping."))
    return(NULL)
  }
  
  # Sample points (tries two methods)
  sampled_points <- try(sample_srs(clipped_lu, nSamp = number_TSUs), silent = TRUE)
  if (inherits(sampled_points, "try-error") || is.null(sampled_points) || nrow(sampled_points) == 0) {
    sampled_points <- try(spatSample(clipped_lu, size = number_TSUs, na.rm = TRUE, method = "random"), silent = TRUE)
  }
  
  if (inherits(sampled_points, "try-error") || is.null(sampled_points) || nrow(sampled_points) == 0) {
    warning(paste("SSU", index, "failed to generate TSUs. Skipping."))
    return(NULL)
  }
  
  # Add metadata
  sampled_points$PSU_ID <- selected_psu$ID
  sampled_points$SSU_ID <- index
  sampled_points$TSU_ID <- seq_len(nrow(sampled_points))
  sampled_points$SSU_Type <- ssu_type
  sampled_points$TSU_Name <- paste0(sampled_points$PSU_ID, ".", index, ".", seq_len(nrow(sampled_points)))
  
  return(sampled_points)
}

## 4 - LOAD COUNTRY BOUNDARIES AND LEGACY DATA =================================
# Purpose: Import study area boundaries and existing soil sample locations

# Load country/region boundaries
country_boundaries <- file.path(paste0(shp.path,"roi_kansas_us_epsg_4326.shp"))
country_boundaries <- sf::st_read(country_boundaries, quiet=TRUE)

# Reproject if necessary
if(crs(country_boundaries)!=epsg){
  country_boundaries <- country_boundaries %>%
    st_as_sf() %>% sf::st_transform(crs=epsg)
}

# Load legacy soil data (optional - existing sample points)
legacy <- file.path(paste0(shp.path,"soil_legacy_data_kansas_epsg_4326.shp"))

if(file.exists(legacy)){
  legacy <- sf::st_read(legacy, quiet=TRUE)
  if(crs(legacy)!=epsg){
    legacy <- legacy %>% sf::st_transform(crs=epsg)
  }
} else {
  rm(legacy)  # Remove if doesn't exist
} 

# Clean legacy data
if(exists("legacy")){
  legacy <- dplyr::select(legacy, geometry)
  legacy <- legacy[!duplicated(st_geometry(legacy)), ]  # Remove duplicates
}

png(
  filename = "results/imgs/legacy_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)
# Visualize boundaries and legacy points
ggplot() +
  geom_spatvector(data = country_boundaries, fill = NA, color = "black") +
  geom_spatvector(data = legacy, aes(geometry = geometry), size = 0.7, color = "red") +
  theme_minimal()

dev.off()

## 5 - LOAD OPTIONAL EXCLUSION LAYERS ==========================================
# Purpose: Load masks to exclude protected areas, steep slopes, etc.

# Protected areas (areas to EXCLUDE from sampling)
npa <- file.path(paste0(shp.path,"protected_areas_epsg_4326.shp"))

if(file.exists(npa)){
  npa <- sf::st_read(npa, quiet = FALSE)
  if(crs(npa)!=epsg){
    npa <- npa %>% sf::st_transform(crs = epsg)
  }
  npa <- sf::st_union(npa)
  npa <- sf::st_difference(country_boundaries, npa)  # Create "non-protected" mask
} else {
  rm(npa)
}

png(
  filename = "results/imgs/non_protected_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

ggplot() +
  geom_sf(data = npa, fill = "grey90", color = "black", linewidth = 0.3) +
  coord_sf(datum = NA) +
  labs(title = "Accessible area mask (non-protected in grey)") +
  theme_minimal(base_size = 11)

dev.off()

# Slope mask (exclude areas with slope > threshold)
# This should be a BINARY RASTER where:
  #   - Value = 1 : Accessible slopes (≤ threshold, e.g., ≤30%)
  #   - Value = NA: Inaccessible slopes (> threshold, excluded from sampling)
  #
  # HOW TO CREATE IN QGIS:
  #   Step 1: Analyze your slope raster to determine appropriate threshold
  #           (Raster → Raster Calculator or Slope tool from DEM)
  #   Step 2: Open Raster Calculator (Raster → Raster Calculator)
  #   Step 3: Use this formula to create binary mask:
  #           ( "Slope@1" <= 30 ) * 1
  #           Replace 30 with your chosen threshold (%)
  #   Step 4: This outputs: 1 where slope ≤30%, 0 where slope >30%
  #   Step 5: (Optional) Convert 0 to NA: ( "Slope@1" <= 30 ) * 1 + ( "Slope@1" > 30 ) * -9999
  #           Then use "Set Null" tool to convert -9999 to NA
  #
  # THRESHOLD GUIDELINES:
  #   - Gentle terrain: 0-15%
  #   - Moderate terrain: 15-30% (typical threshold for field work)
  #   - Steep terrain: >30% (usually excluded)
slope <- file.path(paste0(raster.path,"slope_mask_epsg_4326.tif"))

if(file.exists(slope)){
  slope <- rast(slope)
  if(crs(slope)!=epsg){
    slope <- project(slope, epsg, method="near")
  }
  slope <- slope/slope  # Convert to binary mask
  slope <- terra::mask(slope, country_boundaries)
} else {
  rm(slope)
}

png("results/imgs/slope_mask.png",
    width = 4000,
    height = 2800,
    res = 400,
    type = "cairo")

plot(slope,
     col = "#66c2a5",
     legend = FALSE,
     main = "Slope accessibility mask in green")

dev.off()

# Geology data (for stratification) - If available
geo <- file.path(paste0(shp.path,"ecoregions_kansas_epsg_4326.shp"))
geo.classes <- "US_L3NAME"  # Field name for geology classes

if(file.exists(geo)){
  geo <- sf::st_read(geo, quiet=TRUE)
  if(crs(geo)!=epsg){
    geo <- geo %>% sf::st_transform(crs=epsg)
  }
  geo$GEO <- as.numeric(as.factor(geo[[geo.classes]]))
} else {
  rm(geo)
}

png(
  filename = "results/imgs/geology_ecoregion_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

ggplot() +
  geom_sf(data = geo, aes(fill = US_L3NAME), color = "black", linewidth = 0.2) +
  labs(title = "Level III Ecoregions",
       fill = "Ecoregion (US_L3NAME)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

dev.off()

# Geomorphology data
geomorph <- file.path(paste0(raster.path,"/GEE_Exports/Geomorphon_Landforms_KANSAS.tif"))
geomorph.classes <- "Class"

if (file.exists(geomorph)) {
  file_extension <- tools::file_ext(geomorph)
  
  if (file_extension == "tif") {
    geomorph <- rast(geomorph)
    names(geomorph) <- 'GEOMORPH'
    if(crs(geomorph)!=epsg){
      geomorph <- project(geomorph, epsg, method="near")
    }
  } else if (file_extension == "shp") {
    geomorph <- sf::st_read(geomorph, quiet = TRUE)
    if (sf::st_crs(geomorph)$epsg != epsg) {
      geomorph <- sf::st_transform(geomorph, crs = epsg)
    }
    geomorph$GEOMORPH <- as.numeric(as.factor(geomorph[[geomorph.classes]]))
  }
  
  geomorph <- terra::mask(geomorph, country_boundaries)
} else {
  rm(geomorph)
}

# Visualisation
# Create a class lookup table (Geomorpho90m / geomorphons)
geomorph_lut <- data.frame(
  value = 1:10,
  class = c(
    "Flat",
    "Peak / summit",
    "Ridge",
    "Shoulder",
    "Spur",
    "Slope",
    "Hollow",
    "Footslope",
    "Valley",
    "Pit / depression"
  )
)

# Semantically meaningful colors: warm tones for highs, cool tones for lows
geomorph_colors <- c(
  "Flat"             = "#F5F5DC",  # beige      – level ground
  "Peak / summit"    = "#8B0000",  # dark red   – highest points
  "Ridge"            = "#CD5C5C",  # indian red – elongated highs
  "Shoulder"         = "#D2691E",  # chocolate  – convex upper slopes
  "Spur"             = "#DAA520",  # goldenrod  – diverging slopes
  "Slope"            = "#6B8E23",  # olive      – planar slopes
  "Hollow"           = "#4682B4",  # steel blue – converging slopes
  "Footslope"        = "#5F9EA0",  # cadet blue – concave lower slopes
  "Valley"           = "#00008B",  # dark blue  – linear lows
  "Pit / depression" = "#191970"   # midnight   – lowest points
)

# Attach labels to the raster as categories
geomorph_cat <- as.factor(geomorph)
levels(geomorph_cat) <- list(data.frame(
  ID    = geomorph_lut$value,
  class = geomorph_lut$class
))

png(
  filename = "results/imgs/geomorph_classes.png",
  width    = 4000,
  height   = 2800,
  res      = 400
)

par(mar = c(3, 3, 4, 3), bg = "white")

plot(
  geomorph_cat,
  col  = geomorph_colors[levels(geomorph_cat)[[1]]$class],
  main = "Geomorphology",
  axes = FALSE,
  box = FALSE,
  plg = list(
    inset = c(0.02, 0.03),
    cex = 0.75,
    title = "Geomorphons",
    bty = "n"
  )
)

dev.off()

## 6 - LOAD AND PROCESS ENVIRONMENTAL COVARIATES (PSU LEVEL) ===================
# Purpose: Load environmental data at 2km resolution for PSU selection from GEE code
# Note: These covariates determine WHERE PSUs are placed

cov.dat <- list.files(paste0(raster.path, "GEE_Exports/"), 
                      pattern = "Environmental_Covariates_250m_KANSAS.tif$",  
                      recursive = TRUE, full.names = TRUE)
cov.dat <- terra::rast(cov.dat)

# Reproject if necessary
if(crs(cov.dat)!=epsg){
  cov.dat <- terra::project(cov.dat, epsg, method="near")
}

# Resample to PSU resolution (2km)
# IMPORTANT: This aggregation must match your PSU size
psu_size_template <- rast(ext(cov.dat), resolution = psu_size, crs = crs(cov.dat))
cov.dat <- resample(cov.dat, psu_size_template, method = "bilinear")
cov.dat <- terra::mask(cov.dat, country_boundaries)
names(cov.dat)

# Load and process soil climate data
newhall <- list.files(raster.path, pattern = "newhall.tif$", recursive = TRUE, full.names = TRUE)
newhall <- terra::rast(newhall)

if(crs(newhall)!=epsg){
  newhall <- terra::project(newhall, epsg, method="near")
}

# Remove unnecessary layers
newhall$regimeSubdivision1 <- NULL
newhall$regimeSubdivision2 <- NULL

# Process categorical variables (preserve factor levels)
temperatureRegime <- project(newhall$temperatureRegime, cov.dat, method = "near")
moistureRegime <- project(newhall$moistureRegime, cov.dat, method = "near")

newhall$temperatureRegime <- NULL
newhall$moistureRegime <- NULL
newhall <- terra::resample(newhall, cov.dat)

# Convert to dummy variables (one column per category)
temperatureRegime <- as.factor(temperatureRegime)
temperatureRegime <- dummies(ca.rast = temperatureRegime, preval = 1, absval = 0)
moistureRegime <- as.factor(moistureRegime)
moistureRegime <- dummies(ca.rast = moistureRegime, preval = 1, absval = 0)

# Merge all covariates
cov.dat <- c(cov.dat, newhall, temperatureRegime, moistureRegime)

# Add geology if available
if(exists("geo")){
  geo <- rasterize(as(geo,"SpatVector"), cov.dat, field="GEO")
  geo <- dummies(ca.rast = geo$GEO, preval = 1, absval = 0)
  cov.dat <- c(cov.dat, geo)
}

# Add geomorphology if available
if (exists("geomorph")) {
  if (!inherits(geomorph, "SpatRaster")) {
    geomorph <- rasterize(as(geomorph, "SpatVector"), cov.dat, field = "GEOMORPH")
  }
  geomorph <- dummies(ca.rast = geomorph$GEOMORPH, preval = 1, absval = 0)
  
  # Ensure matching extents
  if (!identical(ext(cov.dat), ext(geomorph))) {
    geomorph <- extend(geomorph, cov.dat)
  }
  if (!all(res(cov.dat) == res(geomorph))) {
    geomorph <- resample(geomorph, cov.dat, method = "near")
  }
  
  cov.dat <- c(cov.dat, geomorph)
}

# Clean up
rm(newhall, geomorph)
gc()

# Crop to study area
cov.dat <- crop(cov.dat, country_boundaries, mask=TRUE, overwrite=TRUE)
writeRaster(cov.dat, paste0(raster.path,"cov_dat_stack_psus.tif"), overwrite=TRUE)

names(cov.dat)

## 7 - DIMENSIONALITY REDUCTION WITH PCA =======================================
# Purpose: Reduce many covariates to fewer principal components
# Why: Makes clustering faster and reduces multicollinearity

pca <- scale(cov.dat)
pca <- synoptReg::raster_pca(pca)  # Fast PCA for rasters

cov.dat <- pca$PCA

# Keep only components explaining 99% of variance
n_comps <- first(which(pca$summaryPCA[3,] > 0.99))
cov.dat <- pca$PCA[[1:n_comps]]

cat(sprintf("Using %d principal components (explaining >99%% variance)\n", n_comps))

# Save PCA results
writeRaster(cov.dat, paste0(results.path,"PCA_projected.tif"), overwrite=TRUE)
rm(pca)

# Reload for further processing
# Why this is useful:
#  - Previous sections (data loading, PCA) can take 30+ minutes
#  - If script crashes or needs modification, you can load here
#  - If the file exists in the folder from previous processing
cov.dat <- rast(paste0(results.path,"PCA_projected.tif"))
#plot(cov.dat[[1]])

# Reproject if necessary
if(crs(cov.dat)!=epsg){
  cov.dat <- terra::project(cov.dat, epsg, method="near")
}


png(
  filename = "results/imgs/cum_var_pcas_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

cum_var <- pca$summaryPCA["Cumulative", ]

plot(cum_var,
     type = "l",
     lwd = 2,
     xlab = "Principal Component",
     ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance")

abline(h = 0.99, col = "red", lty = 2)

dev.off()

png(
  filename = "results/imgs/pcas_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

# Consistent color scale across PC1–PC3
minmax_vals <- minmax(pca$PCA[[1:3]])
zlim_vals <- range(minmax_vals)

cols <- hcl.colors(100, "Blue-Red 3", rev = TRUE)

par(mfrow = c(1,3), mar = c(3,3,3,6))  # extra space for legend

plot(pca$PCA[[1]],
     col = cols,
     zlim = zlim_vals,
     main = "PC1",
     axes = FALSE,
     box = FALSE,
     plg = list(title = "PC value"))

plot(pca$PCA[[2]],
     col = cols,
     zlim = zlim_vals,
     axes = FALSE,
     box = FALSE,
     main = "PC2",
     plg = list(title = "PC value"))

plot(pca$PCA[[3]],
     col = cols,
     zlim = zlim_vals,
     main = "PC3",
     axes = FALSE,
     box = FALSE,
     plg = list(title = "PC value"))

dev.off()

par(mfrow = c(1,1))

## 8 - LOAD AND PREPARE LAND USE DATA ==========================================
# Purpose: Define sampling universe (where samples CAN be taken)
# Uses TWO resolutions: 20m for TSU placement, 100m for PSU filtering

# Load cropland mask
landuse_file <- file.path(paste0(raster.path,"Cropland_Mask_KANSAS_merged.tif"))
crops <- rast(landuse_file)
crops <- crops/crops  # Convert to binary (1=crop, NA=other)
names(crops) <- "lu"

# Reproject if necessary
if(crs(crops)!=epsg){
  crops <- terra::project(crops, epsg, method="near")
}

png(
  filename = "results/imgs/crop_20m_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)
# Visualize (it takes a few minutes)
ggplot() +
  geom_spatraster(data = as.factor(crops)) +
  scale_fill_viridis_d(na.value = "transparent") +
  geom_spatvector(data = country_boundaries, fill = NA, color = "black") +
#  geom_spatvector(data = legacy, size = 0.7, color = "red") +
  theme_minimal()

dev.off()

# Resample to 20m resolution (for TSU placement)
# CRITICAL: This resolution determines precision of TSU point placement
lulc_size_template <- rast(ext(crops), resolution = 20, crs = crs(crops))
crops <- as.factor(crops)
crops <- resample(crops, lulc_size_template, method = "near")

# Apply exclusion masks
if(exists("npa")){
  crops <- mask(crops, npa)
}

if(exists("slope")){
  slope <- resample(slope, crops, method="near")
  crops <- crops * slope
}

rm(npa, slope)

# Save 20m resolution crop mask
writeRaster(crops, paste0(raster.path,"crop_mask_20m_clean.tif"), overwrite=TRUE)
# Same situation as loading "cov.dat"
crops <- rast(paste0(raster.path,"crop_mask_20m_clean.tif"))

# Create 100m resolution version for PSU filtering
# Aggregate: (20m × 5) = 100m pixels
lu <- aggregate(crops, 5, fun=modal, cores = 2, na.rm=T)
names(lu) <- "lu"
writeRaster(lu, paste0(raster.path,"crop_mask_100m.tif"), overwrite=TRUE)
# Same situation as loading "cov.dat"
lu <- rast(paste0(raster.path,"crop_mask_100m.tif"))

# Filter legacy data to crop areas
if (exists("legacy")){
  legacy$INSIDE <- terra::extract(crops, legacy) %>% dplyr::select(lu)
  legacy <- legacy[!is.na(legacy$INSIDE),] %>% dplyr::select(-"INSIDE")
}

png(
  filename = "results/imgs/crop_100m_legacy_map.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

# Visualize (it takes a few minutes)
ggplot() +
  geom_spatraster(data = as.factor(lu)) +
  scale_fill_viridis_d(na.value = "transparent") +
  geom_spatvector(data = country_boundaries, fill = NA, color = "black") +
  geom_spatvector(data = legacy, size = 0.7, color = "red") +
  theme_minimal()

dev.off()

## 9 - GENERATE PSU GRID ========================================================
# Purpose: Create 2×2 km grid covering the study area

psu_grid <- st_make_grid(country_boundaries, cellsize = c(psu_size, psu_size), square = TRUE)
psu_grid <- st_sf(geometry = psu_grid)
psu_grid$ID <- 1:nrow(psu_grid)

# Clip to country boundary
psu_grid <- psu_grid[country_boundaries[1],]  # TIME CONSUMING!
write_sf(psu_grid, paste0(results.path,"../grid2k.shp"), overwrite=TRUE)

# Or load pre-saved grid (much faster)
# psu_grid <- sf::st_read(file.path(paste0(results.path,"../grid2k.shp")))

## 10 - FILTER PSUs BY CROP COVERAGE ===========================================
# Purpose: Keep only PSUs with sufficient cropland
# Why: No point sampling in PSUs with <20% crops

# Extract crop percentage for each PSU
extracted_values <- terra::extract(lu, psu_grid)

crop_perc <- extracted_values %>%
  group_by(ID) %>%
  summarize(crop_perc = sum(lu, na.rm = TRUE)*100/400)  # 400 = total of 100m pixels in 2km PSU

rm(extracted_values)

# Join back to PSU grid
psu_grid$crop_perc <- crop_perc$crop_perc
write_sf(psu_grid, file.path(paste0(results.path,"/psu_grid_counts.shp")), overwrite=TRUE)

# Reload and ensure correct projection
## Same as cov.dat and land use data
psu_grid <- sf::st_read(file.path(paste0(results.path,"/psu_grid_counts.shp")))
if(crs(psu_grid)!=epsg){
  psu_grid <- psu_grid %>% sf::st_transform(crs=epsg)
}

png(
  filename = "results/imgs/psu_grid_lu_coverage.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

# Visualize crop coverage
ggplot() +
  geom_sf(data = psu_grid, aes(fill = crop_perc)) +
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "Crop Coverage by PSU", fill = "% Cropland") +
  theme_minimal()

dev.off()

# Filter: Keep only PSUs with n > percent_crop coverage
psu_grid <- psu_grid[psu_grid$crop_perc > percent_crop, "ID"]

cat(sprintf("Retained %d PSUs with > %d%% crop coverage\n", 
            nrow(psu_grid), percent_crop))

## 11 - RASTERIZE PSU GRID =====================================================
# Purpose: Convert vector PSUs to raster format for covariate extraction

template <- rast(vect(psu_grid), res = psu_size)
template <- rasterize(vect(psu_grid), template, field = "ID")

# Crop covariates to eligible PSUs only
cov.dat <- crop(cov.dat, psu_grid, mask=TRUE)
PSU.r <- resample(cov.dat, template)

## 12 - CALCULATE OPTIMAL SAMPLE SIZE ==========================================
# Purpose: Determine how many PSUs needed for representative coverage
# What this does:
#  - Tests different sample sizes (50, 75, 100, ... up to 3000 PSUs)
#  - Measures how well each sample size represents environmental variability
#  - Identifies the "sweet spot" where adding more PSUs gives diminishing returns
#  - Uses Kullback-Leibler divergence to compare sample vs population distributions
#
# Method: Feature Space Coverage (FCS) algorithm
#  - Iteratively samples PSUs and compares to full covariate space
#  - Repeats 4 times per sample size to ensure stability
#  - Selects optimal N where coverage reaches 95% of maximum
#
# COMPUTATION TIME: 
#  - Small areas (<1000 PSUs): 2-6 hours
#  - Medium areas (1000-5000 PSUs): 6-24 hours  
#  - Large areas (>5000 PSUs): 1-3 days
#  - Depends on: # of PSUs, # of covariates, CPU cores available
#
# SKIP THIS SECTION IF:
#  - You already ran it and saved the result (optimal_N_KLD.RDS exists)
#  - You have a predetermined sample size (e.g., budget constraints)
#  - Running quick tests (use arbitrary N like 50-100 PSUs)
#

source("scripts/opt_sample.R")  # Load optimization functions

psu.r.df <- data.frame(PSU.r)

# Optimization parameters
initial.n <- 50
final.n <- 3000
by.n <- 25
iters <- 4

# Run optimization (can take several minutes)
opt_N_fcs <- opt_sample(alg="fcs",
                        s_min=initial.n,
                        s_max=final.n,
                        s_step=by.n,
                        s_reps=iters,
                        covs = psu.r.df,
                        cpus=4,
                        conf=0.95)

optimal_N_KLD <- opt_N_fcs$optimal_sites[1,2]
cat(sprintf("Optimal sample size: %d PSUs\n", optimal_N_KLD))

saveRDS(optimal_N_KLD, paste0(results.path,"../optimal_N_KLD.RDS"))

## 13 - SELECT PSUs USING COVARIATE SPACE COVERAGE =============================
# Purpose: Choose PSU locations that maximize environmental diversity
# Method: Constrained k-means clustering (respects legacy data)

optimal_N_KLD <- readRDS(paste0(results.path,"../optimal_N_KLD.RDS"))

# Allocate samples by land use type
# ADJUST THESE PROPORTIONS based on your study objectives
crop_prop <- 0.85
grass_prop <- 0.10
forest_prop <- 0.05

# Calculate number of PSUs for this land use
n.psu <- round(optimal_N_KLD * crop_prop, 0)  # For cropland

# OPTIONAL: Add buffer to account for skipped PSUs
n.psu <- round(optimal_N_KLD * crop_prop * 1.15, 0)  # 15% buffer

cat(sprintf("Targeting %d PSUs for %s\n", n.psu, landuse))

# Prepare data
PSU.df <- as.data.frame(PSU.r, xy=T)
covs <- names(cov.dat)
mygrd <- data.frame(scale(PSU.df[, covs]))

# If legacy data exists, use constrained clustering
if (exists("legacy")){
  legacy <- st_filter(legacy, psu_grid)
  legacy_df <- st_coordinates(legacy)
  
  # Find nearest PSU for each legacy point
  units <- numeric(nrow(legacy_df))
  for (i in 1:nrow(legacy_df)) {
    distances <- sqrt((PSU.df$x - legacy_df[i, "X"])^2 + (PSU.df$y - legacy_df[i, "Y"])^2)
    units[i] <- which.min(distances)
  }
  
  fixed <- unique(data.frame(units, scale(PSU.df[, covs])[units, ]))
  
  # Run constrained clustering
  res <- CSIS(fixed = fixed, nsup = n.psu, nstarts = iterations, mygrd = mygrd)
  
} else {
  # No legacy data: standard k-means
  res <- kmeans_with_progress(mygrd, centers = n.psu, iter.max = 10000, nstart = iterations)
}

# Assign cluster IDs
PSU.df$cluster <- res$cluster

# Find PSU closest to each cluster center (these become sample locations)
D <- rdist(x1 = res$centers, x2 = scale(PSU.df[, covs]))
units <- apply(D, MARGIN = 1, FUN = which.min)

myCSCsample <- PSU.df[units, c("x", "y", covs)]

# Label legacy vs new PSUs
if (exists("legacy")){
  myCSCsample$type <- c(rep("legacy", nrow(fixed)), rep("new", length(units)-nrow(fixed)))
} else {
  myCSCsample$type <- "new"
}

# Convert to spatial object
myCSCsample <- myCSCsample %>%
  st_as_sf(coords = c("x", "y"), crs = epsg)

# Separate legacy and new
if (exists("legacy")){
  legacy <- myCSCsample[myCSCsample$type=="legacy",]
}
new <- myCSCsample[myCSCsample$type=="new",]

# Extract target PSU IDs
PSUs <- sf::st_intersection(psu_grid, new) %>% dplyr::select(ID)
target.PSUs <- psu_grid[psu_grid$ID %in% PSUs$ID,] %>% dplyr::select(ID)

cat(sprintf("Selected %d target PSUs\n", nrow(target.PSUs)))

png(
  filename = "results/imgs/csc_psu_distribution.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

# Visualize
ggplot() +
  geom_raster(data = as.data.frame(PSU.r$PC1, xy = TRUE), aes(x = x, y = y, fill = PC1)) +
  scale_fill_viridis_c() +
  geom_sf(data = target.PSUs, color = "#101010", fill = NA, lwd = 0.8) +
  geom_sf(data = new[1], color = "#D81B60", size = 0.5, shape = 19) +
  labs(title = "Target Primary Sampling Units") +
  theme_minimal()

dev.off()

## 14 - LOAD HIGH-RESOLUTION COVARIATES (SSU LEVEL) ============================
# Purpose: Load 100m resolution data for SSU clustering WITHIN each PSU
# Note: Different from PSU covariates - these determine SSU placement

cov.dat.ssu <- terra::rast(paste0(raster.path, "HighRes_Covariates_100m_KANSAS_merged.tif"))
names(cov.dat.ssu) <- gsub("(^\\d+_?S2_|^\\d+_|^S2_)", "", names(cov.dat.ssu))

if(crs(cov.dat.ssu)!=epsg){
  cov.dat.ssu <- terra::project(cov.dat.ssu, epsg, method="near")
}

# Check for and replace NA values
# Why: NA values cause complete.cases() to remove SSUs unnecessarily
# Solution: Replace NA with 0 (assumes missing data = no feature present)
if (any(is.na(values(cov.dat.ssu)))) {
  cat("Found NA values in SSU covariates. Replacing with 0...\n")
  na_count <- sum(is.na(values(cov.dat.ssu)))
  total_count <- ncell(cov.dat.ssu) * nlyr(cov.dat.ssu)
  cat(sprintf("Replacing %d NA values (%.2f%% of data)\n", 
              na_count, (na_count/total_count)*100))
  
  cov.dat.ssu[is.na(cov.dat.ssu)] <- 0
  
  cat("NA values replaced with 0\n\n")
} else {
  cat("No NA values found in SSU covariates\n\n")
}

names(cov.dat.ssu)

writeRaster(cov.dat.ssu, paste0(raster.path,"cov_dat_ssu_100m_clean.tif"), overwrite=TRUE)
# Load if needed. Similar process to cov.dat and land use vars.
cov.dat.ssu <- rast(paste0(raster.path,"cov_dat_ssu_100m_clean.tif")) # don't forget to check CRS

## 15 - GENERATE SSUs AND TSUs FOR TARGET PSUs =================================
# Purpose: Within each target PSU, create SSUs and TSUs
# This is the CORE of the three-stage sampling design

# Initialize storage
all_psus_tsus <- list()
selected_ssus <- list()
skipped_psus <- c()

# MAIN LOOP: Process each target PSU
# MAIN LOOP: Process each target PSU to generate SSUs and TSUs
for (psu_id in 1:nrow(target.PSUs)) {
  
  # Select current PSU
  selected_psu <- target.PSUs[psu_id, ]
  
  # STEP 1: Generate 100m×100m SSU grid within the PSU boundary
  ssu_grid <- st_make_grid(selected_psu, cellsize = c(ssu_size, ssu_size), square = TRUE)
  ssu_grid_sf <- st_sf(geometry = ssu_grid)
  
  # Clip SSU grid to exact PSU boundary (removes partial cells outside PSU)
  ssu_grid_sf <- suppressWarnings(st_intersection(ssu_grid_sf, st_geometry(selected_psu)))
  
  # Convert to terra vector format for raster extraction
  ssu_grid_vect <- vect(ssu_grid_sf)
  
  # STEP 2: Calculate crop percentage for each SSU
  # Extracts from 20m resolution crop raster and calculates % coverage
  extracted_values <- extract(crops, ssu_grid_vect, fun = function(x) {
    sum(x > 0, na.rm = TRUE) / length(x) * 100
  })
  
  # Debug output: show range of crop coverage in this PSU
  cat(sprintf("\nPSU %d \n lu values range: %.2f to %.2f\n", 
              psu_id, min(extracted_values[,2]), max(extracted_values[,2])))
  
  # STEP 3: Verify extraction succeeded and assign crop values
  if (ncol(extracted_values) >= 2) {
    ssu_grid_sf$lu <- extracted_values[, 2]
    
    # STEP 4: Handle split geometries from st_intersection
    # st_intersection sometimes creates MULTIPOLYGON - we need to merge them back
    ssu_grid_sf$ssu_temp_id <- 1:nrow(ssu_grid_sf)  # Track original SSU IDs
    ssu_grid_sf <- st_cast(ssu_grid_sf, "POLYGON")  # Split any MULTIPOLYGONs
    
    # Merge split parts back to single polygons per SSU
    ssu_grid_sf <- ssu_grid_sf %>%
      group_by(ssu_temp_id, lu) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      select(-ssu_temp_id)  # Remove temporary ID
    
    # Store count before filtering for reporting
    total_ssus_before <- nrow(ssu_grid_sf)
    
    # STEP 5: Filter SSUs by actual 20m crop pixel count
    # This is more accurate than percentage and prevents TSU generation failures
    cat("Checking crop pixel availability for TSU generation...\n")
    ssu_grid_sf$crop_pixel_count <- sapply(1:nrow(ssu_grid_sf), function(i) {
      ssu_geom <- ssu_grid_sf[i, ]
      ssu_vect <- vect(ssu_geom)
      
      # Crop the 20m raster to this SSU and count valid pixels
      ssu_crop <- try(crop(crops, ssu_vect, mask = TRUE), silent = TRUE)
      if (inherits(ssu_crop, "try-error") || is.null(ssu_crop)) return(0)
      
      crop_vals <- values(ssu_crop, mat = FALSE)
      sum(crop_vals > 0, na.rm = TRUE)
    })
    
    # Minimum pixels needed: number of TSUs + safety buffer
    min_crop_pixels <- number_TSUs + 5
    ssu_grid_sf <- ssu_grid_sf[ssu_grid_sf$crop_pixel_count >= min_crop_pixels, ]
    
    # Report how many SSUs were removed
    cat(sprintf("SSUs after crop pixel filter: %d (removed %d)\n",
                nrow(ssu_grid_sf), total_ssus_before - nrow(ssu_grid_sf)))
    
  } else {
    # Extraction failed - skip this PSU
    warning(paste("PSU", psu_id, "returned insufficient extracted values. Skipping."))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # Progress indicator
  cat(sprintf("\rProgress: %.2f%% (%d out of %d)\n", 
              (psu_id / nrow(target.PSUs)) * 100, psu_id, nrow(target.PSUs)))
  flush.console()
  
  # STEP 6: Check if enough SSUs remain for clustering
  total_ssus <- nrow(ssu_grid_sf)
  min_required_ssus <- max(num_primary_ssus + num_alternative_ssus, 8)  # Default: 4+4=8
  
  if (total_ssus < min_required_ssus) {
    warning(paste("PSU", psu_id, "has only", total_ssus, 
                  "usable SSUs (min required:", min_required_ssus, "). Skipping."))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # STEP 7: Extract 100m environmental covariates for each SSU
  ssu_grid_vect_filtered <- vect(ssu_grid_sf)
  ssu_covariates <- terra::extract(cov.dat.ssu, ssu_grid_vect_filtered, df = TRUE)
  
  # Aggregate covariates if any SSUs were split (collapse to one row per SSU)
  ssu_covariates <- ssu_covariates %>%
    group_by(ID) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
  
  # Verify SSU count matches covariate count (critical for alignment)
  if (nrow(ssu_covariates) != nrow(ssu_grid_sf)) {
    warning(sprintf("PSU %d: Mismatch in SSU and covariate rows (%d vs %d). Skipping.", 
                    psu_id, nrow(ssu_grid_sf), nrow(ssu_covariates)))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # STEP 8: Prepare covariates for k-means clustering
  # Combine spatial data with environmental covariates
  ssu_data <- cbind(ssu_grid_sf, ssu_covariates[, -1])
  ssu_data_values <- st_drop_geometry(ssu_data)
  
  # Separate categorical variables (don't scale these)
  exclude <- grep("^geomorph_|^lu$", names(ssu_data_values), value = TRUE)
  to_scale <- ssu_data_values[, !names(ssu_data_values) %in% exclude]
  to_keep  <- ssu_data_values[, names(ssu_data_values) %in% exclude, drop = FALSE]
  
  # Remove columns with all NA values
  to_scale <- to_scale[, colSums(!is.na(to_scale)) > 0, drop = FALSE]
  
  # Identify zero-variance columns (would cause scaling errors)
  zero_variance_cols <- sapply(to_scale, function(x) sd(x, na.rm = TRUE) == 0)
  zero_variance_cols[is.na(zero_variance_cols)] <- TRUE
  
  # Scale only non-zero-variance columns
  scaled_part <- to_scale
  if (any(!zero_variance_cols)) {
    scaled_part[, !zero_variance_cols] <- scale(to_scale[, !zero_variance_cols])
  }
  
  # STEP 9: Remove incomplete cases and keep ssu_data aligned
  mygrd_ssu <- cbind(to_keep, scaled_part)
  complete_rows <- complete.cases(mygrd_ssu)
  mygrd_ssu <- mygrd_ssu[complete_rows, ]
  ssu_data <- ssu_data[complete_rows, ]  # CRITICAL: Keep aligned with mygrd_ssu!
  
  # Verify enough SSUs remain after removing incomplete cases
  if (nrow(mygrd_ssu) < 4) {
    warning(paste("PSU", psu_id, "has too few SSUs (", nrow(mygrd_ssu), ") to form 4 clusters. Skipping."))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # STEP 10: Cluster SSUs using k-means
  # Number of clusters = number of target SSUs needed
  optimal_k <- num_primary_ssus
  
  kmeans_result <- kmeans(mygrd_ssu[, -1, drop = FALSE], centers = optimal_k, iter.max = 10000, nstart = 10)
  ssu_data$cluster <- as.factor(kmeans_result$cluster)
  
  # STEP 11: Select SSUs closest to cluster centers
  # These become target and replacement SSUs
  D <- rdist(x1 = kmeans_result$centers, x2 = mygrd_ssu[, -1, drop = FALSE])
  target_units <- apply(D, 1, function(x) order(x)[1])      # 1st closest = target
  replacement_units <- apply(D, 1, function(x) order(x)[2]) # 2nd closest = replacement
  
  # Verify we got enough SSUs
  if (length(target_units) < num_primary_ssus || length(replacement_units) < num_alternative_ssus) {
    warning(sprintf("PSU %d did not yield", num_primary_ssus ,"target and", num_alternative_ssus, "replacement SSUs. Skipping.", psu_id))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # STEP 12: Extract selected SSUs and add metadata
  target_ssus <- ssu_data[target_units, ]
  replacement_ssus <- ssu_data[replacement_units, ]
  
  target_ssus$SSU_Type <- "Target"
  replacement_ssus$SSU_Type <- "Replacement"
  target_ssus$SSU_ID <- 1:nrow(target_ssus)
  replacement_ssus$SSU_ID <- (nrow(target_ssus) + 1):(2 * nrow(target_ssus))
  
  # Link each replacement SSU to its corresponding target SSU (same cluster)
  replacement_ssus$replacement_for <- sapply(replacement_ssus$cluster, function(cl) {
    matched <- target_ssus$SSU_ID[target_ssus$cluster == cl]
    if (length(matched) > 0) return(matched[1]) else return(NA)
  })
  
  target_ssus$replacement_for <- NA
  
  # Add PSU identifier
  psu_actual_id <- selected_psu$ID
  target_ssus$PSU_ID <- psu_actual_id
  replacement_ssus$PSU_ID <- psu_actual_id
  
  # Combine target and replacement SSUs
  combined_ssus <- rbind(target_ssus, replacement_ssus)
  
  # STEP 13: Final check - must have exactly expected number of SSUs
  if (nrow(combined_ssus) != (num_primary_ssus + num_alternative_ssus)) {
    warning(sprintf("PSU %d has %d SSUs instead of", num_primary_ssus + num_alternative_ssus, ". Skipping.", psu_id, nrow(combined_ssus)))
    skipped_psus <- c(skipped_psus, psu_id)
    next
  }
  
  # Store SSUs for this PSU
  selected_ssus[[psu_id]] <- combined_ssus
  
  # STEP 14: Generate TSU point samples within each SSU
  primary_tsus <- lapply(1:nrow(target_ssus), function(i) {
    generate_tsu_points_within_ssu(target_ssus[i, ], number_TSUs, target_ssus$SSU_ID[i], "Target", crops)
  })
  
  alternative_tsus <- lapply(1:nrow(replacement_ssus), function(i) {
    generate_tsu_points_within_ssu(replacement_ssus[i, ], number_TSUs, replacement_ssus$SSU_ID[i], "Replacement", crops)
  })
  
  # STEP 15: Verify all TSUs were generated successfully
  all_tsus <- c(primary_tsus, alternative_tsus)
  tsu_counts <- sapply(all_tsus, function(x) if(is.null(x)) 0 else nrow(x))
  
  # Check: each SSU must have exactly number_TSUs points
  if (any(tsu_counts != number_TSUs)) {
    warning(sprintf("PSU %d: TSU generation failed for some SSUs. Expected %d TSUs per SSU, got: %s. Skipping.", 
                    psu_id, number_TSUs, paste(tsu_counts, collapse=",")))
    skipped_psus <- c(skipped_psus, psu_id)
    # Remove this PSU from selected SSUs (failed at final step)
    selected_ssus[[psu_id]] <- NULL
    next
  }
  
  # Store all TSUs for this PSU
  all_psus_tsus[[psu_id]] <- do.call(rbind, Filter(Negate(is.null), all_tsus))
}

# Summary
cat(sprintf("Successfully processed: %d PSUs\n", length(selected_ssus)))
cat(sprintf("Skipped: %d PSUs\n", length(skipped_psus)))
if (length(skipped_psus) > 0) {
  cat(sprintf("Skipped PSU IDs: %s\n", paste(skipped_psus, collapse = ", ")))
}

## 16 - COMBINE AND STRUCTURE TSU DATA =========================================
# Purpose: Merge all TSUs into single spatial dataset with proper structure

all_ssus <- do.call(rbind, selected_ssus)
all_ssus <- all_ssus %>% mutate_at(vars(PSU_ID, SSU_ID), as.numeric)

all_tsus <- do.call(rbind, all_psus_tsus)
all_tsus <- all_tsus %>% mutate_at(vars(PSU_ID, SSU_ID), as.numeric)

# Join TSU and SSU metadata
all_tsus <- st_join(all_tsus, all_ssus[c("PSU_ID", "SSU_ID", "SSU_Type", "replacement_for")])

all_tsus <- all_tsus %>%
  select(PSU_ID = PSU_ID.x, SSU_ID = SSU_ID.y, SSU_Type = SSU_Type.y,
         Replacement_for = replacement_for, TSU_ID, geometry)

# Label TSU types (Target = primary sample, Alternative = backup)
all_tsus <- all_tsus %>%
  group_by(PSU_ID) %>%
  filter(n() == (num_primary_ssus + num_alternative_ssus) * number_TSUs) %>%  # 24 for default
  group_by(PSU_ID, SSU_ID) %>%
  mutate(TSU_Type = ifelse(row_number() == 1, "Target", "Alternative")) %>%
  ungroup()

all_tsus$PSU_Type <- "Target"

all_tsus <- all_tsus %>%
  dplyr::select("PSU_ID", "SSU_ID", "SSU_Type", "Replacement_for", 
                "TSU_ID", "TSU_Type", "geometry")

# Count target samples
n_target_tsus <- all_tsus %>%
  filter(SSU_Type == "Target" & TSU_Type == "Target") %>%
  distinct(PSU_ID, SSU_ID) %>%
  nrow()

cat(sprintf("\nTotal target sampling locations: %d\n", n_target_tsus))

## 17 - VISUALIZE SAMPLING DESIGN ==============================================
# Purpose: Create map showing PSU, SSUs, and TSUs

# Select one PSU for detailed visualization
viz_psu_id <- selected_ssus[[1]]$PSU_ID[1]
selected_psu_viz <- target.PSUs[target.PSUs$ID == viz_psu_id, ]

bbox_psu <- st_bbox(selected_psu_viz)
lu_bbox <- terra::crop(crops, selected_psu_viz, mask = TRUE)

# Filter data for this PSU
tsus_plot <- all_tsus[all_tsus$PSU_ID == viz_psu_id, ]
ssus_plot <- all_ssus[all_ssus$PSU_ID == viz_psu_id, ]

# Create detailed map
labels <- c("Target PSU", "Target SSUs", "Replacement SSUs", "TSUs")

png(
  filename = "results/imgs/target_psu_ssu_tsu.png",
  width = 4000,      # pixels
  height = 2800,     # pixels
  res = 400          # DPI
)

ggplot() +
  geom_raster(data = as.data.frame(lu_bbox, xy = TRUE), aes(x = x, y = y, fill = lu)) +
  guides(fill = "none") +
  geom_sf(data = selected_psu_viz, fill = NA, 
          aes(color = labels[1]), lwd = 0.8, show.legend = TRUE) +
  geom_sf(data = ssus_plot[ssus_plot$SSU_Type == "Target", ], fill = NA, 
          aes(color = labels[2]), lwd = 0.6, show.legend = TRUE) +
  geom_sf(data = ssus_plot[ssus_plot$SSU_Type == "Replacement", ], fill = NA, 
          aes(color = labels[3]), lwd = 0.6, show.legend = TRUE) +
  geom_sf(data = tsus_plot, aes(geometry = geometry, color = labels[4]), 
          size = 0.5, shape = 19, show.legend = TRUE) +
  coord_sf(xlim = c(bbox_psu["xmin"], bbox_psu["xmax"]),
           ylim = c(bbox_psu["ymin"], bbox_psu["ymax"])) +
  labs(title = "Example: three-stage sampling design",
       subtitle = sprintf("PSU %d", viz_psu_id),
       x = "Longitude", y = "Latitude", color = "Legend") +
  scale_color_manual(values = c("Target PSU" = "blue",
                                "Target SSUs" = "blue",
                                "Replacement SSUs" = "red",
                                "TSUs" = "black")) +
  theme_minimal()

dev.off()

ggsave(paste0(results.path,"../sampling_design_example.png"), 
       width = 10, height = 10, dpi = 300)

## 18 - EXPORT TARGET SAMPLING UNITS ===========================================
# Purpose: Save all shapefiles for field work

# Add cluster information
dfr <- PSU.df[,c("x","y","cluster")]
dfr$cluster <- as.numeric(dfr$cluster)
dfr <- rasterFromXYZ(dfr)
crs(dfr) <- epsg

valid.PSU_clusters <- target.PSUs %>% 
  mutate(cluster = extract(dfr, target.PSUs, fun = mean, na.rm = TRUE))

all.PSU_clusters <- psu_grid %>% 
  mutate(cluster = extract(dfr, psu_grid, fun = mean, na.rm = TRUE))

all.PSU_clusters <- na.omit(all.PSU_clusters)

valid.PSU_clusters <- valid.PSU_clusters %>% rename(Replace_ID = cluster)

# Join cluster info to TSUs
all_tsus <- st_join(all_tsus, valid.PSU_clusters)

# Add sampling order
all_tsus <- all_tsus %>%
  group_by(PSU_ID) %>%
  mutate(order = match(SSU_ID, unique(SSU_ID))) %>%
  ungroup()

# Create unique site IDs
# Format: [COUNTRY CODE][PSU ID]-[SSU ID]-[TSU ID][LAND USE]
# Example: KANSAS0001-1-1C (PSU 1, SSU 1, TSU 1, Cropland)
all_tsus$site_id <- paste0(ISO.code, sprintf("%04d", all_tsus$PSU_ID), 
                           "-", all_tsus$SSU_ID, 
                           "-", all_tsus$TSU_ID, "C")  # C for Cropland

# Filter valid PSUs (those with complete TSU sets)
psus_with_tsus <- unique(all_tsus$PSU_ID)
valid.PSU_clusters_filtered <- valid.PSU_clusters %>%
  filter(ID %in% psus_with_tsus)

# Export shapefiles
write_sf(valid.PSU_clusters_filtered, 
         paste0(results.path,"/PSUs_target.shp"), overwrite=TRUE)
write_sf(all_tsus, 
         paste0(results.path,"/TSUs_target.shp"), overwrite=TRUE)
write_sf(all.PSU_clusters, 
         paste0(results.path,"/PSU_pattern_cl.shp"), overwrite=TRUE)
writeRaster(dfr, paste0(results.path,"/clusters.tif"), overwrite=TRUE)


## 19 - CALCULATE REPLACEMENT PSUs ============================================
# Purpose: For each target PSU, find a similar replacement in same cluster
# Why: Field teams need backups if target PSU becomes inaccessible

# Find PSUs NOT selected as targets
remaining.PSU_clusters <- all.PSU_clusters %>%
  filter(!(ID %in% valid.PSU_clusters$ID))

# Get unique cluster IDs from targets
unique_cluster <- distinct(valid.PSU_clusters, Replace_ID)$Replace_ID

# Sample one replacement per cluster
sampled_indices <- integer(0)

for (clust in unique_cluster) {
  candidates_indices <- which(remaining.PSU_clusters$cluster == clust)
  
  if (length(candidates_indices) > 0) {
    sampled_index <- sample(candidates_indices, size = 1)
    sampled_indices <- c(sampled_indices, sampled_index)
  }
}

replacements <- remaining.PSU_clusters[sampled_indices, ]

cat(sprintf("Selected %d replacement PSUs\n", nrow(replacements)))

## 20 - GENERATE SSUs AND TSUs FOR REPLACEMENT PSUs ============================
# Purpose: Create same sampling structure in replacement PSUs
# Note: Code is identical to target PSU loop (Section 15)

alt_psus_tsus_sf <- list()
selected_ssus_sf <- list()
skipped_psus_sf <- c()

# DUPLICATE OF MAIN LOOP FOR REPLACEMENTS
# (See Section 15 for detailed comments)
for (psu_id in 1:nrow(replacements)) {
  selected_psu <- replacements[psu_id, ]
  
  # Generate SSUs within the selected PSU
  ssu_grid <- st_make_grid(selected_psu, cellsize = c(ssu_size, ssu_size), square = TRUE)
  ssu_grid_sf <- st_sf(geometry = ssu_grid)
  ssu_grid_sf <- suppressWarnings(st_intersection(ssu_grid_sf, st_geometry(selected_psu)))
  ssu_grid_vect <- vect(ssu_grid_sf)
  
  # Extract land use values (LU)
  extracted_values <- extract(crops, ssu_grid_vect, fun = function(x) {
    sum(x > 0, na.rm = TRUE) / length(x) * 100
  })
  
  # ADD THIS DEBUG LINE:
  cat(sprintf("\nPSU %d\n lu values range: %.2f to %.2f\n", 
              psu_id, min(extracted_values[,2]), max(extracted_values[,2])))
  
  # Defensive check: ensure extracted_values has at least 2 columns
  if (ncol(extracted_values) >= 2) {
    ssu_grid_sf$lu <- extracted_values[, 2]
    
    # FIX: Handle MULTIPOLYGON geometries that cause row mismatches
    ssu_grid_sf$ssu_temp_id <- 1:nrow(ssu_grid_sf)
    ssu_grid_sf <- st_cast(ssu_grid_sf, "POLYGON")
    
    # Group split geometries back together
    ssu_grid_sf <- ssu_grid_sf %>%
      group_by(ssu_temp_id, lu) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      select(-ssu_temp_id)
    
    # Store count before filtering
    total_ssus_before <- nrow(ssu_grid_sf)
    
    # ONLY USE THIS FILTER: Direct pixel count (accurate!)
    cat("Checking crop pixel availability for TSU generation...\n")
    ssu_grid_sf$crop_pixel_count <- sapply(1:nrow(ssu_grid_sf), function(i) {
      ssu_geom <- ssu_grid_sf[i, ]
      ssu_vect <- vect(ssu_geom)
      ssu_crop <- try(crop(crops, ssu_vect, mask = TRUE), silent = TRUE)
      if (inherits(ssu_crop, "try-error") || is.null(ssu_crop)) return(0)
      crop_vals <- values(ssu_crop, mat = FALSE)
      sum(crop_vals > 0, na.rm = TRUE)
    })
    
    min_crop_pixels <- number_TSUs + 5
    ssu_grid_sf <- ssu_grid_sf[ssu_grid_sf$crop_pixel_count >= min_crop_pixels, ]
    
    cat(sprintf("SSUs after crop pixel filter: %d (removed %d)\n",
                nrow(ssu_grid_sf), total_ssus_before - nrow(ssu_grid_sf)))
    
  } else {
    warning(paste("PSU", psu_id, "returned insufficient extracted values. Skipping."))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  cat(sprintf("\rProgress: %.2f%% (%d out of %d)\n", 
              (psu_id / nrow(replacements)) * 100, psu_id, nrow(replacements)))
  flush.console()
  
  # Count available SSUs
  total_ssus <- nrow(ssu_grid_sf)
  min_required_ssus <- max(num_primary_ssus + num_alternative_ssus, 8)  # 4 clusters x 2 SSUs each
  
  if (total_ssus < min_required_ssus) {
    warning(paste("PSU", psu_id, "has only", total_ssus, 
                  "usable SSUs (min required:", min_required_ssus, "). Skipping."))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  # Extract covariates
  # Convert filtered SSU grid to vector and extract covariates *after filtering*
  ssu_grid_vect_filtered <- vect(ssu_grid_sf)
  ssu_covariates <- terra::extract(cov.dat.ssu, ssu_grid_vect_filtered, df = TRUE)
  
  ssu_covariates <- ssu_covariates %>%
    group_by(ID) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
  
  # Ensure alignment by checking row counts before binding
  if (nrow(ssu_covariates) != nrow(ssu_grid_sf)) {
    warning(sprintf("PSU %d: Mismatch in SSU and covariate rows (%d vs %d). Skipping.", 
                    psu_id, nrow(ssu_grid_sf), nrow(ssu_covariates)))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  ssu_data <- cbind(ssu_grid_sf, ssu_covariates[, -1])
  ssu_data_values <- st_drop_geometry(ssu_data)
  
  exclude <- grep("^geomorph_|^lu$", names(ssu_data_values), value = TRUE)
  to_scale <- ssu_data_values[, !names(ssu_data_values) %in% exclude]
  to_keep  <- ssu_data_values[, names(ssu_data_values) %in% exclude, drop = FALSE]
  
  to_scale <- to_scale[, colSums(!is.na(to_scale)) > 0, drop = FALSE]
  zero_variance_cols <- sapply(to_scale, function(x) sd(x, na.rm = TRUE) == 0)
  zero_variance_cols[is.na(zero_variance_cols)] <- TRUE
  
  scaled_part <- to_scale
  if (any(!zero_variance_cols)) {
    scaled_part[, !zero_variance_cols] <- scale(to_scale[, !zero_variance_cols])
  }
  
  mygrd_ssu <- cbind(to_keep, scaled_part)
  complete_rows <- complete.cases(mygrd_ssu)
  mygrd_ssu <- mygrd_ssu[complete_rows, ]
  # Also filter ssu_data to match
  ssu_data <- ssu_data[complete_rows, ]
  
  if (nrow(mygrd_ssu) < 4) {
    warning(paste("PSU", psu_id, "has too few SSUs (", nrow(mygrd_ssu), ") to form 4 clusters. Skipping."))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  # Fixed number of clusters
  optimal_k <- num_primary_ssus
  
  kmeans_result <- kmeans(mygrd_ssu[, -1, drop = FALSE], centers = optimal_k, iter.max = 10000, nstart = 10)
  ssu_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Compute distances and pick SSUs closest to centers
  D <- rdist(x1 = kmeans_result$centers, x2 = mygrd_ssu[, -1, drop = FALSE])
  target_units <- apply(D, 1, function(x) order(x)[1])
  replacement_units <- apply(D, 1, function(x) order(x)[2])
  
  if (length(target_units) < num_primary_ssus || length(replacement_units) < num_alternative_ssus) {
    warning(sprintf("PSU %d did not yield", num_primary_ssus ,"target and", num_alternative_ssus, "replacement SSUs. Skipping.", psu_id))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  target_ssus <- ssu_data[target_units, ]
  replacement_ssus <- ssu_data[replacement_units, ]
  
  target_ssus$SSU_Type <- "Target"
  replacement_ssus$SSU_Type <- "Replacement"
  target_ssus$SSU_ID <- 1:nrow(target_ssus)
  replacement_ssus$SSU_ID <- (nrow(target_ssus) + 1):(2 * nrow(target_ssus))
  
  replacement_ssus$replacement_for <- sapply(replacement_ssus$cluster, function(cl) {
    matched <- target_ssus$SSU_ID[target_ssus$cluster == cl]
    if (length(matched) > 0) return(matched[1]) else return(NA)
  })
  
  target_ssus$replacement_for <- NA
  
  # Add PSU_ID and combine
  psu_actual_id <- selected_psu$ID
  target_ssus$PSU_ID <- psu_actual_id
  replacement_ssus$PSU_ID <- psu_actual_id
  # Combine SSUs
  combined_ssus <- rbind(target_ssus, replacement_ssus)
  
  # CHECK: Must have exactly 8 SSUs (4 target + 4 replacement)
  if (nrow(combined_ssus) != (num_primary_ssus + num_alternative_ssus)) {
    warning(sprintf("PSU %d has %d SSUs instead of", num_primary_ssus + num_alternative_ssus, ". Skipping.", psu_id, nrow(combined_ssus)))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    next
  }
  
  # Store only if we have exactly 8 SSUs
  selected_ssus_sf[[psu_id]] <- combined_ssus
  
  ### Generate TSUs ###
  primary_tsus <- lapply(1:nrow(target_ssus), function(i) {
    generate_tsu_points_within_ssu(target_ssus[i, ], number_TSUs, target_ssus$SSU_ID[i], "Target", crops)
  })
  
  alternative_tsus <- lapply(1:nrow(replacement_ssus), function(i) {
    generate_tsu_points_within_ssu(replacement_ssus[i, ], number_TSUs, replacement_ssus$SSU_ID[i], "Replacement", crops)
  })
  
  # CHECK: Verify all TSUs were generated successfully
  all_tsus <- c(primary_tsus, alternative_tsus)
  tsu_counts <- sapply(all_tsus, function(x) if(is.null(x)) 0 else nrow(x))
  
  # Each SSU should have exactly number_TSUs (3) TSUs
  if (any(tsu_counts != number_TSUs)) {
    warning(sprintf("PSU %d: TSU generation failed for some SSUs. Expected %d TSUs per SSU, got: %s. Skipping.", 
                    psu_id, number_TSUs, paste(tsu_counts, collapse=",")))
    skipped_psus_sf <- c(skipped_psus_sf, psu_id)
    # Remove this PSU from selected_ssus
    selected_ssus_sf[[psu_id]] <- NULL
    next
  }
  
  alt_psus_tsus_sf[[psu_id]] <- do.call(rbind, Filter(Negate(is.null), all_tsus))
}

cat(sprintf("Replacement PSUs: %d successful, %d skipped\n", 
            length(selected_ssus_sf), length(skipped_psus_sf)))

## 21 - STRUCTURE REPLACEMENT TSU DATA =========================================

all_ssus_combined_sf <- do.call(rbind, selected_ssus_sf)
all_ssus_combined_sf <- all_ssus_combined_sf %>%
  mutate_at(vars(PSU_ID, SSU_ID), as.numeric)

alt_tsus_combined_sf <- do.call(rbind, alt_psus_tsus_sf)
alt_tsus_combined_sf <- alt_tsus_combined_sf %>%
  mutate_at(vars(PSU_ID, SSU_ID), as.numeric)

alt_tsus_combined_sf <- st_join(alt_tsus_combined_sf, 
                                all_ssus_combined_sf[c("PSU_ID", "SSU_ID", "SSU_Type", "replacement_for")])

alt_tsus_combined_sf <- alt_tsus_combined_sf %>%
  select(PSU_ID = PSU_ID.x, SSU_ID = SSU_ID.y, SSU_Type = SSU_Type.y,
         Replacement_for = replacement_for, TSU_ID, geometry)

alt_tsus_combined_sf <- alt_tsus_combined_sf %>%
  group_by(PSU_ID) %>%
  filter(n() == (num_primary_ssus + num_alternative_ssus) * number_TSUs) %>%
  group_by(PSU_ID, SSU_ID) %>%
  mutate(TSU_Type = ifelse(row_number() == 1, "Target", "Alternative")) %>%
  ungroup()

alt_tsus_combined_sf$PSU_Type <- "Replacement"

alt_tsus_combined_sf <- alt_tsus_combined_sf %>%
  dplyr::select("PSU_ID", "SSU_ID", "SSU_Type", "Replacement_for", 
                "TSU_ID", "TSU_Type", "geometry")

n_replacement_tsus <- alt_tsus_combined_sf %>%
  filter(SSU_Type == "Target" & TSU_Type == "Target") %>%
  distinct(PSU_ID, SSU_ID) %>%
  nrow()

cat(sprintf("Total replacement sampling locations: %d\n", n_replacement_tsus))

## 22 - EXPORT REPLACEMENT SAMPLING UNITS ======================================

replacements <- replacements %>% rename(Replace_ID = cluster)

alt_tsus_combined_sf <- st_join(alt_tsus_combined_sf, replacements)

alt_tsus_combined_sf <- alt_tsus_combined_sf %>%
  group_by(PSU_ID) %>%
  mutate(order = match(SSU_ID, unique(SSU_ID))) %>%
  ungroup()

# Create site IDs
alt_tsus_combined_sf$site_id <- paste0(ISO.code, sprintf("%04d", alt_tsus_combined_sf$PSU_ID), 
                                       "-", alt_tsus_combined_sf$SSU_ID, 
                                       "-", alt_tsus_combined_sf$TSU_ID, "C")

# Filter and export
psus_with_tsus_sf <- unique(alt_tsus_combined_sf$PSU_ID)
replacements_filtered <- replacements %>%
  filter(ID %in% psus_with_tsus_sf)

write_sf(replacements_filtered, 
         paste0(results.path,"/PSUs_replacements.shp"), overwrite=TRUE)
write_sf(alt_tsus_combined_sf, 
         paste0(results.path,"/TSUs_replacements.shp"), overwrite=TRUE)


## 23 - AVAILABILITY ANALYSIS ==================================================
# Purpose: Count how many backup PSUs are available per cluster
# Useful for risk assessment and field planning

valid_counts <- valid.PSU_clusters %>%
  group_by(Replace_ID) %>%
  summarise(Count = n())

remaining_counts <- remaining.PSU_clusters %>%
  group_by(cluster) %>%
  summarise(Count = n())

availability <- st_join(valid_counts, remaining_counts, 
                        by = "cluster", suffix = c("_valid", "_remaining"))

write_sf(availability, paste0(results.path,"/availability.shp"), overwrite=TRUE)

################################################################################
## PART 2: MERGE MULTIPLE LAND USES AND CREATE UNIFIED IDs
################################################################################
# Purpose: Combine cropland, grassland, and forest sampling designs
# Creates country-wide unique identifiers for all sites

## 24 - SETUP FOR MERGING ======================================================

# Define folder for merged outputs
folder <- "results/"
folder_all <- paste0(folder, "all/")

if (!file.exists(folder_all)){
  dir.create(folder_all)
}

# Load country boundaries (for provincial statistics)
country_boundaries <- sf::st_read(paste0(folder,"../shapes/roi_kansas_adm2_us_epsg_4326.shp"), quiet=TRUE)
country_boundaries$country <- ISO.code
head(country_boundaries, 5)
country_boundaries$province <- country_boundaries$NAM_2

if(crs(country_boundaries)!=epsg){
  country_boundaries <- country_boundaries %>%
    st_as_sf() %>% sf::st_transform(crs=epsg)
}

## 25 - IMPORT ALL LAND USE SHAPEFILES =========================================
# Purpose: Load PSUs and TSUs from all three land use types

# Initialize empty lists
psus_target_list <- list()
tsus_target_list <- list()
psus_repl_list <- list()
tsus_repl_list <- list()

# Define land use types to check
landuse_types <- c("cropland", "grassland", "forest")
landuse_codes <- c("C", "G", "F")

# TARGET PSUs - Load only if files exist
for (i in seq_along(landuse_types)) {
  file_path <- paste0(folder, landuse_types[i], "/PSUs_target.shp")
  
  if (file.exists(file_path)) {
    temp_psu <- sf::st_read(file_path, quiet = TRUE)
    temp_psu$lulc <- landuse_codes[i]
    psus_target_list[[landuse_types[i]]] <- temp_psu
    cat(sprintf("  ✓ Loaded %s (%d PSUs)\n", landuse_types[i], nrow(temp_psu)))
  } else {
    cat(sprintf("  ✗ Skipped %s (file not found)\n", landuse_types[i]))
  }
}

# TARGET TSUs - Load only if files exist
for (i in seq_along(landuse_types)) {
  file_path <- paste0(folder, landuse_types[i], "/TSUs_target.shp")
  
  if (file.exists(file_path)) {
    temp_tsu <- sf::st_read(file_path, quiet = TRUE)
    temp_tsu$lulc <- landuse_codes[i]
    tsus_target_list[[landuse_types[i]]] <- temp_tsu
    cat(sprintf("  ✓ Loaded %s (%d TSUs)\n", landuse_types[i], nrow(temp_tsu)))
  } else {
    cat(sprintf("  ✗ Skipped %s (file not found)\n", landuse_types[i]))
  }
}

# REPLACEMENT PSUs - Load only if files exist
for (i in seq_along(landuse_types)) {
  file_path <- paste0(folder, landuse_types[i], "/PSUs_replacements.shp")
  
  if (file.exists(file_path)) {
    temp_psu <- sf::st_read(file_path, quiet = TRUE)
    temp_psu$lulc <- landuse_codes[i]
    psus_repl_list[[landuse_types[i]]] <- temp_psu
    cat(sprintf("  ✓ Loaded %s (%d PSUs)\n", landuse_types[i], nrow(temp_psu)))
  } else {
    cat(sprintf("  ✗ Skipped %s (file not found)\n", landuse_types[i]))
  }
}

# REPLACEMENT TSUs - Load only if files exist
for (i in seq_along(landuse_types)) {
  file_path <- paste0(folder, landuse_types[i], "/TSUs_replacements.shp")
  
  if (file.exists(file_path)) {
    temp_tsu <- sf::st_read(file_path, quiet = TRUE)
    temp_tsu$lulc <- landuse_codes[i]
    tsus_repl_list[[landuse_types[i]]] <- temp_tsu
    cat(sprintf("  ✓ Loaded %s (%d TSUs)\n", landuse_types[i], nrow(temp_tsu)))
  } else {
    cat(sprintf("  ✗ Skipped %s (file not found)\n", landuse_types[i]))
  }
}

## 26 - MERGE LAND USE TYPES ===================================================
# Purpose: Combine all available land uses into single datasets

# Merge TARGET PSUs
psus_target <- sf::st_as_sf(data.table::rbindlist(psus_target_list))
cat(sprintf("✓ Merged %d target PSUs from %d land use type(s)\n", 
            nrow(psus_target), length(psus_target_list)))

# Merge TARGET TSUs
tsus_target <- sf::st_as_sf(data.table::rbindlist(tsus_target_list))
cat(sprintf("✓ Merged %d target TSUs from %d land use type(s)\n", 
            nrow(tsus_target), length(tsus_target_list)))

# Merge REPLACEMENT PSUs (if any exist)
if (length(psus_repl_list) > 0) {
  psus_repl <- sf::st_as_sf(data.table::rbindlist(psus_repl_list))
  cat(sprintf("✓ Merged %d replacement PSUs from %d land use type(s)\n", 
              nrow(psus_repl), length(psus_repl_list)))
} else {
  warning("No replacement PSU files found - skipping replacement PSU merge")
}

# Merge REPLACEMENT TSUs (if any exist)
if (length(tsus_repl_list) > 0) {
  tsus_repl <- sf::st_as_sf(data.table::rbindlist(tsus_repl_list))
  cat(sprintf("✓ Merged %d replacement TSUs from %d land use type(s)\n", 
              nrow(tsus_repl), length(tsus_repl_list)))
} else {
  warning("No replacement TSU files found - skipping replacement TSU merge")
}

# Summary by land use
for (lu in unique(psus_target$lulc)) {
  lu_name <- switch(lu,
                    "C" = "Cropland",
                    "G" = "Grassland", 
                    "F" = "Forest",
                    lu)
  n_psus <- sum(psus_target$lulc == lu)
  n_tsus <- sum(tsus_target$lulc == lu)
  cat(sprintf("  %s: %d PSUs, %d TSUs\n", lu_name, n_psus, n_tsus))
}

## 27 - CREATE UNIQUE IDs FOR TARGET PSUs ======================================
# Purpose: Assign sequential IDs across all land uses
# ID Structure: Country-wide sequential number

# Create composite ID (cluster + land use)
psus_target$PSU_R_LULC_ID <- paste0(psus_target$Replace_ID, "-", psus_target$lulc)

# Assign sequential country-wide IDs
psus_target[[paste0(ISO.code,"_PSU_ID")]] <- 1:nrow(psus_target)

psus_target <- psus_target %>%
  select(ID, Replace_ID, lulc, PSU_R_LULC_ID, 
         all_of(paste0(ISO.code, "_PSU_ID")), everything())

cat(sprintf("Assigned %d unique PSU IDs\n", nrow(psus_target)))

## 28 - CREATE UNIQUE IDs FOR REPLACEMENT PSUs =================================

psus_repl$PSU_R_LULC_ID <- paste0(psus_repl$Replace_ID, "-", psus_repl$lulc)

# Continue numbering after target PSUs
start_id <- nrow(psus_target) + 1
psus_repl[[paste0(ISO.code,"_PSU_ID")]] <- start_id:(start_id + nrow(psus_repl) - 1)

psus_repl <- psus_repl %>%
  select(ID, Replace_ID, lulc, PSU_R_LULC_ID, 
         all_of(paste0(ISO.code, "_PSU_ID")), everything())

## 29 - LINK TARGETS AND REPLACEMENTS ==========================================
# Purpose: Each target PSU knows its replacement PSU ID

# Match replacement IDs to targets
index <- match(psus_target$PSU_R_LULC_ID, psus_repl$PSU_R_LULC_ID)
psus_target[["PSU_R_ID"]] <- psus_repl[[paste0(ISO.code,"_PSU_ID")]][index]

psus_target <- psus_target %>%
  select(ID, Replace_ID, lulc, PSU_R_LULC_ID, PSU_R_ID, 
         all_of(paste0(ISO.code, "_PSU_ID")), everything())

## 30 - ASSIGN UNIQUE IDs TO TARGET TSUs =======================================
# Purpose: Create site-level unique identifiers
# ID Format: COUNTRY[####]-[#]-[#]L
# Example: TUN0001-1-1C = Tunisia, PSU 1, SSU 1, TSU 1, Cropland

# Create matching key
psus_target$PSU_T_LULC_ID <- paste0(psus_target$ID, "-", psus_target$lulc)
tsus_target$PSU_T_LULC_ID <- paste0(tsus_target$PSU_ID, "-", tsus_target$lulc)

# Transfer PSU IDs to TSUs
index <- match(tsus_target$PSU_T_LULC_ID, psus_target$PSU_T_LULC_ID)
tsus_target[[paste0(ISO.code,"_PSU_ID")]] <- psus_target[[paste0(ISO.code,"_PSU_ID")]][index]
tsus_target[["PSU_R_ID"]] <- psus_target[["PSU_R_ID"]][index]

# Clean up column names
tsus_target <- tsus_target %>%
  rename_with(~ str_replace_all(., c("Typ" = "Type", "^Rplcmn_" = "SSU_Repl")))

tsus_target$PSU_Type <- "Target"

tsus_target <- tsus_target %>%
  select(all_of(paste0(ISO.code, "_PSU_ID")), PSU_Type, order, 
         SSU_Type, SSU_Repl, TSU_ID, TSU_Type, site_id, lulc, PSU_R_ID)

# Rename for clarity
names(tsus_target) <- gsub(paste0(ISO.code, "_PSU_ID"), "PSU_ID", names(tsus_target))
names(tsus_target) <- gsub("order", "SSU_ID", names(tsus_target))

# FINAL SITE ID GENERATION
# This is the ID field teams will use in the field
tsus_target$site_id <- paste0(ISO.code, sprintf("%04d", tsus_target$PSU_ID), 
                              "-", tsus_target$SSU_ID, 
                              "-", tsus_target$TSU_ID, tsus_target$lulc)

cat(sprintf("Created %d unique site IDs\n", nrow(tsus_target)))

## 31 - ASSIGN UNIQUE IDs TO REPLACEMENT TSUs ==================================

psus_repl$PSU_T_LULC_ID <- paste0(psus_repl$ID, "-", psus_repl$lulc)
tsus_repl$PSU_T_LULC_ID <- paste0(tsus_repl$PSU_ID, "-", tsus_repl$lulc)

# Transfer IDs
index <- match(tsus_repl$PSU_T_LULC_ID, psus_repl$PSU_T_LULC_ID)
tsus_repl[[paste0(ISO.code,"_PSU_ID")]] <- psus_repl[[paste0(ISO.code,"_PSU_ID")]][index]

# Link to target PSUs
tsus_repl[["PSU_T_ID"]] <- psus_target[[paste0(ISO.code, "_PSU_ID")]][
  match(tsus_repl[[paste0(ISO.code,"_PSU_ID")]], psus_target$PSU_R_ID)]

tsus_repl <- tsus_repl %>%
  rename_with(~ str_replace_all(., c("Typ" = "Type", "^Rplcmn_" = "SSU_Repl")))

tsus_repl$PSU_Type <- "Replacement"

tsus_repl <- tsus_repl %>%
  select(all_of(paste0(ISO.code, "_PSU_ID")), PSU_Type, order, 
         SSU_Type, SSU_Repl, TSU_ID, TSU_Type, site_id, lulc, PSU_T_ID)

names(tsus_repl) <- gsub(paste0(ISO.code, "_PSU_ID"), "PSU_ID", names(tsus_repl))
names(tsus_repl) <- gsub("order", "SSU_ID", names(tsus_repl))

tsus_repl$site_id <- paste0(ISO.code, sprintf("%04d", tsus_repl$PSU_ID), 
                            "-", tsus_repl$SSU_ID, 
                            "-", tsus_repl$TSU_ID, tsus_repl$lulc)

## 32 - EXPORT MERGED SHAPEFILES ===============================================
# Purpose: Save final, field-ready shapefiles

# TARGET PSUs (simplified)
psus_target_export <- psus_target %>%
  select(PSU_ID = all_of(paste0(ISO.code, "_PSU_ID")), 
         Replace_ID = PSU_R_ID, lulc)

write_sf(psus_target_export, paste0(folder_all, "all_psus_target.shp"), overwrite = T)

# REPLACEMENT PSUs
psus_repl_export <- psus_repl %>%
  select(Replace_ID = all_of(paste0(ISO.code, "_PSU_ID")), lulc)

write_sf(psus_repl_export, paste0(folder_all, "all_psus_replacements.shp"), overwrite = T)

# TARGET TSUs
names(tsus_target) <- gsub("PSU_R_ID", "Replace_ID", names(tsus_target))

write_sf(tsus_target, paste0(folder_all, "all_tsus_target.shp"), overwrite = T)

# REPLACEMENT TSUs
names(tsus_repl) <- gsub("PSU_T_ID", "PSU_Target_ID", names(tsus_repl))

write_sf(tsus_repl, paste0(folder_all, "all_tsus_replacements.shp"), overwrite = T)

cat("\n✓ All merged shapefiles exported to:", folder_all, "\n")

## 33 - GENERATE SUMMARY STATISTICS ============================================
# Purpose: Create distribution tables and graphs

# Extract unique target sites only (1 per SSU)
tsus_uniq_sites <- dplyr::filter(tsus_target, 
                                 SSU_Type == "Target" & TSU_Type == "Target")

# Join with provinces
tsus_uniq_sites <- sf::st_join(tsus_uniq_sites, country_boundaries)
tsus_uniq_sites <- tsus_uniq_sites %>%
  dplyr::select(c(site_id, country, province, lulc, geometry))

# Country-level statistics
sites_distribution <- as.data.frame(tsus_uniq_sites) %>%
  group_by(country, lulc) %>%
  summarise(Sites = n(), .groups = "drop")

print(sites_distribution)

# Visualize country distribution
custom_colors <- RColorBrewer::brewer.pal(3, "BrBG")

ggplot(sites_distribution, aes(x = lulc, y = Sites, fill = lulc)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Sites), vjust = -0.5, size = 4) +
  labs(title = "Sampling Site Distribution by Land Use",
       x = "Land Use", y = "Number of Sites", fill = "Land Use") +
  scale_x_discrete(labels = c("C" = "Cropland", "F" = "Forest", "G" = "Grassland")) +
  scale_fill_manual(values = custom_colors, 
                    labels = c("C" = "Cropland", "F" = "Forest", "G" = "Grassland")) +
  ylim(0, max(sites_distribution$Sites) * 1.2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "top")

ggsave(paste0(folder, "/final_site_distribution.png"), 
       width = 10, height = 6, dpi = 300)

## 34 - PROVINCIAL STATISTICS ==================================================

# Province-level statistics
sites_distribution_prov <- as.data.frame(tsus_uniq_sites) %>%
  group_by(province, lulc) %>%
  summarise(Sites = n(), .groups = "drop")

print(sites_distribution_prov)

# Visualize provincial distribution
ncolors <- length(unique(sites_distribution_prov$province))
custom_colors_prov <- colorRampPalette(brewer.pal(11, "Spectral"))(ncolors)

ggplot(sites_distribution_prov, aes(x = lulc, y = Sites, fill = province)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Sites), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(title = "Site Distribution by Province and Land Use",
       x = "Land Use", y = "Number of Sites", fill = "Province") +
  scale_x_discrete(labels = c("C" = "Cropland", "F" = "Forest", "G" = "Grassland")) +
  scale_fill_manual(values = custom_colors_prov) +
  ylim(0, max(sites_distribution_prov$Sites) * 1.2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "top")

# The user can adjust the width and height for better visualisation
ggsave(paste0(folder, "/final_site_distribution_province.png"), 
       width = 12, height = 8, dpi = 300)

################################################################################
## END
################################################################################

