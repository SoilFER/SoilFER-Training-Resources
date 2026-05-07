#
# Digital Soil Mapping: Modelling & Mapping
# SoilFER Training - Module 5
#
# This script demonstrates DSM workflows for three variable types:
#   - Continuous (pH) using Quantile Regression Forest
#   - Nominal (Clay_pH_Class) using Classification Random Forest
#   - Ordinal (pH_Class) using integer-encoded Regression RF
#
# Structure:
#   SESSION 1: Data preparation and Continuous modelling
#   SESSION 4: Area of Applicability (AOA) using CAST
#

rm(list = ls())
gc()

# SESSION 1 ====================================================================

# 1. Setup and packages --------------------------------------------------------

library(tidyverse)
library(caret)
library(terra)
library(Boruta)
library(ranger)
library(mapview)


Sys.setenv(PROJ_LIB = "")  
Sys.setenv(PROJ_DATA = system.file("proj", package = "terra"))

# Set working directory to script location (works in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../")

# Create output directories
for (d in c("03_outputs/module3/models/", 
            "03_outputs/module3/validation/", 
            "03_outputs/module3/tiles/",
            "03_outputs/module3/maps/",
            "03_outputs/module3/maps/aoa/",
            "terra_tmp")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Terra settings: show progress bar, use 60% of RAM, use local temp folder
terraOptions(progress = 1, memfrac = 0.6, tempdir = file.path(getwd(), "terra_tmp"))


# 2. Load covariates -----------------------------------------------------------

covs <- rast("01_data/module5/Env_Cov_250m_KANSAS.tif")  # EDIT THIS: your covariate stack

cov_names <- names(covs)

# Inspect the covariate raster
covs[[1]]  # Shows first covariate resolution, extent, CRS
nlyr(covs) # Number of layers
plot(covs[[1]])

# 3. Load and transform soil data ----------------------------------------------

dat <- read_csv("03_outputs/module1/KSSL_DSM_0-30.csv") 

# Inspect the data
dat
summary(dat)

## 3.1 Create bulk density with a pedotransfer function (Saxton)----------------

dat <- dat %>%
  mutate(BD = 1.35 + 0.0045 * Sand + 0.0035 * Clay - 0.06 * 1.72 * SOC)

# Bonus track: Estimate Available Water Capacity (Rawl and Saxto 1982)

# dat <- dat %>% 
#   mutate(
#     # 1. Convert percentage inputs to decimal fractions for model constants
#     S = Sand / 100,
#     C = Clay / 100,
#     # Convert SOC to Organic Matter (OM) using the Van Bemmelen factor and scale to decimal
#     OM = (SOC * 1.724) / 100,
#     
#     # 2. Estimate Wilting Point (WP) at 1500 kPa
#     # First solution for 1500 kPa moisture
#     theta_1500t = -0.024 * S + 0.487 * C + 0.006 * OM + 
#       0.005 * (S * OM) - 0.013 * (C * OM) + 
#       0.068 * (S * C) + 0.031,
#     
#     # Final WP adjustment using the lack-of-fit equation
#     WP = theta_1500t + (0.14 * theta_1500t - 0.02),
#     
#     # 3. Estimate Field Capacity (FC) at 33 kPa
#     # First solution for 33 kPa moisture
#     theta_33t = -0.251 * S + 0.195 * C + 0.011 * OM + 
#       0.006 * (S * OM) - 0.027 * (C * OM) - 
#       0.452 * (S * C) + 0.299,
#     
#     # Final FC adjustment using the lack-of-fit equation
#     FC = theta_33t + (1.283 * (theta_33t^2) - 0.374 * theta_33t - 0.015),
#     
#     # 4. Calculate Available Water Capacity (AWC)
#     # Result is in volumetric water content (%v) as a decimal fraction
#     AWC = FC - WP
#   ) %>%
#   # Clean up auxiliary variables
#   select(-S, -C, -theta_1500t, -theta_33t, OM)


## 4. dat to spatial points and to match covariates ----------------------------
dat_pts <- vect(dat, geom = c("lon", "lat"), crs = "epsg:4326")
dat_pts <- terra::project(dat_pts, covs)
mapview(dat_pts, cex=1.5) #+ mapview(covs[[1]])

# Extract covariates at sample locations 
extracted_covs <- terra::extract(x = covs, y = dat_pts, xy = TRUE, ID = FALSE)
summary(extracted_covs)
dat <- as.data.frame(dat_pts)
dat_cov <- bind_cols(dat, extracted_covs)

# 5. Feature selection with Boruta ---------------------------------------------
# Define the target soil property
target <- "SOC"  # EDIT THIS: your continuous target variable

# Prepare training data (target + covariates, complete cases only)
d <- dat_cov %>%
  dplyr::select(all_of(target), all_of(cov_names)) %>%
  na.omit() %>% 
  as.data.frame()
# Observe d
View(d)

# Run Boruta feature selection
set.seed(1)
boruta_result <- Boruta(
  y = d[,target],
  x = d[,cov_names],
  maxRuns = 25, # this should be >100
  doTrace = 1  # Show progress
)

# Save Boruta plot
png(paste0("03_outputs/module3/figures/boruta_",target,".png"), 
    width = 15, height = 20, units = "cm", res = 150)
par(las = 1, mar = c(4, 10, 4, 2) + 0.1)
plot(boruta_result, horizontal = TRUE,las=1, 
          ylab = "", xlab = "Importance", cex.axis = 0.6)
dev.off()

# Observe the figure
par(las = 1, mar = c(4, 10, 4, 2) + 0.1)
plot(boruta_result, horizontal = TRUE,las=1, 
     ylab = "", xlab = "Importance", cex.axis = 0.6)

# Get selected features (confirmed (green) + tentative (yellow))
selected_features <- getSelectedAttributes(boruta_result, withTentative = TRUE)


# 6. Train and validate the Quantile Regression Forest (qrf) model -------------

# Set up Cross-validation process
cv_control <- trainControl(
  method = "repeatedcv",
  number = 5,   # ideal number 10-20
  repeats = 5,   # idel number 10-20
  savePredictions = TRUE
)

# Set up hyperparameter grid for ranger
mtry_base <- round(length(selected_features) / 3)
tune_grid <- expand.grid(
  mtry = c(mtry_base - round(mtry_base/2), 
           mtry_base, 
           mtry_base + round(mtry_base/2)
           ),
  min.node.size = 5,
  splitrule = c("variance", "extratrees")
)

# Train the model
# quantreg=TRUE enables quantile predictions (for uncertainty maps)
# num.threads uses multiple CPU cores for faster training
model <- caret::train(
  y = d[,target],
  x = d[,selected_features],
  method = "ranger",
  quantreg = TRUE,
  importance = "permutation",
  trControl = cv_control,
  tuneGrid = tune_grid,
  num.threads = max(1, parallel::detectCores() - 1)  # EDIT THIS: number of CPU threads
)

model

model$bestTune

# Variance importance
varImp(model_cont)

# Save variable importance plot
png(paste0("03_outputs/module3/figures/varImp_",target,".png"),
    width = 15, height = 15, units = "cm", res = 150)
plot(varImp(model_cont), main = "Variable Importance - pH")
dev.off()

# View the variable importance
plot(varImp(model_cont), main = "Variable Importance - pH")


# 7. Accuracy assessment -------------------------------------------------------

# Extract CV predictions for the best hyperparameter combination
cv_preds <- model$pred %>%
  filter(mtry == model$bestTune$mtry,
         splitrule == model$bestTune$splitrule,
         min.node.size == model$bestTune$min.node.size)

obs_vals <- cv_preds$obs
pred_vals <- cv_preds$pred

# Calculate validation metrics
load("02_scripts/module3/eval.RData")
accuracy <- eval(pred_vals, obs_vals)[, 1:6]

accuracy

# Scatterplot of observed vs predicted
residuals <- tibble(Observed = obs_vals, Predicted = pred_vals)
g_scatter <- ggplot(residuals, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  ylim(c(min(residuals$Observed), max(residuals$Observed))) + theme(aspect.ratio=1)+
  coord_fixed() +
  labs(title = paste("Observed vs Predicted -", target)) +
  theme_minimal()

g_scatter

ggsave(g_scatter, 
       filename = paste0("03_outputs/module3/figures/scatterplot_",target,".png"),
       width = 12, height = 12, units = "cm")


write_csv(accuracy, "03_outputs/module3/validation/accuracy.csv")


# Save the qrf model
saveRDS(model, paste0("03_outputs/module3/models/ranger_model_",target,".rds"))

# SESSION 2 ====================================================================

# Set working directory to script location (works in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../")

# Load required libraries and objects

library(tidyverse)
library(caret)
library(terra)
library(Boruta)
library(ranger)
library(mapview)
Sys.setenv(PROJ_LIB = "")  
Sys.setenv(PROJ_DATA = system.file("proj", package = "terra"))

covs <- rast("01_data/module5/Env_Cov_250m_KANSAS.tif")
cov_names <- names(covs)
dat <- read_csv("03_outputs/module1/KSSL_DSM_0-30.csv") 
target <- "SOC"
model <- readRDS(paste0("03_outputs/module3/models/ranger_model_",target,".rds"))


# 8. Spatial prediction (tiled) ------------------------------------------------

## 8.1 Create tile grid for memory-efficient prediction ------------------------

r <-covs[[1]]
t <- rast(nrows = 5, ncols = 10, extent = ext(r), crs = crs(r))
tile <- makeTiles(r, t,overwrite=TRUE,filename="03_outputs/module3/tiles/tiles.tif")

# vew the first tile
rast(tile[1]) %>% plot

## 8.2 Prepare model for prediction (single-threaded per tile) -----------------
ranger_model <- model$finalModel

# Prediction function for quantile RF (returns transposed predictions)
pfun <- function(...) {
  predict(...)$predictions |> t()
}

# GDAL write options: compress output, use tiled format for faster reading
gdal_opts <- c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")


## 8.3 Prediction for one tile -------------------------------------------------

t1 <- rast(tile[1])

# crop the selected covariates with the tile 1
covs_t1 <- crop(covs, t1)

plot(covs_t1[[1:9]])

# Prediction of the conditional mean
pred_mean_t1 <- terra::interpolate(covs_t1, 
                              model = ranger_model, 
                              fun=pfun, 
                              na.rm=TRUE,
                              type = "quantiles",
                              what=mean)

# Prediction of the conditional standard deviation
pred_sd_t1 <- terra::interpolate(covs_t1, 
                                 model = ranger_model, 
                                 fun=pfun, 
                                 na.rm=TRUE,
                                 type = "quantiles",
                                 what=sd)

# Compute the coeficient of variation
pred_cv_t1 <- pred_sd_t1/pred_mean_t1


# Explore the results at one tile
mapview(pred_mean_t1) 



## 8.4 Prediction at all tiles (it may take time) ------------------------------

# Loop through tiles and predict mean + SD
for (j in seq_along(tile)) {
  print(paste("Processing tile", j, "of", length(tile)))

  # Crop covariates to this tile
  t <- rast(tile[j])
  covs_tile <- crop(covs, t)

  # Predict MEAN
  mean_file <- paste0("03_outputs/module3/tiles/", target, "_mean_", j, ".tif")
  terra::interpolate(
    covs_tile,
    model = ranger_model,
    fun = pfun,
    na.rm = TRUE,
    type = "quantiles",
    what = mean,
    filename = mean_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = gdal_opts)
  )

  # Predict SD (uncertainty)
  sd_file <- paste0("03_outputs/module3/tiles/", target, "_sd_", j, ".tif")
  terra::interpolate(
    covs_tile,
    model = ranger_model,
    fun = pfun,
    na.rm = TRUE,
    type = "quantiles",
    what = sd,
    filename = sd_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = gdal_opts)
  )
}

# 9. Mosaic tiles into final maps ----------------------------------------------

## 9.1 Mosaic all mean tiles ---------------------------------------------------
# Find all mean tiles and mosaic them
mean_tiles <- list.files("03_outputs/module3/tiles/",
                         pattern = paste0("^", target, "_mean_.*\\.tif$"),
                         full.names = TRUE)

# Create a SpatRasterCollection and mosaic
mean_rasters <- list()
for (i in 1:length(mean_tiles)) {
  mean_rasters[[i]] <- rast(mean_tiles[i])
}
pred_mean <- mosaic(sprc(mean_rasters))
names(pred_mean) <- paste(target, "_mean")

# Save final mean map
writeRaster(pred_mean, paste0("03_outputs/module3/maps/mean_", target, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

## 9.2 Mosaic all sd tiles -----------------------------------------------------
# Same for SD tiles
sd_tiles <- list.files("03_outputs/module3/tiles/",
                       pattern = paste0("^", target, "_sd_.*\\.tif$"),
                       full.names = TRUE)

sd_rasters <- list()
for (i in 1:length(sd_tiles)) {
  sd_rasters[[i]] <- rast(sd_tiles[i])
}
pred_sd <- mosaic(sprc(sd_rasters))
names(pred_sd) <- paste(target, "_sd")

writeRaster(pred_sd, paste0("outputs/maps/continuous/sd_", target_cont, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

# Display maps
plot(pred_mean, main = paste("Mean", target), col = hcl.colors(100, "Viridis"))
plot(pred_sd, main = paste("SD", target), col = hcl.colors(100, "Viridis"))



