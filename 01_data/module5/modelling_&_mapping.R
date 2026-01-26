#
# Digital Soil Mapping: Modelling & Mapping
# SoilFER Training - Module 5
#
# This script demonstrates DSM workflows for three variable types:
#   - Continuous (pH, Clay)
#   - Nominal (Clay_pH_Class)
#   - Ordinal (pH_Class)
#
# Structure:
#   Session 1: Configuration, Data Preparation, Continuous Modelling
#   Session 2: Nominal Classification
#   Session 3: Ordinal Modelling
#

rm(list = ls())
gc()

# SECTION 0: CONFIGURATION (Edit only this section) ----

# File paths
PATH_SOIL_DATA   <- "harmonized_subset_data.csv"
PATH_COVARIATES  <- "HighRes_Cov_100m_KANSAS.tif"
PATH_MASK        <- "Cropland_Mask_KANSAS.tif"

# Target variables
TARGET_CONTINUOUS <- c("pH", "Clay")
TARGET_NOMINAL    <- "Clay_pH_Class"
TARGET_ORDINAL    <- "pH_Class"

# Cross-validation settings
CV_FOLDS   <- 10
CV_REPEATS <- 5
SEED       <- 2025

# Tile grid parameters (fewer, larger tiles = faster)
TILE_ROWS <- 5
TILE_COLS <- 1

# Parallel processing (Windows-compatible PSOCK)
N_WORKERS <- max(1, parallel::detectCores() - 1)
USE_PARALLEL_TILES <- TRUE

# Output directories
DIR_MODELS     <- "outputs/models/"
DIR_VALIDATION <- "outputs/validation/"
DIR_TILES      <- "outputs/tiles/"
DIR_MAPS_CONT  <- "outputs/maps/continuous/"
DIR_MAPS_NOM   <- "outputs/maps/nominal/"
DIR_MAPS_ORD   <- "outputs/maps/ordinal/"

# GDAL write options for faster I/O
GDAL_OPTIONS <- c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")


# SECTION 0.1: Setup packages and directories ----

library(tidyverse)
library(caret)
library(terra)
library(Boruta)
library(ranger)
library(future)
library(future.apply)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Create output directories including terra temp
dirs <- c(DIR_MODELS, DIR_VALIDATION, DIR_TILES,
          DIR_MAPS_CONT, DIR_MAPS_NOM, DIR_MAPS_ORD,
          file.path(getwd(), "terra_tmp"))
for (d in dirs) {
 if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Terra performance settings (Windows-friendly)
terraOptions(progress = 1, memfrac = 0.6,
            tempdir = file.path(getwd(), "terra_tmp"))

set.seed(SEED)

cat("=== Configuration loaded ===\n")
cat("Soil data:", PATH_SOIL_DATA, "\n")
cat("Covariates:", PATH_COVARIATES, "\n")
cat("CV:", CV_FOLDS, "folds x", CV_REPEATS, "repeats\n")
cat("Parallel workers:", N_WORKERS, "\n")


# SECTION 0.2: Helper functions ----

# Validation metrics for continuous variables
calc_validation_metrics <- function(obs, pred) {
 data.frame(
   ME   = mean(pred - obs),
   RMSE = sqrt(mean((pred - obs)^2)),
   R2   = cor(obs, pred)^2,
   NSE  = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
 )
}

# Prediction function for quantile RF
pfun_qrf <- function(...) {
 predict(...)$predictions |> t()
}

# Single tile prediction (continuous/ordinal with mean+sd)
predict_tile_continuous <- function(tile_ext, covs_subset, model, out_prefix,
                                   tile_idx, out_dir) {
 covs_tile <- crop(covs_subset, tile_ext)

 # Predict mean
 mean_file <- paste0(out_dir, out_prefix, "_mean_tile_", tile_idx, ".tif")
 terra::interpolate(covs_tile, model = model, fun = pfun_qrf, na.rm = TRUE,
                    type = "quantiles", what = mean, filename = mean_file,
                    overwrite = TRUE, wopt = list(datatype = "FLT4S",
                    gdal = GDAL_OPTIONS))

 # Predict SD
 sd_file <- paste0(out_dir, out_prefix, "_sd_tile_", tile_idx, ".tif")
 terra::interpolate(covs_tile, model = model, fun = pfun_qrf, na.rm = TRUE,
                    type = "quantiles", what = sd, filename = sd_file,
                    overwrite = TRUE, wopt = list(datatype = "FLT4S",
                    gdal = GDAL_OPTIONS))

 return(c(mean_file, sd_file))
}

# Single tile prediction (nominal - class probabilities)
predict_tile_nominal <- function(tile_ext, covs_subset, model, class_names,
                                out_prefix, tile_idx, out_dir) {
 covs_tile <- crop(covs_subset, tile_ext)

 pred_probs <- terra::predict(covs_tile, model = model, na.rm = TRUE,
                              type = "response",
                              fun = function(m, ...) predict(m, ...)$predictions)
 names(pred_probs) <- class_names

 out_file <- paste0(out_dir, out_prefix, "_probs_tile_", tile_idx, ".tif")
 writeRaster(pred_probs, out_file, overwrite = TRUE,
             wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))
 return(out_file)
}

# Mosaic tiles using VRT for memory efficiency
mosaic_tiles_vrt <- function(tile_files, output_file) {
 if (length(tile_files) == 1) {
   r <- rast(tile_files[1])
   writeRaster(r, output_file, overwrite = TRUE,
               wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))
   return(rast(output_file))
 }
 vrt_file <- paste0(tools::file_path_sans_ext(output_file), ".vrt")
 vrt(tile_files, vrt_file, overwrite = TRUE)
 r <- rast(vrt_file)
 writeRaster(r, output_file, overwrite = TRUE,
             wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))
 return(rast(output_file))
}


# SESSION 1: CONTINUOUS MODELLING ----

# SECTION 1: Load data and covariates ----

cat("\n=== Loading covariates ===\n")
covs <- rast(PATH_COVARIATES)
ncovs <- names(covs)
cat("Loaded", nlyr(covs), "covariate layers\n")

# Load and apply mask to covariates ONCE (reduces per-tile work)
if (file.exists(PATH_MASK)) {
 mask_raster <- rast(PATH_MASK)
 covs <- mask(covs, mask_raster)
 cat("Mask applied to covariates:", PATH_MASK, "\n")
} else {
 mask_raster <- NULL
 cat("No mask file found, using full extent\n")
}

cat("\n=== Loading soil data ===\n")
dat <- read_csv(PATH_SOIL_DATA, show_col_types = FALSE)
cat("Loaded", nrow(dat), "observations\n")

# Convert to spatial and extract covariates
dat_vect <- vect(dat, geom = c("lon", "lat"), crs = "EPSG:4326")
dat_vect <- terra::project(dat_vect, covs)

cat("\n=== Extracting covariates at soil locations ===\n")
pv <- terra::extract(x = covs, y = dat_vect, xy = FALSE)
dat_full <- cbind(as.data.frame(dat_vect), pv)
dat_full$ID <- NULL
cat("Extraction complete:", nrow(dat_full), "rows,", ncol(dat_full), "columns\n")

# Create tile extents (not files) for prediction
cat("\n=== Creating prediction tile grid ===\n")
r_ext <- ext(covs)
tile_width <- (r_ext$xmax - r_ext$xmin) / TILE_COLS
tile_height <- (r_ext$ymax - r_ext$ymin) / TILE_ROWS

tile_extents <- list()
idx <- 1
for (row in 1:TILE_ROWS) {
 for (col in 1:TILE_COLS) {
   xmin <- r_ext$xmin + (col - 1) * tile_width
   xmax <- r_ext$xmin + col * tile_width
   ymax <- r_ext$ymax - (row - 1) * tile_height
   ymin <- r_ext$ymax - row * tile_height
   tile_extents[[idx]] <- ext(xmin, xmax, ymin, ymax)
   idx <- idx + 1
 }
}
cat("Created", length(tile_extents), "tile extents\n")


# SECTION 2: Feature selection (Boruta) ----

soilatt <- TARGET_CONTINUOUS[1]
cat("\n=== Feature Selection for:", soilatt, "===\n")

d <- dat_full %>%
 dplyr::select(all_of(soilatt), all_of(ncovs)) %>%
 na.omit()
cat("Working with", nrow(d), "complete observations\n")

set.seed(SEED)
fs_bor <- Boruta(y = d[[soilatt]], x = d[, -1], maxRuns = 100, doTrace = 1)

# Plot with horizontal labels
png(filename = paste0(DIR_VALIDATION, "boruta_", soilatt, ".png"),
   width = 15, height = 20, units = "cm", res = 150)
par(las = 1, mar = c(4, 10, 4, 2) + 0.1)
plot(fs_bor, horizontal = TRUE, ylab = "", xlab = "Importance", cex.axis = 0.6)
dev.off()

fs_vars <- getSelectedAttributes(fs_bor, withTentative = TRUE)
cat("Selected", length(fs_vars), "features\n")
saveRDS(fs_vars, file = paste0(DIR_MODELS, "selected_features_", soilatt, ".rds"))

# Pre-subset covariates for continuous model (do ONCE)
covs_cont <- covs[[fs_vars]]


# SECTION 3: Model calibration (Continuous) ----

cat("\n=== Model Calibration for:", soilatt, "(Continuous) ===\n")

fitControl <- trainControl(
 method = "repeatedcv",
 number = CV_FOLDS,
 repeats = CV_REPEATS,
 savePredictions = TRUE
)

mtry <- round(length(fs_vars) / 3)
tuneGrid <- expand.grid(
 mtry = abs(c(mtry - round(mtry/2), mtry, mtry + round(mtry/2))),
 min.node.size = 5,
 splitrule = c("variance", "extratrees")
)

# Use ranger threads during training (not parallelizing tiles yet)
cat("Training Quantile Regression Forest...\n")
set.seed(SEED)
model_continuous <- caret::train(
 y = d[[soilatt]],
 x = d[, fs_vars],
 method = "ranger",
 quantreg = TRUE,
 importance = "permutation",
 trControl = fitControl,
 tuneGrid = tuneGrid,
 num.threads = N_WORKERS
)

print(model_continuous)
cat("\nBest hyperparameters:\n")
print(model_continuous$bestTune)

png(filename = paste0(DIR_VALIDATION, "varimp_", soilatt, ".png"),
   width = 15, height = 15, units = "cm", res = 150)
plot(varImp(model_continuous), main = paste("Variable Importance -", soilatt))
dev.off()

saveRDS(model_continuous, file = paste0(DIR_MODELS, "ranger_model_", soilatt, ".rds"))


# SECTION 4: Validation (Continuous) ----

cat("\n=== Validation for:", soilatt, "===\n")

cv_results <- model_continuous$pred %>%
 filter(mtry == model_continuous$bestTune$mtry,
        splitrule == model_continuous$bestTune$splitrule,
        min.node.size == model_continuous$bestTune$min.node.size)

obs <- cv_results$obs
pred <- cv_results$pred
metrics <- calc_validation_metrics(obs, pred)

cat("Validation metrics:\n")
cat("  ME (Bias):", round(metrics$ME, 4), "\n")
cat("  RMSE:", round(metrics$RMSE, 4), "\n")
cat("  R2:", round(metrics$R2, 4), "\n")
cat("  NSE:", round(metrics$NSE, 4), "\n")

df_val <- data.frame(Observed = obs, Predicted = pred)
g_scatter <- ggplot(df_val, aes(x = Observed, y = Predicted)) +
 geom_point(alpha = 0.3) +
 geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
 ylim(c(min(df_val$Observed), max(df_val$Observed))) +
 theme(aspect.ratio = 1) +
 labs(title = paste("Observed vs Predicted -", soilatt),
      subtitle = paste0("R2 = ", round(metrics$R2, 3),
                        ", RMSE = ", round(metrics$RMSE, 3))) +
 theme_minimal()

ggsave(g_scatter, filename = paste0(DIR_VALIDATION, "scatterplot_", soilatt, ".png"),
      width = 12, height = 12, units = "cm")
print(g_scatter)

write_csv(data.frame(Variable = soilatt, Type = "Continuous", metrics),
         paste0(DIR_VALIDATION, "metrics_", soilatt, ".csv"))


# SECTION 5: Spatial prediction (Continuous) ----

cat("\n=== Spatial Prediction for:", soilatt, "===\n")

# Retrain final model with num.threads=1 for parallel tile prediction
model_for_pred <- model_continuous$finalModel
model_for_pred$num.threads <- 1

if (USE_PARALLEL_TILES && N_WORKERS > 1) {
 cat("Using parallel prediction with", N_WORKERS, "workers...\n")
 plan(multisession, workers = N_WORKERS)

 tile_results <- future_lapply(seq_along(tile_extents), function(j) {
   predict_tile_continuous(tile_extents[[j]], covs_cont, model_for_pred,
                           soilatt, j, DIR_TILES)
 }, future.seed = TRUE)

 plan(sequential)
} else {
 cat("Using sequential prediction...\n")
 tile_results <- lapply(seq_along(tile_extents), function(j) {
   cat("  Tile", j, "of", length(tile_extents), "\n")
   predict_tile_continuous(tile_extents[[j]], covs_cont, model_for_pred,
                           soilatt, j, DIR_TILES)
 })
}

# Mosaic tiles
cat("Merging tiles...\n")
f_mean <- list.files(DIR_TILES, pattern = paste0("^", soilatt, "_mean_tile_.*\\.tif$"),
                    full.names = TRUE)
f_sd <- list.files(DIR_TILES, pattern = paste0("^", soilatt, "_sd_tile_.*\\.tif$"),
                  full.names = TRUE)

pred_mean <- mosaic_tiles_vrt(f_mean, paste0(DIR_MAPS_CONT, "mean_", soilatt, ".tif"))
pred_sd <- mosaic_tiles_vrt(f_sd, paste0(DIR_MAPS_CONT, "sd_", soilatt, ".tif"))

# Calculate and save CV
pred_cv <- (pred_sd / pred_mean) * 100
writeRaster(pred_cv, paste0(DIR_MAPS_CONT, "cv_", soilatt, ".tif"),
           overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))

cat("Maps saved to:", DIR_MAPS_CONT, "\n")

par(mfrow = c(1, 3))
plot(pred_mean, main = paste("Mean", soilatt), col = hcl.colors(100, "Viridis"))
plot(pred_sd, main = paste("SD", soilatt), col = hcl.colors(100, "Reds"))
plot(pred_cv, main = paste("CV%", soilatt), col = hcl.colors(100, "YlOrRd"))
par(mfrow = c(1, 1))

cat("\n=== SESSION 1 COMPLETE ===\n")


# SESSION 2: NOMINAL CLASSIFICATION ----

# SECTION 6: Model calibration (Nominal) ----

cat("\n\n=== SESSION 2: NOMINAL CLASSIFICATION ===\n")
cat("Target variable:", TARGET_NOMINAL, "\n")

d_nom <- dat_full %>%
 dplyr::select(all_of(TARGET_NOMINAL), all_of(ncovs)) %>%
 na.omit() %>%
 mutate(!!TARGET_NOMINAL := as.factor(.data[[TARGET_NOMINAL]]))

cat("Working with", nrow(d_nom), "complete observations\n")
cat("Classes:\n")
print(table(d_nom[[TARGET_NOMINAL]]))

cat("\nRunning Boruta for nominal target...\n")
set.seed(SEED)
fs_bor_nom <- Boruta(y = d_nom[[TARGET_NOMINAL]], x = d_nom[, ncovs],
                    maxRuns = 100, doTrace = 1)

fs_vars_nom <- getSelectedAttributes(fs_bor_nom, withTentative = TRUE)
cat("Selected", length(fs_vars_nom), "features\n")
saveRDS(fs_vars_nom, file = paste0(DIR_MODELS, "selected_features_", TARGET_NOMINAL, ".rds"))

# Pre-subset covariates for nominal model
covs_nom <- covs[[fs_vars_nom]]

fitControl_class <- trainControl(
 method = "repeatedcv",
 number = CV_FOLDS,
 repeats = CV_REPEATS,
 savePredictions = TRUE,
 classProbs = TRUE
)

mtry_nom <- round(length(fs_vars_nom) / 3)
tuneGrid_nom <- expand.grid(
 mtry = abs(c(mtry_nom - round(mtry_nom/2), mtry_nom, mtry_nom + round(mtry_nom/2))),
 min.node.size = 5,
 splitrule = c("gini", "extratrees")
)

cat("Training Classification Random Forest...\n")
set.seed(SEED)
model_nominal <- caret::train(
 y = d_nom[[TARGET_NOMINAL]],
 x = d_nom[, fs_vars_nom],
 method = "ranger",
 importance = "permutation",
 trControl = fitControl_class,
 tuneGrid = tuneGrid_nom,
 num.threads = N_WORKERS
)

print(model_nominal)
saveRDS(model_nominal, file = paste0(DIR_MODELS, "ranger_model_", TARGET_NOMINAL, ".rds"))


# SECTION 7: Validation (Nominal) ----

cat("\n=== Validation for:", TARGET_NOMINAL, "(Nominal) ===\n")

cv_results_nom <- model_nominal$pred %>%
 filter(mtry == model_nominal$bestTune$mtry,
        splitrule == model_nominal$bestTune$splitrule,
        min.node.size == model_nominal$bestTune$min.node.size)

obs_nom <- cv_results_nom$obs
pred_nom <- cv_results_nom$pred

conf_mat <- confusionMatrix(pred_nom, obs_nom)
print(conf_mat)

accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]

cat("\nClassification metrics:\n")
cat("  Accuracy:", round(accuracy, 4), "\n")
cat("  Kappa:", round(kappa, 4), "\n")

conf_table <- as.data.frame(conf_mat$table)
g_conf <- ggplot(conf_table, aes(x = Reference, y = Prediction, fill = Freq)) +
 geom_tile() +
 geom_text(aes(label = Freq), color = "white", size = 3) +
 scale_fill_gradient(low = "lightblue", high = "darkblue") +
 labs(title = paste("Confusion Matrix -", TARGET_NOMINAL),
      subtitle = paste0("Accuracy = ", round(accuracy, 3), ", Kappa = ", round(kappa, 3))) +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(g_conf, filename = paste0(DIR_VALIDATION, "confusion_matrix_", TARGET_NOMINAL, ".png"),
      width = 18, height = 15, units = "cm")
print(g_conf)

write_csv(data.frame(Variable = TARGET_NOMINAL, Type = "Nominal",
                    Accuracy = accuracy, Kappa = kappa),
         paste0(DIR_VALIDATION, "metrics_", TARGET_NOMINAL, ".csv"))


# SECTION 8: Spatial prediction (Nominal) ----

cat("\n=== Spatial Prediction for:", TARGET_NOMINAL, "===\n")

class_names <- levels(d_nom[[TARGET_NOMINAL]])
cat("Predicting probabilities for", length(class_names), "classes\n")

model_nom_pred <- model_nominal$finalModel
model_nom_pred$num.threads <- 1

if (USE_PARALLEL_TILES && N_WORKERS > 1) {
 cat("Using parallel prediction...\n")
 plan(multisession, workers = N_WORKERS)

 tile_results_nom <- future_lapply(seq_along(tile_extents), function(j) {
   predict_tile_nominal(tile_extents[[j]], covs_nom, model_nom_pred,
                        class_names, TARGET_NOMINAL, j, DIR_TILES)
 }, future.seed = TRUE)

 plan(sequential)
} else {
 cat("Using sequential prediction...\n")
 tile_results_nom <- lapply(seq_along(tile_extents), function(j) {
   cat("  Tile", j, "of", length(tile_extents), "\n")
   predict_tile_nominal(tile_extents[[j]], covs_nom, model_nom_pred,
                        class_names, TARGET_NOMINAL, j, DIR_TILES)
 })
}

cat("Merging probability tiles...\n")
f_probs <- list.files(DIR_TILES, pattern = paste0("^", TARGET_NOMINAL, "_probs_tile_.*\\.tif$"),
                     full.names = TRUE)

# Mosaic multi-band probability rasters
r_probs_list <- lapply(f_probs, rast)
pred_probs_full <- mosaic(sprc(r_probs_list))
names(pred_probs_full) <- class_names

# Derive max probability class
class_max <- which.max(pred_probs_full)
prob_max <- max(pred_probs_full)

levels(class_max) <- data.frame(id = 1:length(class_names), class = class_names)

# Save maps
for (i in seq_along(class_names)) {
 writeRaster(pred_probs_full[[i]], paste0(DIR_MAPS_NOM, "prob_", class_names[i], ".tif"),
             overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))
}

writeRaster(class_max, paste0(DIR_MAPS_NOM, "class_max_prob.tif"),
           overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = GDAL_OPTIONS))
writeRaster(prob_max, paste0(DIR_MAPS_NOM, "max_prob_value.tif"),
           overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))

cat("Maps saved to:", DIR_MAPS_NOM, "\n")

par(mfrow = c(1, 2))
plot(class_max, main = "Predicted Class (Max Probability)",
    col = hcl.colors(length(class_names), "Set2"))
plot(prob_max, main = "Maximum Probability Value", col = hcl.colors(100, "Viridis"))
par(mfrow = c(1, 1))

cat("\n=== SESSION 2 COMPLETE ===\n")


# SESSION 3: ORDINAL MODELLING ----

# SECTION 9: Ordinal encoding ----

cat("\n\n=== SESSION 3: ORDINAL MODELLING ===\n")
cat("Target variable:", TARGET_ORDINAL, "\n")

# IMPORTANT: This is a compromise approach. We treat ordinal as continuous
# by encoding ordered classes as integers. This assumes equal spacing between
# classes, which may not reflect true soil differences.

ordinal_lookup <- data.frame(
 level = c("very_acid", "acid", "neutral", "alkaline", "very_alkaline"),
 integer = 1:5,
 stringsAsFactors = FALSE
)

cat("\nOrdinal encoding lookup table:\n")
print(ordinal_lookup)

ordinal_thresholds <- c(-Inf, 1.5, 2.5, 3.5, 4.5, Inf)

reclassify_matrix <- data.frame(
 from = ordinal_thresholds[-length(ordinal_thresholds)],
 to = ordinal_thresholds[-1],
 becomes = 1:5
)

cat("\nReclassification thresholds (midpoints):\n")
cat("  < 1.5 -> very_acid (1)\n")
cat("  1.5 - 2.5 -> acid (2)\n")
cat("  2.5 - 3.5 -> neutral (3)\n")
cat("  3.5 - 4.5 -> alkaline (4)\n")
cat("  > 4.5 -> very_alkaline (5)\n")

ordinal_encoding <- list(
 lookup = ordinal_lookup,
 thresholds = ordinal_thresholds,
 reclassify_matrix = as.matrix(reclassify_matrix),
 note = "Ordinal regression via integer encoding is a compromise. It assumes equal spacing between classes."
)

saveRDS(ordinal_encoding, file = paste0(DIR_MODELS, "ordinal_encoding.rds"))


# SECTION 10: Model calibration (Ordinal) ----

cat("\n=== Ordinal Model Calibration ===\n")

d_ord <- dat_full %>%
 dplyr::select(all_of(TARGET_ORDINAL), all_of(ncovs)) %>%
 na.omit()

d_ord$target_integer <- as.integer(factor(d_ord[[TARGET_ORDINAL]],
                                          levels = ordinal_lookup$level))

cat("Integer encoding applied:\n")
print(table(Original = d_ord[[TARGET_ORDINAL]], Integer = d_ord$target_integer))

cat("\nRunning Boruta for ordinal target...\n")
set.seed(SEED)
fs_bor_ord <- Boruta(y = d_ord$target_integer, x = d_ord[, ncovs],
                    maxRuns = 100, doTrace = 1)

fs_vars_ord <- getSelectedAttributes(fs_bor_ord, withTentative = TRUE)
cat("Selected", length(fs_vars_ord), "features\n")
saveRDS(fs_vars_ord, file = paste0(DIR_MODELS, "selected_features_", TARGET_ORDINAL, ".rds"))

# Pre-subset covariates for ordinal model
covs_ord <- covs[[fs_vars_ord]]

mtry_ord <- round(length(fs_vars_ord) / 3)
tuneGrid_ord <- expand.grid(
 mtry = abs(c(mtry_ord - round(mtry_ord/2), mtry_ord, mtry_ord + round(mtry_ord/2))),
 min.node.size = 5,
 splitrule = c("variance", "extratrees")
)

cat("Training Regression RF on integer-encoded ordinal...\n")
set.seed(SEED)
model_ordinal <- caret::train(
 y = d_ord$target_integer,
 x = d_ord[, fs_vars_ord],
 method = "ranger",
 quantreg = TRUE,
 importance = "permutation",
 trControl = fitControl,
 tuneGrid = tuneGrid_ord,
 num.threads = N_WORKERS
)

print(model_ordinal)
saveRDS(model_ordinal, file = paste0(DIR_MODELS, "ranger_model_", TARGET_ORDINAL, "_ordinal.rds"))


# SECTION 11: Validation (Ordinal) ----

cat("\n=== Validation for:", TARGET_ORDINAL, "(Ordinal) ===\n")

cv_results_ord <- model_ordinal$pred %>%
 filter(mtry == model_ordinal$bestTune$mtry,
        splitrule == model_ordinal$bestTune$splitrule,
        min.node.size == model_ordinal$bestTune$min.node.size)

obs_ord <- cv_results_ord$obs
pred_ord <- cv_results_ord$pred

pred_class <- cut(pred_ord, breaks = ordinal_thresholds, labels = 1:5)
pred_class <- as.integer(as.character(pred_class))

MAE_rank <- mean(abs(obs_ord - pred_class), na.rm = TRUE)

cat("Ordinal validation metrics:\n")
cat("  Mean Absolute Rank Error:", round(MAE_rank, 4), "\n")

conf_ord <- table(Observed = obs_ord, Predicted = pred_class)
cat("\nOrdinal Confusion Matrix:\n")
print(conf_ord)

distances <- abs(outer(1:5, 1:5, "-"))
weighted_errors <- sum(conf_ord * distances) / sum(conf_ord)
cat("\nMean classification distance:", round(weighted_errors, 4), "\n")

conf_ord_df <- as.data.frame(conf_ord)
g_conf_ord <- ggplot(conf_ord_df, aes(x = Observed, y = Predicted, fill = Freq)) +
 geom_tile() +
 geom_text(aes(label = Freq), color = "white", size = 4) +
 scale_fill_gradient(low = "lightblue", high = "darkblue") +
 labs(title = paste("Ordinal Confusion Matrix -", TARGET_ORDINAL),
      subtitle = paste0("MAE Rank = ", round(MAE_rank, 3),
                        ", Mean Distance = ", round(weighted_errors, 3)),
      x = "Observed (Integer)", y = "Predicted (Integer)") +
 theme_minimal()

ggsave(g_conf_ord, filename = paste0(DIR_VALIDATION, "ordinal_confusion_", TARGET_ORDINAL, ".png"),
      width = 15, height = 12, units = "cm")
print(g_conf_ord)

write_csv(data.frame(Variable = TARGET_ORDINAL, Type = "Ordinal",
                    MAE_rank = MAE_rank, Mean_distance = weighted_errors),
         paste0(DIR_VALIDATION, "metrics_", TARGET_ORDINAL, "_ordinal.csv"))


# SECTION 12: Spatial prediction (Ordinal) ----

cat("\n=== Spatial Prediction for:", TARGET_ORDINAL, "(Ordinal) ===\n")

model_ord_pred <- model_ordinal$finalModel
model_ord_pred$num.threads <- 1

if (USE_PARALLEL_TILES && N_WORKERS > 1) {
 cat("Using parallel prediction...\n")
 plan(multisession, workers = N_WORKERS)

 tile_results_ord <- future_lapply(seq_along(tile_extents), function(j) {
   predict_tile_continuous(tile_extents[[j]], covs_ord, model_ord_pred,
                           TARGET_ORDINAL, j, DIR_TILES)
 }, future.seed = TRUE)

 plan(sequential)
} else {
 cat("Using sequential prediction...\n")
 tile_results_ord <- lapply(seq_along(tile_extents), function(j) {
   cat("  Tile", j, "of", length(tile_extents), "\n")
   predict_tile_continuous(tile_extents[[j]], covs_ord, model_ord_pred,
                           TARGET_ORDINAL, j, DIR_TILES)
 })
}

cat("Merging tiles...\n")
f_latent <- list.files(DIR_TILES, pattern = paste0("^", TARGET_ORDINAL, "_mean_tile_.*\\.tif$"),
                      full.names = TRUE)
f_sd_ord <- list.files(DIR_TILES, pattern = paste0("^", TARGET_ORDINAL, "_sd_tile_.*\\.tif$"),
                      full.names = TRUE)

pred_latent <- mosaic_tiles_vrt(f_latent, paste0(DIR_MAPS_ORD, "latent_", TARGET_ORDINAL, ".tif"))
pred_sd_ord <- mosaic_tiles_vrt(f_sd_ord, paste0(DIR_MAPS_ORD, "sd_", TARGET_ORDINAL, ".tif"))

# Reclassify latent values to ordinal classes
reclass_mat <- as.matrix(reclassify_matrix)
pred_class_rast <- classify(pred_latent, reclass_mat)
levels(pred_class_rast) <- data.frame(id = 1:5, class = ordinal_lookup$level)

# Class instability: SD relative to distance from nearest threshold
pred_instability <- pred_sd_ord / (abs(pred_latent - round(pred_latent)) + 0.1)

writeRaster(pred_class_rast, paste0(DIR_MAPS_ORD, "class_", TARGET_ORDINAL, ".tif"),
           overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = GDAL_OPTIONS))
writeRaster(pred_instability, paste0(DIR_MAPS_ORD, "instability_", TARGET_ORDINAL, ".tif"),
           overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = GDAL_OPTIONS))

cat("Maps saved to:", DIR_MAPS_ORD, "\n")

par(mfrow = c(2, 2))
plot(pred_latent, main = "Latent Values (continuous)", col = hcl.colors(100, "Viridis"))
plot(pred_class_rast, main = "Ordinal Class (reclassified)", col = hcl.colors(5, "Spectral"))
plot(pred_sd_ord, main = "Standard Deviation", col = hcl.colors(100, "Reds"))
plot(pred_instability, main = "Class Instability", col = hcl.colors(100, "YlOrRd"))
par(mfrow = c(1, 1))


# SECTION 13: Final summary ----

cat("\n")
cat("================================================================\n")
cat("           DIGITAL SOIL MAPPING WORKFLOW COMPLETE\n")
cat("================================================================\n")
cat("\nOutputs generated:\n")
cat("\n1. MODELS (", DIR_MODELS, ")\n")
print(list.files(DIR_MODELS, pattern = "\\.rds$"))

cat("\n2. VALIDATION (", DIR_VALIDATION, ")\n")
print(list.files(DIR_VALIDATION))

cat("\n3. MAPS\n")
cat("   Continuous (", DIR_MAPS_CONT, "):", length(list.files(DIR_MAPS_CONT)), "files\n")
cat("   Nominal (", DIR_MAPS_NOM, "):", length(list.files(DIR_MAPS_NOM)), "files\n")
cat("   Ordinal (", DIR_MAPS_ORD, "):", length(list.files(DIR_MAPS_ORD)), "files\n")

cat("\n================================================================\n")
cat("Key learning points:\n")
cat("  - Continuous: Regression RF -> mean, SD, CV maps\n")
cat("  - Nominal: Classification RF -> class probabilities\n")
cat("  - Ordinal: Regression on integers (compromise) -> latent + reclassify\n")
cat("================================================================\n")
