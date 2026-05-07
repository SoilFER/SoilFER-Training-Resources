
# Create tile grid for memory-efficient prediction
# EDIT THIS: increase TILE_ROWS if you run out of memory; decrease for faster prediction
TILE_ROWS <- 5
TILE_COLS <- 5

r_ext <- ext(covs)
tile_width <- (r_ext$xmax - r_ext$xmin) / TILE_COLS
tile_height <- (r_ext$ymax - r_ext$ymin) / TILE_ROWS

# Build list of tile extents
tile_extents <- list()
tile_idx <- 1
for (row in 1:TILE_ROWS) {
  for (col in 1:TILE_COLS) {
    xmin <- r_ext$xmin + (col - 1) * tile_width
    xmax <- r_ext$xmin + col * tile_width
    ymax <- r_ext$ymax - (row - 1) * tile_height
    ymin <- r_ext$ymax - row * tile_height
    tile_extents[[tile_idx]] <- ext(xmin, xmax, ymin, ymax)
    tile_idx <- tile_idx + 1
  }
}
n_tiles <- length(tile_extents)


# SECTION 10: Prepare nominal data ----

target_nom <- "Clay_pH_Class"  # EDIT THIS: your nominal target variable

train_df_nom <- soil_cov_df %>%
  dplyr::select(all_of(target_nom), all_of(cov_names)) %>%
  na.omit() %>%
  mutate(!!target_nom := as.factor(.data[[target_nom]]))

print(nrow(train_df_nom))
print(table(train_df_nom[[target_nom]]))


# SECTION 11: Feature selection for nominal ----

set.seed(2025)
boruta_nom <- Boruta(
  y = train_df_nom[[target_nom]],
  x = train_df_nom[, cov_names],
  maxRuns = 100,
  doTrace = 1
)

selected_features_nom <- getSelectedAttributes(boruta_nom, withTentative = TRUE)

print(length(selected_features_nom))
print(selected_features_nom)

covs_nom <- covs[[selected_features_nom]]

saveRDS(selected_features_nom, paste0("outputs/models/selected_features_", target_nom, ".rds"))

par(las = 1, mar = c(4, 10, 4, 2) + 0.1)
plot(boruta_nom, horizontal = TRUE,las=1, 
     ylab = "", xlab = "Importance", cex.axis = 0.6)

# SECTION 12: Train Classification RF ----

cv_control_class <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  savePredictions = TRUE,
  classProbs = TRUE#,  # Enable class probability predictions
  # sampling="up" # Rebalance the classes may improve class separability.
)

mtry_nom <- round(length(selected_features_nom) / 3)
tune_grid_nom <- expand.grid(
  mtry = c(mtry_nom - round(mtry_nom/2), mtry_nom, mtry_nom + round(mtry_nom/2)),
  min.node.size = 5,
  splitrule = c("gini", "extratrees")
)

set.seed(2025)
model_nom <- caret::train(
  y = train_df_nom[[target_nom]],
  x = train_df_nom[, selected_features_nom],
  method = "ranger",
  importance = "permutation",
  trControl = cv_control_class,
  tuneGrid = tune_grid_nom,
  num.threads = max(1, parallel::detectCores() - 1),
  metric = "Kappa",      # Optimize for Kappa instead of Accuracy
  maximize = TRUE
)

print(model_nom)
print(model_nom$bestTune)

saveRDS(model_nom, paste0("outputs/models/ranger_model_", target_nom, ".rds"))


# SECTION 13: Validation for nominal ----

cv_preds_nom <- model_nom$pred %>%
  filter(mtry == model_nom$bestTune$mtry,
         splitrule == model_nom$bestTune$splitrule,
         min.node.size == model_nom$bestTune$min.node.size)

conf_mat <- confusionMatrix(cv_preds_nom$pred, cv_preds_nom$obs)
print(conf_mat)

accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]

print(data.frame(Accuracy = accuracy, Kappa = kappa))

# Confusion matrix heatmap
conf_table <- as.data.frame(conf_mat$table)
g_conf <- ggplot(conf_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = paste("Confusion Matrix -", target_nom),
       subtitle = paste0("Accuracy = ", round(accuracy, 3), ", Kappa = ", round(kappa, 3))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_conf)
ggsave(g_conf, filename = paste0("outputs/validation/confusion_matrix_", target_nom, ".png"),
       width = 18, height = 15, units = "cm")

write_csv(data.frame(Variable = target_nom, Type = "Nominal",
                     Accuracy = accuracy, Kappa = kappa),
          paste0("outputs/validation/metrics_", target_nom, ".csv"))


# SECTION 14: Spatial prediction (nominal) ----

class_names <- levels(train_df_nom[[target_nom]])

print(class_names)

model_nom_pred <- model_nom$finalModel
model_nom_pred$num.threads <- 1

# Loop through tiles and predict class probabilities
for (j in 1:n_tiles) {
  print(paste("Processing tile", j, "of", n_tiles))

  covs_tile <- crop(covs_nom, tile_extents[[j]])

  # Predict class probabilities
  pred_probs <- terra::predict(
    covs_tile,
    model = model_nom_pred,
    na.rm = TRUE,
    type = "response",
    fun = function(m, ...) predict(m, ...)$predictions
  )
  names(pred_probs) <- class_names

  out_file <- paste0("outputs/tiles/", target_nom, "_probs_tile_", j, ".tif")
  writeRaster(pred_probs, out_file, overwrite = TRUE,
              wopt = list(datatype = "FLT4S", gdal = gdal_opts))
}

print("Tile prediction complete")


# SECTION 15: Mosaic nominal probability maps ----

prob_tiles <- list.files("outputs/tiles/",
                         pattern = paste0("^", target_nom, "_probs_tile_.*\\.tif$"),
                         full.names = TRUE)

prob_rasters <- list()
for (i in 1:length(prob_tiles)) {
  prob_rasters[[i]] <- rast(prob_tiles[i])
}
pred_probs_full <- mosaic(sprc(prob_rasters))
names(pred_probs_full) <- class_names

# Derive class with maximum probability
class_max <- which.max(pred_probs_full)
prob_max <- max(pred_probs_full)

# Add class labels
levels(class_max) <- data.frame(id = 1:length(class_names), class = class_names)

# Save individual probability maps
for (i in 1:length(class_names)) {
  writeRaster(pred_probs_full[[i]],
              paste0("outputs/maps/nominal/prob_", class_names[i], ".tif"),
              overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))
}

# Save maximum probability class map
writeRaster(class_max, "outputs/maps/nominal/class_max_prob.tif",
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

writeRaster(prob_max, "outputs/maps/nominal/max_prob_value.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

# --- Uncertainty mapping using probabilities ---
# The max probability tells us how confident the model is.
# Low max probability = model is uncertain between classes.

# Calculate uncertainty as 1 - max_probability
uncertainty <- 1 - prob_max

# Apply "reject option": if max probability < threshold, mark as uncertain
# EDIT THIS: adjust threshold based on your needs (0.5 = random for binary, higher for multi-class)
prob_threshold <- 0.4

# Create masked class map: set uncertain pixels to NA
class_confident <- class_max
class_confident[prob_max < prob_threshold] <- NA

# Save uncertainty maps
writeRaster(uncertainty, "outputs/maps/nominal/uncertainty.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

writeRaster(class_confident, "outputs/maps/nominal/class_confident.tif",
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

# Display maps
plot(class_max, main = "Predicted Class (Max Probability)",
     col = hcl.colors(length(class_names), "Set2"))
plot(prob_max, main = "Maximum Probability Value", col = hcl.colors(100, "Viridis"))
plot(uncertainty, main = "Uncertainty (1 - max prob)", col = hcl.colors(100, "YlOrRd"))
plot(class_confident, main = paste0("Confident Predictions (prob >= ", prob_threshold, ")"),
     col = hcl.colors(length(class_names), "Set2"))

print("SESSION 2 COMPLETE: Nominal classification finished")


# SESSION 3: ORDINAL MODELLING ----

# Create tile grid for memory-efficient prediction
# EDIT THIS: increase TILE_ROWS if you run out of memory; decrease for faster prediction
TILE_ROWS <- 5
TILE_COLS <- 5

r_ext <- ext(covs)
tile_width <- (r_ext$xmax - r_ext$xmin) / TILE_COLS
tile_height <- (r_ext$ymax - r_ext$ymin) / TILE_ROWS

# Build list of tile extents
tile_extents <- list()
tile_idx <- 1
for (row in 1:TILE_ROWS) {
  for (col in 1:TILE_COLS) {
    xmin <- r_ext$xmin + (col - 1) * tile_width
    xmax <- r_ext$xmin + col * tile_width
    ymax <- r_ext$ymax - (row - 1) * tile_height
    ymin <- r_ext$ymax - row * tile_height
    tile_extents[[tile_idx]] <- ext(xmin, xmax, ymin, ymax)
    tile_idx <- tile_idx + 1
  }
}
n_tiles <- length(tile_extents)


# SECTION 16: Ordinal encoding ----

target_ord <- "pH_Class"  # EDIT THIS: your ordinal target variable

# Define the ordinal levels in order (low to high)
# EDIT THIS: change levels to match your ordinal variable
ordinal_levels <- c("very_acid", "acid", "neutral", "alkaline", "very_alkaline")

# Create lookup table: ordinal levels -> integers
ordinal_lookup <- data.frame(
  level = ordinal_levels,
  integer = 1:length(ordinal_levels),
  stringsAsFactors = FALSE
)

print(ordinal_lookup)

# Define thresholds for reclassifying continuous predictions back to classes
# Midpoints between integers: 1.5, 2.5, 3.5, 4.5
ordinal_thresholds <- c(-Inf, 1.5, 2.5, 3.5, 4.5, Inf)

# Reclassification matrix for terra::classify()
reclass_matrix <- matrix(
  c(-Inf, 1.5, 1,
    1.5, 2.5, 2,
    2.5, 3.5, 3,
    3.5, 4.5, 4,
    4.5, Inf, 5),
  ncol = 3, byrow = TRUE
)

print(reclass_matrix)

saveRDS(list(lookup = ordinal_lookup, thresholds = ordinal_thresholds,
             reclass_matrix = reclass_matrix),
        "outputs/models/ordinal_encoding.rds")


# SECTION 17: Prepare ordinal data ----

train_df_ord <- soil_cov_df %>%
  dplyr::select(all_of(target_ord), all_of(cov_names)) %>%
  na.omit()

# Convert ordinal classes to integers
train_df_ord$target_integer <- as.integer(factor(train_df_ord[[target_ord]],
                                                  levels = ordinal_levels))

print(table(Original = train_df_ord[[target_ord]], Integer = train_df_ord$target_integer))


# SECTION 18: Feature selection for ordinal ----

set.seed(2025)
boruta_ord <- Boruta(
  y = train_df_ord$target_integer,
  x = train_df_ord[, cov_names],
  maxRuns = 100,
  doTrace = 1
)

selected_features_ord <- getSelectedAttributes(boruta_ord, withTentative = TRUE)

print(length(selected_features_ord))
print(selected_features_ord)

covs_ord <- covs[[selected_features_ord]]

par(las = 1, mar = c(4, 10, 4, 2) + 0.1)
plot(boruta_ord, horizontal = TRUE,las=1, 
     ylab = "", xlab = "Importance", cex.axis = 0.6)

saveRDS(selected_features_ord, paste0("outputs/models/selected_features_", target_ord, ".rds"))


# SECTION 19: Train Regression RF on integer-encoded ordinal ----

# Note: This is a compromise approach. We treat ordinal as continuous
# by encoding ordered classes as integers. This assumes equal spacing
# between classes, which may not reflect true soil differences.

# Cross-validation setup: 10-fold CV repeated 5 times
cv_control <- trainControl(
  method = "repeatedcv",
  number = 10,   # EDIT THIS: number of folds
  repeats = 5,   # EDIT THIS: number of repeats
  savePredictions = TRUE
)

mtry_ord <- round(length(selected_features_ord) / 3)
tune_grid_ord <- expand.grid(
  mtry = c(mtry_ord - round(mtry_ord/2), mtry_ord, mtry_ord + round(mtry_ord/2)),
  min.node.size = 5,
  splitrule = c("variance", "extratrees")
)

set.seed(2025)
model_ord <- caret::train(
  y = train_df_ord$target_integer,
  x = train_df_ord[, selected_features_ord],
  method = "ranger",
  quantreg = TRUE,
  importance = "permutation",
  trControl = cv_control,  # Reuse from continuous section
  tuneGrid = tune_grid_ord,
  num.threads = max(1, parallel::detectCores() - 1)
)

print(model_ord)
print(model_ord$bestTune)

saveRDS(model_ord, paste0("outputs/models/ranger_model_", target_ord, "_ordinal.rds"))


# SECTION 20: Validation for ordinal ----

cv_preds_ord <- model_ord$pred %>%
  filter(mtry == model_ord$bestTune$mtry,
         splitrule == model_ord$bestTune$splitrule,
         min.node.size == model_ord$bestTune$min.node.size)

obs_ord <- cv_preds_ord$obs
pred_ord <- cv_preds_ord$pred

# Reclassify continuous predictions to integer classes
pred_class_ord <- cut(pred_ord, breaks = ordinal_thresholds, labels = 1:5)
pred_class_ord <- as.integer(as.character(pred_class_ord))

# Mean Absolute Rank Error
MAE_rank <- mean(abs(obs_ord - pred_class_ord), na.rm = TRUE)

# Confusion matrix - convert to factors with all levels to ensure 5x5 matrix
# This is needed because some classes may never be predicted
obs_factor <- factor(obs_ord, levels = 1:5)
pred_factor <- factor(pred_class_ord, levels = 1:5)
conf_ord <- table(Observed = obs_factor, Predicted = pred_factor)
print(conf_ord)

# Mean classification distance (penalizes far-off predictions more)
distances <- abs(outer(1:5, 1:5, "-"))
weighted_errors <- sum(conf_ord * distances) / sum(conf_ord)

print(data.frame(MAE_rank = MAE_rank, Mean_distance = weighted_errors))

# Ordinal confusion matrix heatmap
conf_ord_df <- as.data.frame(conf_ord)
g_conf_ord <- ggplot(conf_ord_df, aes(x = Observed, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = paste("Ordinal Confusion Matrix -", target_ord),
       subtitle = paste0("MAE Rank = ", round(MAE_rank, 3),
                         ", Mean Distance = ", round(weighted_errors, 3)),
       x = "Observed (Integer)", y = "Predicted (Integer)") +
  theme_minimal()

print(g_conf_ord)
ggsave(g_conf_ord, filename = paste0("outputs/validation/ordinal_confusion_", target_ord, ".png"),
       width = 15, height = 12, units = "cm")

write_csv(data.frame(Variable = target_ord, Type = "Ordinal",
                     MAE_rank = MAE_rank, Mean_distance = weighted_errors),
          paste0("outputs/validation/metrics_", target_ord, "_ordinal.csv"))


# SECTION 21: Spatial prediction (ordinal) ----

model_ord_pred <- model_ord$finalModel
model_ord_pred$num.threads <- 1

# Loop through tiles and predict latent (continuous) values + SD
for (j in 1:n_tiles) {
  print(paste("Processing tile", j, "of", n_tiles))

  covs_tile <- crop(covs_ord, tile_extents[[j]])

  # Predict latent mean
  mean_file <- paste0("outputs/tiles/", target_ord, "_mean_tile_", j, ".tif")
  terra::interpolate(
    covs_tile,
    model = model_ord_pred,
    fun = pfun_qrf,
    na.rm = TRUE,
    type = "quantiles",
    what = mean,
    filename = mean_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = gdal_opts)
  )

  # Predict SD
  sd_file <- paste0("outputs/tiles/", target_ord, "_sd_tile_", j, ".tif")
  terra::interpolate(
    covs_tile,
    model = model_ord_pred,
    fun = pfun_qrf,
    na.rm = TRUE,
    type = "quantiles",
    what = sd,
    filename = sd_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = gdal_opts)
  )
}

print("Tile prediction complete")


# SECTION 22: Mosaic ordinal maps and reclassify ----

# Mosaic latent (continuous) predictions
latent_tiles <- list.files("outputs/tiles/",
                           pattern = paste0("^", target_ord, "_mean_tile_.*\\.tif$"),
                           full.names = TRUE)

latent_rasters <- list()
for (i in 1:length(latent_tiles)) {
  latent_rasters[[i]] <- rast(latent_tiles[i])
}
pred_latent <- mosaic(sprc(latent_rasters))

writeRaster(pred_latent, paste0("outputs/maps/ordinal/latent_", target_ord, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

# Mosaic SD predictions
sd_tiles_ord <- list.files("outputs/tiles/",
                           pattern = paste0("^", target_ord, "_sd_tile_.*\\.tif$"),
                           full.names = TRUE)

sd_rasters_ord <- list()
for (i in 1:length(sd_tiles_ord)) {
  sd_rasters_ord[[i]] <- rast(sd_tiles_ord[i])
}
pred_sd_ord <- mosaic(sprc(sd_rasters_ord))

writeRaster(pred_sd_ord, paste0("outputs/maps/ordinal/sd_", target_ord, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

# Reclassify latent values to ordinal classes
pred_class_rast <- classify(pred_latent, reclass_matrix)
levels(pred_class_rast) <- data.frame(id = 1:5, class = ordinal_levels)

writeRaster(pred_class_rast, paste0("outputs/maps/ordinal/class_", target_ord, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

# Calculate class instability: high SD near class boundaries = uncertain classification
pred_instability <- pred_sd_ord / (abs(pred_latent - round(pred_latent)) + 0.1)

writeRaster(pred_instability, paste0("outputs/maps/ordinal/instability_", target_ord, ".tif"),
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

# Display maps
plot(pred_latent, main = "Latent Values (continuous)", col = hcl.colors(100, "Viridis"))
plot(pred_class_rast, main = "Ordinal Class (reclassified)", col = hcl.colors(5, "Spectral"))
plot(pred_sd_ord, main = "Standard Deviation", col = hcl.colors(100, "Reds"))
plot(pred_instability, main = "Class Instability", col = hcl.colors(100, "YlOrRd"))

print("SESSION 3 COMPLETE: Ordinal modelling finished")


# SESSION 4: AREA OF APPLICABILITY (AOA) ----

# The Area of Applicability (AOA) identifies where our model can be
# trusted. It measures how similar each prediction location is to the
# training data in covariate space. Locations too dissimilar from what
# the model has seen are flagged as "outside" the AOA.
#
# Reference: Meyer & Pebesma (2021), Methods in Ecology and Evolution
# Vignette: https://hannameyer.github.io/CAST/articles/cast02-AOA-tutorial.html

# SECTION 23: Compute AOA for continuous model ----

# We use the continuous pH model as the example. CAST::aoa() needs:
#   1) The predictor raster stack (same layers the model was trained on)
#   2) The trained caret model object (it extracts variable importance from it)

# aoa() computes three things:
#   - DI:  Dissimilarity Index (how far each pixel is from training data)
#   - LPD: Local Point Density (how many training points are nearby in covariate space)
#   - AOA: Binary mask (1 = inside applicability, 0 = outside)

print("Computing Area of Applicability for pH model...")
print("This may take a while for large rasters")

# EDIT THIS: set LPD = FALSE if you want faster computation (skips point density)
aoa_result <- aoa(covs_cont, model_cont, LPD = TRUE, verbose = TRUE)

# Inspect the result
print(aoa_result)
print(aoa_result$parameters)


# SECTION 24: Visualize AOA results ----

# The Dissimilarity Index (DI): low values = similar to training data
plot(aoa_result$DI, col = hcl.colors(100, "Viridis"),
     main = "Dissimilarity Index (DI) - pH model")

# The AOA binary mask: shows where predictions are trustworthy
plot(aoa_result$AOA, col = c("grey", "green"),
     main = "Area of Applicability (grey = outside)")

# Local Point Density: how many training points support each prediction
plot(aoa_result$LPD, col = hcl.colors(100, "Viridis"),
     main = "Local Point Density (LPD)")

# Overlay AOA on the prediction map to see which areas are reliable
plot(pred_mean, col = hcl.colors(100, "Viridis"), main = "pH prediction with AOA overlay")
plot(aoa_result$AOA, col = c("red", "transparent"), add = TRUE, legend = FALSE)

# The diagnostic plot shows the DI distribution of training data vs new data
plot(aoa_result)


# SECTION 25: Mask predictions to AOA ----

# Set predictions outside the AOA to NA (unreliable areas)
pred_mean_aoa <- mask(pred_mean, aoa_result$AOA, maskvalues = 0)
pred_sd_aoa <- mask(pred_sd, aoa_result$AOA, maskvalues = 0)

plot(pred_mean_aoa, col = hcl.colors(100, "Viridis"),
     main = "pH Mean (only within AOA)")
plot(pred_sd_aoa, col = hcl.colors(100, "Reds"),
     main = "pH SD (only within AOA)")


# SECTION 26: Error profiles ----

# Error profiles relate the DI to expected prediction error.
# This tells us: "at DI = X, the expected RMSE is Y"
error_profile <- errorProfiles(model_cont, aoa_result$parameters,
                               variable = "DI")

print(error_profile)
plot(error_profile)

# Create a spatially explicit expected error map
# Each pixel gets an estimated RMSE based on its DI value
expected_error <- terra::predict(aoa_result$DI, error_profile)

plot(expected_error, col = hcl.colors(100, "YlOrRd"),
     main = "Expected RMSE based on DI")


# SECTION 27: Save AOA outputs ----

writeRaster(aoa_result$DI, "outputs/maps/aoa/DI_pH.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

writeRaster(aoa_result$AOA, "outputs/maps/aoa/AOA_pH.tif",
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

writeRaster(aoa_result$LPD, "outputs/maps/aoa/LPD_pH.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

writeRaster(pred_mean_aoa, "outputs/maps/aoa/mean_pH_masked_AOA.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

writeRaster(expected_error, "outputs/maps/aoa/expected_RMSE_pH.tif",
            overwrite = TRUE, wopt = list(datatype = "FLT4S", gdal = gdal_opts))

saveRDS(aoa_result$parameters, "outputs/models/aoa_parameters_pH.rds")

print("SESSION 4 COMPLETE: Area of Applicability finished")


# SECTION 28: Summary ----

print("================================================================")
print("         DIGITAL SOIL MAPPING WORKFLOW COMPLETE")
print("================================================================")

print("Models saved:")
print(list.files("outputs/models/", pattern = "\\.rds$"))

print("Validation outputs:")
print(list.files("outputs/validation/"))

print("Maps generated:")
print(paste("Continuous:", length(list.files("outputs/maps/continuous/")), "files"))
print(paste("Nominal:", length(list.files("outputs/maps/nominal/")), "files"))
print(paste("Ordinal:", length(list.files("outputs/maps/ordinal/")), "files"))
print(paste("AOA:", length(list.files("outputs/maps/aoa/")), "files"))

print("================================================================")
print("Key learning points:")
print("  - Continuous: Regression RF -> mean, SD, CV maps")
print("  - Nominal: Classification RF -> class probabilities")
print("  - Ordinal: Regression on integers (compromise) -> latent + reclassify")
print("  - AOA: Dissimilarity Index identifies where predictions are trustworthy")
print("================================================================")
