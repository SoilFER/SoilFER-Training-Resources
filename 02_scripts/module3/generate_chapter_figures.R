# =============================================================================
# generate_chapter_figures.R
# =============================================================================
# PURPOSE:
#   Generates all pre-rendered figures for Chapter 5 of the SoilFER Training
#   Manual. Run this script after updating modelling_&_mapping.R to refresh
#   the figures used by the Rmd chapter.
#
# USAGE:
#   Rscript generate_chapter_figures.R
#
# INPUTS:
#   - Output rasters from modelling_&_mapping.R (maps/continuous, maps/nominal,
#     maps/ordinal, maps/aoa)
#   - Input covariates (01_data/module5/Env_Cov_250m_KANSAS.tif)
#
# OUTPUTS:
#   - PNG files in 03_outputs/module5/figures/
# =============================================================================

library(terra)

cat("=== Generating Chapter 5 figures ===\n")

# ── Paths (relative to this script's location in 02_scripts/module5/) ──
# Adjust if running from a different working directory
RESOURCES_ROOT <- file.path(dirname(dirname(dirname(
  if (interactive()) rstudioapi::getActiveDocumentContext()$path
  else commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _)
))))

# Fallback: assume working directory is the Resources repo root
if (!dir.exists(file.path(RESOURCES_ROOT, "01_data"))) {
  RESOURCES_ROOT <- "."
}

DATA_DIR    <- file.path(RESOURCES_ROOT, "01_data", "module5")
OUTPUTS_DIR <- file.path(RESOURCES_ROOT, "03_outputs", "module5")
FIGURES_DIR <- file.path(OUTPUTS_DIR, "figures")

# Source directories for rasters produced by modelling_&_mapping.R
# These are currently under 01_data/module5/outputs/ but will be moved
# to 03_outputs/module5/ during the migration. Adjust as needed.
MAPS_DIR   <- file.path(OUTPUTS_DIR, "maps")
MODELS_DIR <- file.path(OUTPUTS_DIR, "models")

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Helper function ──
save_map_png <- function(raster_path, output_name, title, palette, ...) {
  if (!file.exists(raster_path)) {
    cat("  SKIP (file not found):", raster_path, "\n")
    return(invisible(NULL))
  }
  r <- rast(raster_path)
  outfile <- file.path(FIGURES_DIR, output_name)
  png(outfile, width = 2400, height = 1800, res = 200)
  plot(r, main = title, col = palette, ...)
  dev.off()
  cat("  OK:", outfile, "\n")
}

# =============================================================================
# 1. Example covariate (from input data)
# =============================================================================
cat("\n[1/17] Example covariate...\n")
cov_file <- file.path(DATA_DIR, "Env_Cov_250m_KANSAS.tif")
if (file.exists(cov_file)) {
  covs <- rast(cov_file)
  png(file.path(FIGURES_DIR, "example_covariate.png"),
      width = 2400, height = 1800, res = 200)
  plot(covs[[1]],
       main = paste("Example covariate:", names(covs)[1]),
       col = hcl.colors(100, "Viridis"))
  dev.off()
  cat("  OK:", file.path(FIGURES_DIR, "example_covariate.png"), "\n")
  rm(covs)
} else {
  cat("  SKIP (file not found):", cov_file, "\n")
}

# =============================================================================
# 2-4. Continuous pH maps (mean, sd, cv)
# =============================================================================
cat("\n[2/17] Mean pH map...\n")
save_map_png(
  file.path(MAPS_DIR, "continuous", "mean_pH.tif"),
  "map_mean_pH.png",
  "Predicted Mean pH",
  hcl.colors(100, "Viridis")
)

cat("[3/17] SD pH map...\n")
save_map_png(
  file.path(MAPS_DIR, "continuous", "sd_pH.tif"),
  "map_sd_pH.png",
  "Prediction Standard Deviation",
  hcl.colors(100, "Reds")
)

cat("[4/17] CV pH map...\n")
save_map_png(
  file.path(MAPS_DIR, "continuous", "cv_pH.tif"),
  "map_cv_pH.png",
  "Coefficient of Variation (%)",
  hcl.colors(100, "YlOrRd")
)

# =============================================================================
# 5-8. Nominal classification maps
# =============================================================================
cat("\n[5/17] Predicted class (max prob)...\n")
save_map_png(
  file.path(MAPS_DIR, "nominal", "class_max_prob.tif"),
  "map_class_max_prob.png",
  "Predicted Class (Max Probability)",
  hcl.colors(9, "Set2")
)

cat("[6/17] Maximum probability value...\n")
save_map_png(
  file.path(MAPS_DIR, "nominal", "max_prob_value.tif"),
  "map_max_prob_value.png",
  "Maximum Probability Value",
  hcl.colors(100, "Viridis")
)

cat("[7/17] Nominal uncertainty...\n")
save_map_png(
  file.path(MAPS_DIR, "nominal", "uncertainty.tif"),
  "map_uncertainty_nominal.png",
  "Uncertainty (1 - max prob)",
  hcl.colors(100, "YlOrRd")
)

cat("[8/17] Confident predictions...\n")
save_map_png(
  file.path(MAPS_DIR, "nominal", "class_confident.tif"),
  "map_class_confident.png",
  "Confident Predictions (prob >= 0.4)",
  hcl.colors(9, "Set2")
)

# =============================================================================
# 9-12. Ordinal maps
# =============================================================================
cat("\n[9/17] Latent values...\n")
save_map_png(
  file.path(MAPS_DIR, "ordinal", "latent_pH_Class.tif"),
  "map_latent_pH_Class.png",
  "Latent Values (continuous 1-5 scale)",
  hcl.colors(100, "Viridis")
)

cat("[10/17] Ordinal class...\n")
save_map_png(
  file.path(MAPS_DIR, "ordinal", "class_pH_Class.tif"),
  "map_class_pH_Class.png",
  "Ordinal Class (reclassified)",
  hcl.colors(5, "Spectral")
)

cat("[11/17] Ordinal SD...\n")
save_map_png(
  file.path(MAPS_DIR, "ordinal", "sd_pH_Class.tif"),
  "map_sd_pH_Class.png",
  "Standard Deviation",
  hcl.colors(100, "Reds")
)

cat("[12/17] Class instability...\n")
save_map_png(
  file.path(MAPS_DIR, "ordinal", "instability_pH_Class.tif"),
  "map_instability_pH_Class.png",
  "Class Instability",
  hcl.colors(100, "YlOrRd")
)

# =============================================================================
# 13-16. AOA maps
# =============================================================================
cat("\n[13/17] Dissimilarity Index...\n")
save_map_png(
  file.path(MAPS_DIR, "aoa", "DI_pH.tif"),
  "map_DI_pH.png",
  "Dissimilarity Index (DI)",
  hcl.colors(100, "Viridis")
)

cat("[14/17] AOA mask...\n")
aoa_file <- file.path(MAPS_DIR, "aoa", "AOA_pH.tif")
if (file.exists(aoa_file)) {
  r <- rast(aoa_file)
  png(file.path(FIGURES_DIR, "map_AOA_pH.png"),
      width = 2400, height = 1800, res = 200)
  plot(r, main = "Area of Applicability (grey = outside)",
       col = c("grey", "green"))
  dev.off()
  cat("  OK:", file.path(FIGURES_DIR, "map_AOA_pH.png"), "\n")
  rm(r)
} else {
  cat("  SKIP (file not found):", aoa_file, "\n")
}

cat("[15/17] Local Point Density...\n")
save_map_png(
  file.path(MAPS_DIR, "aoa", "LPD_pH.tif"),
  "map_LPD_pH.png",
  "Local Point Density (LPD)",
  hcl.colors(100, "Viridis")
)

cat("[16/17] pH masked to AOA...\n")
save_map_png(
  file.path(MAPS_DIR, "aoa", "mean_pH_masked_AOA.tif"),
  "map_mean_pH_masked_AOA.png",
  "pH Mean (only within AOA)",
  hcl.colors(100, "Viridis")
)

# =============================================================================
# 17. AOA coverage statistic (saved as text for reference)
# =============================================================================
cat("\n[17/17] AOA coverage statistic...\n")
if (file.exists(aoa_file)) {
  aoa_mask <- rast(aoa_file)
  aoa_values <- values(aoa_mask, na.rm = TRUE)
  coverage <- sum(aoa_values == 1) / length(aoa_values) * 100
  writeLines(
    paste("AOA coverage:", round(coverage, 1), "% of study area"),
    file.path(FIGURES_DIR, "aoa_coverage.txt")
  )
  cat("  AOA coverage:", round(coverage, 1), "%\n")
  rm(aoa_mask, aoa_values)
} else {
  cat("  SKIP (AOA raster not found)\n")
}

# =============================================================================
# NOTE: The following 5 validation/model figures are NOT generated here
# because they are produced directly by modelling_&_mapping.R:
#   - boruta_pH.png
#   - varimp_pH.png
#   - scatterplot_pH.png
#   - confusion_matrix_Clay_pH_Class.png
#   - ordinal_confusion_pH_Class.png
#
# Those PNGs should be copied from outputs/validation/ to
# 03_outputs/module5/figures/ after running the modelling script.
# =============================================================================

cat("\n=== Done. Figures saved to:", FIGURES_DIR, "===\n")
