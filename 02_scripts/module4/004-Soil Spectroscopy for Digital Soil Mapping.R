## CODE FOR SOILFER MODULE 4: 
## SOIL SECTROSCOPY FOR DIGITAL SOIL MAPPING
## Alex Wadoux and Leonardo Ramirez-Lopez

## Additional excerisies part

## Preceeding code: 003-Soil Spectroscopy for Digital Soil Mapping.R

################################################################################
################ Step 1: PCA on processed spectra ##############################
################################################################################

library(prospectr)
library(ggplot2)

pca_out <- prcomp(dat$spc_processed, center = TRUE, scale. = FALSE)
scores_raw <- pca_out$x[, 1:10]
scores_sd <- apply(scores_raw, 2, sd)
scores <- sweep(scores_raw, 2, scores_sd, "/")


################################################################################
################ Step 2: KDE of the full population ############################
################################################################################

n_grid <- 512
kde_full <- vector("list", ncol(scores))

for (j in seq_len(ncol(scores))) {
  kde_full[[j]] <- density(
    scores[, j],
    bw = "nrd0", n = n_grid,
    from = min(scores[, j]), to = max(scores[, j])
  )
}


################################################################################
################ Step 3: MSD across set sizes ##################################
################################################################################

# Range of calibration set sizes to evaluate
set_sizes <- seq(10, 200, by = 10)

# k-means is stochastic → repeat each set size several times
repetitions <- 10

# Storage: MSD matrix (rows = sizes, cols = reps), PC1 KDE per size, mean sample count
msd_matrix <- matrix(NA, nrow = length(set_sizes), ncol = repetitions)
kde_subsets_pc1 <- vector("list", length(set_sizes))
n_samples_vec <- numeric(length(set_sizes))

# Loop over candidate calibration set sizes
for (i in seq_along(set_sizes)) {
  n_samp_rep <- numeric(repetitions)
  
  # Repeat to capture k-means initialisation variability
  for (r in seq_len(repetitions)) {
    set.seed(r * i)
    
    # Select k profiles by k-means in PC space
    kms_i <- prospectr::naes(scores, k = set_sizes[i], iter.max = 1000)
    
    # Profile-complete: include all horizons of selected profiles
    idx_i <- which(dat$ProfID %in% unique(dat$ProfID[kms_i$model]))
    n_samp_rep[r] <- length(idx_i)
    
    # MSD per PC, then averaged across the 10 PCs
    msd_pcs <- numeric(ncol(scores))
    for (j in seq_len(ncol(scores))) {
      # KDE on the same grid/bandwidth as the full-population KDE
      kde_j <- density(
        scores[idx_i, j],
        bw = kde_full[[j]]$bw, n = n_grid,
        from = min(scores[, j]), to = max(scores[, j])
      )
      # Mean squared distance between the two density estimates
      msd_pcs[j] <- mean((kde_j$y - kde_full[[j]]$y)^2)
    }
    msd_matrix[i, r] <- mean(msd_pcs)
    
    # Keep the first PC1 KDE per set size for the convergence plot
    if (r == 1) {
      kde_subsets_pc1[[i]] <- density(
        scores[idx_i, 1],
        bw = kde_full[[1]]$bw, n = n_grid,
        from = min(scores[, 1]), to = max(scores[, 1])
      )
    }
  }
  # Average number of horizons selected (used for the dual axis later)
  n_samples_vec[i] <- round(mean(n_samp_rep))
}

# Summarise MSD across repetitions
msd_mean <- rowMeans(msd_matrix)
msd_sd <- apply(msd_matrix, 1, sd)


################################################################################
################ Create the DRS calibration subsets ############################
################################################################################

n_profiles <- c(20, 50, 85, 140, 200, 300)
cal_indices <- vector("list", length(n_profiles))
names(cal_indices) <- as.character(n_profiles)

for (i in seq_along(n_profiles)) {
  set.seed(201909)
  kms_i <- prospectr::naes(
    dat$spc_processed,
    k = n_profiles[i],
    pc = 10,
    iter.max = 1000
  )
  cal_indices[[i]] <- which(dat$ProfID %in% unique(dat$ProfID[kms_i$model]))
}


################################################################################
################ Fit DRS models for each size ##################################
################################################################################

drs_preds <- vector("list", length(n_profiles))
names(drs_preds) <- as.character(n_profiles)

for (i in seq_along(n_profiles)) {
  cal_idx <- cal_indices[[i]]
  pred_idx <- setdiff(seq_len(nrow(dat)), cal_idx)
  
  drs_model_i <- ranger(
    x = dat$spc_processed[cal_idx, ], y = dat$SOC[cal_idx],
    quantreg = TRUE, num.trees = 3500,
    sample.fraction = 1, replace = TRUE,
    splitrule = "maxstat", min.node.size = 10, seed = 201909
  )
  
  preds_i <- predict(drs_model_i,
                     data = dat$spc_processed[pred_idx, ],
                     type = "quantiles",
                     what = function(x) sample(x, 100, replace = TRUE)
  )$predictions
  
  drs_preds[[i]] <- data.frame(
    row_idx = pred_idx,
    SOC_predRF = rowMeans(preds_i),
    SOC_sdRF = sqrt(rowVars(preds_i))
  )
}


################################################################################
################ Assemble the augmented datasets ###############################
################################################################################

aug_datasets <- vector("list", length(n_profiles))
names(aug_datasets) <- as.character(n_profiles)

my_col_names <- c("smp_id", "ProfID", "Long_Site.x", "Lat_Site.x",
                  "Top_depth_cm.x", "Bottom_depth_cm.x")

for (i in seq_along(n_profiles)) {
  cal_idx <- cal_indices[[i]]
  pred_idx <- drs_preds[[i]]$row_idx
  
  aug_i <- as.data.frame(dat)[, my_col_names]
  aug_i$SOC <- NA
  aug_i$SOC_sd <- NA
  
  aug_i$SOC[cal_idx] <- dat$SOC[cal_idx]
  aug_i$SOC_sd[cal_idx] <- 0.15
  
  aug_i$SOC[pred_idx] <- drs_preds[[i]]$SOC_predRF
  aug_i$SOC_sd[pred_idx] <- drs_preds[[i]]$SOC_sdRF
  
  aug_datasets[[i]] <- aug_i
}


################################################################################
################ Hold out a fixed validation set ###############################
################################################################################

soil_df_ref$ProfID <- soc_ref_0_30_xy$SOC_0_30[!is.na(soc_ref_0_30_xy$SOC_0_30)]

set.seed(202409)
val_profiles <- sample(unique(soil_df_ref$ProfID), size = 50)
val_data <- soil_df_ref[soil_df_ref$ProfID %in% val_profiles, ]


################################################################################
################ Harmonise to 0-30 cm ##########################################
################################################################################

depth_weighted_mean_0_30 <- function(p) {
  h <- horizons(p)
  w <- pmax(0, pmin(h$Bottom_depth_cm.x, 30) - pmax(h$Top_depth_cm.x, 0))
  keep <- w > 0 & !is.na(h$SOC)
  if (sum(w[keep]) < 30) return(NA)
  sum(w[keep] * h$SOC[keep]) / sum(w[keep])
}

aug_0_30 <- vector("list", length(n_profiles))
names(aug_0_30) <- as.character(n_profiles)

for (i in seq_along(n_profiles)) {
  cal_idx <- cal_indices[[i]]
  aug_i <- aug_datasets[[i]]
  depths(aug_i) <- ProfID ~ Top_depth_cm.x + Bottom_depth_cm.x
  
  soc_mean_i <- profileApply(aug_i, depth_weighted_mean_0_30)
  
  soc_sd_i <- horizons(aug_i) |>
    as.data.frame() |>
    group_by(ProfID) |>
    summarise(SOC_sd_0_30 = weighted_sd_0_30(
      Top_depth_cm.x, Bottom_depth_cm.x, SOC_sd),
      .groups = "drop")
  
  coords_i <- as.data.frame(aug_i)[, c("ProfID", "Lat_Site.x", "Long_Site.x")]
  coords_i <- coords_i[!duplicated(coords_i$ProfID), ]
  
  soc_0_30_i <- soc_sd_i |>
    mutate(SOC_0_30 = soc_mean_i) |>
    select(ProfID, SOC_0_30, SOC_sd_0_30) |>
    merge(coords_i, by = "ProfID", all.x = TRUE)
  
  # Lab uncertainty fixed at profile level
  cal_profiles <- unique(dat$ProfID[cal_idx])
  soc_0_30_i$SOC_sd_0_30[soc_0_30_i$ProfID %in% cal_profiles] <- 0.15
  
  aug_0_30[[i]] <- soc_0_30_i[!soc_0_30_i$ProfID %in% val_profiles, ]
}


################################################################################
################ Fit DSM models for each scenario ##############################
################################################################################

dsm_models <- vector("list", length(n_profiles))
soc_maps <- vector("list", length(n_profiles))
names(dsm_models) <- as.character(n_profiles)
names(soc_maps) <- as.character(n_profiles)

my_formula <- as.formula(paste("SOC_0_30 ~", paste(cov_names, collapse = " + ")))
nms <- c("x", "y", "SOC_0_30", "SOC_sd_0_30", cov_names)

for (i in seq_along(n_profiles)) {
  pts_i <- vect(aug_0_30[[i]],
                geom = c("Long_Site.x", "Lat_Site.x"),
                crs = "EPSG:4326")
  pts_i <- project(pts_i, covs)
  pts_i <- cbind(pts_i, terra::extract(covs, pts_i)[, -1])
  
  df_i <- as.data.frame(pts_i, geom = "XY")[, nms]
  df_i <- df_i[!is.na(df_i$SOC_0_30) & !is.na(df_i$SOC_sd_0_30), ]
  df_i$weight <- 1 / df_i$SOC_sd_0_30^2
  df_i$weight <- df_i$weight / max(df_i$weight, na.rm = TRUE)
  
  dsm_models[[i]] <- ranger(
    formula = my_formula, data = df_i,
    case.weights = df_i$weight,
    replace = FALSE, sample.fraction = 0.632,
    num.trees = 500, seed = 201909
  )
  soc_maps[[i]] <- predict(covs, dsm_models[[i]], na.rm = TRUE)
}


################################################################################
################ Evaluate on the held-out set ##################################
################################################################################

dsm_metrics <- data.frame()

for (i in seq_along(n_profiles)) {
  preds_val <- predict(dsm_models[[i]], data = val_data)$predictions
  
  obs <- val_data$SOC_0_30
  resid <- preds_val - obs
  
  me <- mean(resid, na.rm = TRUE)
  rmse <- sqrt(mean(resid^2, na.rm = TRUE))
  nse <- 1 - sum(resid^2, na.rm = TRUE) /
    sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  
  dsm_metrics <- rbind(
    dsm_metrics,
    data.frame(
      n_profiles = n_profiles[i],
      n_samples = length(cal_indices[[i]]),
      ME = round(me, 3),
      RMSE = round(rmse, 3),
      NSE = round(nse, 3)
    )
  )
}
