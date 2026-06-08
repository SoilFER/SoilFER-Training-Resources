## CODE FOR SOILFER MODULE 4: 
## SOIL SECTROSCOPY FOR DIGITAL SOIL MAPPING
## Alex Wadoux and Leonardo Ramirez-Lopez

################################################################################
################ Load and plot the DRS data ####################################
################################################################################

library(readxl)
library(dplyr)

original_dat <- read_excel(
  "../SoilFER-Training-Resources/01_data/module1/training_data/MIR_KANSAS_data.xlsx",
  .name_repair = "minimal",
  na = c("", "NA", "N/A", "NaN"),
  progress = FALSE
)

original_dat <- as.data.frame(original_dat)

dim(original_dat)
sum(duplicated(original_dat$smp_id))


################################################################################
################ Separate spectral from non-spectral data ######################
################################################################################

# Identify spectral columns
spc_cols <- grep("^X[0-9]{3,}", colnames(original_dat))

# Extract spectral matrix
spc_original <- original_dat[, spc_cols]
colnames(spc_original) <- gsub("^X", "", colnames(spc_original))
original_wavs <- as.numeric(colnames(spc_original))

range(original_wavs)


################################################################################
################ Resample spectra ##############################################
################################################################################

library(prospectr)

wavs <- seq(600, 3992, by = 8)

spc_resampled <- prospectr::resample(
  spc_original,
  wav = original_wavs,
  new.wav = wavs
)

dim(spc_resampled)


################################################################################
################ Attach spectra and average replicates #########################
################################################################################

repeated_dat <- original_dat[, -spc_cols]
repeated_dat$spc <- spc_resampled

# Average replicate spectra by sample ID
spc_avg <- aggregate(
  repeated_dat$spc,
  by = list(smp_id = repeated_dat$smp_id),
  FUN = mean
)
spc_avg <- spc_avg[order(spc_avg$smp_id), ]
rownames(spc_avg) <- spc_avg$smp_id
spc_avg <- as.matrix(spc_avg[, -1])

dat <- repeated_dat[!duplicated(repeated_dat$smp_id), ]
dat <- dat[order(dat$smp_id), ]
stopifnot(all(rownames(spc_avg) == dat$smp_id))
dat$spc <- spc_avg


################################################################################
################ Plot the spectra ##############################################
################################################################################

matplot(
  wavs,
  t(dat$spc),
  type = "l",
  lty = 1,
  xlim = c(4000, 500),
  xlab = expression(Wavenumber~(cm^{-1})),
  ylab = "Absorbance",
  col = rgb(0.8, 0, 0, 0.3)
)
grid(lty = 1)


################################################################################
################ Profile identifier ############################################
################################################################################

coords <- paste(dat$Long_Site.x, dat$Lat_Site.x, sep = "_")
prof_num <- match(coords, unique(coords))
dat$ProfID <- sprintf("PROF%04d", prof_num)


################################################################################
################ Example: one spectrum, raw ####################################
################################################################################

a_spectrum <- dat$spc[1, ]
plot(
  wavs, a_spectrum,
  type = "l", lty = 1,
  xlim = c(4000, 500),
  xlab = expression(Wavenumber~(cm^{-1})),
  ylab = "Absorbance",
  col = rgb(0.3, 0, 0.8)
)


################################################################################
################ First derivative (Savitzky-Golay) #############################
################################################################################

a_spectrum_first_der <- prospectr::savitzkyGolay(
  a_spectrum, m = 1, p = 1, w = 25
)
der_wavs <- as.numeric(names(a_spectrum_first_der))

plot(
  der_wavs, a_spectrum_first_der,
  type = "l", lty = 1,
  xlim = c(4000, 500),
  xlab = expression(Wavenumber~(cm^{-1})),
  ylab = "First derivative absorbance",
  col = rgb(0, 0.3, 0.8)
)


################################################################################
################ After SNV correction ##########################################
################################################################################

a_spectrum_first_der_snv <- prospectr::standardNormalVariate(
  matrix(a_spectrum_first_der, nrow = 1)
)

plot(
  der_wavs, a_spectrum_first_der_snv,
  type = "l", lty = 1,
  xlim = c(4000, 500),
  xlab = expression(Wavenumber~(cm^{-1})),
  ylab = "SNV of first derivative absorbance",
  col = rgb(0.8, 0.3, 0)
)


################################################################################
################ Apply preprocessing to all spectra ############################
################################################################################

dat$spc_processed <- dat$spc |>
  prospectr::savitzkyGolay(m = 1, p = 1, w = 25) |>
  prospectr::standardNormalVariate()

wavs_processed <- as.numeric(colnames(dat$spc_processed))

matplot(
  wavs_processed, t(dat$spc_processed),
  type = "l", lty = 1,
  xlim = c(4000, 500),
  xlab = expression(Wavenumber~(cm^{-1})),
  ylab = "SNV of first derivative absorbance",
  col = rgb(0.3, 0, 0.8, 0.3)
)
grid(lty = 1)


################################################################################
################ Select samples and split ######################################
################################################################################

n_cal <- 85

set.seed(201909)
kms <- prospectr::naes(
  dat$spc_processed,
  k = n_cal,
  pc = 10,
  iter.max = 1000
)

# Profile-complete the selection
kms_samples <- which(dat$ProfID %in% unique(dat$ProfID[kms$model]))

# Split into calibration / prediction subsets
dat_cal <- dat[kms_samples, ]
dat_pred <- dat[-kms_samples, ]


################################################################################
################ Define the CV folds ###########################################
################################################################################

set.seed(14092019)
nfolds <- 10

# Assign folds at the profile level (not at the observation level)
cal_profile_ids <- unique(dat_cal$ProfID)
fold_index <- sample(rep(1:nfolds, length.out = length(cal_profile_ids)))

# Each observation inherits the fold of its parent profile
dat_cal$fold <- fold_index[match(dat_cal$ProfID, cal_profile_ids)]


################################################################################
################ Run 10-fold cross-validation ##################################
################################################################################

library(ranger)
library(matrixStats)

SOC_predRF <- rep(NA, nrow(dat_cal))
SOC_varRF <- rep(NA, nrow(dat_cal))
SOC_sdRF <- rep(NA, nrow(dat_cal))

for (k in seq_len(nfolds)) {
  test_rows <- which(dat_cal$fold == k)
  train_rows <- which(dat_cal$fold != k)
  
  model <- ranger(
    x = dat_cal$spc_processed[train_rows, ],
    y = dat_cal$SOC[train_rows],
    quantreg = TRUE, num.trees = 3500,
    sample.fraction = 1, replace = TRUE,
    splitrule = "maxstat", min.node.size = 10,
    seed = 201909
  )
  
  pred_matrix <- predict(
    model, data = dat_cal$spc_processed[test_rows, ],
    type = "quantiles",
    what = function(x) sample(x, 100, replace = TRUE)
  )$predictions
  
  SOC_predRF[test_rows] <- rowMeans(pred_matrix)
  SOC_varRF[test_rows] <- rowVars(pred_matrix)
  SOC_sdRF[test_rows] <- sqrt(SOC_varRF[test_rows])
}

dat_cal$SOC_predRF <- SOC_predRF
dat_cal$SOC_varRF <- SOC_varRF
dat_cal$SOC_sdRF <- SOC_sdRF


################################################################################
################ The validation plot (base R version) ##########################
################################################################################

xy_lims <- range(dat_cal$SOC_predRF, dat_cal$SOC)

plot(
  x = dat_cal$SOC_predRF,
  y = dat_cal$SOC,
  xlab = "Predicted SOC (%)",
  ylab = "Measured SOC (%)",
  xlim = xy_lims,
  ylim = xy_lims,
  col = rgb(0, 0, 0, 0.5),
  pch = 16,
  cex = 1.5
)

# Error bars (prediction SD)
arrows(
  x0 = dat_cal$SOC_predRF - dat_cal$SOC_sdRF,
  y0 = dat_cal$SOC,
  x1 = dat_cal$SOC_predRF + dat_cal$SOC_sdRF,
  y1 = dat_cal$SOC,
  code = 3,
  length = 0,
  col = rgb(0, 0, 0, 0.5)
)

abline(0, 1, col = col_ref, lwd = 1.5, lty = 2)
grid(col = rgb(0.8, 0.8, 0.8, 0.6), lty = 1)


################################################################################
################ Computing the error (RMSE) ####################################
################################################################################

drs_rmse <- sqrt(mean((dat_cal$SOC_predRF - dat_cal$SOC)^2, na.rm = TRUE))
cat("DRS model RMSE:", round(drs_rmse, 3), "% SOC\n")


################################################################################
################ Final model fitting ###########################################
################################################################################

final_drs_model <- ranger(
  x = dat_cal$spc_processed,
  y = dat_cal$SOC,
  quantreg = TRUE,
  num.trees = 3500,
  sample.fraction = 1,
  replace = TRUE,
  splitrule = "maxstat",
  min.node.size = 10,
  seed = 201909
)


################################################################################
################ Predict SOC for the remaining 80% #############################
################################################################################

drs_soc_preds <- predict(
  final_drs_model,
  data = dat_pred$spc_processed,
  type = "quantiles",
  what = function(x) sample(x, 100, replace = TRUE)
)$predictions

dat_pred$SOC_predRF <- rowMeans(drs_soc_preds)
dat_pred$SOC_varRF <- rowVars(drs_soc_preds)
dat_pred$SOC_sdRF <- sqrt(dat_pred$SOC_varRF)


################################################################################
################ Assemble the augmented dataset ################################
################################################################################

dat_augmented <- dat[, c(
  "smp_id", "ProfID", "Long_Site.x", "Lat_Site.x",
  "Top_depth_cm.x", "Bottom_depth_cm.x"
)]

dat_augmented$SOC <- NA
dat_augmented$SOC_sd <- NA

# Lab SOC for calibration samples
dat_augmented$SOC[kms_samples] <- dat$SOC[kms_samples]

# DRS-predicted SOC for the rest
dat_augmented$SOC[-kms_samples] <- dat_pred$SOC_predRF

# DRS prediction SD as uncertainty for predicted samples
dat_augmented$SOC_sd[-kms_samples] <- dat_pred$SOC_sdRF


################################################################################
################ Apply lab uncertainty #########################################
################################################################################

# Lab uncertainty (Stevens et al. 2013)
dat$SOC_sd <- 0.15

# Same value for calibration samples in the augmented dataset
dat_augmented$SOC_sd[kms_samples] <- 0.15


################################################################################
################ Build SoilProfileCollection objects ###########################
################################################################################

library(aqp)
depths(dat) <- ProfID ~ Top_depth_cm.x + Bottom_depth_cm.x
depths(dat_augmented) <- ProfID ~ Top_depth_cm.x + Bottom_depth_cm.x


################################################################################
################ Helper for weighted SD over 0-30 cm ###########################
################################################################################

weighted_sd_0_30 <- function(top, bottom, sd, z1 = 0, z2 = 30) {
  w <- pmax(0, pmin(bottom, z2) - pmax(top, z1))
  keep <- w > 0 & !is.na(sd)
  w <- w[keep]
  sd <- sd[keep]
  if (sum(w) < (z2 - z1)) return(NA)
  sqrt(sum((w * sd)^2)) / sum(w)
}


################################################################################
################ Harmonise the reference dataset ###############################
################################################################################

soc_mean_ref <- profileApply(dat, function(p) {
  h <- horizons(p)
  w <- pmax(0, pmin(h$Bottom_depth_cm.x, 30) - pmax(h$Top_depth_cm.x, 0))
  keep <- w > 0 & !is.na(h$SOC)
  if (sum(w[keep]) < 30) return(NA)
  sum(w[keep] * h$SOC[keep]) / sum(w[keep])
})

soc_sd_ref <- horizons(dat) |>
  as.data.frame() |>
  group_by(ProfID) |>
  summarise(
    SOC_sd_0_30 = weighted_sd_0_30(Top_depth_cm.x, Bottom_depth_cm.x, SOC_sd),
    .groups = "drop"
  )

coords_ref <- as.data.frame(dat)[, c("ProfID", "Lat_Site.x", "Long_Site.x")]
coords_ref <- coords_ref[!duplicated(coords_ref$ProfID), ]

soc_ref_0_30_xy <- soc_sd_ref |>
  mutate(SOC_0_30 = soc_mean_ref) |>
  select(ProfID, SOC_0_30, SOC_sd_0_30) |>
  merge(coords_ref, by = "ProfID", all.x = TRUE)


################################################################################
################ Harmonise the augmented dataset ###############################
################################################################################

soc_mean_aug <- profileApply(dat_augmented, function(p) {
  h <- horizons(p)
  w <- pmax(0, pmin(h$Bottom_depth_cm.x, 30) - pmax(h$Top_depth_cm.x, 0))
  keep <- w > 0 & !is.na(h$SOC)
  if (sum(w[keep]) < 30) return(NA)
  sum(w[keep] * h$SOC[keep]) / sum(w[keep])
})

soc_sd_aug <- horizons(dat_augmented) |>
  as.data.frame() |>
  group_by(ProfID) |>
  summarise(
    SOC_sd_0_30 = weighted_sd_0_30(Top_depth_cm.x, Bottom_depth_cm.x, SOC_sd),
    .groups = "drop"
  )

coords_aug <- as.data.frame(dat_augmented)[, c("ProfID", "Lat_Site.x", "Long_Site.x")]
coords_aug <- coords_aug[!duplicated(coords_aug$ProfID), ]

soc_aug_0_30_xy <- soc_sd_aug |>
  mutate(SOC_0_30 = soc_mean_aug) |>
  select(ProfID, SOC_0_30, SOC_sd_0_30) |>
  merge(coords_aug, by = "ProfID", all.x = TRUE)


################################################################################
################ Caveat on uncertainty propagation #############################
################################################################################

# Lab uncertainty fixed at the profile level (Stevens et al. 2013)
soc_ref_0_30_xy$SOC_sd_0_30 <- 0.15

soc_aug_0_30_xy$SOC_sd_0_30[
  soc_aug_0_30_xy$ProfID %in% unique(dat$ProfID[kms_samples])
] <- 0.15


################################################################################
################ Load covariates and extract ###################################
################################################################################

library(terra)

covs <- rast(
  "../SoilFER-Training-Resources/01_data/module1/training_data/Environmental_Covariates_250m_KANSAS.tif"
)
cov_names <- names(covs)

# Reference (lab only)
soil_pts_ref <- vect(
  soc_ref_0_30_xy,
  geom = c("Long_Site.x", "Lat_Site.x"),
  crs = "EPSG:4326"
)
soil_pts_ref <- project(soil_pts_ref, covs)
soil_pts_ref <- cbind(soil_pts_ref, terra::extract(covs, soil_pts_ref)[, -1])

# Augmented (lab + DRS)
soil_pts_aug <- vect(
  soc_aug_0_30_xy,
  geom = c("Long_Site.x", "Lat_Site.x"),
  crs = "EPSG:4326"
)
soil_pts_aug <- project(soil_pts_aug, covs)
soil_pts_aug <- cbind(soil_pts_aug, terra::extract(covs, soil_pts_aug)[, -1])


################################################################################
################ Compute weights ###############################################
################################################################################

soil_pts_ref$weight <- 1 / soil_pts_ref$SOC_sd_0_30^2
soil_pts_aug$weight <- 1 / soil_pts_aug$SOC_sd_0_30^2

# Normalise by the maximum weight
soil_pts_ref$weight <- soil_pts_ref$weight / max(soil_pts_ref$weight, na.rm = T)
soil_pts_aug$weight <- soil_pts_aug$weight / max(soil_pts_aug$weight, na.rm = T)


################################################################################
################ Baseline model (lab only) #####################################
################################################################################

library(ranger)

soil_df_ref <- as.data.frame(soil_pts_ref, geom = "XY")[, c("x", "y", "SOC_0_30", "SOC_sd_0_30", "weight", cov_names)
]
soil_df_ref <- soil_df_ref[!is.na(soil_df_ref$SOC_0_30), ]

form <- as.formula(paste("SOC_0_30 ~", paste(cov_names, collapse = " + ")))

mod_ref <- ranger(
  formula = form, data = soil_df_ref,
  case.weights = soil_df_ref$weight,
  replace = FALSE, sample.fraction = 0.632,
  num.trees = 500, seed = 201909
)

soc_map_ref <- predict(covs, mod_ref, na.rm = TRUE)
terra::writeRaster(soc_map_ref, "soc_map_ref.tif", overwrite = TRUE)


################################################################################
################ Augmented model (lab + DRS) ###################################
################################################################################

soil_df_aug <- as.data.frame(soil_pts_aug, geom = "XY")[, c("x", "y", "SOC_0_30", "SOC_sd_0_30", "weight", cov_names)
]
soil_df_aug <- soil_df_aug[!is.na(soil_df_aug$SOC_0_30), ]

mod_aug <- ranger(
  formula = form, data = soil_df_aug,
  case.weights = soil_df_aug$weight,
  replace = FALSE, sample.fraction = 0.632,
  num.trees = 500, seed = 201909
)

soc_map_aug <- predict(covs, mod_aug, na.rm = TRUE)
terra::writeRaster(soc_map_aug, "soc_map_aug.tif", overwrite = TRUE)


################################################################################
################ Weighted performance helper functions #########################
################################################################################

wME <- function(obs, pred, w) {
  sum(w * (pred - obs), na.rm = TRUE) / sum(w, na.rm = TRUE)
}

wRMSE <- function(obs, pred, w) {
  sqrt(sum(w * (pred - obs)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
}

wNSE <- function(obs, pred, w) {
  zbar_w <- sum(w * obs, na.rm = TRUE) / sum(w, na.rm = TRUE)
  1 - sum(w * (pred - obs)^2, na.rm = TRUE) /
    sum(w * (obs - zbar_w)^2, na.rm = TRUE)
}


################################################################################
################ 10-fold cross-validation of DSM models ########################
################################################################################

run_cv <- function(data, response, predictors, nfolds = 10, seed = 201909) {
  set.seed(seed)
  n <- nrow(data)
  fold_id <- sample(rep(seq_len(nfolds), length.out = n))
  cv_pred <- rep(NA_real_, n)
  form <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  
  for (fold in seq_len(nfolds)) {
    train <- data[fold_id != fold, ]
    test <- data[fold_id == fold, ]
    mod <- ranger(
      formula = form, data = train,
      case.weights = train$weight,
      replace = FALSE, sample.fraction = 0.632,
      seed = seed
    )
    cv_pred[fold_id == fold] <- predict(mod, data = test)$predictions
  }
  
  data.frame(obs = data[[response]], pred = cv_pred, weight = data$weight)
}

cv_ref <- run_cv(soil_df_ref, "SOC_0_30", cov_names)
cv_aug <- run_cv(soil_df_aug, "SOC_0_30", cov_names)


################################################################################
################ Compare the two DSM models ####################################
################################################################################

compute_metrics <- function(cv) {
  data.frame(
    wME = round(wME(cv$obs, cv$pred, cv$weight), 3),
    wRMSE = round(wRMSE(cv$obs, cv$pred, cv$weight), 3),
    wNSE = round(wNSE(cv$obs, cv$pred, cv$weight), 3)
  )
}

metrics <- rbind(
  data.frame(Model = "Reference (lab only)", compute_metrics(cv_ref)),
  data.frame(Model = "Augmented (lab + DRS)", compute_metrics(cv_aug))
)

metrics


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
