## CODE FOR SOILFER MODULE 4: 
## SOIL SECTROSCOPY FOR DIGITAL SOIL MAPPING
## Alex Wadoux and Leonardo Ramirez-Lopez

## Soil spectroscopy models part

## Preceeding code: 001-Soil Spectroscopy for Digital Soil Mapping.R

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

