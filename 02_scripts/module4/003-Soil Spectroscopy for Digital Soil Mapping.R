## CODE FOR SOILFER MODULE 4: 
## SOIL SECTROSCOPY FOR DIGITAL SOIL MAPPING
## Alex Wadoux and Leonardo Ramirez-Lopez

## Digital Soil Mapping part

## Preceeding code: 002-Soil Spectroscopy for Digital Soil Mapping.R


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

