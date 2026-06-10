## CODE FOR SOILFER MODULE 4: 
## SOIL SECTROSCOPY FOR DIGITAL SOIL MAPPING
## Alex Wadoux and Leonardo Ramirez-Lopez

## Read and pre-process spectra part


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

