###############################################################################
# SoilFER Online Training Programme — Module 1
# SESSION 3: Data Preparation with the KSSL Dataset — Part 2 (1.5 hours)
# Sections: Lab data, Duplicates, Harmonization, Spectroscopy prep, Export
###############################################################################
#
# LEARNING OBJECTIVES
# -------------------
# By the end of this session, participants will be able to:
#   1. Attach laboratory (wet chemistry) data to the cleaned site structure
#   2. Validate analytical values against feasible thresholds
#   3. Report and document out-of-bounds issues
#   4. Validate texture data (particle-size fractions)
#   5. Apply targeted corrections or NA replacement for erroneous values
#   6. Detect and resolve duplicated horizons and competing depth sequences
#   7. Harmonize data to standard depth intervals (0–30, 30–60 cm)
#   8. Export cleaned and standardized datasets for DSM and spectroscopy
#
# PREREQUISITE
# ------------
# This session continues directly from Session 2.
#
# The `raw_data` and `site` objects produced in Session 2 must be available
# in the R environment. If they are not available, re-run Session 2 before
# executing this script.
#
# IMPORTANT:
# Session 3 does NOT reconstruct ProfID, coordinates, or horizon depths from
# raw_data. The cleaned `site` object is the structural basis of the workflow.
# Laboratory values are attached to it using the persistent `rowID`.
#
# TIMING GUIDE (approximate)
# ---------------------------
#  0:00 – 0:25  Lab data extraction, joining to site data, threshold validation
#  0:25 – 0:45  Texture validation; targeted correction and NA replacement
#  0:45 – 1:10  Duplicate detection and resolution (average horizons,
#               chain_horizons function, surface coverage check)
#  1:10 – 1:30  Depth standardization with aqp::slab(); wide-format output;
#               DSM subset; spectroscopy merge; export
###############################################################################


# =============================================================================
# PART 1 — PREPARING LAB DATA
# =============================================================================
# NOTE: `raw_data` and `site` from Session 2 must already be in the environment.
#
# NEW WORKFLOW:
# raw_data -> site -> site_lab
#
# The `site` object already contains the cleaned coordinates, sampling dates,
# profile IDs, horizon IDs, and validated depth structure. Therefore these
# steps are not repeated here.
# =============================================================================

library(readxl)           # Read Excel files
library(tidyverse)        # Data manipulation and visualization
library(writexl)          # Write Excel files

# Define the folder to store the results of the exercise
output_dir <- "03_outputs/module1/"

# Define the relative path to the folder with the MIR data
training_dir <- "01_data/module1/training_data/"


# -----------------------------------------------------------------------------
# 1.1  Extract and Standardize Laboratory Columns
# -----------------------------------------------------------------------------
# NOTE: All analytical parameters must be numeric.
#
# APPROACH:
# 1. Extract analytical data from the original raw_data object
# 2. Preserve rowID as the link to the original observations
# 3. Rename analytical columns to consistent names
# 4. Convert analytical parameters to numeric
# 5. Attach laboratory measurements to the already-cleaned `site` object
#
# This prevents the profile/site cleaning performed in Session 2 from being
# duplicated.
# -----------------------------------------------------------------------------

# Extract laboratory measurements from the original data.
# rowID links each laboratory record back to the cleaned site record.
lab_data <- raw_data %>%
  select(
    rowID,
    `Estimated Organic Carbon`, `Carbon, Total`,                                     # Soil Organic Carbon and Total Carbon (%)
    `Bulk Density, <2mm Fraction, 1/3 Bar`, `Bulk Density, <2mm Fraction, Ovendry`,  # Bulk density at 1/3 bar and oven dry (g/cm³)
    `Sand, Total`, `Silt, Total`, `Clay`,                                            # Texture (%)
    `pH, 1:1 Soil-Water Suspension`,                                                 # pH H2O
    `CEC, NH4OAc, pH 7.0, 2M KCl displacement`,                                     # CEC in cmol(+)/kg
    `Nitrogen, Total`,                                                               # Total nitrogen (%)
    `Phosphorus, Mehlich3 Extractable`, `Phosphorus, Olsen Extractable`,             # Available P (mg/kg)
    `Potassium, NH4OAc Extractable, 2M KCl displacement`,                            # Extractable K (cmol(+)/kg)
    `Calcium, NH4OAc Extractable, 2M KCl displacement`                               # Extractable Ca (cmol(+)/kg)
  )

# Rename laboratory columns to standard, consistent names
names(lab_data) <- c(
  "rowID",
  "SOC",                             # Soil Organic Carbon (%)
  "Carbon_Total",                    # Total carbon (%)
  "Bulk.Density_1_3.BAR",            # BD at 1/3 bar (g/cm³)
  "Bulk.Density_ovendry",            # BD oven dry (g/cm³)
  "Sand",                            # Sand content (%)
  "Silt",                            # Silt content (%)
  "Clay",                            # Clay content (%)
  "pH",                              # Soil pH (H2O)
  "CEC",                             # Cation exchange capacity (cmol(+)/kg)
  "Nitrogen_Total",                  # Total nitrogen (%)
  "Phosphorus_Mehlich3",             # Available P (mg/kg)
  "Phosphorus_Olsen",                # Available P (mg/kg)
  "Potassium",                       # Exchangeable K (cmol(+)/kg)
  "Calcium"                          # Extractable Ca (cmol(+)/kg)
)

# Ensure numeric type for all analytical parameters
lab_data <- lab_data %>%
  mutate(across(-rowID, as.numeric))

# Attach laboratory measurements to the cleaned site records.
# Only rows retained in `site` are carried forward.
site_lab <- site %>%
  left_join(lab_data, by = "rowID")

# Explore the combined site + laboratory dataset
site_lab


# =============================================================================
# PART 2 — LABORATORY DATA VALIDATION
# WHY: Soil properties have known valid ranges. Values outside these ranges
#      may indicate measurement errors, unit mistakes, or data-entry errors.
#      All analytical parameters must be numeric.
#
# APPROACH:
# 1. Load thresholds for analytical soil properties
# 2. Find values outside these thresholds
# 3. Generate a detailed report of issues
# 4. Apply corrections where possible
#
# SOURCES for valid ranges:
# NOTE: The analytical thresholds used in this tutorial are based on global
#       soil datasets and literature and use the same measurement units as
#       the KSSL dataset. Adjust them for your region, soil types, methods,
#       and measurement units.
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1  Check 1: Load Property Thresholds and Identify Out-of-Bounds Values
# -----------------------------------------------------------------------------
# Analytical thresholds are stored in a CSV file for transparency.

property_thresholds <- read_csv("01_data/module1/kssl/property_thresholds.csv")
property_thresholds

# Identify out-of-bounds values; create a list to store issues
out_of_bounds_issues <- list()

for (i in seq_len(nrow(property_thresholds))) {
  prop <- property_thresholds$property[i]
  prop_desc <- property_thresholds$description[i]
  min_val <- property_thresholds$min_valid[i]
  max_val <- property_thresholds$max_valid[i]
  
  # Check property exists in the dataset
  if (prop %in% names(site_lab)) {
    x <- site_lab[[prop]]
    
    # Detect out-of-bounds: non-missing values outside [min_val, max_val]
    idx <- which(!is.na(x) & (x < min_val | x > max_val))
    
    if (length(idx) > 0) {
      out_of_bounds_issues[[prop]] <- tibble(
        rowID = site_lab$rowID[idx],
        property = prop,
        description = prop_desc,
        value = x[idx],
        min_valid = min_val,
        max_valid = max_val,
        issue = ifelse(
          x[idx] < min_val,
          paste0("Below minimum: ", round(x[idx], 2), " < ", min_val),
          paste0("Above maximum: ", round(x[idx], 2), " > ", max_val)
        )
      )
    }
  }
}

# We can easily detect potential issues with site_lab data
out_of_bounds_issues

# Remove temporary objects created by the loop, if present
rm(i, max_val, min_val, prop, prop_desc, x)
if (exists("idx")) rm(idx)


# -----------------------------------------------------------------------------
# 2.2  Reporting Out-of-Bounds Values and Creating an Audit Trail
# -----------------------------------------------------------------------------
# Export a QC report for review and documentation before correcting data.

if (length(out_of_bounds_issues) > 0) {
  all_issues <- bind_rows(out_of_bounds_issues)
  cat("\n Out-of-bounds properties found\n")
  
  # Summary by property
  issue_summary <- all_issues %>%
    group_by(property, description) %>%
    summarise(
      count = n(),
      min_value_found = min(value, na.rm = TRUE),
      max_value_found = max(value, na.rm = TRUE),
      min_valid = first(min_valid),
      max_valid = first(max_valid),
      .groups = "drop"
    ) %>%
    arrange(desc(count))
  
  cat("Issues by property:\n")
  print(issue_summary)
  
  # Rows with multiple issues
  rows_with_multiple_issues <- all_issues %>%
    group_by(rowID) %>%
    summarise(
      n_issues = n(),
      properties = paste(property, collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(n_issues > 1) %>%
    arrange(desc(n_issues))
  
  if (nrow(rows_with_multiple_issues) > 0) {
    cat("\n Records with MULTIPLE property issues:\n")
    print(head(rows_with_multiple_issues, 10))
    cat("\nThese records likely have data entry errors and should be reviewed.\n")
  }
  
  # Export QC report
  write_xlsx(
    list(
      Summary = issue_summary,
      Issues_by_record = rows_with_multiple_issues,
      All_issues = all_issues
    ),
    paste0(output_dir, "soil_property_validation_report.xlsx")
  )
  
  cat("\n Detailed report saved to: 03_outputs/module1/soil_property_validation_report.xlsx\n")
  
  rm(all_issues, issue_summary, rows_with_multiple_issues)
  
} else {
  cat("\n All soil properties within valid ranges!\n")
}


# -----------------------------------------------------------------------------
# 2.3  Check 2: Texture Validation
# -----------------------------------------------------------------------------
# Particle-size fractions (Clay + Silt + Sand) should sum to approximately 100%.
# Values failing this check are flagged for review — NOT automatically removed.

texture_problems <- site_lab %>%
  mutate(
    texture_sum = Clay + Silt + Sand,
    texture_valid = abs(texture_sum - 100) < 2
  ) %>%
  filter(!texture_valid)

# View texture issues
if (nrow(texture_problems) > 0) {
  cat(" Found", nrow(texture_problems),
      "records with invalid texture sums\n\n")
  print(
    texture_problems %>%
      select(rowID, ProfID, Clay, Silt, Sand, texture_sum)
  )
  # Flag for review (do not automatically remove)
} else {
  cat(" Texture problems not found")
}


# -----------------------------------------------------------------------------
# 2.4  Check 3: Correction of Out-of-Bounds Laboratory Values
# -----------------------------------------------------------------------------
# Two options:
#   Option 1 (preferred): Targeted correction when the error mechanism is known.
#     Use this option if the true value can be recovered from the source or if
#     the problem is clearly attributable to an identifiable error.
#
#     In this database, SOC is negative in some rows while
#     Phosphorus_Mehlich3 contains an extreme value in one row.
#     If a Phosphorus_Mehlich3 value is known to use the wrong units
#     (e.g. ppb instead of mg/kg), it can be corrected.
#
#   Option 2: Replace suspect values with NA when the true value cannot be
#             reliably reconstructed.
#
# WARNING:
# - Other datasets may contain errors in different properties.
# - Always inspect potential issues before applying corrections.
# - Only apply automatic corrections when the cause is known and justified.
# -----------------------------------------------------------------------------

# --- Option 1: Targeted corrections (when error mechanism is known) ---

# Inspect the nature of each issue before correcting
for (property in names(out_of_bounds_issues)) {
  cat(
    "Total errors in", property, ":",
    n_distinct(out_of_bounds_issues[[property]]$rowID), "\n"
  )
  print(summary(data.frame(out_of_bounds_issues[property])[4]))
}

# Correction: Phosphorus Mehlich 3 > 2000 mg/kg
# Example of a likely 1000x unit error (ppb instead of ppm/mg kg-1).
idx <- !is.na(site_lab$Phosphorus_Mehlich3) &
  site_lab$Phosphorus_Mehlich3 > 2000

n_idx <- sum(idx)

if (n_idx > 0) {
  site_lab$Phosphorus_Mehlich3[idx] <-
    site_lab$Phosphorus_Mehlich3[idx] / 1000
}

rm(idx, n_idx)


# --- Option 2: Replace out-of-bounds values with NA ---
# Use when the true value cannot be reliably reconstructed.
#
# NOTE:
# The issue list was generated before the targeted correction above.
# Consequently, values corrected under Option 1 should not subsequently be
# replaced with NA. Here the correction is re-checked against the thresholds
# before replacement.

for (property in names(out_of_bounds_issues)) {
  
  min_valid <- property_thresholds %>%
    filter(.data$property == property) %>%
    pull(min_valid)
  
  max_valid <- property_thresholds %>%
    filter(.data$property == property) %>%
    pull(max_valid)
  
  # Replace only values that remain outside the valid range
  site_lab <- site_lab %>%
    mutate(
      "{property}" := if_else(
        !is.na(.data[[property]]) &
          (.data[[property]] < min_valid | .data[[property]] > max_valid),
        NA_real_,
        .data[[property]]
      )
    )
}

if (exists("min_valid")) rm(min_valid)
if (exists("max_valid")) rm(max_valid)


# =============================================================================
# PART 3 — RESOLVING DUPLICATED DATA IN SOIL PROFILES
# =============================================================================
# Repeated or apparently duplicated profiles can arise when:
#   - The same location is sampled on different dates
#   - Multiple laboratory analyses exist for the same horizon
#   - Multiple depth sequences have been associated with one initial profile
#   - Identifiers are reused across merged surveys
#
# Because ProfID already includes sampling date, true temporal observations are
# separated before this stage.
#
# Resolution order:
#   1. Average duplicated horizons with identical depth intervals
#   2. Resolve competing depth sequences
#   3. Remove newly separated sequences that do not start at the surface
#
# IMPORTANT:
# Basic coordinate and depth validation was already performed on `site` in
# Session 2 and inherited by `site_lab`. Those checks are NOT repeated here.
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1  Check 1: Detect Potential Horizon Duplicates Within Profiles
# -----------------------------------------------------------------------------

profile_analysis <- site_lab %>%
  group_by(ProfID) %>%
  summarise(
    n_horizons = n(),
    n_unique_tops = n_distinct(top),
    n_unique_bottoms = n_distinct(bottom),
    max_depth = max(bottom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # If all horizons have unique top/bottom values,
    # depths are consistent (no repeated intervals)
    consistent = (
      n_unique_tops == n_horizons &
        n_unique_bottoms == n_horizons
    ),
    likely_duplicates = !consistent
  )

# Find profiles with likely duplicates
duplicates <- profile_analysis %>%
  filter(likely_duplicates)

if (nrow(duplicates) > 0) {
  cat(
    " Found", nrow(duplicates),
    "profiles with likely duplicate measurement sequences\n\n"
  )
  print(duplicates)
}

# Select all profiles presenting repeated horizons
duplicates <- site_lab %>%
  filter(ProfID %in% duplicates$ProfID)

# Explore duplicates
duplicates


# -----------------------------------------------------------------------------
# 3.2  Resolution Step 1: Average Duplicated Horizons (Same Depth Intervals)
# -----------------------------------------------------------------------------
# When multiple records share identical ProfID + top + bottom, they represent
# repeated measurements of the same layer.
#
# Average numeric analytical properties and retain the first occurrence of
# identifiers and site metadata.
# -----------------------------------------------------------------------------

site_lab <- site_lab %>%
  group_by(ProfID, top, bottom) %>%
  summarise(
    # Keep identifiers and site metadata as the first value in each group
    across(c(rowID, HorID, date, lon, lat), ~ first(.x)),
    
    # Compute mean for all remaining numeric analytical columns (NA-safe)
    across(
      where(is.numeric) &
        !any_of(c("rowID", "HorID", "lon", "lat", "top", "bottom")),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  select(names(site_lab))  # Restore original column order

site_lab


# -----------------------------------------------------------------------------
# 3.3  Resolution Step 2: Resolve ProfID for Multiple Depth Sequences
# -----------------------------------------------------------------------------
# This step separates records that share an initial ProfID but form different,
# non-continuous depth sequences.
#
# The chain_horizons() function identifies consecutive depth chains and assigns
# a numeric suffix to each sequence.
#
# NOTE:
# Missing/negative depths, zero thickness, invalid depth logic, and the initial
# surface-horizon check were already applied to `site` in Session 2.
# -----------------------------------------------------------------------------

# Create a function to identify sequences of horizons for each profile
chain_horizons <- function(top, bottom) {
  n <- length(top)
  remaining <- seq_len(n)
  chain_id <- integer(n)
  cid <- 1
  
  while (length(remaining) > 0) {
    # Start a new chain at the smallest top depth
    cur <- remaining[which.min(top[remaining])]
    
    repeat {
      chain_id[cur] <- cid
      remaining <- setdiff(remaining, cur)
      
      # Find the next horizon starting where the current horizon ends
      nxt <- remaining[top[remaining] == bottom[cur]]
      
      if (length(nxt) == 0) break
      cur <- nxt[1]
    }
    
    cid <- cid + 1
  }
  
  chain_id
}

site_lab <- site_lab %>%
  group_by(lon, lat, ProfID) %>%
  mutate(chain = chain_horizons(top, bottom)) %>%  # Detect sequences
  arrange(chain, top, .by_group = TRUE) %>%        # Sort within each chain
  mutate(
    ProfID = paste0(ProfID, "_", chain)            # Add numeric suffix
  ) %>%
  ungroup()

if (max(site_lab$chain, na.rm = TRUE) > 1) {
  corrected_profiles <- unique(site_lab$ProfID[site_lab$chain >= 2])
  
  cat(
    "→ Corrected depth continuity in",
    length(corrected_profiles), "profiles\n"
  )
  
  cat(
    "  Corrected Profiles:",
    paste(sub("_2$", "", corrected_profiles), collapse = ", "),
    "\n"
  )
} else {
  cat("→ No depth continuity corrections were needed\n")
}

# Delete the chain column
site_lab <- site_lab %>%
  select(-chain)

# Delete temporary objects
rm(chain_horizons)
if (exists("corrected_profiles")) rm(corrected_profiles)


# -----------------------------------------------------------------------------
# 3.4  Resolution Step 3: Remove Profiles Not Starting at the Surface
# -----------------------------------------------------------------------------
# After competing depth sequences are separated, a new sequence may start below
# 0 cm even though the original ProfID contained a surface horizon.
#
# Therefore the surface check is repeated ONLY for the newly separated profile
# sequences. Keep profiles whose first horizon starts at 0 cm.
# -----------------------------------------------------------------------------

site_lab <- site_lab %>%
  group_by(ProfID) %>%
  filter(min(top, na.rm = TRUE) == 0) %>%
  arrange(ProfID, top, bottom, HorID) %>%
  ungroup()


# -----------------------------------------------------------------------------
# 3.5  Export Cleaned Horizon-Level Dataset
# -----------------------------------------------------------------------------

# Save to CSV
output <- paste0(output_dir, "KSSL_cleaned.csv")
write.csv(site_lab, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir, "KSSL_cleaned.xlsx")
write_xlsx(site_lab, output)


# =============================================================================
# PART 4 — STANDARDIZING DATA FOR DSM
# DSM requires data from one profile per location.
#
# PURPOSE:
# Convert variable-depth horizon data to fixed standard depths
# (0–30 cm and 30–60 cm) for Digital Soil Mapping applications.
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1  Select One Profile per Location (here, Most Complete)
# -----------------------------------------------------------------------------
# When multiple valid profiles exist at the same coordinates, select one using
# an appropriate criterion:
#   - Most complete: most horizons (most depth detail)      <- used here
#   - Best coverage: deepest profile
#   - Best quality: fewest missing values
#   - Monitoring: profile from period of interest
# -----------------------------------------------------------------------------

# Keep the most complete profile at each location
horizons <- site_lab %>%
  group_by(lon, lat, ProfID) %>%
  summarise(
    n_hz = n_distinct(paste(top, bottom)),
    .groups = "drop"
  ) %>%
  group_by(lon, lat) %>%
  dplyr::slice_max(n_hz, n = 1, with_ties = FALSE) %>%
  select(lon, lat, ProfID) %>%
  inner_join(site_lab, by = c("lon", "lat", "ProfID")) %>%
  ungroup()

# Explore horizons
horizons


# -----------------------------------------------------------------------------
# 4.2  Depth Standardization with aqp::slab()
# -----------------------------------------------------------------------------
# Conceptual explanation: What is slab() doing?
#
# PROBLEM: Profiles have different horizon depths
# Profile 1:   0-10 cm (SOC=3.0%)
#             10-25 cm (SOC=2.5%)
#             25-50 cm (SOC=2.0%)
#
# Profile 2:   0-15 cm (SOC=2.8%)
#             15-40 cm (SOC=2.2%)
#             40-100 cm (SOC=1.5%)
#
# GOAL: Obtain values at standard depths (0-30 cm, 30-60 cm)
#
# DSM requires analytical data at the same depth intervals for every profile.
# The slab() function summarizes horizon data over specified depth intervals.
# -----------------------------------------------------------------------------

library(aqp)

# Define standard depth intervals
standard_depths <- c(0, 30, 60)  # 0-30, 30-60 cm

# Select properties to standardize
properties_to_standardize <- c(
  "SOC",
  "Carbon_Total",
  "Bulk.Density_1_3.BAR",
  "Bulk.Density_ovendry",
  "Sand",
  "Silt",
  "Clay",
  "pH",
  "CEC",
  "Nitrogen_Total",
  "Phosphorus_Mehlich3",
  "Phosphorus_Olsen",
  "Potassium",
  "Calcium"
)

# Create SoilProfileCollection object
# aqp needs profile IDs and horizon depth structure
depths(horizons) <- ProfID ~ top + bottom

# Add spatial information
initSpatial(horizons, crs = "EPSG:4326") <- ~ lon + lat


# Visual check of the first profiles ===========================================

# Empty horizons
plotSPC(horizons[1:5])

# Colour horizons by selected soil properties
plotSPC(horizons[1:5], color = "SOC")
plotSPC(horizons[1:5], color = "pH")
plotSPC(horizons[1:5], color = "Clay")

# Plot first 10 profiles over the interval 0-30 cm
clods <- profileApply(horizons[1:10], glom, z1 = 0, z2 = 30)
clods <- combine(clods)

plotSPC(clods, name = "rowID", color = "SOC")
rect(
  xleft = 0.1,
  ybottom = 30,
  xright = length(horizons[1:10]) + 0.5,
  ytop = 0,
  border = "red",
  lty = "dashed"
)

# Density plots of soil parameters
plot(density(horizons$SOC, na.rm = TRUE), main = "Density plot of SOC")
plot(density(horizons$pH, na.rm = TRUE), main = "Density plot of pH")


# Standardize Properties to Fixed Depths ======================================
#
# slab() returns one row per profile, variable, and target depth interval.
# Quantile columns summarize the values contributing to each slab:
#   p.q50 = median
#   p.q5 / p.q95 = 5th / 95th percentiles
#   p.q25 / p.q75 = interquartile range

# Build the standardization formula
fml <- as.formula(
  paste("ProfID ~", paste(properties_to_standardize, collapse = " + "))
)

# View formula
fml

# Apply slab() to standard depths
KSSL_standardized <- slab(
  horizons,
  fml,
  slab.structure = standard_depths,
  na.rm = TRUE
)

# The output is in long format
KSSL_standardized


# -----------------------------------------------------------------------------
# 4.3  Add Percentile-Range Column
# -----------------------------------------------------------------------------
# slab() outputs p.q5, p.q50, and p.q95 among other quantiles.
# Here the 5th–95th percentile range is stored as a compact text field.
# -----------------------------------------------------------------------------

KSSL_standardized <- KSSL_standardized %>%
  mutate(
    CI = paste0(
      round(p.q5, 3),
      "-",
      round(p.q95, 3)
    )
  )

KSSL_standardized


# -----------------------------------------------------------------------------
# 4.4  Reshape from Long to Wide Format
# -----------------------------------------------------------------------------
# DSM models typically require one row per profile and depth interval.
# Transform variable names into columns and retain the median plus the
# 5th–95th percentile range.
# -----------------------------------------------------------------------------

KSSL_standardized <- KSSL_standardized %>%
  pivot_wider(
    id_cols = c(ProfID, top, bottom),
    names_from = variable,
    values_from = c(p.q50, CI),
    names_glue = "{variable}_{.value}"
  )

KSSL_standardized

# Add geographic coordinates back
KSSL_standardized <- KSSL_standardized %>%
  left_join(
    site_lab %>%
      distinct(ProfID, .keep_all = TRUE) %>%
      select(ProfID, lon, lat),
    by = "ProfID"
  ) %>%
  relocate(lon, lat, .after = ProfID)

KSSL_standardized

# Remove the chain suffix added during depth-sequence resolution.
# At this stage, one selected profile is retained per location.
KSSL_standardized$ProfID <- sub("_[0-9]+$", "", KSSL_standardized$ProfID)

# Result: one row per profile-depth interval with standardized soil properties
head(KSSL_standardized)


# -----------------------------------------------------------------------------
# 4.5  Export Standardized Dataset
# -----------------------------------------------------------------------------

# Save to CSV
output <- paste0(output_dir, "KSSL_standardized.csv")
write.csv(KSSL_standardized, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir, "KSSL_standardized.xlsx")
write_xlsx(KSSL_standardized, output)


# -----------------------------------------------------------------------------
# 4.6  Create DSM Subset (0–30 cm, Key Properties)
# -----------------------------------------------------------------------------
# For Digital Soil Mapping: topsoil (0–30 cm), median estimates (p.q50),
# five properties: Clay, Silt, Sand, SOC, and pH.

subset_data <- KSSL_standardized %>%
  filter(top == 0 & bottom == 30) %>%
  select(
    ProfID,
    lon,
    lat,
    top,
    bottom,
    Clay = Clay_p.q50,
    Silt = Silt_p.q50,
    Sand = Sand_p.q50,
    SOC = SOC_p.q50,
    pH = pH_p.q50
  )

subset_data

# Save to CSV
output_csv <- paste0(output_dir, "KSSL_DSM_0-30.csv")
write.csv(subset_data, output_csv, row.names = FALSE)
cat(" Saved to:", output_csv, "\n")

# Save to Excel
output_xlsx <- paste0(output_dir, "KSSL_DSM_0-30.xlsx")
write_xlsx(subset_data, output_xlsx)
cat(" Saved to:", output_xlsx, "\n")

cat(" Subset data ready for Digital Soil Mapping\n")
cat("  Output file: KSSL_DSM_0-30\n")


# =============================================================================
# PART 5 — PREPARING DATA FOR SPECTROSCOPY ANALYSES
# =============================================================================
# PURPOSE:
# Create a clean horizon-level dataset with consistent profile structure,
# corrected analytical parameters, and related spectral information for
# estimation of soil properties by spectroscopy.
#
# WHY THIS MATTERS:
# Clean, depth-consistent site and laboratory data improve the reliability of
# relationships developed with spectral measurements.
#
# Merge the cleaned horizon dataset (`site_lab`) with the MIR spectral data.
# Join key: HorID (site_lab) = smp_id (spec).
# =============================================================================

# Read and subset spectral data
spectral_data <- read_excel(
  paste0(training_dir, "/MIR_KANSAS_data.xlsx"),
  sheet = 1
)

spec <- spectral_data[, -c(1, 3:22)]

# Merge cleaned site + laboratory data with spectral data by sample identifier
site_lab_spec <- left_join(
  site_lab,
  spec,
  by = c("HorID" = "smp_id")
)

# Save to CSV
output <- paste0(output_dir, "KSSL_spectral_cleaned.csv")
write.csv(site_lab_spec, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir, "KSSL_spectral_cleaned.xlsx")
write_xlsx(site_lab_spec, output)

# Remove spectral data object
rm(spec)


###############################################################################
# END OF SESSION 3
#
# Summary of exported files:
#   KSSL_cleaned                    — Validated horizon-level dataset
#   KSSL_spectral_cleaned           — Horizon-level dataset + MIR spectra
#   KSSL_standardized               — Depth-harmonized (0–30 cm; 30–60 cm)
#   KSSL_DSM_0-30                   — DSM-ready topsoil dataset (0–30 cm)
#   soil_property_validation_report — QC report of out-of-range values
#
# Next session: Session 4 — Spatial Analysis + Covariate Preparation
###############################################################################