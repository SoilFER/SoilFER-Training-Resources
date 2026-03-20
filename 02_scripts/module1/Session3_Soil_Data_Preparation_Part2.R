###############################################################################
# SoilFER Online Training Programme — Module 1
# SESSION 3: Data Preparation with the KSSL Dataset — Part 2 (1.5 hours)
# Sections: Lab data, Duplicates, Harmonization, Spectroscopy prep, Export
###############################################################################
#
# LEARNING OBJECTIVES
# -------------------
# By the end of this session, participants will be able to:
#   1. Extract and prepare laboratory (wet chemistry) columns
#   2. Validate analytical values against feasible thresholds
#   3. Report and document out-of-bounds issues
#   4. Validate texture data (particle-size fractions)
#   5. Apply targeted corrections or NA replacement for erroneous values
#   6. Detect and resolve duplicate soil profiles
#   7. Harmonize data to standard depth intervals (0–30, 30–60 cm)
#   8. Export cleaned and standardized datasets for DSM and spectroscopy
#
# PREREQUISITE
# ------------
# This session continues directly from Session 2. The 'site' object
# produced in Session 2 must be available in your R environment, OR
# you must reload the raw data and re-run Session 2 cleaning steps
# before executing this script.
#
# TIMING GUIDE (approximate)
# ---------------------------
#  0:00 – 0:25  Lab data extraction, type conversion, joining to site data
#               Threshold-based out-of-bounds validation and reporting
#  0:25 – 0:45  Texture validation; targeted correction and NA replacement
#  0:45 – 1:10  Duplicate detection and resolution (average horizons,
#               chain_horizons function, surface coverage check)
#  1:10 – 1:30  Depth standardization with aqp::slab(); wide-format output;
#               DSM subset; spectroscopy merge; export
###############################################################################


# =============================================================================
# PART 1 — PREPARING LAB DATA
# =============================================================================
# NOTE: 'raw_data' and 'site' (from Session 2) must already be in environment.

library(readxl)           # Read Excel files
library(tidyverse)        # Data manipulation and visualization
library(writexl)          # Write Excel files

# Define the folder to store the results of the exercise
output_dir <-"03_outputs/module1/"

# Define the relative path to the folder with the MIR data
training_dir <-"01_data/module1/training_data"

# Read Excel file containing raw soil data
raw_data <- read_excel("01_data/module1/kssl/KSSL_data.xlsx", sheet = 1) 

# Remove records with missing coordinates
raw_data <- raw_data %>%
  dplyr::filter(!is.na(Long_Site) & !is.na(Lat_Site))

# Add Row and Profile Identifiers
site_lab <- raw_data %>%
  mutate(rowID = row_number(), .before = 1) %>%
  # Group all horizons at the same location
  group_by(Long_Site, Lat_Site) %>%
  mutate(HorID = smp_id, .before = 2) %>%
  # Assign sequential ID to each unique Profile location (cur_group_id() returns group number)
  mutate(ProfID = cur_group_id(), .before = 3) %>%
  ungroup() %>%
  # Format as standardized IDs: PROF0001, PROF0002, etc. with 4 digit resolution
  mutate(ProfID = sprintf("PROF%04d", ProfID))

# Read the CSV file containing site data from the previous session
#site <- read_csv("03_outputs/module1/site_KSSL.csv")

# -----------------------------------------------------------------------------
# 1.1  Extract and Standardize Laboratory Columns
# NOTE: All analytical parameters must be numeric
# APPROACH:
# 1. Load analytical data from the original raw file
# 2. Convert analytical parameters to numeric
# 3. Filter records to match with the site data generate in the previous session
# 4. Merge site (positional) and laboratory analytical data
# -----------------------------------------------------------------------------

# Extract laboratory columns with standardized names
site_lab <- site_lab %>%
    select(
      rowID, ProfID, HorID, Lat_Site ,Long_Site, Top_depth_cm, Bottom_depth_cm,
      `Estimated Organic Carbon`, `Carbon, Total`,                                     # Soil Organic Carbon and Total Carbon (%)                    
      `Bulk Density, <2mm Fraction, 1/3 Bar`, `Bulk Density, <2mm Fraction, Ovendry`,  # Bulk density at 1.3 bar and oven dry (g/cm³)
      `Sand, Total`, `Silt, Total`, `Clay`,                                            # Texture (%)
      `pH, 1:1 Soil-Water Suspension`,                                                 # pH H2O
      `CEC, NH4OAc, pH 7.0, 2M KCl displacement`,                                      # CEC in cmol(+)/kg
      `Nitrogen, Total`,                                                               # Total nitrogen (%),
      `Phosphorus, Mehlich3 Extractable`, `Phosphorus, Olsen Extractable`,             # Available P (mg/kg)
      `Potassium, NH4OAc Extractable, 2M KCl displacement`,                            # Extractable K (cmol(+)/kg)
      `Calcium, NH4OAc Extractable, 2M KCl displacement`                               # Extractable Ca (cmol(+)/kg)
    )
  site_lab

  # Ensure numeric type for all analytical parameters (prevents issues if stored as text)
  site_lab <- site_lab %>%
    mutate(across(-c(rowID,ProfID, HorID), as.numeric)) 
  site_lab 
  
  
  # Rename columns in site_lab
  names(site_lab) <-
  c("rowID", "ProfID", "HorID",      # Unique row and profile identifier
  "lon", "lat",                      # Coordinates (WGS84)
  "top", "bottom",                   # Depth boundaries (cm)
  "SOC",                             # Soil Organic Carbon (%)
  "Carbon_Total",                    # Total carbon (%)
  "Bulk.Density_1_3.BAR",            # BD at 1.3 bar (g/cm³)
  "Bulk.Density_ovendry",            # BD oven dry (g/cm³)
  "Sand",                            # Sand content (%)
  "Silt",                            # Silt content (%)
  "Clay",                            # Clay content (%)
  "pH",                              # Soil pH (H₂O)
  "CEC",                             # Cation exchange capacity (cmol(+)/kg)
  "Nitrogen_Total",                  # Total nitrogen (%)
  "Phosphorus_Mehlich3",             # Available P (mg/kg)
  "Phosphorus_Olsen",                # Available P (mg/kg)
  "Potassium",                       # Extractable K (cmol(+)/kg)
  "Calcium")                         # Extractable Ca (cmol(+)/kg)
  
  
# =============================================================================
# PART 2 — LABORATORY DATA VALIDATION
# WHY: Soil properties have known valid ranges. Values outside these ranges
#      indicate measurement errors, unit mistakes, or data entry errors. All 
#      analytical parameters must be numeric
#
# APPROACH:
# 1. Load thresholds for analytical soil properties
# 2. Find values outside these thresholds
# 3. Generate detailed report of issues
# 4. Apply corrections where possible
#
# SOURCES for valid ranges:
# NOTE: The analytical thresholds used in this tutorial are based on global soil datasets 
# 	and literature and using the same measurement units as the KSSL dataset
#       Adjust for your specific region and soil types and units.
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1  Check 1: Load Property Thresholds and Identify Out-of-Bounds Values
# -----------------------------------------------------------------------------
# Analytical thresholds are stored in a CSV file for transparency.

# Load thresholds for analytical soil properties
property_thresholds <- read_csv("01_data/module1/kssl/property_thresholds.csv")
property_thresholds

# Identify out-of-bounds values, create a list to populate out-of-bounds issues
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

# Remove temporary objects
rm(i,idx,max_val,min_val, prop,prop_desc,x)

# -----------------------------------------------------------------------------
# 2.2  Reporting Out-of-Bounds Values and Creating an Audit Trail
# -----------------------------------------------------------------------------
# Export a QC report for review and documentation before correcting data.

# Report out-of-bounds if present
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
    paste0(output_dir,"soil_property_validation_report.xlsx")
  )
  
  cat("\n Detailed report saved to: 03_outputs/module1/soil_property_validation_report.xlsx\n")
  
  rm(all_issues, issue_summary, rows_with_multiple_issues)
  
} else {
  cat("\n All soil properties within valid ranges!\n")
}

# -----------------------------------------------------------------------------
# 2.3  Check 2: Texture Validation
# -----------------------------------------------------------------------------
# Particle-size fractions (Clay + Silt + Sand) should sum to ~100%.
# Values failing this check are flagged for review — NOT automatically removed.

# Print rows with texture validation inconsistencies 
texture_problems <- site_lab %>%
  mutate(
    texture_sum = Clay + Silt + Sand,
    texture_valid = abs(texture_sum - 100) < 2
  )
# Select only samples with texture issues
texture_problems <- texture_problems %>%
  filter(!texture_valid)

# View texture issues
if (nrow(texture_problems) > 0) {
  cat(" Found", nrow(texture_problems),
      "records with invalid texture sums\n\n")
  print(texture_problems %>%
          select(rowID, ProfID, Clay, Silt, Sand, texture_sum))
  # Flag for review (do not automatically remove)
} else {
  cat(" Texture problems not found")
}

# -----------------------------------------------------------------------------
# 2.4  Check 3: Correction of Out-of-Bounds Laboratory Values
# -----------------------------------------------------------------------------
# Two options:
#   Option 1 (preferred): Targeted correction when error mechanism is known
# 		Use this option if you can ensure the true values for the out-of-bound properties recovered from the source of if
# 		mistakes are clearly attributable to some identifiable mistake.
#		In this database SOC is negative in 43 rows while Phosphorus_Mehlich3 has wrong values in 1 row
#		If we ensure that negative SOC are due to typing mistakes, and Phosphorus_Mehlich3 values are due to wrong units
#		(ppb instead of mg/kg), we can use Option 1 for correction. 

#   Option 2: Replace suspect values with NA (keep the row, just replace the problematic values)

# WARNING: =====================================================================
# - Other datasets may contain errors in properties other than SOC or Phosphorus.
# - Always check for other potential issues.
# - Only apply automatic corrections when the error is obvious and you are confident.


# --- Option 1: Targeted corrections (when error mechanism is known) ---

# Inspect the nature of each issue before correcting:

for (property in names(out_of_bounds_issues)){
  cat("Total errors in",property, ":",n_distinct(out_of_bounds_issues[[property]]$rowID), "\n")
  print(summary(data.frame(out_of_bounds_issues[property])[4]))
}

# Correction: Phosphorus Mehlich 3 > 2000 mg/kg (likely 1000× error - ppb instead of ppm)
idx <- !is.na(site_lab$Phosphorus_Mehlich3) & site_lab$Phosphorus_Mehlich3 > 2000
# idx <- !is.na(site_lab$Phosphorus_Mehlich3) & site_lab$Phosphorus_Mehlich3 > property_thresholds[property_thresholds$property=="Phosphorus_Mehlich3","max_valid"][[1]]
n_idx <- sum(idx)
if (n_idx > 0) site_lab$Phosphorus_Mehlich3[idx] <- site_lab$Phosphorus_Mehlich3[idx] / 1000
# Remove temporary objects
rm(idx, n_idx)


# --- Option 2: Replace out-of-bounds values with NA ---
# Use when true value cannot be reliably reconstructed.

# Loop through each property in the out_of_bounds_issues list
for (property in names(out_of_bounds_issues)) {
  # Get the rowIDs with issues for this property
  rowIDs_with_issues <- unique(out_of_bounds_issues[[property]]$rowID)
  # Change the values of the property in those rows to NA
  site_lab <- site_lab %>%
    dplyr::mutate(
      "{property}" := dplyr::if_else(rowID %in% rowIDs_with_issues,
                                     as.numeric(NA), .data[[property]])
    )
}


# =============================================================================
# PART 3 — RESOLVING DUPLICATED DATA IN SOIL PROFILES
# =============================================================================
# Duplicate profiles arise when:
#   - The same location is re-sampled (temporal duplicates)
#   - Multiple lab analyses of the same sample (analytical replicates)
#   - Identifier re-use across merged surveys
#
# Resolution order:
#   1. Average duplicated horizons (same depth intervals)
#   2. Resolve competing depth sequences (different depths)
#   3. Remove incomplete profiles (no surface horizon)

# -----------------------------------------------------------------------------
# 3.1  Check 1: Detect Potential Horizon Duplicates Within Profiles
# -----------------------------------------------------------------------------

### Detect potential horizon duplicates within profiles
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
    # depths are consistent (no duplicates)
    consistent = (n_unique_tops == n_horizons & n_unique_bottoms == n_horizons),
    likely_duplicates = !consistent
  )

# Find profiles with likely duplicates
duplicates <- profile_analysis %>%
  filter(likely_duplicates)
# View duplicates
if (nrow(duplicates) > 0) {
  cat(" Found", nrow(duplicates), 
      "profiles with likely duplicates measurement sequences\n\n")
  print(duplicates)
}

# Select all profiles presenting duplicate horizons
duplicates <- site_lab %>%
  filter(ProfID %in% duplicates$ProfID)
# Explore duplicates
duplicates

# -----------------------------------------------------------------------------
# 3.2  Resolution Step 1: Average Duplicated Horizons (Same Depth Intervals)
# -----------------------------------------------------------------------------
# When multiple records share identical ProfID + top + bottom, they represent
# repeated measurements of the same layer. Average numeric properties and
# keep the first occurrence of identifiers.

# -----
# Correction 1: Summarize property in duplicated horizons my mean
# (e.g. PROF0237, PROF0262, PROF0271, PROF0284, PROF0368)
# -----
site_lab <- site_lab %>%
  group_by(ProfID, top, bottom) %>%
  summarise(
    # keep identifiers as the first value in each group
    across(c(rowID, HorID, lon, lat), ~ first(.x)),
    
    # compute mean for all other numeric columns (NA-safe)
    across(
      where(is.numeric) & !any_of(c("rowID","HorID","lon","lat","top","bottom")),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  select(names(site_lab))   # <- restores original column order
site_lab



# -----------------------------------------------------------------------------
# 3.3  Resolution Step 2: Resolve ProfID for Multiple Depth Sequences
# -----------------------------------------------------------------------------
# This step separates profiles that contain different, non-continuous depth
# sequences (Case B duplicates). The chain_horizons() function identifies
# consecutive depth chains and assigns a suffix to each.

# Detect ProfID series with different top-bottom depth sequences
# 1. For each profile, check if all rows form ONE continuous depth sequence
# 2. If YES → Single profile (done)
# 3. If NO → Find the consecutive horizons that ARE continuous
# 4. If we find blocks with no gaps → Split the series into subprofiles

# 3.3.1  Check 1: Missing Depth Boundaries
# -----------------------------------------------------------------------------

# Keep records where `top` or `bottom` are not NA
site_lab <- site_lab %>%
  dplyr::filter(!is.na(top) & !is.na(bottom))

# Keep records where `top` or `bottom` are positive
site_lab <- site_lab %>%
  filter(!(top < 0 | bottom < 0))

# Remove zero-thickness horizons
site_lab <- site_lab %>%
  filter(!(bottom - top == 0))

# Remove invalid depth logic
site_lab <- site_lab %>%
  filter(bottom > top)

## Keep only profiles that start at the surface (min top == 0)
site_lab <- site_lab %>%
  dplyr::group_by(ProfID) %>%
  dplyr::filter(!is.na(top) & min(top, na.rm = TRUE) == 0) %>%
  dplyr::ungroup()

# Create a function to identify sequences of horizons for each profile
chain_horizons <- function(top, bottom) {
  n <- length(top)
  remaining <- seq_len(n)
  chain_id <- integer(n)
  cid <- 1
  
  while(length(remaining) > 0) {
    # start new chain at smallest top
    cur <- remaining[which.min(top[remaining])]
    repeat {
      chain_id[cur] <- cid
      remaining <- setdiff(remaining, cur)
      nxt <- remaining[top[remaining] == bottom[cur]]
      if(length(nxt) == 0) break
      cur <- nxt[1]
    }
    cid <- cid + 1
  }
  chain_id
}


site_lab <- site_lab %>%
  group_by(lon, lat, ProfID) %>%
  mutate(chain = chain_horizons(top, bottom)) %>%   # detect sequences
  arrange(chain, top, .by_group = TRUE) %>%         # sort within each chain
  mutate(
    ProfID = paste0(ProfID, "_", chain)             # add a numeric suffix
  ) %>%
  ungroup()


if (max(site_lab$chain, na.rm = TRUE) > 1) {
  corrected_profiles <- unique(site_lab$ProfID[site_lab$chain >= 2])
  cat("→ Corrected depth continuity in", length(corrected_profiles), "profiles\n")
  cat("  Corrected Profiles:", paste(sub("_2$", "", corrected_profiles), collapse = ", "), "\n")
} else {
  cat("→ No depth continuity corrections were needed\n")
}

# Delete the chain column
site_lab <- site_lab %>%
  select(-chain)

# Delete temporary objects
rm(corrected_profiles,chain_horizons)

# -----------------------------------------------------------------------------
# 3.4  Resolution Step 3: Remove Profiles Not Starting at the Surface
# Some depth continuity errors may be caused by missing soil depth layers.
# To avoid incomplete profiles, keep only profiles whose first horizon starts at 0.
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
output <- paste0(output_dir,"KSSL_cleaned.csv")
write.csv(site_lab, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir,"KSSL_cleaned.xlsx")
write_xlsx(site_lab, output)


# =============================================================================
# PART 4 — STANDARDIZING DATA FOR DSM
# DSM requires data from one profile per location
# PURPOSE: Convert variable-depth horizon data to fixed standard depths
#          (0-30 cm and 30-60 cm) for Digital Soil Mapping applications
#
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1  Select One Profile per Location (here, Most Complete)
# -----------------------------------------------------------------------------

# When multiple valid profiles exist at the same coordinates, select one using the appropriate criterion:
#   - Most complete: most horizons (most depth detail)      ← used here
#   - Best coverage: deepest profile
#   - Best quality:  fewest missing values
#   - Monitoring:    profile from period of interest

# Create a new object to store DSM harmonized data

# Keep most complete profiles at each location to avoid duplicated profiles  
horizons <- site_lab %>%
  group_by(lon, lat, ProfID) %>%
  summarise(n_hz = n_distinct(paste(top, bottom)), .groups = "drop") %>%
  group_by(lon, lat) %>%
  dplyr::slice_max(n_hz, n = 1, with_ties = FALSE) %>%
  select(lon, lat, ProfID) %>%
  inner_join(site_lab, by = c("lon", "lat", "ProfID")) %>%
  ungroup()
#Explore horizons
horizons

# -----------------------------------------------------------------------------
# 4.2  Depth Standardization with aqp::slab()
# Conceptual explanation: What is slab() Doing?
  # PROBLEM: Profiles have different horizon depths
  # Profile 1:   0-10 cm (SOC=3.0%)
  #             10-25 cm (SOC=2.5%)
  #             25-50 cm (SOC=2.0%)
  #
  # Profile 2:   0-15 cm (SOC=2.8%)
  #             15-40 cm (SOC=2.2%)
  #             40-100 cm (SOC=1.5%)
  #
  # GOAL: Get values at standard depths (0-30 cm, 30-60 cm)
  # Profile 1 at 0-30: Need to combine 0-10 + 10-25 + part of 25-50
  # Profile 2 at 0-30: Need to combine 0-15 + part of 15-40
  #
  # SOLUTION: slab() uses weighted averaging to interpolate
  # OUTPUT: One value per profile per depth with confidence intervals
  # 
# -----------------------------------------------------------------------------
# DSM requires analytical data at standardized depth intervals (same depths
# for every profile). Standard depths: 0–30 cm, 30–60 cm.
# The slab() function uses depth-weighted averaging for standardization.

library(aqp)

# Define standard depth intervals
standard_depths <- c(0, 30, 60)  # 0-30, 30-60 cm

# Description of the columns in site_lab
#   "rowID",                           # Unique row identifier
#   "ProfID",                          # Profile unique identifier
#   "HorID",                           # Horizon unique identifier
#   "lon", "lat",                      # Coordinates (WGS84)
#   "top", "bottom",                   # Depth boundaries (cm)
#   "SOC",                             # Soil Organic Carbon (%)
#   "Carbon_Total",                    # Total carbon (%)
#   "Bulk.Density_1_3.BAR",            # BD at 1.3 bar (g/cm³)
#   "Bulk.Density_ovendry",            # BD oven dry (g/cm³)
#   "Sand",                            # Sand content (%)
#   "Silt",                            # Silt content (%)
#   "Clay",                            # Clay content (%)
#   "pH",                              # Soil pH (H₂O)
#   "CEC",                             # Cation exchange capacity (cmol(+)/kg)
#   "Nitrogen_Total",                  # Total nitrogen (%)
#   "Phosphorus_Mehlich3",             # Available P (mg/kg)
#   "Phosphorus_Olsen",                # Available P (mg/kg)
#   "Potassium",                       # Exchangeable K (cmol(+)/kg)
#   "Calcium"                          # Extractable Ca (cmol(+)/kg)

# Select properties to standardize
properties_to_standardize <- c("SOC","Carbon_Total","Bulk.Density_1_3.BAR","Bulk.Density_ovendry","Sand","Silt","Clay",
                               "pH","CEC","Nitrogen_Total","Phosphorus_Mehlich3","Phosphorus_Olsen","Potassium","Calcium")

# Prepare data for aqp

# Create SoilProfileCollection Object
# aqp needs profiles + depth structure for proper interpolation

depths(horizons) <- ProfID ~ top + bottom

# Add Spatial Information to SoilProfileCollection
#   Links geographic location to soil profiles

initSpatial(horizons, crs = "EPSG:4326") <- ~ lon + lat

# Visual Check of 5 first Profiles in depth ==================================

  # Empty horizons
  plotSPC(horizons[1:5])
  # Coloured horizons by SOC
  plotSPC(horizons[1:5], color = 'SOC')
  plotSPC(horizons[1:5], color = 'pH')
  plotSPC(horizons[1:5], color = 'Clay')

  # Plot first 10 profiles to the interval 0-30cm
  clods <- profileApply(horizons[1:10], glom, z1 = 0, z2 = 30)
  clods <- combine(clods)
  # check
  plotSPC(clods, name='rowID', color='SOC')
  rect(xleft = 0.1, ybottom = 30, xright = length(horizons[1:10]) + 0.5, ytop = 0, border="red",lty="dashed")

  # Density plots of soil parameters
  plot(density(horizons$SOC, na.rm=TRUE), main="Density plot of SOC")
  plot(density(horizons$pH, na.rm=TRUE), main="Density plot of pH")


# Standardize Properties to Fixed Depths 
  # HOW SLAB WORKS:
  # Input: Profiles with variable-depth horizons
  # Process: For each standard depth interval (0-30, 30-60):
  #   1. Find all horizons that overlap with the interval 
  #   2. Weight values by overlap proportion
  #   3. Calculate percentiles (5%, 25%, 50%, 75%, 95%)
  #
  # Output: One row per profile per depth with:
  #   - p.q50 = median value (point estimate)
  #   - p.q5, p.q95 = 90% confidence interval bounds
  #   - p.q25, p.q75 = interquartile range (middle 50%)
  #
  # p.q50 is the empirical median, not an interpolated value
  # Represents actual weighted average of overlapping horizons
  # Confidence intervals shows uncertainty in standardization

  # Build the standardization formula  
  # slab() needs a formula specifying which properties to standardize
  #      Using paste() and as.formula() makes code flexible

fml <- as.formula(
  paste("ProfID ~", paste(properties_to_standardize, collapse = " + "))
)
# View formula
fml

# Apply slab() to interpolate to standard depths
KSSL_standardized <- slab(
  horizons,
  fml,
  slab.structure = standard_depths,  # Target standard depths
  na.rm = TRUE                        # Ignore NA values in calculations
)
# The output is in Long Format
KSSL_standardized 
# -----------------------------------------------------------------------------
# 4.3  Add Confidence Interval Column
# -----------------------------------------------------------------------------
# slab() outputs: p.q5 (5th pct), p.q50 (median), p.q95 (95th pct)
# The 5th–95th range is a 90% confidence interval.

# Create Confidence Interval of the estimations (CI column)
# Shows range of uncertainty (p.q5 to p.q95)

KSSL_standardized <- KSSL_standardized %>%
  mutate(
    # Create 90% confidence interval string (p.q5 to p.q95)
    CI = paste0(
      round(p.q5, 3),                 # Lower bound (5th percentile)
      "-",
      round(p.q95, 3)                 # Upper bound (95th percentile)
    )
  )
# The output is still in Long Format, only added the CI column
KSSL_standardized

# -----------------------------------------------------------------------------
# 4.4  Reshape from Long to Wide Format
    # DSM models expect one row per profile per depth
    # Wide format (columns = properties) is standard for analysis
    # WHAT TO DO: Transform from long to wide format
# -----------------------------------------------------------------------------

# Convert from long to wide format
KSSL_standardized <- KSSL_standardized %>%
  pivot_wider(
    id_cols = c(ProfID, top, bottom), # Keep these as-is
    names_from = variable,             # Property names become column names
    values_from = c(p.q50, CI),        # Both point estimate and CI
    names_glue = "{variable}_{.value}" # Create names like "SOC_p.q50", "SOC_CI"
  )
# The output is now in Wide Format
KSSL_standardized

# Add geographic coordinates back
  # WHY: Spatial data needed for mapping and spatial analysis
  #      Links each depth interval to sample location
  # WHAT TO DO: Join coordinates from original data
  #
  # NOTE: One coordinate pair per profile (same for both depth intervals)
  #       We use distinct() to keep only one location per profile

KSSL_standardized <- KSSL_standardized %>%
  # Get coordinates from original data (one per profile)
  left_join(
    site_lab %>%
      distinct(ProfID, .keep_all = TRUE) %>%  # One row per profile
      select(ProfID, lon, lat),
    by = "ProfID"
  ) %>%
  # Move coordinates to front for readability
  relocate(lon, lat, .after = ProfID)
# Registers have now coordinates
KSSL_standardized

# Since ProfIDs are now unique at each location, remove tailings ProfID values
KSSL_standardized$ProfID <- sub("_[12]$", "", KSSL_standardized$ProfID)

# Result: One row per profile-depth, with standardized soil properties
  #     Verify standardization worked correctly
  #     Check for realistic values and proper structure
head(KSSL_standardized)

# -----------------------------------------------------------------------------
# 4.5  Export Standardized Dataset
# -----------------------------------------------------------------------------

# Save to CSV
output <- paste0(output_dir,"KSSL_standardized.csv")
write.csv(KSSL_standardized, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir,"KSSL_standardized.xlsx")
write_xlsx(KSSL_standardized, output)

# -----------------------------------------------------------------------------
# 4.6  Create DSM Subset (0–30 cm, Key Properties)
# -----------------------------------------------------------------------------
# For Digital Soil Mapping: topsoil (0–30 cm), median estimates (p50),
# five properties: Clay, Silt, Sand, SOC, pH.

# Keep only 0-30 cm depth and select relevant columns (Clay, Silt, Sand, SOC & pH)
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
# Only mean values for Clay, Silt, Sand, SOC and pH for layer 0-30
subset_data

# Save to CSV
output_csv <- paste0(output_dir,"KSSL_DSM_0-30.csv")
write.csv(subset_data, output_csv, row.names = FALSE)
cat(" Saved to:", output_csv, "\n")

# Save to Excel
output_xlsx <- paste0(output_dir,"KSSL_DSM_0-30.xlsx")
write_xlsx(subset_data, output_xlsx)
cat(" Saved to:", output_xlsx, "\n")

cat(" Subset data ready for Digital Soil Mapping\n")
cat("  Output file: KSSL_DSM_0-30\n")


# =============================================================================
# PART 5 — PREPARING DATA FOR SPECTROSCOPY ANALYSES
  # PURPOSE:  Create a clean dataset with unique soil profiles, consistent horizon
  #           depths, corrected analytical parameters and related spectral information
  #           for estimation of soil properties by spectroscopy
  #
  # WHY THIS MATTERS:
  # - Clean and depth consistent depths data ensures better parameter estimation
  #   using spectral data. 

# =============================================================================
# Merge the cleaned horizon dataset (site_lab) with the spectral data
# (MIR) from the original raw file. Join key: HorID (site_lab) = smp_id (spec).

# Read and subset spectral data from the original dataset
raw_data <- read_excel(paste0(training_dir,"MIR_KANSAS_data.xlsx"), sheet = 1) 
spec <- raw_data[,-c(1,3:22)]

# Merge site_lab data to the original Spectral data by their common IDs
site_lab_spec <- left_join (site_lab,spec, by=c("HorID"="smp_id") )

# Save to CSV
output <- paste0(output_dir,"KSSL_spectral_cleaned.csv")
write.csv(site_lab_spec, output, row.names = FALSE)

# Save to Excel
output <- paste0(output_dir,"KSSL_spectral_cleaned.xlsx")
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
