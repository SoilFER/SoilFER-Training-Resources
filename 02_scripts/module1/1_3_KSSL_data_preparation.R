# ============================================================================
# SOIL DATA VALIDATION AND CLEANING FOR DIGITAL SOIL MAPPING (DSM)
# Training Version - Simplified with detailed explanations
# ============================================================================
# Purpose: Learn how to systematically validate and clean soil laboratory data
# Author: Luis Rodriguez Lado / FAO-GSP SoilFER Project
# Date: 2026
#
# This script demonstrates best practices for soil data quality assurance:
#   1. Loading and exploring raw soil data
#   2. Validating geographic coordinates
#   3. Checking soil depth intervals
#   4. Detecting out-of-range soil property values
#   5. Correcting known data entry errors
#   6. Generating quality reports
#   7. Exporting cleaned data
#
# ============================================================================

# Clear workspace and set options
rm(list = ls())              # Remove all previous objects
options(stringsAsFactors = FALSE)  # Keep character strings as characters


# ============================================================================
# SECTION 0: PREPARE THE SYSTEM
# ============================================================================
# Load packages and set working directory
#
library(tidyverse)  # Data manipulation (dplyr, tidyr, ggplot2)
library(readxl)     # Read Excel files
library(writexl)    # Write Excel files

# Set working directory to script location (recommended)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define folder to store the results
output_dir <-"../../03_outputs/module1/"

# check if output directory exists if not create it
if (!file.exists(output_dir)){
  # create a new sub directory inside the main path
  dir.create(output_dir)
}

# ============================================================================
# SECTION 1: LOAD AND EXPLORE RAW DATA
# ============================================================================
# WHY: Before cleaning, always understand what you're working with
#
# WHAT TO DO:
# - Replace file_path with your actual data location
# - Check column names and data types
# - Inspect the first few rows

# Read Excel file containing raw soil data
raw_data <- read_excel("../../01_data/module1/MIR_KANSAS_data.xlsx", sheet = 1) 

# -----
# EXPLORE: What do we have?
# -----

# Examine the structure and types of the data
str(raw_data)
# Show first 5 rows
head(raw_data, 5)
# Summarize the data in 30 first columns
summary(raw_data[,1:25])


# ============================================================================
# SECTION 2: PREPARE SITE DATA - EXTRACT AND RENAME KEY COLUMNS
# ============================================================================
# WHY: 
# - Start understanding how many valid points you have in the dataset
# - Standardized column names make cleaning easier and less error-prone
#
# The raw daya includes information of:
# - Position and depth (SITE)
# - Analytical values of soil parameters (LAB)
# - Dry Chemistry Data (SPECTRAL)
#
# WHAT TO DO:
# - Extract only the columns you need d
# - Rename to standard names: lon, lat, top, bottom, etc.
# - Create a unique profile identifier
#

# EXTRACT site information (select)

cat("\n=== PREPARE SITE DATA ===\n")

# Add row identifier to track records throughout cleaning process
raw_data <- raw_data %>%
  dplyr::mutate(rowID = dplyr::row_number(), .before = 1)


# CUSTOMIZE: Update column names to match your data (rename)
site <- raw_data %>%
  select(rowID, Long_Site.x, Lat_Site.x, smp_id, 
         Top_depth_cm.x, Bottom_depth_cm.x) %>%
  # Rename to standard column names
  rename(
    lon = Long_Site.x,           # Longitude (WGS84)
    lat = Lat_Site.x,            # Latitude (WGS84)
    HorID = smp_id,              # Horizon identifier
    top = Top_depth_cm.x,        # Upper depth (cm)
    bottom = Bottom_depth_cm.x    # Lower depth (cm)
  )

# Create unique profile ID from coordinates
# WHY: Each unique location = one soil profile
# HOW: Group by coordinates, assign sequential ID, format as "PROF0001"
site <- site %>%
  group_by(lon, lat) %>%
  mutate(ProfID = cur_group_id()) %>%
  ungroup() %>%
  mutate(ProfID = sprintf("PROF%04d", ProfID))

# Reorder columns for readability 
site <- site %>%
  select(rowID, ProfID, HorID, lon, lat, top, bottom)

# Remove exact duplicate rows
site <- site %>%
  distinct(across(-rowID), .keep_all = TRUE)

cat("\n✓ Data prepared. Now have", nrow(site), "unique records\n")

# ============================================================================
# SECTION 3: COORDINATE VALIDATION
# ============================================================================
# WHY: Geographic coordinates define location. Bad coordinates = bad maps
#
# VALIDATION CHECKS:
# 1. Check for missing coordinates (NA values)
# 2. Check for out-of-bounds values
#    - Valid longitude: -180 to +180 degrees
#    - Valid latitude: -90 to +90 degrees
# 3. Attempt to correct obvious position errors
#
# COMMON ERRORS:
# - Missing coordinates (NA)
# - Swapped lon/lat
# - lon/lat too large (e.g., 45000º instead of 45º)
# - Projected coordinates instead of geographic

cat("\n=== COORDINATE VALIDATION ===\n")

# -----
# Check 1: Identify missing coordinates
# -----
missing_coords <- site %>%
  dplyr::slice(which(is.na(lon) | is.na(lat))) %>%
  mutate(issue = case_when(
    is.na(lon) & is.na(lat) ~ "Missing BOTH lon and lat",
    is.na(lon) ~ "Missing lon (longitude)",
    is.na(lat) ~ "Missing lat (latitude)"
  ))

# Remove records with missing coordinates if exist
if (nrow(missing_coords) > 0) {
  cat("⚠ Missing coordinates:", nrow(missing_coords), "records\n")
  print(missing_coords)
  
  # Remove records with missing coordinates
  site <- site %>%
    filter(!is.na(lon) & !is.na(lat))
  
  cat("→ Removed missing coordinates. Remaining:", nrow(site), "records\n\n")
} else {
  cat("✓ No missing coordinates\n\n")
}

# -----
# Check 2: Identify out-of-bounds coordinates
# -----
out_of_bounds <- site %>%
  dplyr::slice(which((lon < -180 | lon > 180) | (lat < -90 | lat > 90))) %>%
  mutate(issue = case_when(
    lon < -180 | lon > 180 ~ paste0("lon out of bounds: ", round(lon, 2)),
    lat < -90 | lat > 90 ~ paste0("lat out of bounds: ", round(lat, 2)),
    TRUE ~ "Invalid coordinates"
  ))

if (nrow(out_of_bounds) > 0) {
  cat("⚠ Out-of-bounds coordinates:", nrow(out_of_bounds), "records\n")
  print(out_of_bounds)
  
  # -----
  # Attempt correction: Divide by 1000 if >180 or >90
  # -----
  # WHY: Common error is decimal/unit confusion (45000 instead of 45)
  # WARNING: This assumes systematic errors. Always verify!
  
  out_of_bounds <- out_of_bounds %>%
    mutate(
      lon_corrected = if_else((lon < -180 | lon > 180), lon / 1000, lon),
      lat_corrected = if_else((lat < -90 | lat > 90), lat / 1000, lat),
      correction_applied = (lon != lon_corrected | lat != lat_corrected)
    )
  
  # Check if corrections worked
  corrected_valid <- out_of_bounds %>%
    filter((lon_corrected >= -180 & lon_corrected <= 180) &
           (lat_corrected >= -90 & lat_corrected <= 90))
  
  if (nrow(corrected_valid) > 0) {
    cat("→ Successfully corrected", nrow(corrected_valid), "records\n")
    cat("  Applied: divided by 1000\n\n")
    
    # Update site data with corrected coordinates
    site <- site %>%
      left_join(
        out_of_bounds %>% 
          select(rowID, lon_corrected, lat_corrected),
        by = "rowID"
      ) %>%
      mutate(
        lon = coalesce(lon_corrected, lon),
        lat = coalesce(lat_corrected, lat)
      ) %>%
      select(-lon_corrected, -lat_corrected)
  }
  
  # Remove records that still have invalid coordinates
  still_invalid <- nrow(out_of_bounds) - nrow(corrected_valid)
  if (still_invalid > 0) {
    cat("→ Removing", still_invalid, "records with still-invalid coordinates\n\n")
    
    site <- site %>%
      filter((lon >= -180 & lon <= 180) &
             (lat >= -90 & lat <= 90))
  }
} else {
  cat("✓ All coordinates within valid geographic bounds\n\n")
}
cat("Remaining after coordinate validation:", nrow(site), "records\n")

# 
rm(missing_coords,out_of_bounds)

# ============================================================================
# SECTION 4: DEPTH INTERVAL VALIDATION
# ============================================================================
# WHY: Soil properties change with depth. Accurate depth intervals are essential
#
# VALIDATION CHECKS:
# 1. Check for missing depth values (top or bottom)
# 2. Check that top < bottom (intervals not reversed)
# 3. Check that profiles start at 0 cm (surface)
#
# COMMON ERRORS:
# - Missing top or bottom depth
# - top >= bottom (reversed interval)
# - Profiles starting at depth (missing topsoil)

cat("\n=== DEPTH INTERVAL VALIDATION ===\n")

# -----
# Check 1: Missing depth values
# -----
missing_depths <- site %>%
  dplyr::slice(which(is.na(top) | is.na(bottom))) %>%
  mutate(issue = case_when(
    is.na(top) & is.na(bottom) ~ "Missing BOTH top and bottom",
    is.na(top) ~ "Missing top (upper) depth",
    is.na(bottom) ~ "Missing bottom (lower) depth"
  ))

if (nrow(missing_depths) > 0) {
  cat("⚠ Missing depth values:", nrow(missing_depths), "records\n")
  
  site <- site %>%
    filter(!is.na(top) & !is.na(bottom))
  
  cat("→ Removed. Remaining:", nrow(site), "records\n\n")

} else {
  cat("✓ No missing depth values\n\n")
}
rm(missing_depths)

# -----
### Check 2: Negative depth values
# Depth values must be non-negative. 
# -----

negative <- site %>%
  filter(top < 0 | bottom < 0)

if (nrow(negative) > 0) {
  cat("Found", nrow(negative), "records with negative depths\n")
  site <- site %>%
    filter(!(top < 0 | bottom < 0))
} else {
  cat("No negative depth values found\n")
  rm(negative)
}

# -----
### Check 3: Zero-thickness intervals
# Horizons with top = bottom have zero thickness and do not represent a measurable soil layer.
# -----
zero_thick <- site %>%
  filter(bottom - top == 0)

if (nrow(zero_thick) > 0) {
  cat("Found", nrow(zero_thick), "zero-thickness horizons\n")
  site <- site %>%
    filter(!(bottom - top == 0))
} else {
  cat("No Zero-thickness intervals found\n")
  rm(zero_thick)
}

# -----
# Check 4: Invalid depth intervals (top >= bottom)
# -----
invalid_logic <- site %>%
  filter(bottom <= top)

if (nrow(invalid_logic) > 0) {
  cat("Found", nrow(invalid_logic), 
      "records with invalid depth logic (bottom <= top)\n\n")
  print(invalid_logic %>% select(ProfID, HorID, top, bottom))
  
  # Attempt correction by swapping values
  invalid_logic <- invalid_logic %>%
    mutate(
      top_corrected = bottom,
      bottom_corrected = top
    )
  
  # Update site data
  site <- site %>%
    left_join(
      invalid_logic %>% select(rowID, top_corrected, bottom_corrected),
      by = "rowID"
    ) %>%
    mutate(
      top = coalesce(top_corrected, top),
      bottom = coalesce(bottom_corrected, bottom)
    ) %>%
    select(-ends_with("_corrected"))
  
  cat("Corrected by swapping depths\n\n")
} else {
  cat("Invalid depth logic not found\n")
  rm(invalid_logic)
}


# -----
# Check 5: Profiles starting at surface (top = 0)
# -----
# WHY: Each profile should represent the complete soil column starting at surface
# This is essential for depth harmonization later

profiles_check <- site %>%
  group_by(ProfID) %>%
  summarise(
    min_top = min(top, na.rm = TRUE),
    max_bottom = max(bottom, na.rm = TRUE),
    n_horizons = n(),
    .groups = "drop"
  )

profiles_no_surface <- profiles_check %>%
  dplyr::slice(which(min_top != 0))

if (nrow(profiles_no_surface) > 0) {
  cat("⚠ Profiles NOT starting at surface (top ≠ 0):", 
      nrow(profiles_no_surface), "profiles\n")
  
  profiles_to_remove <- profiles_no_surface$ProfID
  
  site <- site %>%
    filter(!ProfID %in% profiles_to_remove)
  
  cat("→ Removed", nrow(profiles_no_surface), "incomplete profiles\n")
  cat("→ Remaining:", nrow(site), "records\n\n")
} else {
  cat("✓ All profiles start at surface (top = 0)\n\n")
}

cat("Total profiles remaining:", n_distinct(site$ProfID), "\n")

rm(profiles_check, profiles_no_surface, profiles_to_remove)

# ============================================================================
# SECTION 5: SOIL PROPERTY VALIDATION (WET CHEMISTRY DATA)
# ============================================================================
# WHY: Soil properties have known valid ranges. Values outside these ranges
#      indicate measurement errors, unit mistakes, or data entry errors. All 
#      analytical parameters must be numeric
#
# APPROACH:
# 1. Select lab data properties
# 1. Ensure that all are of numeric type
# 1. Define valid ranges for each soil property
# 2. Find values outside these ranges
# 3. Generate detailed report of issues
# 4. Apply corrections where possible
#
# SOURCES for valid ranges:
# - Adjust valid ranges in relevant scientific literature

cat("\n=== SOIL PROPERTY VALIDATION ===\n")

# Extract laboratory data
lab <- raw_data %>%
  select(
    rowID,
    SOC, Carbon_Total,                                # Soil Organic Carbon and Total Carbon (%)                    
    Bulk.Density_1_3.BAR, Bulk.Density_ovendry,       # Bulk density at 1.3 bar and oven dry (g/cm³)
    Sand, Silt, Clay,                                 # Texture (%)
    pH,                                               # pH H₂O
    CEC,                                              # CEC in cmol(+)/kg
    Nitrogen_Total,                                   # Total nitrogen (%),
    Phosphorus_Mehlich3, Phosphorus_Olsen, Potassium, # Available P (mg/kg), Exchangeable K (cmol(+)/kg)
    Calcium_Carbonate_equivalent                      # CaCO₃ equivalent (%)
  )

# Convert all parameters to numeric (orevent errors in case they were originally stored as text)
lab <- lab %>%
  mutate(across(-rowID, as.numeric))

# Keep only lab records that are present in the cleaned site data
lab <- lab %>%
  filter(rowID %in% site$rowID)

# Join both site and lab data by the common identifier 'rowID'
site_lab <- site %>%
  left_join(lab, by = "rowID")

rm(site)

# -----
# Define valid ranges for soil properties
# -----
# WHY: These thresholds are based on global soil datasets
# CUSTOMIZE: Adjust for your specific region and soil types

property_thresholds <- read_csv("property_thresholds.csv")


# Display thresholds
cat("Property validation thresholds:\n")
print(property_thresholds)

# -----
# Check each property against thresholds
# -----
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

# Remove temporary objects
rm(i,idx,max_val,min_val, prop,prop_desc,x)

# -----
# Generate quality report
# -----
if (length(out_of_bounds_issues) > 0) {
  all_issues <- bind_rows(out_of_bounds_issues)
  
  cat("\n⚠ OUT-OF-BOUNDS SOIL PROPERTIES FOUND\n")
  cat("═════════════════════════════════════════\n\n")
  
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
  
  # Rows with multiple issues (likely data entry errors)
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
    cat("\n⚠ Records with MULTIPLE property issues:\n")
    print(head(rows_with_multiple_issues, 10))
    cat("\nThese records likely have data entry errors and should be reviewed.\n")
  }
  
  # Export detailed report to Excel
  write_xlsx(
    list(
      Summary = issue_summary,
      Issues_by_record = rows_with_multiple_issues,
      All_issues = all_issues
    ),
    paste0(output_dir,"soil_property_validation_report.xlsx")
  )
  
  cat("\n✓ Detailed report saved to: soil_property_validation_report.xlsx\n")
  
  rm(all_issues, issue_summary, rows_with_multiple_issues)
  
} else {
  cat("\n✓ All soil properties within valid ranges!\n")
}


# ============================================================================
# SECTION 6: CORRECTION OF OUT-OF-BOUNDS VALUES
# ============================================================================
# Option 1. Correct them manually after inspection of out_of_bounds_issues
# Use this option if you can ensure the true values for the out-of-bound properties by using recovered from the source of if mistakes are clearly attributable to some identifiable mistake.

# The summary of the critical values provides information on the type of issue
for (property in names(out_of_bounds_issues)){
  #print(data.frame(out_of_bounds_issues[property]))
  cat("Total errors in",property, ":",n_distinct(out_of_bounds_issues[[property]]$rowID), "\n")
  print(summary(data.frame(out_of_bounds_issues[property])[4]))
  #cat("Total errors in",property, ":",n_distinct(out_of_bounds_issues[property]), "\n")
}

# SOC is negative in 43 rows while Phosphorus_Mehlich3 has wrong values in 1 row
# If we ensure that negative SOC are due to a typing mistake, and Phosphorus_Mehlich3 values are due to wrong units (ppb instead of mg/kg), then: 

# Correction: Negative SOC values
negative_soc <- sum(site_lab$SOC < 0, na.rm = TRUE)
if (negative_soc > 0) {
  cat("→ Correcting", negative_soc, "negative SOC values\n")
  cat("  Action: Take absolute value\n")
  site_lab$SOC <- abs(site_lab$SOC)
}

# Correction: Phosphorus Mehlich 3 > 2000 mg/kg (unlikely, likely 1000× error)

extreme_idx <- !is.na(site_lab$Phosphorus_Mehlich3) & site_lab$Phosphorus_Mehlich3 > 2000
extreme_p <- sum(extreme_idx) # Number of Phosphorus_Mehlich3 > 2000 mg/kg

if (extreme_p > 0) {
  cat("→ Correcting", extreme_p, "extremely high Phosphorus values\n")
  cat("  Action: Divided by 1000\n")
  site_lab$Phosphorus_Mehlich3[extreme_idx] <- site_lab$Phosphorus_Mehlich3[extreme_idx] / 1000
}

rm(site.lab_duplicates,extreme_idx, extreme_p, negative_soc)


# Option 2: Set out-of-bounds values to NA (keep the row, just replace the problematic value)

# Loop through each property in the out_of_bounds_issues list
for (property in names(out_of_bounds_issues)) {
  # Get the rowIDs with issues for this property
  rowIDs_with_issues <- out_of_bounds_issues[[property]]$rowID
  
  # Set the property values to NA for these rows
  site_lab <- site_lab %>%
    mutate(
      !!sym(property) := ifelse(rowID %in% rowIDs_with_issues, NA, !!sym(property))
    ) 
  cat("Property", property, ":", 
      length(rowIDs_with_issues), "values set to NA\n")
}



# ============================================================================
# SECTION 6: DATA CORRECTIONS
# ============================================================================
# WHY: Some errors can be corrected automatically. Others require manual review.
#      Lab corrections must account for site identification (join both datasets) 
#
# CORRECTIONS APPLIED:
# 1) Summarize duplicated analyses:
#    - Some horizons have been analysed more than once.
#    - We retain the mean value to avoid duplicate horizons in the dataset.
# 2) Negative SOC → absolute value (likely a sign-entry error)
# 3) Phosphorus > 20000 → divide by 1000 (likely a unit conversion error)
#
# WARNING:
# - Other datasets may contain errors in properties other than SOC or phosphorus.
# - Always check for other potential issues.
# - Only apply automatic corrections when the error is obvious and you are confident.


cat("\n=== APPLYING DATA CORRECTIONS ===\n")


# -----
# Correction 1: Summarize property duplicates in duplicated horizons my mean
# (e.g. PROF0237, PROF0262, PROF0271, PROF0284, PROF0368)
# -----

# Idenfify if property duplicates exist

site.lab_duplicates <- site_lab %>%
  arrange(ProfID, top, bottom, rowID) %>%   # decide what "first" means
  group_by(ProfID, top, bottom) %>%
  filter(row_number() > 1) %>%             # everything after the first is "dropped/merged"
  ungroup()

if (nrow(site.lab_duplicates) > 0) {
  cat("→ Identified", nrow(site.lab_duplicates), "duplicated horizons\n")
  cat("Duplicated horizons in Profiles",unique(site.lab_duplicates$ProfID), "\n")
} else {
  cat("→ No duplicated horizons have been found\n")
}

# Summarise properties by mean in duplicated in horizons
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

# -----
# Correction 4: Detect non-depth-continuous Profile Series)
# -----
# 1. For each profile, check if all rows form ONE continuous depth sequence
# 2. If YES → Single profile (done)
# 3. If NO → Find the consecutive horizons that ARE continuous
# 4. If we find blocks with no gaps → Split the series into subprofiles
#

# Create a funtion to identify continuity in horizons for each profile
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
  mutate(chain = chain_horizons(top, bottom)) %>%      # detect sequences
  arrange(chain, top, .by_group = TRUE) %>%          # sort within each chain
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

rm(corrected_profiles,chain_horizons)

# Some continuity errors may be caused by missing soil depth layers.
# To avoid incomplete profiles, keep only profiles whose first horizon starts at 0.

site_lab <- site_lab %>%
  group_by(ProfID) %>%
  filter(min(top, na.rm = TRUE) == 0) %>%   
  arrange(ProfID, top, bottom, HorID) %>%
  ungroup() %>%
  select(-chain)


cat("\n✓ Corrections applied\n")


# ============================================================================
# SECTION 7: HARMONIZE SOIL PROPERTIES TO STANDARD DEPTHS
# ============================================================================
# PURPOSE: Convert variable-depth horizon data to fixed standard depths
#          (0-30 cm and 30-60 cm) for Digital Soil Mapping applications

# WHY THIS MATTERS:
# - Soil profiles have different horizon thicknesses
# - DSM needs consistent depths for spatial modeling
# - Standard depths (0-30, 30-60) enable comparison across profiles
# - Harmonization uses the weighted average median to estimate values at fixed depths
# - Confidence intervals (p.q5, p.q50, p.q95) show uncertainty in estimates
#
# WHAT YOU'LL DO:
# 1. Load aqp package (Algorithms for Quantitative Pedology)
# 2. Prepare data as SoilProfileCollection object
# 3. Use slab() function to interpolate to standard depths (weighted average)
# 4. Calculate median (p.q50) and confidence intervals
# 5. Pivot results to analysis-ready format
# 6. Add coordinates back for spatial analysis
# 7. Save standardized data for DSM modeling
#
# OUTPUT: Clean data ready for digital soil mapping
# ============================================================================


cat("\n=== HARMONIZE DATA TO STANDARD DEPTHS ===\n")


# Load libraries
library(aqp)

# Standardize Column Names

names(site_lab) <- c(
  "rowID",                           # Unique row identifier
  "ProfID",                          # Profile unique identifier
  "HorID",                           # Horizon unique identifier
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
  "Potassium",                       # Exchangeable K (cmol(+)/kg)
  "Calcium_Carbonate_equivalent"     # CaCO₃ equivalent (%)
)

# Define target properties
target <- names(site_lab)

# ============================================================================
# CONCEPTUAL EXPLANATION: What is slab() Doing?
# ============================================================================
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
# GOAL: Get values at standard depths (0-30 cm, 30-60 cm)
# Profile 1 at 0-30: Need to combine 0-10 + 10-25 + part of 25-50
# Profile 2 at 0-30: Need to combine 0-15 + part of 15-40
#
# SOLUTION: slab() uses weighted averaging to interpolate
# OUTPUT: One value per profile per depth with confidence intervals
# 
# ============================================================================

# -----
# Prepare Data for aqp (as SoilProfileCollection)
# aqp requires profiles with valid depth intervals
#      Removes rows with missing or invalid depths
# WHAT TO DO: Filter to keep only complete, valid profiles
# -----

horizons <- site_lab %>%
  filter(
    !is.na(top) & !is.na(bottom) &    # Both depths present?
      top < bottom                        # Valid interval (top < bottom)?
  )

# Keep deeper profiles at each location to avoid duplicated profiles  
horizons <- horizons %>%
  group_by(lon, lat, ProfID) %>%
  summarise(n_hz = n_distinct(paste(top, bottom)), .groups = "drop") %>%
  group_by(lon, lat) %>%
  dplyr::slice_max(n_hz, n = 1, with_ties = FALSE) %>%
  select(lon, lat, ProfID) %>%
  inner_join(site_lab, by = c("lon", "lat", "ProfID")) %>%
  ungroup()

# remove trailing "_1" from ProfID values
horizons$ProfID <- sub("_1$", "", horizons$ProfID)

cat("Total horizons after cleaning:", nrow(horizons), "\n")
cat("Total profiles:", n_distinct(horizons$ProfID), "\n\n")


# -----
# Create SoilProfileCollection Object
# aqp needs profiles + depth structure for proper interpolation
# -----
depths(horizons) <- ProfID ~ top + bottom

cat("✓ SoilProfileCollection created\n")
cat("  Class:", class(horizons)[1], "\n")
cat("  Profiles:", length(horizons), "\n\n")

# -----
# Add Spatial Information to SoilProfileCollection
#   Links geographic location to soil profiles
#   Enables spatial mapping and analysis later
# WHAT TO DO: Define coordinates and projection

initSpatial(horizons, crs = "EPSG:4326") <- ~ lon + lat

cat("✓ Spatial data added\n","  CRS:", horizons@metadata$crs,"\n")

# -----
# Visual Check of 5 first Profiles in depth
# -----
# Empty plots
plotSPC(horizons[1:5])
# Coloured by SOC
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


# -----
# HARMONIZE PROPERTIES TO STANDARD DEPTHS
# -----

# ============================================================================
# 
# HOW SLAB WORKS:
#
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
# Confidence intervals shows uncertainty in harmonization
#
# ============================================================================

# -----
# Build Formula for All Target Properties
# slab() needs a formula specifying which properties to harmonize
#      Using paste() and as.formula() makes code flexible
# -----
fml <- as.formula(
  paste("ProfID ~", paste(target, collapse = " + "))
)

# -----
# Apply slab() to Interpolate to Standard Depths
# -----

KSSL_standardized <- slab(
  horizons,
  fml,                                # Formula with all properties
  slab.structure = c(0, 30, 60)       # Standard depths: 0-30, 30-60
)

cat("✓ Harmonization complete\n")
cat("  Rows in output:", nrow(KSSL_standardized), "\n")
cat("  Expected:", n_distinct(horizons$ProfID) * 2, "rows")
cat("  Properties included:", n_distinct(target), "parameters")
cat("  (profiles × 2 depth intervals)\n\n")

# -----
# Create Confidence Interval Column
# -----
# WHY: Combines lower and upper bounds for easy interpretation
#      Shows range of uncertainty (p.q5 to p.q95)
# WHAT TO DO: Create CI column as text "lower-upper"

KSSL_standardized <- KSSL_standardized %>%
  mutate(
    # Create 90% confidence interval string (p.q5 to p.q95)
    CI = paste0(
      round(p.q5, 3),                 # Lower bound (5th percentile)
      "-",
      round(p.q95, 3)                 # Upper bound (95th percentile)
    )
  )

cat("✓ Confidence intervals created\n")

# -----
# Pivot to Wide Format for Analysis
# -----
# DSM models expect one row per profile per depth
#      Wide format (columns = properties) is standard for analysis
# WHAT TO DO: Transform from long to wide format
#

KSSL_standardized <- KSSL_standardized %>%
  pivot_wider(
    id_cols = c(ProfID, top, bottom), # Keep these as-is
    names_from = variable,             # Property names become column names
    values_from = c(p.q50, CI),        # Both point estimate and CI
    names_glue = "{variable}_{.value}" # Create names like "SOC_p.q50", "SOC_CI"
  )

cat("✓ Data pivoted to wide format\n")
cat("  Rows:", nrow(KSSL_standardized), "\n")
cat("  Columns:", ncol(KSSL_standardized), "\n")
cat("  Format: One row per profile per depth interval\n\n")

# -----
# Add Geographic Coordinates Back
# -----
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

cat("✓ Coordinates added to each row\n")
cat("  Both depth intervals for each profile have same coordinates\n\n")

# -----
# Review Standardized Output
# -----
#     Verify harmonization worked correctly
#     Check for realistic values and proper structure
# 

# Display first few rows to inspect

print(head(KSSL_standardized, n = 6))


# ============================================================================
# SAVE STANDARDIZED DATA FOR DSM
# ============================================================================
# Save harmonized data for digital soil mapping models
#      CSV format for universal compatibility

# Save to CSV
output_csv <- paste0(output_dir,"KSSL_standardized.csv")
write.csv2(KSSL_standardized, output_csv, row.names = FALSE)
cat("✓ Saved to:", output_csv, "\n")

# Save to Excel
output_xlsx <- paste0(output_dir,"KSSL_standardized.xlsx")
write_xlsx(site_lab, output_xlsx)
cat("✓ Saved to:", output_xlsx, "\n")

cat("✓ Data ready for Digital Soil Mapping\n")
cat("  Output file: KSSL_standardized.csv\n")
cat("  Use this for: covariate modeling, spatial prediction\n\n")

rm(fml)

# ============================================================================
#
# WHAT YOU CAN DO NEXT:
#
# 1. Digital Soil Mapping
#    - Use as training data for soil property models
#    - Predict properties across landscape
#    - Create spatial maps
#
# 2. Further Analysis
#    - Compare properties by depth
#    - Analyze uncertainty
#    - Extract specific depths if needed
#
# 3. Quality Control
#    - Review confidence intervals (narrower = more certain)
#    - Identify unreliable estimates (very wide intervals)
#    - Compare with original measurements
#
# ============================================================================

# ============================================================================
# SECTION 8: EXPORT POINT CLEANED DATA
# ============================================================================
# Save cleaned data for use in further analyses
#
# FORMATS:
# - CSV: Simple, universal format
# - Excel: Better for visual inspection

cat("\n=== EXPORTING CLEANED DATA ===\n")

# remove trailing "_1" from ProfID values
site_lab$ProfID <- sub("_1$", "", site_lab$ProfID)
# check
unique(site_lab$ProfID)

# Save to CSV
output1_csv <- paste0(output_dir,"KSSL_cleaned.csv")
write_csv(site_lab, output1_csv)
cat("✓ Saved to:", output1_csv, "\n")

# Save to Excel
output1_xlsx <- paste0(output_dir,"KSSL_cleaned.xlsx")
write_xlsx(site_lab, output1_xlsx)
cat("✓ Saved to:", output1_xlsx, "\n")



# ============================================================================
# SECTION 9: MERGE SITE LAB AND SPECTRAL DATA
# ============================================================================
# PURPOSE:  Create a clean dataset with unique soil profiles, consistent horizon
#           depths, corrected analytical parameters and related spectral information
#           for estimation of soil properties by spectroscopy

# WHY THIS MATTERS:
# - Clean and depth consistent depths data ensures better parameter estimation
#   using spectral data. 

# Store Spectral data
spec <- raw_data[,-c(1,2,4:22)]

# Merge site_lab data to the original Spectral data by common ID
site_lab_spec <- full_join (site_lab,spec, by=c("HorID"="smp_id") )
cat("✓ Spectral Data added to site and wet chemistry data\n")

# Save to CSV
output2_csv <- paste0(output_dir,"KSSL_spectral_cleaned.csv")
write_csv(site_lab_spec, output2_csv)
cat("✓ Saved to:", output2_csv, "\n")

# Save to Excel
output2_xlsx <- paste0(output_dir,"KSSL_spectral_cleaned.xlsx")
write_xlsx(site_lab_spec, output2_xlsx)
cat("✓ Saved to:", output2_xlsx, "\n")

rm(spec)

# ============================================================================
# FINAL SUMMARY REPORT
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("DATA CLEANING SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Initial records:        ", nrow(raw_data), "\n")
cat("Final records:          ", nrow(site_lab), "\n")
cat("Records removed:        ", nrow(raw_data) - nrow(site_lab), 
    " (", round(100*(nrow(raw_data) - nrow(site_lab))/nrow(raw_data), 1), "%)", "\n")
cat("Final profiles:         ", n_distinct(site_lab$ProfID), "\n\n")

cat("Output files:\n")
cat("  1.", output_csv, "\n")
cat("  2.", output_xlsx, "\n\n")
cat("  4.", output1_csv, "\n")
cat("  5.", output1_xlsx, "\n")
cat("  6.", output2_csv, "\n")
cat("  7.", output2_xlsx, "\n")
if (length(out_of_bounds_issues) > 0) {
  cat("  8. soil_property_validation_report.xlsx\n\n")
}
cat("═══════════════════════════════════════════════════════════════\n")

# ============================================================================
# END OF SCRIPT
# ============================================================================

