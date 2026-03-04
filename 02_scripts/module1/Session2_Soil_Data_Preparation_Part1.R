###############################################################################
# SoilFER Online Training Programme — Module 1
# SESSION 2: Data Preparation with the KSSL Dataset — Part 1 (1.5 hours)
# Section: Working with Soil Data, Loading, Site data, Coordinates, Depths
###############################################################################
#
# LEARNING OBJECTIVES
# -------------------
# By the end of this session, participants will be able to:
#   1. Set the system and organize project files
#   2. Load R packages for soil data analysis
#   3. Import soil data from CSV and Excel files
#   4. Explore and understand the structure of a real soil database
#   5. Add unique row identifiers for data traceability
#   6. Extract, standardize, and rename site columns
#   7. Create unique profile identifiers from geographic coordinates
#   8. Remove exact duplicate records
#   9. Validate and filter geographic coordinates
#  10. Validate and correct soil depth boundaries
#
# DATASET
# -------
# KSSL (Kellogg Soil Survey Laboratory) Kansas dataset
#   - File: 01_data/module1/kssl/KSSL_data.xlsx  (or KSSL_data.csv)
#   - 10,352 soil horizon measurements from 2,584 soil profiles
#   - Columns: site info (location, depth), 14 soil properties, spectral IDs
#
# TIMING GUIDE (approximate)
# ---------------------------
#  0:00 – 0:20  Working directory, packages, importing data
#  0:20 – 0:40  Exploring the dataset; adding rowID; renaming columns
#  0:40 – 1:00  Creating profile IDs; removing exact duplicates
#  1:00 – 1:30  Coordinate validation (missing, bounds)
#               Depth validation (missing, negative, zero-thickness,
#               invalid logic, profiles without surface horizon)
###############################################################################


# =============================================================================
# PART 1 — SETTING UP THE ENVIRONMENT
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1  Setting the Working Directory
# -----------------------------------------------------------------------------
# The working directory is where R looks for data files and saves outputs.
# Use relative paths (e.g., "01_data/module1/kssl/KSSL_data.csv") whenever possible.

# Check current working directory
# getwd()

# Set new working directory
# setwd("C:/Users/YourName/Documents/YourSoilProject")

# On Mac/Linux
# setwd("/Users/YourName/Documents/YourSoilProject")

# Set working directory to script location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -----------------------------------------------------------------------------
# 1.2  Load Packages
# -----------------------------------------------------------------------------

library(readxl)           # Read Excel files
library(tidyverse)        # Data manipulation and visualization

# -----------------------------------------------------------------------------
# 1.3  Importing Data from Files
# -----------------------------------------------------------------------------

# Read a CSV file (generic syntax)
soil_data <- read.csv("path/to/soil_data.csv")

# If CSV uses different separator (semicolon, tab) (generic syntax)
soil_data <- read.csv("path/to/file.csv", sep = ";")
soil_data <- read.delim("path/to/file.txt", sep = "\t")
# Read a CSV file using the `readr::read_csv` in tidyverse  (generic syntax)
soil_data <- read_csv("path/to/file.txt")

# Read the KSSL data from a CSV file
soil_data <- read_csv("01_data/module1/kssl/KSSL_data.csv")

# View structure
str(soil_data)

# View first rows
head(soil_data)

# Read Excel file
soil_data <- read_excel("01_data/module1/kssl/KSSL_data.xlsx", sheet = 1) 

# Or read a specific sheet by name
soil_data <- read_excel("01_data/module1/kssl/KSSL_data.xlsx", sheet = "SoilData")

# -----------------------------------------------------------------------------
# 1.4  Exploring the Imported Data
# -----------------------------------------------------------------------------

str(soil_data)        # Structure + column types
summary(soil_data)    # Quick summaries for each column
names(soil_data)      # Column names

head(soil_data)       # First rows
tail(soil_data)       # Last rows

View(soil_data) # opens a spreadsheet-style viewer in RStudio


# =============================================================================
# PART 2 — LOADING THE KSSL DATASET
# The KSSL dataset is the primary dataset used throughout the sessions.
# All code below assumes the working directory is set to the project root.
# Before cleaning, always understand what you're working with
#
# WHAT TO DO:
# - Define  the file_path to the actual data location
# - Check column names and data types
# - Inspect the first few rows
# =============================================================================
# -----------------------------------------------------------------------------
# 2.1  Basic Data Loading and Structure
# -----------------------------------------------------------------------------

# Load libraries
library(tidyverse)        # Data manipulation and visualization
library(readxl)           # Read Excel files
library(writexl)          # Write Excel files
library(knitr)            # For formatted tables
library(sf)               # Manipulation of Vector Data
library(terra)            # Manipulation of Raster Data

# Read Excel file containing raw soil data
raw_data <- read_excel("01_data/module1/kssl/KSSL_data.xlsx", sheet = 1) 

# Define the folder to store the results of the exercise
output_dir <-"03_outputs/module1/"

# Create the output directory if not existing
if (!file.exists(output_dir)){
  # create a new sub directory inside the main path
  dir.create(output_dir)
}

str(raw_data) # Examine the structure of the data
head(raw_data, 10) # Show the first 10 rows
summary(raw_data) # Summarize the data 

# =============================================================================
# PART 3 — PREPARING SITE DATA
# - Start understanding how many valid points you have in the dataset
# - Standardized column names make cleaning easier and less error-prone
# The raw data includes information of:
# - Position and depth (SITE)
# - Analytical values of soil parameters (LAB)
# - The Dry Chemistry Data (SPECTRAL) has been stored in a separate file (MIR_KANSAS_data.xlsx) due to his size
#
# WHAT TO DO:
# - Extract only the columns you need for site identification
# - Rename to standard names: lon, lat, top, bottom, etc.
# - Create a unique profile identifier

# =============================================================================
# We define a "site" as the set of horizons sharing the same geographic
# location (identical coordinates). Site data includes:
#   - Location (longitude, latitude)
#   - Depth (top/bottom boundaries in cm)
#   - Horizon identifiers

# -----------------------------------------------------------------------------
# 3.1  Adding a Unique Row Identifier
# -----------------------------------------------------------------------------
# Add rowID BEFORE making any modifications so each original record can be
# traced throughout the entire cleaning workflow.

# Add unique row identifier to track individual records through processing
raw_data <- raw_data %>%
  mutate(rowID = row_number(), .before = 1)

# -----------------------------------------------------------------------------
# 3.2  Extracting and Standardizing Column Names for Sites
# -----------------------------------------------------------------------------
# Select relevant columns and rename to a consistent naming convention.

# Select only the columns needed for site data preparation
site <- raw_data %>%
  select(
    rowID,
    Long_Site.x,              # Raw column name for longitude
    Lat_Site.x,               # Raw column name for latitude
    smp_id,                   # Sample/horizon identifier
    Top_depth_cm.x,           # Top depth in centimeters
    Bottom_depth_cm.x         # Bottom depth in centimeters
  )

  # Rename columns to standard, consistent names
site <- site %>%
  rename(
    lon = Long_Site.x,
    lat = Lat_Site.x,
    HorID = smp_id,
    top = Top_depth_cm.x,
    bottom = Bottom_depth_cm.x
  ) 

# -----------------------------------------------------------------------------
# 3.3  Creating Unique Profile Identifiers
  # WHY: Each unique location = one soil profile ID
  # HOW: Group by coordinates, assign sequential ID, format as "PROF0001"
# -----------------------------------------------------------------------------

# Group horizons by coordinate and assign a standardized ProfID (e.g., "PROF0001").
site <- site %>%
  # Group all horizons at the same location
  group_by(lon, lat) %>%
  # Assign sequential ID to each unique location (cur_group_id() returns group number)
  mutate(ProfID = cur_group_id()) %>%
  ungroup() %>%
  # Format as standardized IDs: PROF0001, PROF0002, etc. with 4 digit resolution
  mutate(ProfID = sprintf("PROF%04d", ProfID))

  # Reorder columns for clarity
site <- site %>%
  select(rowID, ProfID, HorID, lon, lat, top, bottom)

# -----------------------------------------------------------------------------
# 3.4  Remove Duplicate Site Records
# -----------------------------------------------------------------------------
# In this dataset, spectroscopic measurements were performed four times per
# sample, resulting in duplicate entries. Remove duplicates and retain one
# record per horizon.

# Remove duplicate rows
site <- site %>%
  distinct(across(-rowID), .keep_all = TRUE)


# =============================================================================
# PART 4 — COORDINATE VALIDATION AND CORRECTION
# WHY: Geographic coordinates define location. Bad coordinates = bad maps

# VALIDATION CHECKS:
# 1. Check for missing coordinates (NA values)
# 2. Check for out-of-bounds values
#    - Valid longitude: -180 to +180 degrees
#    - Valid latitude: -90 to +90 degrees
#  
# COMMON ERRORS:
# - Missing coordinates (NA)
# - Swapped lon/lat
# - lon/lat too large (e.g., 45000º instead of 45º)
# - Projected coordinates instead of geographic
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1  Check 1: Missing Coordinates
# -----------------------------------------------------------------------------

  # Remove records with missing coordinates
  site <- site %>%
    dplyr::filter(!is.na(lon) & !is.na(lat))

# -----------------------------------------------------------------------------
# 4.2  Check 2: Valid Coordinate Ranges
# -----------------------------------------------------------------------------
# For geographic coordinates (decimal degrees):
#   Longitude: -180° to +180°
#   Latitude:   -90° to +90°

# Keep only rows with valid lon/lat geographic coordinates inside valid ranges
site <- site %>%
  dplyr::filter(
    lon >= -180, lon <= 180,
    lat >=  -90, lat <=  90
  )


# =============================================================================
# PART 5 — SOIL DEPTH VALIDATION AND CORRECTION
# =============================================================================
# Depth intervals define the soil layer each observation represents.

# VALIDATION CHECKS:
# Five sequential quality-control checks are applied:
#   Check 1: Missing depth boundaries
#   Check 2: Negative depth values
#   Check 3: Zero-thickness intervals
#   Check 4: Invalid depth logic (bottom <= top)
#   Check 5: Profiles without a surface horizon (top > 0)
# NOTE: Do NOT resolve duplicate depth sequences at this stage.
#       Handle duplicates after lab data has been cleaned (Session 3).

# -----------------------------------------------------------------------------
# 5.1  Check 1: Missing Depth Boundaries
# -----------------------------------------------------------------------------

# Keep records where `top` or `bottom` are not NA
site <- site %>%
  dplyr::filter(!is.na(top) & !is.na(bottom))

# -----------------------------------------------------------------------------
# 5.2  Check 2: Negative Depth Values
# -----------------------------------------------------------------------------

# Keep records where `top` or `bottom` are positive
  site <- site %>%
    filter(!(top < 0 | bottom < 0))

# -----------------------------------------------------------------------------
# 5.3  Check 3: Zero-Thickness Intervals
# -----------------------------------------------------------------------------

# Remove zero-thickness horizons
  site <- site %>%
    filter(!(bottom - top == 0))

# -----------------------------------------------------------------------------
# 5.4  Check 4: Invalid Depth Logic
# -----------------------------------------------------------------------------

# Remove invalid depth logic
site <- site %>%
  filter(bottom > top)

# -----------------------------------------------------------------------------
# 5.5  Check 5: Profiles Without a Surface Horizon (top > 0)
# WHY: Each profile should represent the complete soil column starting at surface
# DSM requires profiles that start at the soil surface (top = 0 cm).
# -----------------------------------------------------------------------------

## Keep only profiles that start at the surface (min top == 0)
site <- site %>%
  dplyr::group_by(ProfID) %>%
  dplyr::filter(!is.na(top) & min(top, na.rm = TRUE) == 0) %>%
  dplyr::ungroup()


# =============================================================================
# PART 6 — SAVE SITE TIBBLE TO DISK
# =============================================================================
# Save site results to 'site_KSSL.csv.
output <- paste0(output_dir, "site_KSSL.csv")
write.csv(site, output, row.names = FALSE)


###############################################################################
# END OF SESSION 2
# Summary of what was accomplished:
#   - KSSL data loaded and explored
#   - Row IDs added for traceability
#   - Site columns extracted, renamed, and standardized
#   - Profile IDs created from geographic coordinates
#   - Exact duplicate records removed
#   - Coordinates validated: missing and out-of-range records removed
#   - Depth boundaries validated: 5 checks applied sequentially
#
# The 'site' object is now ready for integration with lab data in Session 3.
#
# Next session: Session 3 — Soil Data Preparation with KSSL Dataset (Part 2)
###############################################################################

