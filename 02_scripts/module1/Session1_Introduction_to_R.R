###############################################################################
# SoilFER Online Training Programme — Module 1
# SESSION 1: Introduction to R for Soil Science (1.5 hours)
###############################################################################
#
# LEARNING OBJECTIVES
# -------------------
# By the end of this session, participants will be able to:
#   1. Navigate the RStudio interface
#   2. Install and load R packages
#   3. Create and work with R objects and data types
#   4. Use vectors, factors, matrices, data frames, and lists
#   5. Apply arithmetic, comparison, and logical operators
#   6. Write conditional statements and loops
#   7. Create and call custom functions
#   8. Manipulate data using base R and the tidyverse
#   9. Handle missing values
#  10. Reshape tabular data between wide and long formats
#
# DATASET
# -------
# All examples in this session use small toy datasets constructed inline.
# The KSSL Kansas dataset is introduced in Session 2.
#
# TIMING GUIDE (approximate)
# ---------------------------
#  0:00 – 0:15  R Packages: install, load, find help
#  0:15 – 0:35  Objects, data types, and data structures
#  0:35 – 0:55  Operators and control structures
#  0:55 – 1:15  Functions and data manipulation
#  1:15 – 1:30  Missing values, reshaping, and BDL data
###############################################################################


# =============================================================================
# PART 1 — PACKAGES
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1  Installing Packages
# -----------------------------------------------------------------------------
# Packages need to be installed only ONCE. Use install.packages():

# Install a single package
install.packages("tidyverse")

# Install multiple packages at once
install.packages(c("terra", "sf", "aqp"))

# Check which packages are already installed
installed.packages()

# -----------------------------------------------------------------------------
# 1.2  Installing from GitHub or Other Sources
# -----------------------------------------------------------------------------

# First, install remotes package (if not already installed)
install.packages("remotes")

# Install an R package from GitHub
remotes::install_github("rspatial/terra")

# -----------------------------------------------------------------------------
# 1.3  Manual Installation (offline environments)
# -----------------------------------------------------------------------------

# Installing from a compressed source package file (e.g., mypackage_1.0.0.tar.gz)
install.packages("path/to/mypackage_1.0.0.tar.gz", repos = NULL, type = "source")

# Installing from a Local .zip file (Windows Binary)
install.packages("path/to/mypackage.zip", repos = NULL, type = "win.binary")

# -----------------------------------------------------------------------------
# 1.4  Loading Packages
# -----------------------------------------------------------------------------
# After installation, LOAD a package each time you start a new R session:

# Load tidyverse package
library(tidyverse)

# Load multiple packages
library(terra)
library(sf)
library(aqp)

# -----------------------------------------------------------------------------
# 1.5  Finding Help on Packages
# -----------------------------------------------------------------------------

# Get help on a package
help(package = "tidyverse")

# Or use
?tidyverse

# View vignettes (tutorials) for a package
vignette(package = "ggplot2")
vignette("ggplot2", package = "ggplot2")

# =============================================================================
# PART 2 — OBJECTS AND DATA TYPES
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1  Creating Objects with Assignment
# -----------------------------------------------------------------------------

# Assign a value to a variable
soil_depth <- 30
soil_depth

# Assignment `<-` to objects and `=` to function arguments
soil_depth <- c(10,20,30)
mean_soil_depth <- mean(x = soil_depth, na.rm = TRUE)
mean_soil_depth

# Good naming practices — descriptive, consistent
# -   Object names must start with a letter
# -   Names can contain letters, numbers, underscores `_`, and periods `.`
# -   Names are case-sensitive: `SoilDepth` is different from `soildepth`
# -   Avoid using reserved words like `TRUE`, `FALSE`, `NA`, `function`, etc.

soil_ph <- 6.5
organic_carbon_percent <- 2.1
clay_content_gkg <- 350

# Use consistent style
plot_id <- "P001"      # snake_case (recommended)
plotID <- "P001"       # camelCase (alternative)

# Bad naming practices — too short, too long, unclear
# Avoid — too short, unclear
x <- 6.5
a <- 2.1

# Avoid — too long
the_ph_value_of_the_topsoil_at_site_one <- 6.5

# -----------------------------------------------------------------------------
# 2.2  Data Types in R 
# -----------------------------------------------------------------------------

# Numeric values
ph_value <- 6.8
clay_percent <- 25.5
temperature <- 15.2

# Integer values (use L suffix)
sample_count <- 100L
plot_number <- 5L

# Character values (use quotes)
soil_type <- "Acrisol"
location <- "Kansas"
notes <- "Sample collected from topsoil"

# Logical values
is_valid <- TRUE
has_missing_data <- FALSE

# Dates: The ISO 8601 date format, YYYY-MM-DD
sampling_date <- "2024-03-12"
# Convert to date (%Y = 4-digit year; %y = 2-digit year)
sampling_date <- as.Date(sampling_date, format = "%Y-%m-%d") 
class(sampling_date)

# -----------------------------------------------------------------------------
# 2.3  Checking Data Types
# -----------------------------------------------------------------------------

# Check the class (high-level type)
class(ph_value)   # "numeric"
class(soil_type)  # "character"
class(is_valid)   # "logical"

# Check the internal storage type
typeof(sample_count)
typeof(ph_value)

# Helpful checks
is.numeric(ph_value)
is.character(soil_type)
is.logical(is_valid)


# =============================================================================
# PART 3 — DATA STRUCTURES
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1  Vectors
# -----------------------------------------------------------------------------

#### Creating Vectors ----

# Create a numeric vector using c() (combine)
ph_values <- c(5.2, 6.5, 7.1, 5.8, 6.9)
print(ph_values)
ph_values

# Character vector
soil_types <- c("Acrisol", "Ferralsol", "Vertisol", "Andosol", "Cambisol")
soil_types

# Logical vector
valid_samples <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
valid_samples

# Create sequences
depths <- 0:100                    # Integers from 0 to 100
depths_seq <- seq(0, 100, by=10)   # 0, 10, 20, ..., 100

#### Vector Operations ----

# Arithmetic on vectors (element-wise)
ph_values * 10
ph_values + 1

# Summary statistics
mean(ph_values)
median(ph_values)
sd(ph_values)        # standard deviation
min(ph_values)
max(ph_values)
# NAs affect the statistic results
mean(c(ph_values,NA))
mean(c(ph_values,NA), na.rm=TRUE) # Remove NAs from calculations

#### Accessing Vector Elements ----

# Access by index (position)
ph_values           # Complete data
ph_values[1]        # First element
ph_values[3]        # Third element

# Access multiple elements
ph_values[c(1,3,5)]  # Elements 1, 3, and 5

# Access by logical condition
ph_values[ph_values > 6]     # All pH values greater than 6

# Negative indices exclude elements
ph_values[-1]       # All except first
ph_values[-c(1,2)]  # All except first two

# -----------------------------------------------------------------------------
# 3.2  Factors
# -----------------------------------------------------------------------------

# Create a factor from character vector
soil_class <- factor(c("Clay", "Loam", "Sand", "Clay", "Loam"))
soil_class

# Check levels
levels(soil_class)

# Count observations per level
table(soil_class)

# Ordered factors (when order matters)
texture_class <- factor(
  c("Coarse", "Fine", "Medium", "Fine", "Coarse"),
  levels = c("Coarse", "Medium", "Fine"),
  ordered = TRUE
)
texture_class

# -----------------------------------------------------------------------------
# 3.3  Matrices
# -----------------------------------------------------------------------------

# Create a matrix
soil_matrix <- matrix(
  c(5.2, 25, 30,
    6.5, 30, 28,
    7.1, 18, 35),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)
soil_matrix

# Add column names
colnames(soil_matrix) <- c("pH", "Clay", "Sand")
rownames(soil_matrix) <- c("Sample1", "Sample2", "Sample3")
soil_matrix

# Access elements
soil_matrix[1, 2]        # Row 1, Column 2
soil_matrix[1, ]         # All of row 1
soil_matrix[, 2]         # All of column 2

# -----------------------------------------------------------------------------
# 3.4  Data Frames
# -----------------------------------------------------------------------------

# Create a data frame
soil_data <- data.frame(
  plot_id = c("P001", "P002", "P003", "P004", "P005"),
  latitude = c(-1.25, -1.27, -1.23, -1.29, -1.26),
  longitude = c(36.85, 36.83, 36.87, 36.81, 36.84),
  ph = c(5.2, 6.5, 7.1, 5.8, 6.9),
  organic_carbon = c(2.1, 3.2, 1.8, 2.7, 2.9),
  clay_content = c(25, 30, 18, 42, 35),
  soil_type = c("Acrisol", "Ferralsol", "Vertisol", "Andosol", "Cambisol")
)

# View the data frame
soil_data

# View structure
str(soil_data)

# View first rows
head(soil_data)

# View last rows
tail(soil_data)

summary(soil_data)


# Get dimensions
dim(soil_data)         # rows, columns
nrow(soil_data)        # number of rows
ncol(soil_data)        # number of columns

# Access columns and rows
soil_data$ph
soil_data[["ph"]]

# Access rows
soil_data[1, ]           # First row
soil_data[c(1,3,5), ]    # Rows 1, 3, and 5

# Access specific cells
soil_data[2, 4]          # Row 2, Column 4 (pH of second plot)

# Subset based on conditions
soil_data[soil_data$ph > 6, ]              # Plots with pH > 6
soil_data[soil_data$soil_type == "Acrisol", ]  # Only Acrisols

# Add a new column
soil_data$silt_content <- c(40, 35, 52, 23, 30)

# Calculate new columns from existing ones
soil_data$clay_plus_silt <- soil_data$clay_content + soil_data$silt_content

# View updated data frame
head(soil_data)

# -----------------------------------------------------------------------------
# 3.5  Lists
# -----------------------------------------------------------------------------

# Create a list
soil_analysis <- list(
  site_name = "Kansas Field",
  coordinates = c(lat = -1.25, lon = 36.85),
  measurements = data.frame(
    depth = c(0, 10, 20, 30),
    ph = c(6.5, 6.2, 5.8, 5.5)
  ),
  notes = "Collected during dry season"
)

# View list structure
str(soil_analysis)

# Access list elements
soil_analysis$site_name
soil_analysis[[1]]                # First element
soil_analysis[["measurements"]]   # View measurements data frame


# =============================================================================
# PART 4 — OPERATORS
# =============================================================================

# -----------------------------------------------------------------------------
# 4.1  Arithmetic Operators
# -----------------------------------------------------------------------------

# Basic arithmetic
10 + 5      # Addition
10 - 5      # Subtraction
10 * 5      # Multiplication
10 / 5      # Division
10 ^ 2      # Exponentiation (10 squared)
10 %% 3     # Modulus (remainder: 10 mod 3 = 1)
10 %/% 3    # Integer division (10 divided by 3 = 3)

# Order of operations (PEMDAS)
result <- (10 + 5) * 2 / 4 - 1
result

# -----------------------------------------------------------------------------
# 4.2  Comparison Operators
# -----------------------------------------------------------------------------

# Comparison operators return TRUE or FALSE
5 == 5      # Equal to
5 != 3      # Not equal to
5 > 3       # Greater than
5 < 3       # Less than
5 >= 5      # Greater than or equal to
5 <= 6      # Less than or equal to

# Use in subsetting
ph_values <- c(5.2, 6.5, 7.1, 5.8, 6.9)
ph_values > 6                     # Logical vector
ph_values[ph_values > 6]          # Values greater than 6

# -----------------------------------------------------------------------------
# 4.3  Logical Operators
# -----------------------------------------------------------------------------

# AND operator: & (element-wise) or && (single values)
TRUE & TRUE      # TRUE
TRUE & FALSE     # FALSE

# OR operator: | (element-wise) or || (single values)
TRUE | FALSE     # TRUE
FALSE | FALSE    # FALSE

# NOT operator: !
!TRUE            # FALSE
!FALSE           # TRUE

# Combining conditions
ph_values <- c(5.2, 6.5, 7.1, 5.8, 6.9)
clay_content <- c(25, 30, 18, 42, 35)

# Find samples with pH > 6 AND clay > 25
ph_values > 6 & clay_content > 25

# Find samples with pH > 6 OR clay > 40
ph_values > 6 | clay_content > 40


# =============================================================================
# PART 5 — CONTROL STRUCTURES
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1  If-Else Statements
# -----------------------------------------------------------------------------

# Basic if statement
ph_value <- 7.5

# Basic if statement
if (ph_value > 7) {
  print("Alkaline soil")
}

# If-else
if (ph_value > 7) {
  print("Alkaline soil")
} else {
  print("Neutral or acidic soil")
}

# Multiple conditions (only the first TRUE branch runs)
if (ph_value > 7.5) {
  print("Strongly alkaline")
} else if (ph_value > 7) {
  print("Slightly alkaline")
} else if (ph_value == 7) {
  print("Neutral")
} else {
  print("Acidic")
}

# -----------------------------------------------------------------------------
# 5.2  Vectorized If-Else: ifelse()
# -----------------------------------------------------------------------------

# Vectorized conditional assignment
ph_values <- c(5.2, 6.5, 7.1, 5.8, 7.0)

# Simple two-class example
soil_reaction <- ifelse(ph_values > 7, "Alkaline", "Not alkaline")
soil_reaction

# -----------------------------------------------------------------------------
# 5.3  Vectorized Cut: cut()
# -----------------------------------------------------------------------------

soil_class <- cut(
  ph_values,
  breaks = c(-Inf, 5.5, 7.0, Inf),
  labels = c("Acidic", "Neutral", "Alkaline"),
  right = TRUE, include.lowest = TRUE
)
soil_class

# Notes:
# - `right = TRUE` means intervals are **right-closed** (e.g., `(5.5, 7.0]`).
# - `include.lowest = TRUE` ensures the smallest value is included in the first interval.

# -----------------------------------------------------------------------------
# 5.4  For Loops
# -----------------------------------------------------------------------------

# Loop through a sequence
for (i in 1:5) {
  print(paste("Iteration:", i))
}

# Loop through a vector
soil_types <- c("Acrisol", "Ferralsol", "Vertisol")

for (soil in soil_types) {
  print(paste("Soil type:", soil))
}

# Loop with conditional logic
ph_values <- c(5.2, 6.5, 7.1, 5.8, 6.9)

for (i in 1:length(ph_values)) {
  if (ph_values[i] > 6) {
    print(paste("Sample", i, "has pH =", ph_values[i], "(Acceptable)"))
  } else {
    print(paste("Sample", i, "has pH =", ph_values[i], "(Too acidic)"))
  }
}

# -----------------------------------------------------------------------------
# 5.5  While Loops
# -----------------------------------------------------------------------------

# While loop continues until condition is FALSE
counter <- 1

while (counter <= 5) {
  print(paste("Counter value:", counter))
  counter <- counter + 1   # Increment counter
}


# =============================================================================
# PART 6 — FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# 6.1  Using Built-in Functions
# -----------------------------------------------------------------------------

# Statistical functions
mean(c(5, 10, 15, 20))
median(c(5, 10, 15, 20))
sd(c(5, 10, 15, 20))
sum(c(5, 10, 15, 20))

# String functions
toupper("acrisol")
tolower("FERRALSOL")
nchar("soil science")         # Count characters

# Math functions
sqrt(16)
log(10)
exp(2)
abs(-5)
round(pi, 2)

# -----------------------------------------------------------------------------
# 6.2  Creating Custom Functions
# -----------------------------------------------------------------------------

# Example: Bulk density (mass / volume)
calculate_bulk_density <- function(mass, volume) {
  if (any(volume <= 0)) stop("volume must be > 0")
  mass / volume
}

calculate_bulk_density(mass = 150, volume = 100)

# Function with a default argument
classify_soil_ph <- function(ph, threshold = 7) {
  if (ph > threshold) {
    "Alkaline"
  } else if (ph == threshold) {
    "Neutral"
  } else {
    "Acidic"
  }
}

classify_soil_ph(6.5)
classify_soil_ph(6.5, threshold = 6)

# -----------------------------------------------------------------------------
# 6.3  Function Arguments and Defaults
# -----------------------------------------------------------------------------

# SOC stock calculation (example)
# Assumptions:
# - soc_percent is in %
# - bulk_density is in g/cm^3
# - depth is in cm
# Output: SOC stock in Mg/ha

calculate_soc_stock <- function(soc_percent, bulk_density, depth, coarse_fragment = 0) {
  if (any(coarse_fragment < 0 | coarse_fragment > 100)) stop("coarse_fragment must be between 0 and 100")
  soc_percent * bulk_density * depth * (1 - coarse_fragment / 100)
}

calculate_soc_stock(soc_percent = 2.5, bulk_density = 1.3, depth = 30)
calculate_soc_stock(soc_percent = 2.5, bulk_density = 1.3, depth = 30, coarse_fragment = 15)


# =============================================================================
# PART 7 — DATA MANIPULATION WITH BASE R
# =============================================================================

# -----------------------------------------------------------------------------
# 7.1  Subsetting and Filtering
# -----------------------------------------------------------------------------

# Example data frame
soil_data <- data.frame(
  plot_id = c("P001", "P002", "P003", "P004", "P005"),
  latitude = c(-1.25, -1.27, -1.23, -1.29, -1.26),
  longitude = c(36.85, 36.83, 36.87, 36.81, 36.84),
  ph = c(5.2, 6.5, 7.1, 5.8, 6.9),
  clay_content = c(25, 30, 18, 42, 35),
  soil_type = c("Acrisol", "Ferralsol", "Vertisol", "Andosol", "Cambisol")
)

# Select column by index: Show second column
soil_data[, 2]
# Select column by index: Show first and third columns
soil_data[, c(1,3)]
# Select column  by name: Show texture columns
soil_data[, c("ph","clay_content","soil_type")]
# Filter rows by index: keep first 2 records in the data frame
soil_data[c(1:2),]

# Filter rows by index: keep texture properties for the first 100 records in the data frame
soil_data[c(1:2), c("ph","clay_content","soil_type")]

# Filter rows based on conditions
high_ph <- soil_data[soil_data$ph > 6, ]
high_ph

# Multiple conditions with & and |
high_ph_clay <- soil_data[soil_data$ph > 6 & soil_data$clay_content > 25, ]
high_ph_clay

# Select columns by index: Show second column
subset_data <- soil_data[, 2]
# Select several columns by index
subset_data <- soil_data[, c(1:3)]

# Select by name
subset_data <- soil_data[, c("plot_id", "ph", "soil_type")]
subset_data

# -----------------------------------------------------------------------------
# 7.2  Deleting Columns and Rows
# -----------------------------------------------------------------------------

# Example data frame
data <- data.frame(
  Plot = c("P001", "P002", "P003"),
  Clay = c(25, 30, 18),
  Silt = c(40, 35, 52),
  pH   = c(5.2, 6.5, 7.1)
)

# --- Delete columns ---

# Delete one column by name
data$Clay <- NULL
data$Clay <- c() # Empty vector
data

# Delete multiple columns by name
data[, c("Silt", "pH")] <- NULL
data

# --- Delete rows ---

# Delete a row by index (e.g., remove the 2nd row)
data <- data[-2, ]
data

# Delete rows based on a condition (e.g., remove rows with pH < 6)
# Rebuild the dataframe
data <- data.frame(
  Plot = c("P001", "P002", "P003"),
  Clay = c(25, 30, 18),
  Silt = c(40, 35, 52),
  pH   = c(5.2, 6.5, 7.1)
)
data <- data[data$pH >= 6, ]
data

# -----------------------------------------------------------------------------
# 7.3  Sorting Data
# -----------------------------------------------------------------------------

# Sort by pH (ascending)
soil_data[order(soil_data$ph), ]

# Sort by pH (descending)
soil_data[order(-soil_data$ph), ]

# Sort by multiple columns
soil_data[order(soil_data$soil_type, soil_data$ph), ]

# -----------------------------------------------------------------------------
# 7.4  Aggregating Data
# -----------------------------------------------------------------------------

# Example data frame with repeated soil_type groups
soil_data <- data.frame(
  plot_id = paste0("P", sprintf("%03d", 1:12)),
  latitude = c(-1.25, -1.26, -1.27, -1.28, -1.23, -1.24, -1.22, -1.21, -1.29, -1.30, -1.31, -1.32),
  longitude = c(36.85, 36.86, 36.83, 36.84, 36.87, 36.88, 36.82, 36.81, 36.84, 36.83, 36.86, 36.85),
  ph = c(5.2, 5.6, 6.5, 6.2, 7.1, 6.9, 5.8, 6.0, 6.9, 6.7, 5.4, 5.1),
  clay_content = c(25, 28, 30, 33, 18, 20, 42, 40, 35, 34, 27, 26),
  soil_type = c(
    "Acrisol","Acrisol",
    "Ferralsol","Ferralsol",
    "Vertisol","Vertisol",
    "Andosol","Andosol",
    "Cambisol","Cambisol",
    "Acrisol","Acrisol"
  )
)

# Multiple summary values by group (mean and standard deviation of pH by soil_type)
aggregate(
  ph ~ soil_type,
  data = soil_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)


# =============================================================================
# PART 8 — DATA MANIPULATION WITH THE TIDYVERSE
# =============================================================================

# -----------------------------------------------------------------------------
# 8.1  Loading the Tidyverse
# -----------------------------------------------------------------------------

# Install if not already installed
# install.packages("tidyverse")

# Load tidyverse
library(tidyverse)

# -----------------------------------------------------------------------------
# 8.2  Tibbles: Modern Data Frames
# -----------------------------------------------------------------------------

# Create tibble directly
soil_tbl <- tibble(
  plot_id = c("P001", "P002", "P003"),
  ph = c(5.2, 6.5, 7.1),
  clay = c(25, 30, 18)
)
soil_tbl

soil_data <- data.frame(
  plot_id = c("P001", "P002", "P003"),
  ph = c(5.2, 6.5, 7.1),
  clay = c(25, 30, 18)
)

# Convert data frame to tibble
soil_tbl <- as_tibble(soil_data)
soil_tbl



# -----------------------------------------------------------------------------
# 8.3  The Pipe Operator: %>%
# -----------------------------------------------------------------------------
soil_data <- tibble(
  plot_id = c("P001", "P002", "P003"),
  ph = c(5.2, 6.5, 7.1),
  clay = c(25, 30, 18)
)

# Without pipe (nested functions)
round(mean(soil_data$ph), 2)

# With pipe (sequential operations)
soil_data$ph %>%
  mean() %>%
  round(2)

# More complex example
soil_data %>%
  filter(ph > 6) %>%
  select(plot_id, ph, clay) %>%
  arrange(desc(ph))

# -----------------------------------------------------------------------------
# 8.4  Data Manipulation with {dplyr}
# -----------------------------------------------------------------------------

# Select specific columns
soil_data %>%
  select(plot_id, ph, clay)

# Select range of columns
soil_data %>%
  select(plot_id:ph)

# Remove columns
soil_data %>%
  select(-plot_id, -ph)

# Select columns matching pattern
soil_data %>%
  select(contains("p"))

# Filter rows based on condition
soil_data %>%
  filter(ph > 6)

# Multiple conditions
soil_data %>%
  filter(ph > 6 & clay > 25)

# Filter with OR
soil_data %>%
  filter(plot_id == "P001" | plot_id == "P002")

# Use %in% for multiple values
soil_data %>%
  filter(plot_id %in% c("P001", "P002"))

# Rename columns for clarity
rename(soil_data,
    site_id = plot_id)

# You can rename multiple columns at once
soil_data %>%
 rename(
  location = plot_id,
  acidity = ph,
  clay_percent = clay
 ) 

# Create new columns
soil_data %>%
  mutate(
    ph_class = ifelse(ph > 7, "Alkaline", "Acidic"),
    clay_gr_kg = clay * 10 
  )

# Modify existing columns
soil_data %>%
  mutate(
    ph = round(ph, 1),
    plot_id = tolower(plot_id)
  )

# Sort ascending
soil_data %>%
  arrange(ph)

# Sort descending
soil_data %>%
  arrange(desc(ph))

# Multiple sort keys
soil_data %>%
  arrange(ph, desc(clay))

# -----------------------------------------------------------------------------
# 8.5  Grouping and Summarizing Data
# -----------------------------------------------------------------------------

# Create example dataset
soil_data <- data.frame(
site = c("Forest_A", "Forest_B", "Grassland_A", "Grassland_B", "Urban_A", "Urban_B"),
ecosystem = c("Forest", "Forest", "Grassland", "Grassland", "Urban", "Urban"),
pH = c(6.2, 6.8, 7.1, 6.9, 5.8, 6.0),
organic_carbon = c(3.2, 2.8, 2.1, 2.4, 1.5, 1.8)
)

# Summarize soil properties
soil_data %>%
  summarize(
    mean_ph = mean(pH),
    sd_ph = sd(pH),
    min_soc = min(organic_carbon),
    max_soc = max(organic_carbon),
    n_samples = n()
  )

# Group by and summarize
soil_data %>%
  group_by(ecosystem) %>%
  summarize(
    mean_ph = mean(pH),
    mean_soc = mean(organic_carbon), # Average organic carbon
    count = n(),  
  .groups = "drop"                   # Remove grouping
)

# Alternative: count()
soil_data <- data.frame(
site = c("Forest_A", "Forest_B", "Grassland_A", "Grassland_B", "Urban_A", "Urban_B"),
ecosystem = c("Forest", "Forest", "Grassland", "Grassland", "Urban", "Urban"),
pH = c(6.2, 6.8, 7.1, 6.9, 5.8, 6.0),
organic_carbon = c(3.2, 2.8, 2.1, 2.4, 1.5, 1.8)
)

# Count observations by group
count(soil_data, ecosystem)

# Count by multiple groups
count(soil_data, ecosystem, site)

# Count with weights of a column (sum of carbon instead of count)
soil_data %>%
 count(ecosystem, wt = organic_carbon, name = "total SOC")

# -----------------------------------------------------------------------------
# 8.6  Combining Data Frames (Joins)
# -----------------------------------------------------------------------------

# Soil data example
soil_basic <- data.frame(
 site_id = c("A", "B", "C", "D"),
 pH = c(6.2, 6.8, 7.1, 6.9),
 organic_carbon = c(3.2, 2.8, 2.1, 2.4)
)
# Additional measurements (note: includes data on site E, but not on site A)
nutrients <- data.frame(
 site_id = c("B", "C", "D", "E"),
 phosphorus = c(0.15, 0.12, 0.18, 0.14),
 potassium = c(0.8, 0.9, 0.7, 0.6)
)
# Site information
site_info <- data.frame(
 site_id = c("A", "B", "C", "D"),
 ecosystem = c("Forest", "Forest", "Grassland", "Grassland"),
 elevation = c(450, 520, 380, 420)
)

# Join examples (same keys, different rules for which rows are kept)

# 1) `left_join()`: Keep all rows from soil_basic
left_join(soil_basic, nutrients, by = "site_id")

# 2) `right_join()`: Keep all rows from nutrients
right_join(soil_basic, nutrients, by = "site_id")

# 3) `inner_join()`: Keep only rows that exist in both tables
inner_join(soil_basic, nutrients, by = "site_id")

# 4) `full_join()`: Keep all rows from both tables (missing values become NA)
full_join(soil_basic, nutrients, by = "site_id")

## Joining more than two tables sequentially
soil_basic %>%
  left_join(nutrients, by = "site_id") %>%
  left_join(site_info, by = "site_id")

# -----------------------------------------------------------------------------
# 8.7  Stacking Data with bind_rows()
# -----------------------------------------------------------------------------

# Data from different time periods
spring_data <- data.frame(
 site = c("A", "B"),
 season = "Spring",
 pH = c(6.1, 6.7),
 temperature = c(12.5, 11.8)
)

summer_data <- data.frame(
 site = c("A", "B"),
 season = "Summer", 
 pH = c(6.3, 6.9),
 temperature = c(18.2, 17.5)
)

# Combine the datasets
bind_rows(spring_data, summer_data)


# =============================================================================
# PART 9 — MISSING DATA
# =============================================================================

# -----------------------------------------------------------------------------
# 9.1  Identifying Missing Data
# -----------------------------------------------------------------------------

# Create data with missing values
soil_data_na <- data.frame(
  plot = c("P001", "P002", "P003", "P004"),
  ph = c(5.2, NA, 7.1, 6.5),
  clay = c(25, 30, NA, 35)
)

# Check for missing values
is.na(soil_data_na)

# Count missing values per column
colSums(is.na(soil_data_na))

# Identify complete cases (rows with no missing values)
complete.cases(soil_data_na)

# Extract complete cases
soil_complete <- soil_data_na[complete.cases(soil_data_na), ]
soil_complete

# -----------------------------------------------------------------------------
# 9.2  Handling Missing Data (Base R)
# -----------------------------------------------------------------------------

# Remove rows with any missing values (base R)
na.omit(soil_data_na)

# Remove rows where a specific column is missing (keep rows with non-missing pH)
soil_data_na[!is.na(soil_data_na$ph), ]

# Replace missing pH values with the mean pH (na.rm = TRUE ignores NA in the mean)
soil_data_na$ph[is.na(soil_data_na$ph)] <- mean(soil_data_na$ph, na.rm = TRUE)
soil_data_na

# -----------------------------------------------------------------------------
# 9.3  Handling Missing Data (Tidyverse)
# -----------------------------------------------------------------------------

# Remove all rows with any NA
soil_data_na %>%
  drop_na()

# Remove rows where ph is NA
soil_data_na %>%
  drop_na(ph)

# Replace NA values with specified values
soil_data_na %>%
  replace_na(list(ph = 6.0, clay = 30))


# =============================================================================
# PART 10 — DATA RESHAPING WITH {tidyr}
# =============================================================================

# -----------------------------------------------------------------------------
# 10.1  pivot_longer(): Wide to Long
# -----------------------------------------------------------------------------

library(dplyr) # for piping %>%
library(tidyr) # for pivoting
# Wide format data
wide_soil <- data.frame(
 site = c("A", "B", "C"),
 ecosystem = c("Forest", "Grassland", "Urban"),
 pH = c(6.2, 7.1, 5.8),
 carbon = c(3.2, 2.1, 1.5),
 nitrogen = c(0.25, 0.18, 0.12)
)
wide_soil

# Convert to long format
long_soil <- wide_soil %>%
 pivot_longer(
  cols = c(pH, carbon, nitrogen),     # Columns to pivot
  names_to = "measurement_type",      # Name for the variable column
  values_to = "value"           # Name for the values column
 )
long_soil

# -----------------------------------------------------------------------------
# 10.2  pivot_wider(): Long to Wide
# -----------------------------------------------------------------------------

# Convert back to wide format
long_soil %>%
 pivot_wider(
  names_from = measurement_type, # Column containing variable names
  values_from = value            # Column containing values
 )

# -----------------------------------------------------------------------------
# 10.3  Advanced Pivoting: Multiple Measurements per Site (Replicates)
# -----------------------------------------------------------------------------

# More complex examples with multiple measurements per site
field_data <- data.frame(
 site = rep(c("Forest", "Grassland"), each = 6),
 measurement = rep(c("pH", "carbon", "nitrogen"), 4),
 replicate = rep(c("R1", "R2"), 6),
 value = c(6.2, 6.1, 3.2, 3.0, 0.25, 0.23, 7.1, 7.0, 2.1, 2.3, 0.18, 0.19)
)
field_data

# Pivot to have measurements as columns
print("Pivoted data:")
field_data %>%
 pivot_wider(
  names_from = measurement,
  values_from = value
 )


# =============================================================================
# PART 11 — HANDLING BELOW DETECTION LIMIT (BDL) DATA
# =============================================================================

soil_lab <- data.frame(
  plot_id = c("P001", "P002", "P003", "P004"),
  no3_mgkg_raw = c("0.12", "<0.05", "0.31", "<0.05"),
  stringsAsFactors = FALSE
)
soil_lab

soil_lab <- soil_lab %>%
  mutate(
    censored = str_detect(no3_mgkg_raw, "^\\s*<"),
    dl = if_else(censored,
                 as.numeric(str_remove(no3_mgkg_raw, "^\\s*<\\s*")),
                 NA_real_),
    no3_mgkg = if_else(censored, dl / 2, as.numeric(no3_mgkg_raw))
  )

soil_lab


###############################################################################
# END OF SESSION 1
# Next session: Session 2 — Soil Data Preparation with KSSL Dataset (Part 1)
###############################################################################
