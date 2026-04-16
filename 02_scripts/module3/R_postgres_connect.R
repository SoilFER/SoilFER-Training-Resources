###############################################################################
# SoilFER Online Training Programme — Module 3
# SESSION 1: GloSIS Data Preparation (1.5 hours)
# Script to connect the created kssl GloSIS database in PostgrSQL to R 
#  and export the results in wide format to Geopackage
###############################################################################

# =========================================================
# Load packages
# =========================================================
library(DBI)
library(RPostgres)
library(dplyr)
library(tidyr)
library(sf)

# =========================================================
# Connect to the PostgreSQL database
# =========================================================
# This connection uses the database exposed on localhost:5442,
# which is the port mapped from the Docker PostgreSQL container.
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "kssl",
  host     = "localhost",
  port     = 5442,
  user     = "glosis",
  password = "glosis"
)

# =========================================================
# SQL Query 1: retrieve soil point / profile / horizon metadata
# =========================================================
# This query extracts the structural information for each soil specimen:
# - project and site identifiers
# - plot and profile identifiers
# - horizon/element depth information
# - specimen code
# - longitude and latitude from the plot geometry

query_points <- "
SELECT
  p.project_id,
  p.name AS project_name,
  s.site_id,
  s.site_code,
  pl.plot_id,
  pf.profile_id,
  e.element_id,
  e.order_element,
  e.upper_depth,
  e.lower_depth,
  e.type,
  sp.specimen_id,
  sp.code AS specimen_code,
  ST_X(pl.position::geometry) AS longitude,
  ST_Y(pl.position::geometry) AS latitude
FROM core.project p
JOIN core.project_site ps
  ON ps.project_id = p.project_id
JOIN core.site s
  ON s.site_id = ps.site_id
JOIN core.plot pl
  ON pl.site_id = s.site_id
JOIN core.profile pf
  ON pf.plot_id = pl.plot_id
JOIN core.element e
  ON e.profile_id = pf.profile_id
JOIN core.specimen sp
  ON sp.element_id = e.element_id
ORDER BY
  p.name,
  s.site_id,
  pf.profile_id,
  e.order_element,
  e.upper_depth
"

# =========================================================
# SQL Query 2: retrieve laboratory soil properties
# =========================================================
# This query extracts the measured physico-chemical properties
# linked to each specimen, including:
# - property identifier
# - analytical procedure
# - unit of measure
# - observed value

query_properties <- "
SELECT
  p.name AS project_name,
  s.site_code,
  sp.specimen_id,
  sp.code AS specimen_code,
  r.result_phys_chem_id,
  o.observation_phys_chem_id,
  o.property_phys_chem_id,
  o.procedure_phys_chem_id,
  o.unit_of_measure_id,
  r.value
FROM core.result_phys_chem r
JOIN core.specimen sp
  ON r.specimen_id = sp.specimen_id
JOIN core.element e
  ON sp.element_id = e.element_id
JOIN core.profile pf
  ON e.profile_id = pf.profile_id
JOIN core.plot pl
  ON pf.plot_id = pl.plot_id
JOIN core.site s
  ON pl.site_id = s.site_id
JOIN core.project_site ps
  ON s.site_id = ps.site_id
JOIN core.project p
  ON ps.project_id = p.project_id
JOIN core.observation_phys_chem o
  ON o.observation_phys_chem_id = r.observation_phys_chem_id
ORDER BY
  p.name,
  s.site_code,
  sp.specimen_id,
  o.property_phys_chem_id
"

# =========================================================
# Execute the SQL queries and store results in R objects
# =========================================================
soil_points <- dbGetQuery(con, query_points)
soil_properties <- dbGetQuery(con, query_properties)

# =========================================================
# Convert soil properties from long to wide format
# =========================================================
# A new field called 'property_unit' is created by concatenating:
# - property_phys_chem_id
# - unit_of_measure_id
#
# This ensures that each output column clearly represents both
# the property and its measurement unit.
#
# Then the table is grouped by specimen and reshaped to wide format so that:
# - each row corresponds to one specimen
# - each property becomes a separate column
soil_properties <- soil_properties %>%
  mutate(
    property_unit = paste0(property_phys_chem_id, "_", unit_of_measure_id)
  ) %>%
  group_by(project_name, site_code, specimen_id, specimen_code, property_unit) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = property_unit,
    values_from = value
  )

# =========================================================
# Join specimen metadata with laboratory properties
# =========================================================
# The project name is removed from the properties table before joining
# to avoid duplicated columns such as project_name.x / project_name.y.
#
# The join is performed using:
# - site_code
# - specimen_id
# - specimen_code
soil_points_with_properties <- soil_points %>%
  left_join(
    soil_properties %>% select(-project_name),
    by = c("site_code", "specimen_id", "specimen_code")
  )

# =========================================================
# Convert the final table to an sf spatial object
# =========================================================
# Longitude and latitude are used to create point geometries.
# The coordinate reference system is WGS84 (EPSG:4326).
soil_points_sf <- st_as_sf(
  soil_points_with_properties,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# =========================================================
# Export the spatial dataset to a GeoPackage file
# =========================================================
# GeoPackage is used instead of Shapefile because it preserves:
# - full column names
# - special characters
# - a larger number of fields
st_write(
  soil_points_sf,
  "03_outputs/module3/kssl_points_with_properties.gpkg",
  delete_dsn = TRUE
)

# =========================================================
# Optional: close the database connection when finished
# =========================================================
dbDisconnect(con)