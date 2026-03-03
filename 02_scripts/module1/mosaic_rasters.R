# =============================================================================
# Mosaic cropland mask tiles (Kansas) and write a compact GeoTIFF
# =============================================================================
# Purpose
#   - Merge (mosaic) two adjacent raster tiles that together cover Kansas.
#   - Input rasters are binary:
#       1  = cropland presence
#       NA = no data / no cropland (depending on how the mask was created)
#   - Output is saved with a storage-efficient format (Byte) and GeoTIFF
#     compression to reduce file size.
# Notes
#   - The output keeps NA values as NoData (not converted to 0).
#   - "INT1U" means 8-bit unsigned integer (0–255), ideal for masks.
# =============================================================================

library(terra)

# -----------------------------------------------------------------------------
# 1) Read input rasters (two adjacent tiles)
# -----------------------------------------------------------------------------
r1 <- rast("Cropland_Mask_KANSAS-0000000000-0000000000.tif")
r2 <- rast("Cropland_Mask_KANSAS-0000000000-0000023296.tif")

# -----------------------------------------------------------------------------
# 2) Mosaic tiles into a single raster
# -----------------------------------------------------------------------------
m <- mosaic(r1, r2, fun = "max")

# -----------------------------------------------------------------------------
# 3) Cast to integer to match the intended output datatype (Byte)
# -----------------------------------------------------------------------------
# This ensures the output is written as integer values (1 and NA),
# compatible with an 8-bit mask.
m <- as.int(m)

# -----------------------------------------------------------------------------
# 4) Write output with compression to reduce file size
# -----------------------------------------------------------------------------
# Key options:
#   - datatype="INT1U": store as 8-bit unsigned integer (Byte)
#   - COMPRESS=DEFLATE: strong lossless compression
#   - PREDICTOR=2: improves compression for integer rasters
#   - ZLEVEL=9: maximum DEFLATE compression level (smaller, slower)
#   - TILED=YES: tiled GeoTIFF (better IO; can help compression workflows)
writeRaster(
  m,
  "Cropland_Mask_KANSAS.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",  # Byte (0–255); NA retained as NoData
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
  )
)

# =============================================================================
# End of script
# =============================================================================

# =============================================================================
# Mosaic ESA Landcover tiles (Kansas) and write a compact GeoTIFF
# =============================================================================
# Purpose
#   - Merge (mosaic) two adjacent raster tiles that together cover Kansas.
#   - Landcover classes are categorical:
# | Value | Class                    | Color (hex) |
# | ----: | ------------------------ | ----------- |
# |    10 | Tree cover               | `#006400`   |
# |    20 | Shrubland                | `#ffbb22`   |
# |    30 | Grassland                | `#ffff4c`   |
# |    40 | Cropland                 | `#f096ff`   |
# |    50 | Built-up                 | `#fa0000`   |
# |    60 | Bare / sparse vegetation | `#b4b4b4`   |
# |    70 | Snow and ice             | `#f0f0f0`   |
# |    80 | Permanent water bodies   | `#0064c8`   |
# |    90 | Herbaceous wetland       | `#0096a0`   |
# |    95 | Mangroves                | `#00cf75`   |
# |   100 | Moss and lichen          | `#fae6a0`   |
  
#   - Output is saved with a storage-efficient format (Byte) and GeoTIFF
#     compression to reduce file size.
# Notes
#   - The output keeps NA values as NoData (not converted to 0).
#   - "INT1U" means 8-bit unsigned integer (0–255), ideal for masks.
# =============================================================================

library(terra)

# -----------------------------------------------------------------------------
# 1) Read input rasters (two adjacent tiles)
# -----------------------------------------------------------------------------
r1 <- rast("ESA_WorldCover_2021_KANSAS-0000000000-0000000000.tif")
r2 <- rast("ESA_WorldCover_2021_KANSAS-0000000000-0000065536.tif")

# -----------------------------------------------------------------------------
# 2) Mosaic tiles into a single raster
# -----------------------------------------------------------------------------
m <- mosaic(r1, r2, fun = "max")

# -----------------------------------------------------------------------------
# 3) Cast to integer to match the intended output datatype (Byte)
# -----------------------------------------------------------------------------
# This ensures the output is written as integer values (1 and NA),
# compatible with an 8-bit mask.
m <- as.int(m)

# -----------------------------------------------------------------------------
# 4) Write output with compression to reduce file size
# -----------------------------------------------------------------------------
# Key options:
#   - datatype="INT1U": store as 8-bit unsigned integer (Byte)
#   - COMPRESS=DEFLATE: strong lossless compression
#   - PREDICTOR=2: improves compression for integer rasters
#   - ZLEVEL=9: maximum DEFLATE compression level (smaller, slower)
#   - TILED=YES: tiled GeoTIFF (better IO; can help compression workflows)
writeRaster(
  m,
  "ESA_WorldCover_2021_KANSAS.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",  # Byte (0–255); NA retained as NoData
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
  )
)

# =============================================================================
# End of script
# =============================================================================

