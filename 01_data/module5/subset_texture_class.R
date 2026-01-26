# Subset soil data and estimate USDA texture class
# Author: Generated for SoilFER Training
# Date: 2025-01-25

library(dplyr)
library(aqp)          # For texture classification (ssc_to_texcl)
library(soiltexture)  # For texture triangle plotting

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the harmonized data
data <- read.csv("harmonized.csv")

# Subset: keep only 0-30 cm depth and select relevant columns
subset_data <- data %>%
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

# Apply texture classification using aqp package
subset_data$Texture_Class <- ssc_to_texcl(
  sand = subset_data$Sand,
  clay = subset_data$Clay
)

# Display summary
cat("=== Subset Data Summary ===\n")
cat("Number of profiles:", nrow(subset_data), "\n\n")

cat("Texture class distribution:\n")
print(table(subset_data$Texture_Class, useNA = "ifany"))

cat("\n=== First 10 rows ===\n")
print(head(subset_data, 10))

# Save to CSV
output_file <- "harmonized_subset_texture.csv"
write.csv(subset_data, output_file, row.names = FALSE)

cat("\n\nFile saved as:", output_file, "\n")

# --- Plot texture triangle using soiltexture package ---

# Prepare data for soiltexture package (requires CLAY, SILT, SAND column names)
# Normalize to 100% since some samples may not sum exactly to 100
texture_data <- subset_data %>%
  filter(!is.na(Clay) & !is.na(Silt) & !is.na(Sand)) %>%
  mutate(
    total = Clay + Silt + Sand,
    CLAY = (Clay / total) * 100,
    SILT = (Silt / total) * 100,
    SAND = (Sand / total) * 100
  ) %>%
  select(CLAY, SILT, SAND)

# Create texture triangle plot
png("texture_triangle_plot.png", width = 800, height = 800, res = 120)

TT.plot(
  class.sys = "USDA.TT",
  tri.data = texture_data,
  main = "USDA Soil Texture Classification\n(0-30 cm depth)",
  col = "blue",
  pch = 19,
  cex = 0.8,
  lwd = 1.5,
  class.lab.col = "darkgray",
  class.line.col = "darkgray"
)

dev.off()

cat("Texture triangle plot saved as: texture_triangle_plot.png\n")
