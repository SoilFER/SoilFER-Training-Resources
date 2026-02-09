# Subset soil data and create pH and Clay x pH classes
# Author: Generated for SoilFER Training
# Date: 2025-01-25

library(dplyr)
library(ggplot2)      # For plotting

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

# --- Create ordinal pH variable ---
subset_data$pH_Class <- cut(
  subset_data$pH,
  breaks = c(4, 5, 6, 7, 8, 9),
  labels = c("very_acid", "acid", "neutral", "alkaline", "very_alkaline"),
  include.lowest = TRUE,
  ordered_result = TRUE
)

# --- Create nominal variable (pH x Clay segmentation) ---
# Clay categories: Low (<20%), Medium (20-40%), High (>40%)
# pH categories: Acid (<6), Neutral (6-7), Alkaline (>7)

subset_data <- subset_data %>%
  mutate(
    Clay_Category = case_when(
      Clay < 20 ~ "low_clay",
      Clay >= 20 & Clay < 40 ~ "medium_clay",
      Clay >= 40 ~ "high_clay",
      TRUE ~ NA_character_
    ),
    pH_Category = case_when(
      pH < 6 ~ "acid",
      pH >= 6 & pH < 7 ~ "neutral",
      pH >= 7 ~ "alkaline",
      TRUE ~ NA_character_
    ),
    Clay_pH_Class = case_when(
      !is.na(Clay_Category) & !is.na(pH_Category) ~ paste(Clay_Category, pH_Category, sep = "_"),
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Clay_Category, -pH_Category)

# Display summary
cat("=== Subset Data Summary ===\n")
cat("Number of profiles:", nrow(subset_data), "\n\n")

cat("pH class distribution:\n")
print(table(subset_data$pH_Class, useNA = "ifany"))

cat("\nClay x pH class distribution:\n")
print(table(subset_data$Clay_pH_Class, useNA = "ifany"))

cat("\n=== First 10 rows ===\n")
print(head(subset_data, 10))

# Save to CSV
output_file <- "harmonized_subset_data.csv"
write.csv(subset_data, output_file, row.names = FALSE)

cat("\n\nFile saved as:", output_file, "\n")

# --- Plot pH Class distribution (bar plot) ---
ggplot(subset_data %>% filter(!is.na(pH_Class)), aes(x = pH_Class)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "pH Class Distribution (0-30 cm)",
    x = "pH Class",
    y = "Count"
  ) +
  theme_minimal()

# --- Plot Clay x pH Class (scatter plot with categories) ---
ggplot(subset_data %>% filter(!is.na(Clay_pH_Class)), aes(x = Clay, y = pH, color = Clay_pH_Class)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_vline(xintercept = c(20, 40), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(6, 7), linetype = "dashed", color = "gray50") +
  labs(
    title = "Clay x pH Segmentation (0-30 cm)",
    x = "Clay (%)",
    y = "pH",
    color = "Class"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
