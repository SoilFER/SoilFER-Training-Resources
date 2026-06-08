# Module 3 - Digital Soil Mapping: Modelling & Mapping
 
## 📋 Overview
 
This folder contains the **R script** for training a spatial prediction model for soil organic carbon (SOC) and generating wall-to-wall maps of the predicted mean and uncertainty using a **Quantile Regression Forest (QRF)**.
 
**⚠️ IMPORTANT: Script Execution Order**
 
The script is divided into two sessions that **MUST** be run in sequence:
 
1. **Session 1**: Data preparation, feature selection, model training, and accuracy assessment
2. **Session 2**: Spatial prediction (tiled) and mosaicking into final maps
**Total time**: ~2–6 hours (depending on study area size and CPU cores)
 
---
 
## 📁 Folder Structure

```
03_scripts/module3/
│
├── modelling_&_mapping_v2.R          # MAIN SCRIPT (Sessions 1 & 2)
├── Soil_property_classes_generator.R # Helper script
├── eval.RData                        # Custom accuracy evaluation function
└── README.md                         # This file
```



### R Packages
```r
tidyverse, caret, terra, Boruta, ranger, mapview
```

### Input Files

| File | Description |
|------|-------------|
| `01_data/module1/training_data/Environmental_Covariates_250m_KANSAS.tif` | Multi-layer raster stack of environmental covariates at 250 m resolution |
| `03_outputs/module1/KSSL_DSM_0-30.csv` | Soil point data with SOC, Sand, Clay, and coordinates (lon/lat) |
| `02_scripts/module3/eval.RData` | Custom accuracy evaluation function |

---

## Workflow

### Session 1

#### 1. Setup
- Sets working directory relative to the script location
- Creates output folders for models, validation, tiles, and maps
- Configures `terra` memory and temp directory settings

#### 2. Load Covariates
Loads the environmental covariate raster stack and inspects its properties
(resolution, extent, CRS, number of layers).

#### 3. Load and Transform Soil Data
- Reads soil point data (SOC, Sand, Clay, coordinates)
- Estimates **bulk density** using the Saxton pedotransfer function:

```
BD = 1.35 + 0.0045 * Sand + 0.0035 * Clay - 0.06 * 1.72 * SOC
```

#### 4. Merge Soil Data with Covariates
- Converts soil points to a spatial vector object
- Reprojects to match the covariate CRS
- Extracts covariate values at each sample location

#### 5. Feature Selection (Boruta)
- Runs the **Boruta** algorithm to identify relevant covariates
- Saves and plots variable importance
- Retains confirmed + tentative features (`withTentative = TRUE`)

#### 6. Model Training (QRF with `ranger`)
- Uses **repeated k-fold cross-validation** (5 folds × 5 repeats)
- Tunes `mtry` and `splitrule` hyperparameters via grid search
- Trains a **Quantile Regression Forest** (`quantreg = TRUE`) which enables
  uncertainty quantification in the prediction step
- Uses permutation-based variable importance

#### 7. Accuracy Assessment
- Extracts cross-validation predictions for the best hyperparameter combination
- Computes validation metrics (RMSE, R², bias, etc.) via the custom `eval()` function
- Saves a scatterplot of observed vs. predicted values
- Saves accuracy metrics to CSV

---

### Session 2

#### 8. Spatial Prediction (Tiled)
Prediction is done in tiles to manage memory for large rasters.

- Divides the study area into a **5 × 10 tile grid**
- For each tile, predicts:
  - **Conditional mean** — the best estimate of SOC
  - **Conditional standard deviation** — a measure of prediction uncertainty
- Tiles are saved individually as compressed GeoTIFFs

> **Note:** If visible seam artifacts appear between tiles, add a buffer
> when creating tiles (`buffer` argument in `makeTiles`) and use
> `mosaic(..., fun = "mean")` to blend overlapping areas.

#### 9. Mosaic Tiles
- Collects all mean and SD tiles
- Mosaics them into two final maps using `terra::mosaic()`
- Saves final maps as compressed GeoTIFFs

---

## Outputs

| File | Description |
|------|-------------|
| `03_outputs/module3/models/ranger_model_SOC.rds` | Trained QRF model |
| `03_outputs/module3/validation/accuracy.csv` | Cross-validation accuracy metrics |
| `03_outputs/module3/figures/boruta_SOC.png` | Boruta variable importance plot |
| `03_outputs/module3/figures/varImp_SOC.png` | Ranger variable importance plot |
| `03_outputs/module3/figures/scatterplot_SOC.png` | Observed vs. predicted scatterplot |
| `03_outputs/module3/maps/mean_SOC.tif` | Final predicted SOC mean map |
| `03_outputs/module3/maps/sd_SOC.tif` | Final predicted SOC uncertainty map |

---

## Editing for a New Target Variable

To apply this script to a different soil property, change the `target` variable:

```r
target <- "Clay"  # or "Sand", "BD", "pH", etc.
```

Make sure the target column exists in your soil data CSV.

---

## Notes

- Set `maxRuns > 100` in Boruta for a more robust feature selection in production runs
- Set `number` and `repeats` to 10–20 in `trainControl` for more reliable CV estimates
- The `num.threads` argument automatically uses all available CPU cores minus one
- Tile-based prediction is memory-efficient but can produce boundary artifacts —
  see the tiling note in Section 8 above