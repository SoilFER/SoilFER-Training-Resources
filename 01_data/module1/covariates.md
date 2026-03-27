# Covariates Overview

## 1. Climate Variables (CHELSA)

Source: [CHELSA Technical Specification V2](https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf)

### Temperature

| Description | Code | Resolution | Unit |
|---|---|---|---|
| Mean air temperature (annual) | `bio1` | 1000 m | °C |
| Mean daily temperature of warmest month | `bio5` | 1000 m | °C |
| Mean daily temperature of coldest month | `bio6` | 1000 m | °C |

### Precipitation

| Description | Code | Resolution | Unit |
|---|---|---|---|
| Total precipitation (annual) | `bio12` | 1000 m | kg m⁻² |
| Mean precipitation of wettest month | `bio13` | 1000 m | kg m⁻² month⁻¹ |
| Mean precipitation of driest month | `bio14` | 1000 m | kg m⁻² month⁻¹ |
| Mean monthly precipitation of wettest quarter | `bio16` | 1000 m | kg m⁻² month⁻¹ |
| Mean monthly precipitation of driest quarter | `bio17` | 1000 m | kg m⁻² month⁻¹ |

### Evapotranspiration (Penman PET)

| Description | Code | Resolution | Unit |
|---|---|---|---|
| Mean monthly PET | `pet_penman_mean` | 1000 m | kg m⁻² month⁻¹ |
| Minimum monthly PET | `pet_penman_min` | 1000 m | kg m⁻² month⁻¹ |
| Range monthly PET | `pet_penman_range` | 1000 m | kg m⁻² month⁻¹ |
| Maximum monthly PET | `pet_penman_max` | 1000 m | kg m⁻² month⁻¹ |

### Wind

| Description | Code | Resolution | Unit |
|---|---|---|---|
| Minimum monthly wind speed | `sfcWind_min` | 1000 m | m s⁻¹ |
| Maximum monthly wind speed | `sfcWind_max` | 1000 m | m s⁻¹ |
| Range monthly wind speed | `sfcWind_range` | 1000 m | m s⁻¹ |

### Growing Season

| Description | Code | Resolution | Unit |
|---|---|---|---|
| Number of days with mean daily air temperature > 10 °C | `ngd10` | 1000 m | Number of days |

---

## 2. MODIS Remote Sensing Variables

### Vegetation Indices (MOD13Q1 — 250 m)

Source: [LPDAAC MOD13Q1](https://lpdaac.usgs.gov/products/mod13q1v006/)

Seasonal means and standard deviations of NDVI from 2000–2022:

| Description | Code | Resolution |
|---|---|---|
| NDVI mean March–May | `ndvi_030405_250m_mean` | 250 m |
| NDVI mean June–August | `ndvi_060708_250m_mean` | 250 m |
| NDVI mean September–November | `ndvi_091011_250m_mean` | 250 m |
| NDVI mean December–February | `ndvi_120102_250m_mean` | 250 m |
| NDVI std dev March–May | `ndvi_030405_250m_sd` | 250 m |
| NDVI std dev June–August | `ndvi_060708_250m_sd` | 250 m |
| NDVI std dev September–November | `ndvi_091011_250m_sd` | 250 m |
| NDVI std dev December–February | `ndvi_120102_250m_sd` | 250 m |

### FPAR — Fraction of Photosynthetically Active Radiation (MOD15A2H — 500 m)

Source: [LPDAAC MOD15A2H](https://lpdaac.usgs.gov/products/mod15a2hv006/)

| Description | Code | Resolution |
|---|---|---|
| FPAR mean March–May | `fpar_030405_500m_mean` | 500 m |
| FPAR mean June–August | `fpar_060708_500m_mean` | 500 m |
| FPAR mean September–November | `fpar_091011_500m_mean` | 500 m |
| FPAR mean December–February | `fpar_120102_500m_mean` | 500 m |
| FPAR std dev March–May | `fpar_030405_500m_sd` | 500 m |
| FPAR std dev June–August | `fpar_060708_500m_sd` | 500 m |
| FPAR std dev September–November | `fpar_091011_500m_sd` | 500 m |
| FPAR std dev December–February | `fpar_120102_500m_sd` | 500 m |

### LST — Land Surface Temperature Day (MOD11A2 — 1000 m)

| Description | Code | Resolution |
|---|---|---|
| LST Day mean March–May | `lstd_030405_mean` | 1000 m |
| LST Day mean June–August | `lstd_060708_mean` | 1000 m |
| LST Day mean September–November | `lstd_091011_mean` | 1000 m |
| LST Day mean December–February | `lstd_120102_mean` | 1000 m |
| LST Day std dev March–May | `lstd_030405_sd` | 1000 m |
| LST Day std dev June–August | `lstd_060708_sd` | 1000 m |
| LST Day std dev September–November | `lstd_091011_sd` | 1000 m |
| LST Day std dev December–February | `lstd_120102_sd` | 1000 m |

### NDLST — Normalised Difference LST Day/Night (MOD11A2 — 1000 m)

| Description | Code | Resolution |
|---|---|---|
| NDLST mean March–May | `ndlst_030405_mean` | 1000 m |
| NDLST mean June–August | `ndlst_060708_mean` | 1000 m |
| NDLST mean September–November | `ndlst_091011_mean` | 1000 m |
| NDLST mean December–February | `ndlst_120102_mean` | 1000 m |
| NDLST std dev March–May | `ndlst_030405_sd` | 1000 m |
| NDLST std dev June–August | `ndlst_060708_sd` | 1000 m |
| NDLST std dev September–November | `ndlst_091011_sd` | 1000 m |
| NDLST std dev December–February | `ndlst_120102_sd` | 1000 m |

### SWIR — Black-Sky Albedo Shortwave Broadband (MCD43A3 — 500 m)

| Description | Code | Resolution |
|---|---|---|
| SWIR mean June–August | `swir_060708_500m_mean` | 500 m |

> Note: The CHELSA sheet also lists seasonal means and std devs for all four seasons (March–May, June–August, September–November, December–February).

### Snow Cover (MOD10A1 — 500 m)

Source: [NSIDC MOD10A1](https://nsidc.org/data/MOD10A1)

| Description | Code | Resolution |
|---|---|---|
| MODIS Snow Cover mean | `snow_cover` | 500 m |

---

## 3. Land Cover — Dynamic World (250 m)

Near-real-time (NRT) LULC dataset. Variables represent the mean estimated probability of complete coverage by each class:

| Class | Code | Resolution |
|---|---|---|
| Bare | `bare` | 250 m |
| Trees | `trees` | 250 m |
| Shrub and scrub | `shrub_and_scrub` | 250 m |
| Flooded vegetation | `flooded_vegetation` | 250 m |
| Grass | `grass` | 250 m |
| Crops | `crop` | 250 m |
| Snow and ice | `snow_and_ice` | 250 m |

---

## 4. Terrain (250 m)

| Description | Code | Resolution |
|---|---|---|
| Profile curvature | `curvature` | 250 m |
| Downslope curvature | `downslopecurvature` | 250 m |
| Upslope curvature | `upslopecurvature` | 250 m |
| Deviation from Mean Value | `dvm` | 250 m |
| Deviation from Mean Value (2) | `dvm2` | 250 m |
| Elevation | `elevation` | 250 m |
| Melton Ruggedness Number | `mrn` | 250 m |
| Negative openness | `neg-openness` | 250 m |
| Positive openness | `por-openness` | 250 m |
| Slope | `slope` | 250 m |
| Topographic Position Index | `tpi` | 250 m |
| Terrain Wetness Index | `twi` | 250 m |
| Multiresolution Valley Bottom Flatness | `vbf` | 250 m |

---

## 5. Population / Human Footprint (1000 m)

| Description | Code | Resolution |
|---|---|---|
| Global Terrestrial Human Footprint (2013) | `hfp2013_merisINT` | 1000 m |
| DMSP-OLS Nighttime Lights (2013) | `night_lights_stable_2013` | 1000 m |
| Global Human Settlement Layer (2020) | `population_density_2020` | 1000 m |

---

## 6. MODIS Product Reference Table

| Category | Product Name | TERRA | AQUA | Resolution | GEE Collection |
|---|---|---|---|---|---|
| Vegetation Indices | Vegetation Indices 16-Day L3 Global 250m | MOD13Q1 | MYD13Q1 | 250 m | `MODIS/006/MOD13Q1` |
| Vegetation Indices | Vegetation Indices 16-Day L3 Global 500m | MOD13A2 | MYD13A2 | 500 m | `MODIS/006/MOD13A1` |
| FPAR/LAI | LAI/FPAR 8-Day L4 Global | MOD15A2H | MYD15A2H | 500 m | — |
| GPP/NPP | Gross Primary Production 8-Day L4 Global 1km | MOD17A2 | MYD17A2 | 1 km | `UMT/NTSG/v2/MODIS/GPP` |
| GPP/NPP | Net Primary Production Yearly L4 Global 1km | MOD17A3 | MYD17A3 | 1 km | `MODIS/006/MOD17A3H` |
| Land Cover | Land Cover Type Yearly L3 Global 500m | MCD12Q1 | — | 500 m | `MODIS/006/MCD12Q1` |
| Albedo | BRDF-Albedo Model Parameters 16-Day L3 500m | MCD43A1 | — | 500 m | `MODIS/006/MCD43A1` |
| Surface Reflectance | Surface Reflectance 8-Day L3 Global 250m | MOD09Q1 | MYD09Q1 | 250 m | `MODIS/006/MOD09Q1` |
| Snow Cover | Snow Cover Daily L3 Global 500m | MOD10A1 | MYD10A1 | 500 m | `MODIS/006/MOD10A1` |
| Fires | Burned Area Monthly L3 Global 500m | MCD64A1 | — | 500 m | `MODIS/006/MCD64A1` |
| LST | LST/Emissivity Daily L3 Global 1km | MOD11A1 | MYD11A1 | 1 km | `MODIS/006/MOD11A1` |
| LST | LST/Emissivity 8-Day L3 Global 1km | MOD11A2 | MYD11A2 | 1 km | `MODIS/006/MOD11A2` |
| LST (new) | LST/Emissivity 8-Day L3 Global 1km (v21) | MOD21A2 | MYD21A2 | 1 km | — |
| Evapotranspiration | Net ET 8-Day L4 Global 500m | MOD16A2 | MYD16A2 | 500 m | — |
| Evapotranspiration | Net ET Yearly L4 Global 500m | MOD16A3 | MYD16A3 | 500 m | `MODIS/006/MOD16A2` |
