# Displacement Risk Index

This repository contains the R workflow for the Displacement Risk Index (DRI) for the City of Atlanta and the Atlanta BeltLine. The analysis covers 316 analytic census block groups in three periods: 2000 (Geolytics Census 2000 Long Form, normalized to 2010 boundaries), 2007–2011 ACS, and 2012–2016 ACS (both from IPUMS NHGIS).

The DRI combines a Social Vulnerability Risk Index (15 indicators) and a Housing Market Risk Index (11 indicators). Indicators are standardized and rescaled to 0–100, then summed with equal weights. Value, rent, income, and affordable-unit-change indicators are reversed so that lower values indicate higher risk. Dollar values are inflation-adjusted to 2016 dollars.

Additional sources include the National Housing Preservation Database, Atlanta BeltLine Inc., Atlanta Police Department crime data, Eviction Lab at Princeton University, and the National Center for Education Statistics school-poverty data. These data are geocoded or summarized to block groups.

## Running the workflow

Run scripts from the project root, which must contain `data_raw/` and `outputs/` directories. Raw data are not distributed with this repository.

1. Run `scripts/01_Master_Datasets.R`. It sources the supporting scripts, joins indicators, mean-imputes missing values, standardizes and rescales each period, builds indices, quartiles, and categories, and writes `outputs/DRI_dataset.csv` and `outputs/DRI_origpcts.csv`. This reproduces the published analysis using per-period standardization.
2. Run `scripts/11_Pooled_DRI.R`. It standardizes and assigns quartiles across all block-group-period observations, making scores and risk categories comparable over time. Use this version for longitudinal or trajectory analysis. It also aligns index composition with the published indicator tables.
3. Render `scripts/09_Visualizations.Rmd` for maps, distributions, alluvial trajectory diagrams, and trajectory maps; render `scripts/10_DRI_Analyses.Rmd` for correlations, spatial models, and global and local Moran's I (LISA).

`11_Pooled_DRI.R` expects `outputs/DRI_dataset_origpcts.csv`; `01_Master_Datasets.R` currently writes the similarly named `outputs/DRI_origpcts.csv`.

## Script guide

- `02_GEOS.R`: Atlanta block-group boundaries and BeltLine flag.
- `03_2000_data.R`, `04_07_11_data.R`, and `05_12_16_data.R`: period-specific indicator construction.
- `07_Schools.R`: school poverty, measured as the percentage receiving free or reduced-price lunch at the school nearest each block-group centroid.
- `08_Subs_Hsg.R`: subsidized housing (NHPD), crime, evictions, and the median-income-quintile adjacency flag (LIC).
- `09_Greenspace.R`: BeltLine parks and trails completion status.

Packages used include `tidyverse`, `sf`, `tigris`, `scales`, `ltm`, `spdep`, `tmap`, `readxl`, and `ggalluvial`.

## License

This project is dedicated to the public domain under [CC0 1.0 Universal](LICENSE).
