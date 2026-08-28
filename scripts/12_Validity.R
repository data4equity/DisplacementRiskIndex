# Concurrent and discriminant validity analysis for the Displacement Risk Index
# (DRI), 2012-2016, at the census-tract level.
#
# Concurrent validity is the correlation with the CDC/ATSDR Social Vulnerability
# Index 2016 overall tract percentile ranking (RPL_THEMES), downloaded from the
# official CDC feature service; a high positive correlation supports concurrent
# validity. Discriminant validity is the correlation with median year structure
# built (ACS 2012-2016 B25035, from the Census summary file), a construct
# theoretically distinct from displacement risk, so a low correlation supports
# discriminant validity. Per the manuscript, DRI is aggregated from block groups
# to tracts by population-weighted mean; missing validity values are mean-imputed;
# and all measures are rescaled 0-1 before correlating.

suppressPackageStartupMessages({
  library(dplyr)
})

pooled_file <- "outputs/DRI_pooled_dataset.csv"
published_file <- "outputs/DRI_dataset.csv"
population_file <- "data_raw/NHGIS/nhgis0017_ds225_20165_2016_blck_grp.csv"
svi_file <- "data_raw/SVI/SVI2016_GEORGIA_tract.csv"
acs_file <- "data_raw/ACS/B25035_medyrbuilt_2012_2016_tracts.csv"
output_file <- "outputs/DRI_validity_2016.csv"

required_files <- c(pooled_file, published_file, population_file, svi_file, acs_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required input file(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
}

weighted_tract_mean <- function(values, weights) {
  valid_weights <- !is.na(values) & !is.na(weights) & weights > 0

  if (any(valid_weights)) {
    return(weighted.mean(values[valid_weights], w = weights[valid_weights]))
  }

  mean(values, na.rm = TRUE)
}

rescale01 <- function(x) {
  minimum <- min(x, na.rm = TRUE)
  maximum <- max(x, na.rm = TRUE)

  if (is.infinite(minimum) || is.infinite(maximum)) {
    return(rep(NA_real_, length(x)))
  }
  if (minimum == maximum) {
    return(ifelse(is.na(x), NA_real_, 0))
  }

  (x - minimum) / (maximum - minimum)
}

print_correlation <- function(label, x, y) {
  pearson <- cor.test(x, y, method = "pearson")
  spearman <- cor.test(x, y, method = "spearman", exact = FALSE)

  cat("\n=== ", label, " ===\n", sep = "")
  cat("n = ", length(x), "\n", sep = "")
  cat("Pearson r = ", unname(pearson$estimate), "\n", sep = "")
  cat(
    "95% CI = [", pearson$conf.int[[1]], ", ", pearson$conf.int[[2]], "]\n",
    sep = ""
  )
  cat("Pearson p = ", pearson$p.value, "\n", sep = "")
  cat("Spearman rho = ", unname(spearman$estimate), "\n", sep = "")
  print(pearson)
  print(spearman)
}

pooled_dri <- read.csv(pooled_file, check.names = FALSE) %>%
  filter(as.character(S_Year) == "2016") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, Index_DR_pooled)

population <- read.csv(population_file, colClasses = "character") %>%
  transmute(
    GEOID = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA),
    population = as.numeric(AF2UE001)
  )

pooled_with_population <- pooled_dri %>%
  left_join(population, by = "GEOID")

message(
  "Block groups without matched population: ",
  sum(is.na(pooled_with_population$population)),
  " (expected 0)"
)

dri_tract <- pooled_with_population %>%
  mutate(tract_id = substr(GEOID, 1, 11)) %>%
  group_by(tract_id) %>%
  summarise(
    DRI_tract = weighted_tract_mean(Index_DR_pooled, population),
    .groups = "drop"
  )

published_dri <- read.csv(published_file, check.names = FALSE) %>%
  filter(as.character(S_Year) == "2016") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  filter(GEOID != "131219800001") %>%
  select(GEOID, DRI_Composite)

published_tract <- pooled_with_population %>%
  select(GEOID, population) %>%
  inner_join(published_dri, by = "GEOID") %>%
  mutate(tract_id = substr(GEOID, 1, 11)) %>%
  group_by(tract_id) %>%
  summarise(
    DRI_published_tract = weighted_tract_mean(DRI_Composite, population),
    .groups = "drop"
  )

svi <- read.csv(svi_file, colClasses = "character", check.names = FALSE) %>%
  transmute(
    tract_id = as.character(FIPS),
    RPL_THEMES = as.numeric(RPL_THEMES),
    RPL_THEMES = ifelse(RPL_THEMES < 0, NA_real_, RPL_THEMES)
  )

median_year_built <- read.csv(acs_file, check.names = FALSE) %>%
  transmute(
    tract_id = as.character(GEOID),
    med_yr_built = as.numeric(med_yr_built)
  )

validity <- dri_tract %>%
  inner_join(published_tract, by = "tract_id") %>%
  inner_join(svi, by = "tract_id") %>%
  inner_join(median_year_built, by = "tract_id")

message("Final tract count: ", nrow(validity))

validity <- validity %>%
  mutate(
    RPL_THEMES = ifelse(
      is.na(RPL_THEMES),
      mean(RPL_THEMES, na.rm = TRUE),
      RPL_THEMES
    ),
    med_yr_built = ifelse(
      is.na(med_yr_built),
      mean(med_yr_built, na.rm = TRUE),
      med_yr_built
    ),
    DRI_tract_01 = rescale01(DRI_tract),
    DRI_published_tract_01 = rescale01(DRI_published_tract),
    RPL_THEMES_01 = rescale01(RPL_THEMES),
    med_yr_built_01 = rescale01(med_yr_built)
  )

print_correlation(
  "Pooled DRI vs SVI (concurrent validity)",
  validity$DRI_tract_01,
  validity$RPL_THEMES_01
)
print_correlation(
  "Pooled DRI vs median year built (discriminant validity)",
  validity$DRI_tract_01,
  validity$med_yr_built_01
)
print_correlation(
  "SVI vs median year built (between-validity-measures reference)",
  validity$RPL_THEMES_01,
  validity$med_yr_built_01
)
print_correlation(
  "Published DRI vs SVI (manuscript comparability)",
  validity$DRI_published_tract_01,
  validity$RPL_THEMES_01
)
print_correlation(
  "Published DRI vs median year built (manuscript comparability)",
  validity$DRI_published_tract_01,
  validity$med_yr_built_01
)

write.csv(validity, output_file, row.names = FALSE)
