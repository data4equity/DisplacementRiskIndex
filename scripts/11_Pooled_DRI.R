# Pooled-standardization Displacement Risk Index (DRI) for Atlanta block groups.
#
# Unlike scripts/01_Master_Datasets.R, which z-scores, rescales, and assigns
# quartiles separately within 2000, 2007-2011, and 2012-2016, this script pools
# all block-group-period observations before each of those steps. The resulting
# index scores and quartile categories are therefore comparable across periods,
# making trajectory classifications meaningful.
#
# This script also corrects two index-composition defects in 01_Master_Datasets.R:
# MHHI was included in both Index_Vuln and Index_Housing for 2011 and 2016, but
# only in Index_Housing for 2000; here it belongs only to Index_Housing, as in
# manuscript Table 1. P_HSorLess was rescaled but omitted from Index_Vuln; here
# it is included, also matching Table 1.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scales)
})

input_file <- if (file.exists("outputs/DRI_origpcts.csv")) {
  "outputs/DRI_origpcts.csv"
} else {
  "outputs/DRI_dataset_origpcts.csv"
}

if (!file.exists(input_file)) {
  stop(
    "Missing outputs/DRI_origpcts.csv. Run scripts/01_Master_Datasets.R first to produce it.",
    call. = FALSE
  )
}

vuln_vars <- c(
  "P_Black", "P_Hispanic", "P_AIAN", "P_ASIAN", "P_NHPI", "P_Elderly",
  "P_Single", "P_LEP", "P_HSorLess", "P_Renter", "P_RentCostBurden",
  "P_Own_Cost_Burden", "P_Severe_RentCostBurden", "P_Severe_OwnCostBurden",
  "P_Poverty"
)
hous_vars <- c(
  "P_Expire", "CHU", "MHV", "MGR", "R_Crime", "R_Evict", "R_File",
  "Eligible_FR", "P_Vacant", "LIC", "MHHI"
)
rev_vars <- c("MHV", "MGR", "MHHI", "CHU")
all_vars <- c(vuln_vars, hous_vars)

dri <- read.csv(input_file, check.names = FALSE)
required_vars <- c("GEOID", "belt_flag", "S_Year", all_vars)
missing_vars <- setdiff(required_vars, names(dri))

if (length(missing_vars) > 0) {
  stop(
    "The input dataset is missing required columns: ",
    paste(missing_vars, collapse = ", "),
    call. = FALSE
  )
}

if (anyNA(dri[all_vars])) {
  stop("The input indicator columns contain missing values after imputation.", call. = FALSE)
}

# Z-score and rescale every indicator over all block-group-period observations.
pooled <- dri
pooled[all_vars] <- lapply(
  pooled[all_vars],
  function(x) as.numeric(scale(x))
)

for (indicator in setdiff(all_vars, rev_vars)) {
  pooled[[indicator]] <- rescale(pooled[[indicator]], to = c(0, 100))
}
for (indicator in rev_vars) {
  pooled[[indicator]] <- rescale(pooled[[indicator]], to = c(100, 0))
}

pooled <- pooled %>%
  mutate(
    Index_Vuln_pooled = rowSums(across(all_of(vuln_vars))),
    Index_Housing_pooled = rowSums(across(all_of(hous_vars))),
    Index_DR_pooled = Index_Vuln_pooled + Index_Housing_pooled,
    Index_Q_Vuln_pooled = ntile(Index_Vuln_pooled, 4),
    Index_Q_Housing_pooled = ntile(Index_Housing_pooled, 4),
    Index_Q_DR_pooled = ntile(Index_DR_pooled, 4),
    Vuln_Cat_pooled = case_when(
      Index_Q_Vuln_pooled == 1 ~ "Low",
      Index_Q_Vuln_pooled %in% c(2, 3) ~ "Moderate",
      Index_Q_Vuln_pooled == 4 ~ "High"
    ),
    House_Cat_pooled = case_when(
      Index_Q_Housing_pooled == 1 ~ "Low",
      Index_Q_Housing_pooled %in% c(2, 3) ~ "Moderate",
      Index_Q_Housing_pooled == 4 ~ "High"
    ),
    DR_Cat_pooled = case_when(
      Index_Q_DR_pooled == 1 ~ "Low",
      Index_Q_DR_pooled %in% c(2, 3) ~ "Moderate",
      Index_Q_DR_pooled == 4 ~ "High"
    )
  )

category_codes <- c(Low = 1, Moderate = 2, High = 3)

classify_trajectory <- function(categories) {
  if (length(categories) != 3) {
    stop("Each block group must have exactly three periods for trajectory classification.")
  }

  codes <- unname(category_codes[categories])
  if (length(unique(codes)) == 1) {
    return(paste("Persistently", categories[[1]]))
  }

  changes <- diff(codes)
  if (all(changes >= 0)) {
    return("Increasing")
  }
  if (all(changes <= 0)) {
    return("Decreasing")
  }

  "Fluctuating"
}

trajectories <- pooled %>%
  arrange(GEOID, S_Year) %>%
  group_by(GEOID, belt_flag) %>%
  summarise(
    DR_Trajectory_pooled = classify_trajectory(DR_Cat_pooled),
    .groups = "drop"
  )

pooled_output <- pooled %>%
  select(
    GEOID, belt_flag, S_Year,
    Index_Vuln_pooled, Index_Housing_pooled, Index_DR_pooled,
    Index_Q_Vuln_pooled, Index_Q_Housing_pooled, Index_Q_DR_pooled,
    Vuln_Cat_pooled, House_Cat_pooled, DR_Cat_pooled
  )

write.csv(pooled_output, "outputs/DRI_pooled_dataset.csv", row.names = FALSE)
write.csv(trajectories, "outputs/DRI_pooled_trajectories.csv", row.names = FALSE)

cat("=== Pooled DRI by period ===\n")
print(
  pooled %>%
    group_by(S_Year) %>%
    summarise(
      mean = mean(Index_DR_pooled),
      sd = sd(Index_DR_pooled),
      min = min(Index_DR_pooled),
      max = max(Index_DR_pooled),
      .groups = "drop"
    )
)

# Pooled quartiles are not forced to 25/50/25 within each period; that is the point.
cat("\n=== Pooled DRI category counts by period ===\n")
print(table(pooled$S_Year, pooled$DR_Cat_pooled))

cat("\n=== Pooled DRI trajectory counts by BeltLine flag ===\n")
print(table(trajectories$belt_flag, trajectories$DR_Trajectory_pooled))

scaled_items <- pooled[all_vars]
k <- ncol(scaled_items)
total_score_variance <- var(rowSums(scaled_items))
cronbach_alpha <- (k / (k - 1)) *
  (1 - sum(vapply(scaled_items, var, numeric(1))) / total_score_variance)
cat(
  "\nCronbach alpha (26 pooled scaled indicators): ",
  round(cronbach_alpha, 3),
  "\n",
  sep = ""
)

published_file <- "outputs/DRI_dataset.csv"

if (file.exists(published_file)) {
  published <- read.csv(published_file, check.names = FALSE) %>%
    filter(GEOID != 131219800001) %>%
    select(GEOID, S_Year, DRI_Composite, DR_Cat)

  comparison <- pooled %>%
    select(GEOID, S_Year, Index_DR_pooled, DR_Cat_pooled) %>%
    inner_join(published, by = c("GEOID", "S_Year")) %>%
    mutate(DR_Cat = sub(" Displacement Risk$", "", as.character(DR_Cat)))

  cat("\n=== Published versus pooled DRI comparison by period ===\n")
  print(
    comparison %>%
      group_by(S_Year) %>%
      summarise(
        spearman_correlation = cor(
          DRI_Composite,
          Index_DR_pooled,
          method = "spearman",
          use = "complete.obs"
        ),
        percent_category_agreement = mean(DR_Cat == DR_Cat_pooled) * 100,
        .groups = "drop"
      )
  )
}
