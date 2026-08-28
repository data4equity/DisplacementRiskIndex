# Manuscript tables regenerated from the pooled DRI and the spatial BeltLine flag.
#
# Produces:
#   outputs/tables_2026/Table2_Descriptives.html        (+ .csv)
#   outputs/tables_2026/SuppTables_1-3_Medians.html     (+ .csv per table)
# Table 2: mean indicator values for the City of Atlanta, targeted (n = 91), and
# non-targeted (n = 225) block groups by period, with period-over-period and
# total changes. Supplemental Tables 1-3: median indicator values by trajectory
# class (vulnerability, housing market, and displacement risk trajectories
# respectively), stratified by targeted vs. non-targeted, with percent change
# 2000 to 2012-2016 ("NE" where the 2000 median is zero).
# Convert to Word with:  textutil -convert docx <file>.html
# Run from the project root: Rscript scripts/14_Tables.R

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})

out_dir <- "outputs/tables_2026"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

org <- read_csv(
  if (file.exists("outputs/DRI_origpcts.csv")) "outputs/DRI_origpcts.csv"
  else "outputs/DRI_dataset_origpcts.csv", show_col_types = FALSE) %>%
  mutate(GEOID = as.character(GEOID))
pooled <- read_csv("outputs/DRI_pooled_dataset.csv", show_col_types = FALSE) %>%
  mutate(GEOID = as.character(GEOID))

# trajectory classes per block group (same rules as 11_Pooled_DRI.R / 13_Figures.R)
code3 <- c(Low = 1, Moderate = 2, High = 3)
classify_traj <- function(c00, c11, c16) {
  v <- code3[c(c00, c11, c16)]
  if (length(unique(v)) == 1) return(paste("Persistently", c00))
  d <- diff(v)
  if (all(d >= 0)) return("Increasing")
  if (all(d <= 0)) return("Decreasing")
  "Fluctuating"
}
traj <- pooled %>%
  select(GEOID, S_Year, Vuln_Cat_pooled, House_Cat_pooled, DR_Cat_pooled) %>%
  pivot_wider(names_from = S_Year,
              values_from = c(Vuln_Cat_pooled, House_Cat_pooled, DR_Cat_pooled)) %>%
  rowwise() %>%
  mutate(Vuln_Traj  = classify_traj(Vuln_Cat_pooled_2000, Vuln_Cat_pooled_2011, Vuln_Cat_pooled_2016),
         House_Traj = classify_traj(House_Cat_pooled_2000, House_Cat_pooled_2011, House_Cat_pooled_2016),
         DR_Traj    = classify_traj(DR_Cat_pooled_2000, DR_Cat_pooled_2011, DR_Cat_pooled_2016)) %>%
  ungroup() %>% select(GEOID, Vuln_Traj, House_Traj, DR_Traj)

org <- left_join(org, traj, by = "GEOID")

# indicator definitions: code name, manuscript label, format
fmt_pct <- function(x) ifelse(is.na(x), "", sprintf("%.2f%%", x * 100))
fmt_dol <- function(x) ifelse(is.na(x), "", paste0("$", formatC(round(x), format = "d", big.mark = ",")))
fmt_num <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt_pp  <- function(x) ifelse(is.na(x), "", sprintf("%+.2f%%", x * 100))   # percentage-point change
fmt_dld <- function(x) ifelse(is.na(x), "", paste0(ifelse(x >= 0, "+$", "-$"),
                                                   formatC(abs(round(x)), format = "d", big.mark = ",")))
fmt_dnum <- function(x) ifelse(is.na(x), "", sprintf("%+.3f", x))

sv_vars <- tribble(
  ~var, ~label, ~fmt,
  "P_Black", "African American/Black", "pct",
  "P_Hispanic", "Hispanic/Latinx", "pct",
  "P_AIAN", "American Indian and Alaskan Native", "pct",
  "P_ASIAN", "Asian", "pct",
  "P_NHPI", "Native Hawaiian and Other Pacific Islander", "pct",
  "P_Elderly", "Elderly", "pct",
  "P_Single", "Single-parent householders", "pct",
  "P_LEP", "Speak English less than “very well”", "pct",
  "P_HSorLess", "HS Diploma or equivalent or less", "pct",
  "P_Renter", "Renters", "pct",
  "P_RentCostBurden", "Housing Cost-Burdened Renters", "pct",
  "P_Own_Cost_Burden", "Housing Cost-Burdened Homeowners", "pct",
  "P_Severe_RentCostBurden", "Housing Severely-Cost-Burdened Renters", "pct",
  "P_Severe_OwnCostBurden", "Housing Severely-Cost-Burdened Homeowners", "pct",
  "P_Poverty", "Household Income Below Poverty", "pct")
hm_vars <- tribble(
  ~var, ~label, ~fmt,
  "CHU", "Change in Subsidized Units", "num",
  "P_Expire", "Expiring Subsidized Units", "pct",
  "MHV", "Median Home Value (2016 Dollars)", "dol",
  "MGR", "Median Gross Rent (2016 Dollars)", "dol",
  "R_Crime", "Crime Rate (per capita)", "num",
  "R_Evict", "Residential Eviction Rate", "num",
  "R_File", "Residential Eviction Filing Rate", "num",
  "Eligible_FR", "Students Eligible for Free or Reduced Lunch", "pct",
  "P_Vacant", "Residential Vacancy Rate", "pct",
  "LIC", "Low Income Next to High Income", "pct",
  "MHHI", "Median Household Income (2016 Dollars)", "dol")
all_vars <- bind_rows(sv_vars, hm_vars)

fmt_val <- function(x, f) switch(f, pct = fmt_pct(x), dol = fmt_dol(x), num = fmt_num(x))
fmt_del <- function(x, f) switch(f, pct = fmt_pp(x),  dol = fmt_dld(x), num = fmt_dnum(x))

html_head <- "<meta charset='utf-8'><style>
body{font-family:'Times New Roman',serif;font-size:10pt}
table{border-collapse:collapse;margin-bottom:24px}
caption{text-align:left;font-weight:bold;margin-bottom:6px}
th,td{border:1px solid #999;padding:2px 6px;text-align:right;white-space:nowrap}
th{background:#f2f2f2} td:first-child,th:first-child{text-align:left}
h2{font-size:11pt} h3{font-size:10.5pt;margin:14px 0 4px}
</style>"

# ---- Table 2 ------------------------------------------------------------------
grp_means <- function(dd) {
  dd %>% group_by(S_Year) %>%
    summarise(across(all_of(all_vars$var), ~ mean(., na.rm = TRUE)), .groups = "drop")
}
t2_block <- function(dd) {
  m <- grp_means(dd)
  get <- function(v, yr) m[[v]][m$S_Year == yr]
  all_vars %>% rowwise() %>% mutate(
    y00 = fmt_val(get(var, 2000), fmt),
    y11 = fmt_val(get(var, 2011), fmt),
    y16 = fmt_val(get(var, 2016), fmt),
    d1  = fmt_del(get(var, 2011) - get(var, 2000), fmt),
    d2  = fmt_del(get(var, 2016) - get(var, 2011), fmt),
    dt  = fmt_del(get(var, 2016) - get(var, 2000), fmt)) %>% ungroup() %>%
    select(label, y00, y11, y16, d1, d2, dt)
}
city <- t2_block(org)
tgt  <- t2_block(org %>% filter(belt_flag == "Beltline"))
non  <- t2_block(org %>% filter(belt_flag == "Not Beltline"))
t2 <- city %>%
  left_join(tgt, by = "label", suffix = c("", ".t")) %>%
  left_join(non, by = "label", suffix = c("", ".n"))

hdr_cols <- c("2000", "2007-2011", "2012-2016",
              "Δ00→0711", "Δ0711→1216", "ΔTotal")
t2_html <- paste0(
  html_head,
  "<table><caption>Table 2. Indicator Means Stratified by the City of Atlanta and Targeted and Non-targeted Block Groups for BeltLine Redevelopment (2000, 2007-2011, and 2012-2016)</caption>",
  "<tr><th></th><th colspan='6'>City of Atlanta Block Groups (n = 316)</th>",
  "<th colspan='6'>BeltLine Targeted Block Groups (n = 91)</th>",
  "<th colspan='6'>Non-Targeted Block Groups (n = 225)</th></tr>",
  "<tr><th>Indicator</th>", paste(rep(paste0("<th>", hdr_cols, "</th>", collapse = ""), 3), collapse = ""), "</tr>",
  paste(apply(t2, 1, function(r) paste0("<tr><td>", paste(r, collapse = "</td><td>"), "</td></tr>")), collapse = "\n"),
  "</table>")
writeLines(t2_html, file.path(out_dir, "Table2_Descriptives.html"))
write_csv(t2, file.path(out_dir, "Table2_Descriptives.csv"))
cat("wrote Table2\n")

# ---- Supplemental Tables 1-3 --------------------------------------------------
supp_table <- function(traj_col, vars_tbl, title, file_stub) {
  traj_order <- c("Persistently High", "Persistently Moderate", "Persistently Low",
                  "Fluctuating", "Increasing", "Decreasing")
  sections <- lapply(traj_order, function(tr) {
    med_block <- function(dd) {
      m <- dd %>% group_by(S_Year) %>%
        summarise(across(all_of(vars_tbl$var), ~ median(., na.rm = TRUE)), .groups = "drop")
      get <- function(v, yr) { x <- m[[v]][m$S_Year == yr]; if (length(x) == 0) NA else x }
      vars_tbl %>% rowwise() %>% mutate(
        y00 = fmt_val(get(var, 2000), fmt),
        y11 = fmt_val(get(var, 2011), fmt),
        y16 = fmt_val(get(var, 2016), fmt),
        chg = { a <- get(var, 2000); b <- get(var, 2016)
                if (is.na(a) || is.na(b)) "" else if (a == 0) "NE"
                else sprintf("%+.0f%%", (b - a) / abs(a) * 100) }) %>%
        ungroup() %>% select(label, y00, y11, y16, chg)
    }
    tt <- med_block(org %>% filter(.data[[traj_col]] == tr, belt_flag == "Beltline"))
    nn <- med_block(org %>% filter(.data[[traj_col]] == tr, belt_flag == "Not Beltline"))
    n_t <- org %>% filter(.data[[traj_col]] == tr, belt_flag == "Beltline") %>% distinct(GEOID) %>% nrow()
    n_n <- org %>% filter(.data[[traj_col]] == tr, belt_flag == "Not Beltline") %>% distinct(GEOID) %>% nrow()
    rows <- left_join(tt, nn, by = "label", suffix = c(".t", ".n"))
    paste0("<h3>", tr, "</h3><table>",
           "<tr><th></th><th colspan='4'>BeltLine Targeted Block Groups (n = ", n_t, ")</th>",
           "<th colspan='4'>Non-Targeted Block Groups (n = ", n_n, ")</th></tr>",
           "<tr><th>Indicator</th>",
           paste(rep("<th>2000</th><th>2007-2011</th><th>2012-2016</th><th>% Change</th>", 2), collapse = ""),
           "</tr>",
           paste(apply(rows, 1, function(r) paste0("<tr><td>", paste(r, collapse = "</td><td>"), "</td></tr>")), collapse = "\n"),
           "</table>")
  })
  html <- paste0(html_head, "<h2>", title, "</h2>", paste(unlist(sections), collapse = "\n"))
  writeLines(html, file.path(out_dir, paste0(file_stub, ".html")))
  cat("wrote", file_stub, "\n")
}
supp_table("Vuln_Traj", sv_vars,
  "Supplemental Table 1. Median Social Vulnerability Risk Indicator Values by Vulnerability Trajectory, Stratified by Targeted and Non-targeted Block Groups (2000, 2007-2011, and 2012-2016)",
  "SuppTable1_Vuln_Medians")
supp_table("House_Traj", hm_vars,
  "Supplemental Table 2. Median Housing Market Risk Indicator Values by Housing Market Trajectory, Stratified by Targeted and Non-targeted Block Groups (2000, 2007-2011, and 2012-2016)",
  "SuppTable2_Housing_Medians")
supp_table("DR_Traj", all_vars,
  "Supplemental Table 3. Median Indicator Values by Displacement Risk Trajectory, Stratified by Targeted and Non-targeted Block Groups (2000, 2007-2011, and 2012-2016)",
  "SuppTable3_DRI_Medians")
cat("Done. Tables in", out_dir, "\n")
