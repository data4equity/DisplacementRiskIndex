# Manuscript figures regenerated from the POOLED Displacement Risk Index.
#
# Produces Figures 1-18 (main text numbering) from outputs/DRI_pooled_dataset.csv
# and outputs/DRI_pooled_trajectories.csv, using the spatially derived BeltLine
# flag (block-group centroid within the 10 subareas; see 02_GEOS.R). Figures are
# written to outputs/figures_2026/ as PNG and 300-dpi TIFF (LZW).
#
# Styling: Figures 1 and 6-17 reproduce the manuscript's ORIGINAL visual
# language (RdBu index fills, Pastel1 categories, ColorBrewer Paired trajectory
# palette, default-hue alluvials and densities, composite + facet trajectory
# layouts) with the data updated. Figures 2-5 (new design, no original existed)
# and Figure 18 keep the 2026 treatment.
# Differences from the original 09_Visualizations.Rmd / 10_DRI_Analyses.Rmd:
#  - all index values, categories, and trajectories are the pooled versions,
#    comparable across 2000, 2007-2011, and 2012-2016;
#  - LISA clusters use the standard Anselin quadrants (value vs. mean crossed
#    with the SPATIAL LAG vs. mean). The original quadrant code centered the
#    local Moran statistic on its own mean and its Low-Low / Low-High labels
#    were swapped relative to what that computation yields;
#  - scale bars come from ggspatial (ggsn is archived on CRAN).
#
# Run from the project root: Rscript scripts/13_Figures.R [figure numbers...]
# With no arguments, all figures are rendered.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(readr)
  library(sf); library(patchwork); library(ggalluvial); library(ggspatial)
  library(spdep)
})
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
figs <- if (length(args) == 0) 1:18 else as.integer(args)
want <- function(n) any(n %in% figs)

fig_dir <- "outputs/figures_2026"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(plot, name, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), plot, width = width, height = height,
         dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), plot, width = width, height = height,
         dpi = 300, bg = "white", compression = "lzw")
  cat("wrote", name, "\n")
}

# ---- palettes -----------------------------------------------------------------
# Original manuscript palette: ColorBrewer Paired, in trajectory factor order.
pal_paired <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
pal_lisa <- c("High-High" = "#e31a1c", "High-Low" = "#fb9a99",
              "Low-High" = "#a6cee3", "Low-Low" = "#1f78b4",
              "Not significant" = "#f0f0f0")
col_completed <- "#ffae42"; col_slated <- "#33a02c"   # original GR overlay colors
col_belt_green <- "forestgreen"                        # original Figure 1 green
col_interstate <- "navy"

# original map theme: theme_minimal with graticule labels kept
theme_map_orig <- theme_minimal(base_size = 11) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        plot.title = element_text(hjust = 0.5))
# clean theme used by Figure 18 (kept from the 2026 treatment she approved)
theme_map <- theme_minimal(base_size = 11) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

# ---- geography and flag -------------------------------------------------------
bg <- st_read("data_raw/Geography/BG_Atlanta_City_Limits.shp", quiet = TRUE) %>%
  select(GEOID)
subareas <- st_read("data_raw/Beltline/subareas_shapefiles/beltline_subareas.shp",
                    quiet = TRUE)
bg_p  <- st_transform(bg, 26916)
sub_p <- st_make_valid(st_transform(subareas, 26916))
bg$belt_flag <- ifelse(
  lengths(st_within(st_centroid(st_geometry(bg_p)), st_union(sub_p))) > 0,
  "BeltLine", "Not BeltLine")
bg316 <- bg %>% filter(GEOID != "131219800001")
sub_outline <- st_transform(st_union(sub_p), st_crs(bg))
city_outline <- st_union(bg316)
belt_footprint <- st_union(bg316 %>% filter(belt_flag == "BeltLine"))
expr_city <- st_read("data_raw/Expressways highways/Expressways_Atlanta_Region.shp",
                     quiet = TRUE) %>%
  st_transform(st_crs(bg)) %>%
  st_intersection(st_buffer(city_outline, 0.01))

# ---- pooled index data --------------------------------------------------------
pooled <- read_csv("outputs/DRI_pooled_dataset.csv", show_col_types = FALSE) %>%
  mutate(GEOID = as.character(GEOID))
stopifnot(nrow(pooled) == 948)

period_lab <- c("2000" = "2000", "2011" = "2007-2011", "2016" = "2012-2016")
cat_levels <- c("High", "Moderate", "Low")

wide <- pooled %>%
  select(GEOID, S_Year, Index_Vuln_pooled, Index_Housing_pooled, Index_DR_pooled,
         Vuln_Cat_pooled, House_Cat_pooled, DR_Cat_pooled) %>%
  pivot_wider(names_from = S_Year,
              values_from = c(Index_Vuln_pooled, Index_Housing_pooled, Index_DR_pooled,
                              Vuln_Cat_pooled, House_Cat_pooled, DR_Cat_pooled))

code3 <- c(Low = 1, Moderate = 2, High = 3)
classify_traj <- function(c00, c11, c16) {
  v <- code3[c(c00, c11, c16)]
  if (length(unique(v)) == 1) return(paste("Persistently", c00))
  d <- diff(v)
  if (all(d >= 0)) return("Increasing")
  if (all(d <= 0)) return("Decreasing")
  "Fluctuating"
}
traj_levels <- c("Persistently High", "Persistently Moderate", "Persistently Low",
                 "Increasing", "Decreasing", "Fluctuating")

wide <- wide %>% rowwise() %>%
  mutate(Vuln_Traj  = classify_traj(Vuln_Cat_pooled_2000, Vuln_Cat_pooled_2011, Vuln_Cat_pooled_2016),
         House_Traj = classify_traj(House_Cat_pooled_2000, House_Cat_pooled_2011, House_Cat_pooled_2016),
         DR_Traj    = classify_traj(DR_Cat_pooled_2000, DR_Cat_pooled_2011, DR_Cat_pooled_2016)) %>%
  ungroup() %>%
  mutate(across(c(Vuln_Traj, House_Traj, DR_Traj), ~ factor(., levels = traj_levels)),
         across(starts_with("Vuln_Cat_pooled"), ~ factor(., levels = cat_levels)),
         across(starts_with("House_Cat_pooled"), ~ factor(., levels = cat_levels)),
         across(starts_with("DR_Cat_pooled"), ~ factor(., levels = cat_levels)))

map_data <- left_join(bg316, wide, by = "GEOID")
belt_map <- map_data %>% filter(belt_flag == "BeltLine")

# ---- greenspace project overlays ---------------------------------------------
# Completed / slated BeltLine park and trail improvements, per block group, as
# used in 09_Visualizations.Rmd (including its hardcoded 2007-2011 corrections
# for the West End and Northside trails).
parks_0711 <- st_read("data_raw/Parks/Atlanta_BeltLine_Trails_Parks_07-11.shp", quiet = TRUE)
parks_1219 <- st_read("data_raw/Parks/Atlanta_BeltLine_Trails_Parks_12-19.shp", quiet = TRUE)

completed_0711_ids <- union(
  parks_0711$GEOID[parks_0711$Year_ %in% c(2008, 2010)],
  c("131210062001", "131210042002", "131210041003",   # West End Trail (2008)
    "131210091011", "131210090003", "131210091012"))  # Northside Trail (2010)
slated_0711_ids <- parks_1219$GEOID[parks_1219$Year_ %in% c(2012, 2017, 2019)]

completed_1216_ids <- parks_1219$GEOID[
  parks_1219$Year_ == 2012 | parks_1219$Year == 2013 |
    grepl("Allene Urban Farm", parks_1219$ABI_NAME)]
slated_1216_ids <- parks_1219$GEOID[parks_1219$Year_ %in% c(2017, 2019)]

overlay_sf <- function(completed_ids, slated_ids, slated_label) {
  bg316 %>%
    mutate(Parks_Trails = case_when(GEOID %in% slated_ids ~ slated_label,
                                    GEOID %in% completed_ids ~ "Completed")) %>%
    filter(!is.na(Parks_Trails))
}
green_0711 <- overlay_sf(completed_0711_ids, slated_0711_ids, "Slated")
green_1216 <- overlay_sf(completed_1216_ids, slated_1216_ids, "Slated")
pal_green <- c(Completed = col_completed, Slated = col_slated)

scalebar_layer <- annotation_scale(location = "bl", width_hint = 0.25,
                                   height = unit(0.15, "cm"), text_cex = 0.6,
                                   unit_category = "imperial")

# ==============================================================================
# Figure 1 - study area (original two-panel style: subarea footprint, then
# targeted block groups, both forest green with navy interstates)
# ==============================================================================
if (want(1)) {
  expr_lab <- expr_city %>% filter(!is.na(Label)) %>%
    group_by(Label) %>% slice(1) %>% ungroup()

  f1_left <- ggplot() +
    geom_sf(data = city_outline, fill = NA, color = "gray40", linewidth = 0.3) +
    geom_sf(data = st_make_valid(st_transform(subareas, st_crs(bg))),
            fill = col_belt_green, color = col_belt_green) +
    geom_sf(data = expr_city, color = col_interstate, linewidth = 0.5) +
    geom_sf_label(data = expr_lab, aes(label = Label), size = 2.6,
                  label.padding = unit(0.14, "lines")) +
    ggtitle("City of Atlanta and Atlanta BeltLine Boundaries") +
    scalebar_layer + coord_sf(default_crs = sf::st_crs(4269)) + theme_map_orig

  f1_right <- ggplot() +
    geom_sf(data = bg316, fill = NA, color = "gray70", linewidth = 0.15) +
    geom_sf(data = bg316 %>% filter(belt_flag == "BeltLine"),
            fill = col_belt_green, color = "gray85", linewidth = 0.15) +
    geom_sf(data = expr_city, color = col_interstate, linewidth = 0.5) +
    geom_sf_label(data = expr_lab, aes(label = Label), size = 2.6,
                  label.padding = unit(0.14, "lines")) +
    ggtitle("Targeted vs. Non-Targeted Block Groups") +
    scalebar_layer + coord_sf(default_crs = sf::st_crs(4269)) + theme_map_orig

  f1 <- f1_left + f1_right
  save_fig(f1, "Fig01_StudyArea", 13, 7.5)
}

# ==============================================================================
# Figures 2-5 - "typical" (median) targeted block group per DR trajectory
# ==============================================================================
if (want(2) || want(3) || want(4) || want(5)) {
  org <- read_csv(
    if (file.exists("outputs/DRI_origpcts.csv")) "outputs/DRI_origpcts.csv"
    else "outputs/DRI_dataset_origpcts.csv", show_col_types = FALSE) %>%
    mutate(GEOID = as.character(GEOID))
  org <- org %>%
    left_join(wide %>% select(GEOID, DR_Traj), by = "GEOID") %>%
    left_join(st_drop_geometry(bg316) %>% select(GEOID, flag2 = belt_flag), by = "GEOID")

  pct_vars <- c(P_Black = "Black", P_Hispanic = "Hispanic", P_AIAN = "AIAN",
                P_ASIAN = "Asian", P_NHPI = "NHPI", P_Elderly = "Elderly",
                P_Single = "Single-parent", P_LEP = "Limited English",
                P_HSorLess = "HS diploma or less", P_Renter = "Renters",
                P_RentCostBurden = "Rent cost-burdened",
                P_Own_Cost_Burden = "Owner cost-burdened",
                P_Severe_RentCostBurden = "Severely rent-burdened",
                P_Severe_OwnCostBurden = "Severely owner-burdened",
                P_Poverty = "Below poverty", P_Vacant = "Vacant units",
                Eligible_FR = "Students free/reduced lunch")
  dol_vars <- c(MHV = "Median home value", MGR = "Median gross rent",
                MHHI = "Median household income")
  rate_vars <- c(R_Crime = "Crimes per capita", R_Evict = "Eviction rate",
                 R_File = "Eviction filing rate", CHU = "Change in subsidized units",
                 P_Expire = "Expiring subsidies share")

  typical_fig <- function(traj, fig_name) {
    sub <- org %>% filter(flag2 == "BeltLine", DR_Traj == traj)
    n_bg <- length(unique(sub$GEOID))
    med <- sub %>% group_by(S_Year) %>%
      summarise(across(all_of(c(names(pct_vars), names(dol_vars), names(rate_vars))),
                       ~ median(., na.rm = TRUE)), .groups = "drop") %>%
      mutate(Period = factor(period_lab[as.character(S_Year)], levels = period_lab))

    shades <- setNames(c("#9fc4bd", "#4f9b94", "#15494A"), period_lab)

    dotplot <- function(vars, labels, xlab, pct = FALSE) {
      d <- med %>% select(Period, all_of(vars)) %>%
        pivot_longer(-Period, names_to = "var", values_to = "value") %>%
        mutate(lab = factor(labels[var], levels = rev(unname(labels))))
      if (pct) d$value <- d$value * 100
      ggplot(d, aes(value, lab, color = Period, shape = Period)) +
        geom_line(aes(group = lab), color = "gray75", linewidth = 0.4) +
        geom_point(size = 2.4) +
        scale_color_manual(values = shades) +
        scale_shape_manual(values = c(16, 17, 15)) +
        labs(x = xlab, y = NULL) +
        theme_minimal(base_size = 10) +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank())
    }

    # dollar and rate indicators span very different magnitudes, so each gets
    # its own x scale via one facet per indicator
    facet_dots <- function(vars, labels, xlab, dollars = FALSE) {
      d <- med %>% select(Period, all_of(vars)) %>%
        pivot_longer(-Period, names_to = "var", values_to = "value") %>%
        mutate(lab = factor(labels[var], levels = unname(labels)))
      ggplot(d, aes(value, y = "", color = Period, shape = Period)) +
        geom_line(aes(group = lab), color = "gray75", linewidth = 0.4) +
        geom_point(size = 2.4) +
        facet_wrap(~lab, scales = "free_x", ncol = 1, strip.position = "top") +
        scale_color_manual(values = shades) +
        scale_shape_manual(values = c(16, 17, 15)) +
        {if (dollars) scale_x_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale()))
         else scale_x_continuous(n.breaks = 4)} +
        labs(x = xlab, y = NULL) +
        theme_minimal(base_size = 10) +
        theme(panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.text.y = element_blank(),
              strip.text = element_text(hjust = 0, face = "bold", size = 8.5))
    }

    p1 <- dotplot(names(pct_vars), pct_vars, "Median value (%)", pct = TRUE)
    p2 <- facet_dots(names(dol_vars), dol_vars, "Median value (2016 $)", dollars = TRUE)
    p3 <- facet_dots(names(rate_vars), rate_vars, "Median rate")
    fig <- p1 + (p2 / p3) +
      plot_layout(widths = c(1.35, 1), guides = "collect") +
      plot_annotation(
        title = paste0("The typical ", tolower(traj),
                       " displacement-risk block group targeted for BeltLine redevelopment"),
        subtitle = paste0("Median indicator values across the ", n_bg,
                          " targeted block groups classified ", traj,
                          " (pooled index), 2000 to 2012-2016"),
        theme = theme(plot.title = element_text(face = "bold", size = 13))) &
      theme(legend.position = "bottom")
    save_fig(fig, fig_name, 11, 7.5)
  }

  if (want(2)) typical_fig("Persistently High", "Fig02_Typical_PersistentlyHigh")
  if (want(3)) typical_fig("Persistently Low",  "Fig03_Typical_PersistentlyLow")
  if (want(4)) typical_fig("Increasing",        "Fig04_Typical_Increasing")
  if (want(5)) typical_fig("Decreasing",        "Fig05_Typical_Decreasing")
}

# ==============================================================================
# Figure 6 - index distributions, targeted vs non-targeted, by period
# ==============================================================================
if (want(6)) {
  # Original style: theme_classic line densities, default hue pair, six panels
  dist_d <- pooled %>%
    left_join(st_drop_geometry(bg316) %>% select(GEOID, flag2 = belt_flag), by = "GEOID")

  dens_panel <- function(yr, var, xlab, xlim, title = NULL) {
    p <- ggplot(dist_d %>% filter(S_Year == yr)) +
      geom_density(aes(x = .data[[var]], colour = flag2)) +
      lims(x = xlim) +
      labs(x = xlab, colour = "Location") +
      theme_classic() +
      theme(legend.position = "bottom")
    if (!is.null(title))
      p <- p + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    p
  }
  f6 <- dens_panel(2000, "Index_Vuln_pooled", "Vulnerability Index", c(0, 700), "2000") +
    dens_panel(2011, "Index_Vuln_pooled", "Vulnerability Index", c(0, 700), "2007 - 2011") +
    dens_panel(2016, "Index_Vuln_pooled", "Vulnerability Index", c(0, 700), "2012 - 2016") +
    dens_panel(2000, "Index_Housing_pooled", "Housing Market Index", c(150, 700)) +
    dens_panel(2011, "Index_Housing_pooled", "Housing Market Index", c(150, 700)) +
    dens_panel(2016, "Index_Housing_pooled", "Housing Market Index", c(150, 700)) +
    plot_layout(ncol = 3, nrow = 2, guides = "collect") &
    theme(legend.position = "bottom", legend.title = element_blank())
  save_fig(f6, "Fig06_Index_Distributions", 12, 6.5)
}

# ==============================================================================
# Figures 7, 8, 15 - BeltLine index maps (values + categories) over time
# ==============================================================================
# Original style: top row RdBu index values with fixed breaks and
# Lowest/Highest labels; bottom row Pastel1 categories; mile scale bars.
index_map_fig <- function(idx_prefix, cat_prefix, legend_title, cat_title,
                          lim, brks, brk_labs, fig_name) {
  panel_val <- function(yr, overlay = NULL, show_legend = FALSE) {
    p <- ggplot() +
      geom_sf(data = belt_map,
              aes(fill = .data[[paste0(idx_prefix, "_pooled_", yr)]]),
              color = "gray60") +
      scale_fill_distiller(palette = "RdBu", breaks = brks, labels = brk_labs,
                           limits = lim, name = legend_title)
    if (!is.null(overlay))
      p <- p + geom_sf(data = overlay, aes(color = Parks_Trails), fill = NA,
                       linewidth = 1.1) +
        scale_color_manual(values = pal_green,
                           name = "Park and Trail \nImprovements")
    p + ggtitle(period_lab[as.character(yr)]) + scalebar_layer +
      coord_sf(default_crs = sf::st_crs(4269)) +
      scale_x_continuous(breaks = c(-84.46, -84.42, -84.38, -84.34)) +
      theme_map_orig +
      theme(legend.position = if (show_legend) "right" else "none")
  }
  panel_cat <- function(yr, show_legend = FALSE) {
    ggplot() +
      geom_sf(data = belt_map,
              aes(fill = .data[[paste0(cat_prefix, "_pooled_", yr)]]),
              color = "gray60") +
      scale_fill_brewer(palette = "Pastel1", name = cat_title, drop = FALSE) +
      scalebar_layer + coord_sf(default_crs = sf::st_crs(4269)) +
      scale_x_continuous(breaks = c(-84.46, -84.42, -84.38, -84.34)) +
      theme_map_orig +
      theme(legend.position = if (show_legend) "right" else "none")
  }

  fig <- (panel_val(2000) + panel_val(2011, green_0711) +
            panel_val(2016, green_1216, show_legend = TRUE)) /
    (panel_cat(2000) + panel_cat(2011) + panel_cat(2016, show_legend = TRUE))
  save_fig(fig, fig_name, 12, 8)
}
if (want(7))  index_map_fig("Index_Vuln", "Vuln_Cat", "Vulnerability Index",
                            "Vulnerability Categories", c(0, 750), c(0, 250, 500, 750),
                            c("0 Lowest", "250", "500", "750 Highest"), "Fig07_Vuln_Maps")
if (want(8))  index_map_fig("Index_Housing", "House_Cat", "Housing Market Index",
                            "Housing Market Categories", c(0, 750), c(0, 250, 500, 750),
                            c("0 Lowest", "250", "500", "750 Highest"), "Fig08_Housing_Maps")
if (want(15)) index_map_fig("Index_DR", "DR_Cat", "Displacement Risk Index",
                            "DRI Categories", c(0, 1500), c(0, 300, 600, 900, 1200, 1500),
                            c("Lowest", "300", "600", "900", "1200", "Highest"), "Fig15_DRI_Maps")

# ==============================================================================
# Figures 9, 10 - alluvial diagrams of category change
# ==============================================================================
# Original style: default ggplot hue palette, "... Vulnerability"-suffixed
# trajectory labels, stratum boxes labeled Low/Mod/High, years 2000/2011/2016.
traj_display <- c("Persistently High", "Persistently Moderate", "Persistently Low",
                  "Increasing Vulnerability", "Decreasing Vulnerability",
                  "Fluctuating Vulnerability")

alluvial_fig <- function(cat_prefix, traj_col, legend_title, fig_name) {
  d <- st_drop_geometry(map_data) %>%
    select(GEOID, belt_flag,
           S_2000 = paste0(cat_prefix, "_pooled_2000"),
           S_2011 = paste0(cat_prefix, "_pooled_2011"),
           S_2016 = paste0(cat_prefix, "_pooled_2016"),
           Trajectory = all_of(traj_col)) %>%
    mutate(Trajectory = factor(traj_display[as.integer(Trajectory)],
                               levels = traj_display))

  one <- function(dd, xlab) {
    agg <- dd %>% count(S_2000, S_2011, S_2016, Trajectory, name = "Frequency")
    ggplot(agg, aes(axis1 = S_2000, axis2 = S_2011, axis3 = S_2016, y = Frequency)) +
      geom_alluvium(aes(fill = Trajectory)) +
      geom_stratum() +
      geom_text(stat = "stratum",
                aes(label = after_stat(
                  c(High = "High", Moderate = "Mod", Low = "Low")[as.character(stratum)]))) +
      scale_x_discrete(limits = c("2000", "2011", "2016")) +
      scale_fill_discrete(name = legend_title, drop = FALSE) +
      labs(x = xlab, y = "Frequency") +
      theme_minimal(base_size = 11)
  }
  fig <- one(d, "City of Atlanta") +
    one(d %>% filter(belt_flag == "BeltLine"),
        "Block groups targeted for\nBeltLine Redevelopment") +
    one(d %>% filter(belt_flag == "Not BeltLine"),
        "Block groups not targeted for\nBeltLine Redevelopment") +
    plot_layout(ncol = 3, guides = "collect")
  save_fig(fig, fig_name, 14, 6)
}
if (want(9))  alluvial_fig("Vuln_Cat", "Vuln_Traj", "Social Vulnerability Risk\nTrajectories", "Fig09_Vuln_Alluvial")
if (want(10)) alluvial_fig("House_Cat", "House_Traj", "Housing Market Risk\nTrajectories", "Fig10_Housing_Alluvial")

# ==============================================================================
# Figures 11-14, 16, 17 - trajectory maps
# ==============================================================================
# Original style: left composite map (Paired palette, mile scale bar, graticule)
# plus right 2x3 facet grid of per-trajectory small multiples over a gray base
# with the BeltLine footprint shaded and navy interstates.
traj_map_fig <- function(traj_col, title, fig_name, beltline_only = FALSE,
                         suffix_vuln = TRUE) {
  d <- if (beltline_only) belt_map else map_data
  labs6 <- if (suffix_vuln) traj_display else levels(map_data[[traj_col]])
  d <- d %>% mutate(TrajDisp = factor(labs6[as.integer(.data[[traj_col]])],
                                      levels = labs6))
  base <- if (beltline_only) belt_map else bg316

  composite <- ggplot() +
    geom_sf(data = d, aes(fill = TrajDisp, color = TrajDisp), show.legend = FALSE) +
    scale_color_manual(values = pal_paired, drop = FALSE) +
    scale_fill_manual(values = pal_paired, drop = FALSE) +
    scalebar_layer + coord_sf(default_crs = sf::st_crs(4269)) + theme_map_orig

  facets <- ggplot() +
    geom_sf(data = base, color = "gray80", lwd = 0.15, fill = NA) +
    geom_sf(data = d, aes(fill = TrajDisp, color = TrajDisp), alpha = 0.95,
            show.legend = FALSE) +
    {if (!beltline_only)
      geom_sf(data = belt_footprint, lwd = 0.4, fill = "gray60",
              colour = "gray30", alpha = 0.3)} +
    {if (!beltline_only)
      geom_sf(data = city_outline, color = "gray40", fill = NA)} +
    geom_sf(data = expr_city, color = col_interstate, linewidth = 0.35) +
    scale_color_manual(values = pal_paired, drop = FALSE) +
    scale_fill_manual(values = pal_paired, drop = FALSE) +
    ggtitle(title) +
    coord_sf(datum = NA) +
    theme_minimal() +
    facet_wrap(~TrajDisp, labeller = label_wrap_gen(width = 12), drop = FALSE)

  fig <- (composite + facets) &
    theme(plot.title = element_text(size = 18, hjust = 0.5))
  save_fig(fig, fig_name, 15, 8)
}
if (want(11)) traj_map_fig("Vuln_Traj",  "Social Vulnerability Risk Trajectories", "Fig11_Vuln_Traj_City")
if (want(12)) traj_map_fig("House_Traj", "Housing Market Risk Trajectories", "Fig12_Housing_Traj_City")
if (want(13)) traj_map_fig("Vuln_Traj",  "Vulnerability Trajectories", "Fig13_Vuln_Traj_BeltLine",  beltline_only = TRUE)
if (want(14)) traj_map_fig("House_Traj", "Housing Market Trajectories", "Fig14_Housing_Traj_BeltLine", beltline_only = TRUE)
if (want(16)) traj_map_fig("DR_Traj",    "Displacement Risk Trajectories", "Fig16_DRI_Traj_City", suffix_vuln = FALSE)
if (want(17)) traj_map_fig("DR_Traj",    "Displacement Risk Trajectories", "Fig17_DRI_Traj_BeltLine", beltline_only = TRUE, suffix_vuln = FALSE)

# ==============================================================================
# Figure 18 - global Moran's I and LISA clusters, pooled DRI, by period
# ==============================================================================
if (want(18)) {
  nb <- poly2nb(map_data, row.names = map_data$GEOID, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  lisa_panel_data <- function(yr) {
    x <- st_drop_geometry(map_data)[[paste0("Index_DR_pooled_", yr)]]
    gm <- moran.test(x, lw, zero.policy = TRUE)
    cat(sprintf("Global Moran's I, pooled DRI %s: %.3f (p = %.3g)\n",
                period_lab[as.character(yr)], gm$estimate[1], gm$p.value))
    li <- localmoran(x, lw, zero.policy = TRUE)
    z <- x - mean(x)
    lag <- lag.listw(lw, x); lagc <- lag - mean(lag)
    cluster <- case_when(li[, 5] > 0.05 ~ "Not significant",
                         z > 0 & lagc > 0 ~ "High-High",
                         z < 0 & lagc < 0 ~ "Low-Low",
                         z < 0 & lagc > 0 ~ "Low-High",
                         z > 0 & lagc < 0 ~ "High-Low")
    map_data %>% mutate(cluster = factor(cluster, levels = names(pal_lisa)))
  }

  lisa_panel <- function(yr, overlay = NULL, show_legend = FALSE) {
    d <- lisa_panel_data(yr)
    p <- ggplot() +
      geom_sf(data = d, aes(fill = cluster), color = "gray75", linewidth = 0.12) +
      scale_fill_manual(values = pal_lisa,
                        name = "LISA cluster\n(p < 0.05)", drop = FALSE)
    if (!is.null(overlay))
      p <- p + geom_sf(data = overlay, aes(color = Parks_Trails), fill = NA,
                       linewidth = 0.8) +
        scale_color_manual(values = pal_green, name = "Greenspace\nprojects")
    p + ggtitle(period_lab[as.character(yr)]) + scalebar_layer + theme_map +
      theme(legend.position = if (show_legend) "right" else "none")
  }

  f18 <- lisa_panel(2000) + lisa_panel(2011, green_0711) +
    lisa_panel(2016, green_1216, show_legend = TRUE) + plot_layout(ncol = 3)
  save_fig(f18, "Fig18_LISA", 15, 6.5)
}

cat("Done. Figures in", fig_dir, "\n")
