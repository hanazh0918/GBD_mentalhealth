
# =============================================================================
# File: 12_make_figures_fig4_sdi_cd.R
# Title: Figure 4 (A) — Association between SDI and CD prevalence rate (Global, <20y)
# Purpose:
#   1) Read SDI (1950–2021 or 1990–2021) and reshape to long format (location, year, SDI)
#   2) Merge SDI with GBD prevalence rate for Conduct Disorder (CD), Global regions, <20 years
#   3) Compute Pearson correlation (R and 95% CI) and visualize scatter + LOESS trend
#
# Inputs:
#   - "SDI1990-2021.csv" with columns: Location, 1990, 1991, ..., 2021
#   - conduct_27: data.frame with columns {location, year, age, sex, metric, measure, val, upper, lower}
#       * age == "<20 years", sex == "Both", metric == "Rate", measure == "Prevalence"
#
# Output objects:
#   - f4_a: ggplot object (scatter of SDI vs prevalence rate, per 27 regions)
#
# Notes:
#   - Ensure the SDI CSV has a proper header row ("Location", years). If you downloaded
#     from GHDx and it contains extra title lines, remove those lines before reading.
#   - This plot colors points by GBD region (location). Shapes are kept constant to avoid
#     exceeding the 22-shape limit in ggplot.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# --------------------------- Read & tidy SDI ---------------------------
# SDI CSV should have columns: Location, 1990, 1991, ..., 2021
SDI <- readr::read_csv("SDI1990-2021.csv", show_col_types = FALSE)

# Wide -> long; year may be "1990" or "X1990" depending on how R read the header
SDI <- SDI %>%
  tidyr::pivot_longer(
    cols = -Location,
    names_to = "year",
    values_to = "SDI"
  ) %>%
  dplyr::mutate(
    year = as.integer(gsub("[^0-9]", "", year))  # drop any non-digits like "X"
  ) %>%
  dplyr::rename(location = Location)

# Harmonize a few region names to match GBD location naming used elsewhere
SDI$location[SDI$location == "Eastern sub-Saharan Africa"]  <- "Eastern Sub-Saharan Africa"
SDI$location[SDI$location == "Central sub-Saharan Africa"]  <- "Central Sub-Saharan Africa"
SDI$location[SDI$location == "Western sub-Saharan Africa"]  <- "Western Sub-Saharan Africa"
SDI$location[SDI$location == "Southern sub-Saharan Africa"] <- "Southern Sub-Saharan Africa"

# --------------------------- Prepare GBD data ---------------------------
data <- conduct_27 %>%
  dplyr::filter(
    sex    == "Both",
    age    == "<20 years",
    metric == "Rate",
    measure == "Prevalence"
  ) %>%
  dplyr::select(location, year, val)

# Merge with SDI on (location, year)
data_SDI <- dplyr::inner_join(data, SDI, by = c("location", "year"))

# --------------------------- Pearson correlation ---------------------------
cor_res <- cor.test(data_SDI$val, data_SDI$SDI, method = "pearson")
r_val   <- unname(cor_res$estimate)
ci_low  <- cor_res$conf.int[1]
ci_high <- cor_res$conf.int[2]
p_val   <- cor_res$p.value

p_label <- if (p_val < 0.001) "P < 0.001" else paste0("P = ", formatC(p_val, format = "f", digits = 3))
r_label <- paste0("R = ", round(r_val, 3), " (", round(ci_low, 3), ", ", round(ci_high, 3), ")")

# --------------------------- Figure 4 (A) ---------------------------
f4_a <- ggplot(data_SDI, aes(x = SDI, y = val)) +
  # Points colored by region (location); fixed shape to avoid >22 shapes limitation
  geom_point(aes(color = location), shape = 16, size = 2.5, alpha = 0.9) +
  # LOESS smooth (no CI ribbon)
  geom_smooth(color = "black", method = "loess", se = FALSE, span = 0.5) +
  labs(
    x = "Socio-demographic Index (SDI)",
    y = "Prevalence rate (per 100,000 population)",
    title = "(A) Conduct disorder"
  ) +
  # Annotations: place near the top-left; tweak x/y if overlapping
  annotate(
    "text",
    x = min(data_SDI$SDI, na.rm = TRUE) + 0.05,
    y = max(data_SDI$val, na.rm = TRUE),
    label = r_label, size = 4, color = "black", hjust = 0, vjust = 1
  ) +
  annotate(
    "text",
    x = min(data_SDI$SDI, na.rm = TRUE) + 0.05,
    y = max(data_SDI$val, na.rm = TRUE) * 0.95,
    label = p_label, size = 4, color = "black", hjust = 0, vjust = 1
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13),
    axis.text.x = element_text(angle = 0, hjust = 1),
    # Put legend on top; adjust spacing if many regions
    legend.position = "top",
    legend.key.height = grid::unit(6, "pt"),
    legend.key.width  = grid::unit(14, "pt"),
    plot.title = element_text(hjust = 0, vjust = 0, face = "bold", size = 18),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

f4_a
