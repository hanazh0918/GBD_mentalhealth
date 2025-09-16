# =============================================================================
# File: 12_make_figures_sdi_incidence_mixed_effects.R
# Title: SDI × GBD Incidence (Rate) — Mixed-effects spline models (children/adolescents, <20y)
# Purpose:
#   - Read and tidy SDI (1990–2021), harmonize region names
#   - Merge SDI with GBD 204-country incidence rates for four causes (CD/ADHD/Anxiety/Depression)
#   - Fit mixed-effects models with region random intercepts:
#       * linear: val ~ SDI + Year + (1|Region)
#       * spline: val ~ rcs(SDI,3) + rcs(Year,3) + (1|Region)
#   - Plot predicted incidence–SDI curves per cause with 95% CI ribbons
#   - Assemble a 2×2 panel and export to Results/
#
# Inputs:
#   - conduct_204, ADHD_204, Anxiety_204, Depressive_204 (columns: location, year, age, sex, metric, measure, val, ...)
#   - SDI1990-2021.csv  (columns: Location, 1990, ..., 2021) — remove extra title lines before reading
#
# Outputs:
#   - f1, f2, f3, f4: ggplot objects (per-cause SDI–incidence curves)
#   - combined_plot: 2×2 patchwork figure
#   - Results/Figure_SDI_mixed_effects_Incidence.tiff
#
# Notes:
#   - rcs() (from rms) needs options(datadist=...) set on the modeling data.
#   - Rates must be positive for sensible log/spline behavior.
#   - Keep 'metric == "Rate"' and 'measure == "Incidence"' filters consistent.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lme4)
  library(rms)
  library(ggeffects)
  library(patchwork)
})

# Ensure output folder exists
dir.create("Results", showWarnings = FALSE, recursive = TRUE)

# (Optional) number formatting
options(scipen = 200)

# --------------------------- (A) SDI: read & tidy ---------------------------
# Update path as needed
SDI <- read.csv("/Users/hanazh/900_Article/maomao_2025/0.data/1_Conduct disorder/SDI1990-2021.csv")

# Wide -> long; tolerate headers like X1990
SDI <- SDI %>%
  tidyr::pivot_longer(-Location, names_to = "year", values_to = "SDI") %>%
  mutate(year = as.integer(gsub("[^0-9]", "", year))) %>%
  rename(location = Location)

# Harmonize region names (match your GBD naming)
SDI$location[SDI$location == "Eastern sub-Saharan Africa"]  <- "Eastern Sub-Saharan Africa"
SDI$location[SDI$location == "Central sub-Saharan Africa"]  <- "Central Sub-Saharan Africa"
SDI$location[SDI$location == "Western sub-Saharan Africa"]  <- "Western Sub-Saharan Africa"
SDI$location[SDI$location == "Southern sub-Saharan Africa"] <- "Southern Sub-Saharan Africa"

# --------------------------- (B) Helper: fit & plot one panel ---------------------------
fit_panel_incidence <- function(dat, panel_title) {
  # Filter to Incidence rate for <20y, Both
  d <- subset(dat,
              sex == "Both" &
                age == "<20 years" &
                metric == "Rate" &
                measure == "Incidence")[, c("location", "year", "val")]
  
  # Join SDI
  d <- merge(d, SDI, by = c("location", "year"))
  d$Region <- as.factor(d$location)
  d$Year   <- as.numeric(d$year)
  d$SDI    <- as.numeric(d$SDI)
  
  # Required by rms::rcs
  dd <- datadist(d); options(datadist = "dd")
  
  # Linear vs spline mixed models (random intercept for Region)
  m_lin <- lmer(val ~ SDI + Year + (1 | Region), data = d, REML = TRUE)
  m_spl <- lmer(val ~ rcs(SDI, 3) + rcs(Year, 3) + (1 | Region), data = d, REML = TRUE)
  
  # Print LRT for reference (spline vs linear)
  print(anova(m_lin, m_spl))
  
  # Predicted curve over SDI
  pd <- ggpredict(m_spl, terms = "SDI [all]", type = "fixed")
  
  ggplot(pd, aes(x = x, y = predicted)) +
    geom_line(color = "#636363", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "#636363", alpha = 0.25) +
    labs(
      x = "SDI",
      y = "Incidence rate (per 100,000)",
      title = panel_title,
      subtitle = ""
    ) +
    theme_classic()
}

# --------------------------- (C) Fit four causes ---------------------------
# These objects are assumed to be loaded in your session:
#   conduct_204, ADHD_204, Anxiety_204, Depressive_204
f1 <- fit_panel_incidence(conduct_204,    "(A) CD")
f2 <- fit_panel_incidence(ADHD_204,       "(B) ADHD")
f3 <- fit_panel_incidence(Anxiety_204,    "(C) Anxiety")
f4 <- fit_panel_incidence(Depressive_204, "(D) Depression")

# --------------------------- (D) Assemble & export ---------------------------
combined_plot <- (f1 | f2) / (f3 | f4)
combined_plot

ggsave("Results/Figure_SDI_mixed_effects_Incidence.tiff",
       plot = combined_plot, width = 8, height = 8, dpi = 300)
