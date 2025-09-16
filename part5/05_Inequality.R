# =============================================================================
# File: inequality_sii_ci_cd_prevalence.R
# Title: Inequality metrics (SII & CI) for Conduct Disorder prevalence, <20y
# Purpose:
#   - Prepare GBD disease data (Conduct Disorder), SDI, and population data
#   - Compute Slope Index of Inequality (SII) using weighted relative rank by SDI
#   - Visualize SII with robust regression (rlm) vs linear (lm)
#   - Compute Concentration Index (CI) via trapezoidal integration of the Lorenz curve
#   - Visualize CI (Lorenz curve) for years 1990 and 2021
#
# Inputs (must be preloaded/available):
#   - conduct_204: GBD country-level data (columns include location, year, sex, age, measure, metric, val, etc.)
#   - sdi_2021.RData -> object: sdi_2021      (columns: location, year, sdi or SDI)
#   - pop_2021.RData -> object: pop_2021      (columns: location, sex, year, population)
#
# Outputs (in-memory objects):
#   - fa1: SII scatter + robust/linear fit (Rate)
#   - fa2: Lorenz curves + CI annotation (Number)
#   - Printed coefficients, CIs, heteroskedasticity tests, and CI by year
#
# Notes:
#   - Ensured year is treated as integer before filtering (avoids "1990" vs 1990 mismatch).
#   - Summations use na.rm = TRUE.
#   - If your SDI/population column names differ (e.g., SDI vs sdi), adjust below accordingly.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(mgcv)     # GAM for smooth Lorenz curve
  library(splines)  # ns()
  library(broom)
  library(car)      # ncvTest for heteroskedasticity
  library(MASS)     # rlm robust regression
})

# ---------------------------------------------------------------------
# Working directory (adjust to your environment)
# ---------------------------------------------------------------------
setwd("/Users/hanazh/900_Article/maomao_2025/GBD_MBD_Database2025/C2.1_inequality")
options(scipen = 200)

# =========================== (I) Conduct data =========================
data <- conduct_204
# Keep country-level, <20y, Both sexes (exclude Global)
CD <- subset(
  data,
  age == "<20 years" &
    sex == "Both" &
    location != "Global"
)

# =========================== (II) SDI data ============================
load("SDI官网下载数据库/sdi_2021.RData")  # provides sdi_2021
sdi <- sdi_2021

# Merge CD with SDI (by location, year)
data <- left_join(CD, sdi, by = c("location", "year"))

# =========================== (III) Population data ====================
load("POP官网下载数据库/pop_2021.RData")  # provides pop_2021
Population <- pop_2021

# Merge population with disease+SDI
data <- merge(
  data,
  Population,
  by = c("location", "sex", "year"),
  all.x = TRUE
)

# Filter to Prevalence and two target years
# Ensure year is integer to avoid character mismatch
if (!is.integer(data$year)) data$year <- as.integer(as.character(data$year))
data <- data %>%
  filter(
    measure == "Prevalence",
    year %in% c(1990, 2021)
  )

# =========================== (IV) Slope Index of Inequality (SII) ===========================
# (1) Data preparation
# Total population by year (for Rate only)
a <- data %>%
  filter(metric == "Rate") %>%
  group_by(year) %>%
  summarise(sum = sum(population, na.rm = TRUE), .groups = "drop")

# Expect first is 1990, second is 2021 (safe indexing with matching)
population1990 <- a$sum[a$year == 1990]
population2021 <- a$sum[a$year == 2021]

# Compute weighted relative rank by SDI (midpoint / global population)
rank <- data %>%
  mutate(pop_global = ifelse(year == 1990, population1990, population2021)) %>%
  group_by(year, metric) %>%
  arrange(sdi, .by_group = TRUE) %>%
  mutate(
    cummu = cumsum(population),          # cumulative population
    half = population / 2,               # half of a country's population
    midpoint = cummu - half,             # population midpoint
    weighted_order = midpoint / pop_global  # relative rank in [0,1]
  ) %>%
  ungroup()

# Keep Rate for SII fit, and split by year
rank$year <- factor(rank$year)
temp1 <- rank %>% filter(metric == "Rate", year == 1990)
temp2 <- rank %>% filter(metric == "Rate", year == 2021)

# Linear SII fits
fit1 <- lm(val ~ weighted_order, data = temp1)
fit2 <- lm(val ~ weighted_order, data = temp2)

# Coefficients and (Wald) CIs
print(coef(fit1)); print(confint.default(fit1))
print(coef(fit2)); print(confint.default(fit2))

# Heteroskedasticity tests (p < 0.05 suggests heteroskedasticity)
print(ncvTest(fit1))
print(ncvTest(fit2))

# Robust (Huber) regression as alternative
r.huber1 <- MASS::rlm(val ~ weighted_order, data = temp1)
r.huber2 <- MASS::rlm(val ~ weighted_order, data = temp2)

# Robust coefficients and (approximate) CIs
print(coef(r.huber1)); print(confint.default(r.huber1))
print(coef(r.huber2)); print(confint.default(r.huber2))

# (2) Visualization: SII scatter with robust smooth
rank_rate <- subset(rank, metric == "Rate")
color <- c("#43a2ca", "#B22222")  # 1990, 2021
fa1 <- ggplot(rank_rate, aes(x = weighted_order, y = val,
                             fill = factor(year), group = year, color = factor(year))) +
  geom_point(aes(color = year, size = population / 1e6),
             alpha = 0.8, shape = 21) +
  scale_size_area("Population\n(million)", breaks = c(200, 400, 600, 800, 1000, 1200)) +
  # If heteroskedasticity exists, rlm is suitable; else lm
  geom_smooth(method = "rlm", linewidth = 0.6, alpha = 0.1) +
  scale_color_manual(values = color, name = "Year") +
  scale_fill_manual(values = color,  name = "Year") +
  labs(
    title = "(A) Conduct Disorder",
    x = "Relative rank by SDI",
    y = "Prevalence rate (per 100,000)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = -0.2)
  )
fa1

# =========================== (V) Concentration Index (CI) ===========================
# (1) Data preparation for Lorenz curve and CI
# Recompute rank table (ensure consistency)
rank <- data %>%
  mutate(pop_global = ifelse(year == 1990, population1990, population2021)) %>%
  group_by(year, metric) %>%
  arrange(sdi, .by_group = TRUE) %>%
  mutate(
    cummu = cumsum(population),
    half = population / 2,
    midpoint = cummu - half,
    weighted_order = midpoint / pop_global
  ) %>%
  ungroup()

rank$year <- factor(rank$year)

# Total number of prevalent cases by year (for metric == "Number")
a_num <- data %>%
  filter(metric == "Number") %>%
  group_by(year) %>%
  summarise(sum = sum(val, na.rm = TRUE), .groups = "drop")

prevalence1990 <- a_num$sum[a_num$year == 1990]
prevalence2021 <- a_num$sum[a_num$year == 2021]

ci <- rank %>%
  filter(metric == "Number") %>%
  mutate(total_prevalence = ifelse(year == 1990, prevalence1990, prevalence2021)) %>%
  group_by(year) %>%
  arrange(sdi, .by_group = TRUE) %>%
  mutate(
    cummu_prevalence = cumsum(val),
    frac_prevalence  = cummu_prevalence / total_prevalence,
    frac_population  = cummu / pop_global
  ) %>%
  ungroup()

# CI by trapezoidal integration of Lorenz curve
ci_year <- ci %>%
  arrange(year, frac_population) %>%
  group_by(year) %>%
  summarise({
    xp <- c(0, frac_population, 1)
    yp <- c(0, frac_prevalence, 1)
    auc <- sum((yp[-1] + yp[-length(yp)]) * diff(xp) / 2)  # area under curve
    tibble(CI = 2 * (auc - 0.5))
  }, .groups = "drop")
print(ci_year)

ci_1990 <- round(ci_year$CI[ci_year$year == 1990], 3)
ci_2021 <- round(ci_year$CI[ci_year$year == 2021], 3)

# (2) Visualization: Lorenz curves & CI
fa2 <- ci %>%
  ggplot(aes(x = frac_population, y = frac_prevalence, fill = year, color = year, group = year)) +
  # Axes box (optional): bottom and right lines
  geom_segment(x = 0, xend = 1, y = 0, yend = 0, linetype = 1, linewidth = 1, color = "gray") +
  geom_segment(x = 1, xend = 1, y = 0, yend = 1, linetype = 1, linewidth = 1, color = "gray") +
  # Line of equality
  geom_segment(x = 0, xend = 1, y = 0, yend = 1, color = "#CD853F", linetype = 1, linewidth = 0.7, alpha = 1) +
  # Points sized by population
  geom_point(aes(fill = year, size = population / 1e6), alpha = 0.75, shape = 21) +
  scale_fill_manual(values = color) +
  scale_color_manual(values = color) +
  scale_size_area("Population\n(million)", breaks = c(200, 400, 600, 800, 1000, 1200)) +
  # Smooth Lorenz curve (GAM with natural splines)
  geom_smooth(
    method = "gam",
    formula = y ~ ns(x,
                     knots = c(1e-10, 0.25, 0.5, 0.75, 0.9999999),
                     Boundary.knots = c(0, 1)),
    linetype = 1, linewidth = 0.1, alpha = 0.6, se = TRUE
  ) +
  labs(
    x = "Cumulative fraction of population ranked by SDI",
    y = "Cumulative fraction of prevalence"
  ) +
  annotate("text", x = 0.75, y = 0.20,
           label = "Concentration Index", size = 3, fontface = "bold", hjust = 0.5) +
  theme_classic()
fa2

