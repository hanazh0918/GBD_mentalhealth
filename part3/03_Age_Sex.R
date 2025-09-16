# =============================================================================
# File: 03_make_figures_fig3_cd_global.R
# Title: Figure 3 — Global (CD) Prevalence: Age bars + rate lines (1990 vs 2021)
# Purpose:
#   A) Build a dual-axis panel: bars = number of prevalent cases; lines = prevalence rate.
#      Age groups: 5–9, 10–14, 15–19; Years: 1990 vs 2021; Population: <20 years; Sex: Both.
#   B) Build a time-series panel: prevalence rate (per 100,000) by sex (Male/Female), 1990–2021,
#      Global, <20 years.
#
# Inputs:
#   - conduct_27: data.frame with columns {location, year, age, sex, metric, measure, val, upper, lower}
#   - (Optional) region_order: data.frame with column `location` used elsewhere; not required here.
#
# Outputs (objects):
#   - f3_a1: ggplot object (dual-axis bars + lines by age, 1990 vs 2021, Global)
#   - f3_a2: ggplot object (time-series by sex, Global)
#
# Notes:
#   - Dual axis uses a scale factor to align “rate” magnitude with “number”. Adjust `y2_scale` as needed.
#   - Ensure metric/measure filters are correct (Number vs Rate, Prevalence).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# ----------------------------- Panel A: bars (Number) + lines (Rate) -----------------------------

# Keep Global, Both sex; age groups 5–9, 10–14, 15–19; years 1990 & 2021; measure = Prevalence
data <- conduct_27 %>%
  dplyr::filter(
    location == "Global",
    year %in% c(1990, 2021),
    sex  == "Both",
    age  %in% c("5-9 years", "10-14 years", "15-19 years"),
    measure == "Prevalence"
  ) %>%
  dplyr::mutate(
    age = factor(age,
                 levels = c("5-9 years", "10-14 years", "15-19 years"),
                 labels = c("5-9", "10-14", "15-19"))
  )

# Split to Number / Rate and join back (keep val + CIs for both)
data_num <- data %>%
  dplyr::filter(metric == "Number") %>%
  dplyr::select(measure, age, year, val, upper, lower) %>%
  dplyr::rename(val_number = val, upper_number = upper, lower_number = lower)

data_rate <- data %>%
  dplyr::filter(metric == "Rate") %>%
  dplyr::select(measure, age, year, val, upper, lower) %>%
  dplyr::rename(val_rate = val, upper_rate = upper, lower_rate = lower)

data_plot <- dplyr::left_join(data_num, data_rate, by = c("measure", "age", "year")) %>%
  dplyr::mutate(year = as.character(year))

# Scale factor for secondary axis (align rate*scale with number)
y2_scale <- 5000  # adjust if needed

f3_a1 <- ggplot(data_plot, aes(x = age, group = year)) +
  # Bars: number of prevalent cases (with 95% UI)
  geom_bar(
    aes(y = val_number, fill = year),
    stat = "identity",
    position = position_dodge(width = 0.70),
    width = 0.7, alpha = 0.8
  ) +
  geom_errorbar(
    aes(ymin = lower_number, ymax = upper_number, group = year),
    position = position_dodge(width = 0.8), width = 0.2, color = "black"
  ) +
  # Lines + ribbons: prevalence rate * scale (with 95% UI)
  geom_ribbon(
    aes(ymin = lower_rate * y2_scale, ymax = upper_rate * y2_scale,
        fill = year, group = year),
    alpha = 0.2, position = position_dodge(width = 0)
  ) +
  geom_line(
    aes(y = val_rate * y2_scale, color = year, group = year),
    position = position_dodge(width = 0), linewidth = 0.5
  ) +
  # Dual y-axis
  scale_y_continuous(
    name = "Number of prevalent cases",
    sec.axis = sec_axis(~ . / y2_scale, name = "Prevalence rate (per 100,000)")
  ) +
  # Colors (keep year-consistent across fill/line)
  scale_fill_manual(values = c("1990" = "#74add1", "2021" = "#fdae61")) +
  scale_color_manual(values = c("1990" = "#74add1", "2021" = "#fdae61")) +
  scale_x_discrete(name = "Age (years)") +
  labs(
    title = "(A) Conduct disorder",
    fill  = "Year",
    color = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = -0.2),
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 15),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = c(0.20, 0.98),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75)
  )
f3_a1

# keep a tidy copy for export/trace
df_a1 <- data_plot %>% mutate(cause = "conduct_27")

# ----------------------------- Panel B: time-series by sex (rate only) -----------------------------

data_ts <- conduct_27 %>%
  dplyr::filter(
    age == "<20 years",
    metric == "Rate",
    location == "Global",
    sex %in% c("Male", "Female"),
    measure == "Prevalence"
  )

f3_a2 <- ggplot(data_ts, aes(x = year, y = val, group = sex, color = sex)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex, color = sex),
              alpha = 0.3, linetype = 2) +
  labs(
    y = "Prevalence rate (per 100,000 population)",
    x = "Year",
    fill = "Sex",
    color = "Sex"
  ) +
  scale_fill_manual(values = c("Female" = "#fb9a99", "Male" = "#abd9e9")) +
  scale_color_manual(values = c("Female" = "#fb9a99", "Male" = "#abd9e9")) +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
    legend.position = c(0.5, 0.96),
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.spacing.x = grid::unit(1, "cm"),
    legend.background = element_rect(fill = "transparent", color = NA)
  ) +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(700, 2500), breaks = seq(700, 2500, 500), position = "right")
f3_a2

# keep a tidy copy for export/trace
df_a2 <- data_ts %>% mutate(cause = "conduct_27")
