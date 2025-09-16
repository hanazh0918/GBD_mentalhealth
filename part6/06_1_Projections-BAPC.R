
# =============================================================================
# File: bapc_cd_age_sex_projections.R
# Title: BAPC projections of age- and sex-specific prevalence for Conduct Disorder (Global)
# Purpose:
#   - Clean GBD conduct data ages ("<5" -> "0-4"; remove "<20")
#   - Read WHO 2000–2025 world standard population weights
#   - Build age×year prevalence-number matrices for Female and Male (Global)
#   - Assemble observed (1990–2021) + IHME forecast population (2022–2035)
#   - Run BAPC with standardization (exclude 0–4 when needed) and produce projections
#   - Extract means and 95% credible intervals (approx via Gamma reconstruction) as RATES
#   - Plot time trends with ribbons by age group and sex; mark 2021 cut (historical vs projection)
#
# Inputs (adjust paths to yours):
#   - conduct_27 (not strictly required), conduct_204: GBD disease data
#   - WHO standard population CSV: ".../WHO 2000-2025 new World Standard Population.csv"
#   - GBD population (observed 1990–2021): ".../1990-2021年GBD全球人口.csv"
#   - IHME population forecast 2017–2100 CSV (reference scenario)
#
# Outputs:
#   - ggplot object f5_a (panel by age, colored by sex)
#   - Saved figure (TIFF), and tidy dataframe `df_f5_a` with projected rates + CI
#
# Notes:
#   - BAPC expects integer counts; we build matrices with ages as columns, years as rows.
#   - For standardization, we pass WHO weights and sometimes exclude 0–4 (as in your workflow).
#   - Forecast horizon here is 2022–2035 (14 years). Adjust `npredict` if you change the end year.
# =============================================================================

suppressPackageStartupMessages({
  library(foreach)
  library(caTools)
  library(Epi)
  library(fanplot)
  library(sp)
  library(sf)
  library(INLA)
  library(BAPC)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(reshape2)   # use reshape2::dcast
  library(mgcv)       # GAM smooth for Lorenz-like curves (if needed)
  library(splines)
  library(broom)
})

# ---------------------------- Setup ----------------------------
setwd("/Users/hanazh/900_Article/maomao_2025/GBD_MBD_Database2025/C2.1_TSM")
dir.create("Results", showWarnings = FALSE, recursive = TRUE)
options(scipen = 200)

# ---------------- Conduct data (27-reg to start cleaning) ----------------
BC <- conduct_27
# normalize age labels: drop "<20", map "<5" -> "0-4"
BC$age <- str_replace(BC$age, " years", "")
BC <- BC %>%
  filter(age != "<20") %>%
  mutate(age = ifelse(age == "<5", "0-4", age))

# Target age groups for projections
ages_group <- c("0-4", "5-9", "10-14", "15-19")

# ---------------- WHO standard population ----------------
age_stand <- read.csv("WHO 2000-2025 new World Standard Population.csv")
age_stand <- age_stand %>%
  filter(Age.group %in% ages_group) %>%
  mutate(Age.group = factor(Age.group, levels = ages_group, ordered = TRUE)) %>%
  arrange(Age.group)

wstand <- age_stand$std_population
names(wstand) <- ages_group

# ---------------- Build prevalence-number matrices (Global, by sex) ----------------
# Female counts
BC_Prevalence_Female <- BC %>%
  filter(measure == "Prevalence",
         metric  == "Number",
         sex     == "Female",
         age %in% ages_group,
         location == "Global")

BC_Prevalence_Female_n <- dcast(BC_Prevalence_Female, year ~ age, value.var = "val")
BC_Prevalence_Female_n <- BC_Prevalence_Female_n[, -1] %>%
  apply(2, as.numeric) %>%
  round() %>%
  as.data.frame()

# Male counts
BC_Prevalence_Male <- BC %>%
  filter(measure == "Prevalence",
         metric  == "Number",
         sex     == "Male",
         age %in% ages_group,
         location == "Global")

BC_Prevalence_Male_n <- dcast(BC_Prevalence_Male, year ~ age, value.var = "val")
BC_Prevalence_Male_n <- BC_Prevalence_Male_n[, -1] %>%
  apply(2, as.numeric) %>%
  round() %>%
  as.data.frame()

# ---------------- Observed population 1990–2021 ----------------
pop_csv <- "1990-2021年GBD全球人口.csv"

PopulationFemale <- read.csv(pop_csv)[, c(2:4,6,7)] %>%
  filter(sex == "Female") %>%
  mutate(age = sub(" to ", "-", age),
         age = sub(" years", "", age)) %>%
  mutate(age = ifelse(age == "<5", "0-4", age))

PopulationMale <- read.csv(pop_csv)[, c(2:4,6,7)] %>%
  filter(sex == "Male") %>%
  mutate(age = sub(" to ", "-", age),
         age = sub(" years", "", age)) %>%
  mutate(age = ifelse(age == "<5", "0-4", age))

# ---------------- IHME population forecast 2022–2035 ----------------
ihme_csv <- "IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV"
GBD_population_prediction <- read.csv(ihme_csv) %>%
  filter(year_id %in% 2022:2035) %>%
  select(location_name, sex_name, age_name, year_id, measure_name, val)
colnames(GBD_population_prediction) <- c("location", "sex", "age", "year", "metric", "val")

# Combine neonatal sub-ages into "<1 year"
pred_1y <- GBD_population_prediction %>%
  filter(age %in% c("Early Neonatal","Late Neonatal", "Post Neonatal")) %>%
  group_by(location, sex, year, metric) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(age = "<1 year")

GBD_population_prediction <- GBD_population_prediction %>%
  filter(!(age %in% c("Early Neonatal","Late Neonatal", "Post Neonatal"))) %>%
  bind_rows(pred_1y) %>%
  mutate(age = sub(" to ", "-", age))

# ---------------- Female: combine observed + forecast ----------------
GBD_F <- rbind(PopulationFemale, GBD_population_prediction %>% filter(sex == "Female") %>% select(location, sex, year, age, val))
GBD_age4 <- GBD_F %>%
  filter(age %in% c("<1 year", "1-4")) %>%
  group_by(location, sex, year) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(age = "0-4")
GBD_F <- rbind(GBD_F, GBD_age4) %>%
  mutate(age = sub(" plus", "+", age)) %>%
  filter(age %in% ages_group) %>%
  mutate(age = factor(age, levels = ages_group, ordered = TRUE)) %>%
  arrange(age)

GBD_Global_Female <- GBD_F %>%
  filter(location == "Global", sex == "Female", age %in% ages_group)

GBD_Global_Female_n <- dcast(GBD_Global_Female, year ~ age, value.var = "val")
GBD_Global_Female_n <- GBD_Global_Female_n[, -1]

# ---------------- Male: combine observed + forecast ----------------
GBD_M <- rbind(PopulationMale, GBD_population_prediction %>% filter(sex == "Male") %>% select(location, sex, year, age, val))
GBD_age4 <- GBD_M %>%
  filter(age %in% c("<1 year", "1-4")) %>%
  group_by(location, sex, year) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(age = "0-4")
GBD_M <- rbind(GBD_M, GBD_age4) %>%
  mutate(age = sub(" plus", "+", age)) %>%
  filter(age %in% ages_group) %>%
  mutate(age = factor(age, levels = ages_group, ordered = TRUE)) %>%
  arrange(age)

GBD_Global_Male <- GBD_M %>%
  filter(location == "Global", sex == "Male", age %in% ages_group)

GBD_Global_Male_n <- dcast(GBD_Global_Male, year ~ age, value.var = "val")
GBD_Global_Male_n <- GBD_Global_Male_n[, -1]

# ---------------- Extend disease counts to 2035 (NA rows) ----------------
# 2022–2035 inclusive: 14 rows
BC_predicted_blank <- matrix(NA, nrow = 14, ncol = ncol(GBD_Global_Female_n)) %>% as.data.frame()
rownames(BC_predicted_blank) <- as.character(2022:2035)
colnames(BC_predicted_blank) <- ages_group

BC_Prevalence_Female_n <- rbind(BC_Prevalence_Female_n, BC_predicted_blank)
BC_Prevalence_Male_n   <- rbind(BC_Prevalence_Male_n,   BC_predicted_blank)

rownames(BC_Prevalence_Female_n) <- as.character(1990:2035)
rownames(BC_Prevalence_Male_n)   <- as.character(1990:2035)
rownames(GBD_Global_Female_n)    <- as.character(1990:2035)
rownames(GBD_Global_Male_n)      <- as.character(1990:2035)

# ---------------- BAPC (Female) ----------------
# Exclude 0–4 for age-specific rate plot (as in your workflow)
BC_Prevalence_Female_n0 <- BC_Prevalence_Female_n[, c("5-9", "10-14", "15-19")]
GBD_Global_Female_n0    <- GBD_Global_Female_n[,    c("5-9", "10-14", "15-19")]

Female_esoph <- APCList(BC_Prevalence_Female_n0, GBD_Global_Female_n0, gf = 5)

Female_bapc_result <- BAPC(
  Female_esoph,
  predict   = list(npredict = 14, retro = TRUE),  # 2022–2035
  secondDiff = FALSE,
  stdweight  = wstand[names(wstand) != "0-4"],
  verbose    = FALSE
)

# plotBAPC(Female_bapc_result, scale=1e5, type='ageSpecRate', showdata=TRUE, mfrow=c(1,3))

# ---------------- BAPC (Male) ----------------
BC_Prevalence_Male_n0 <- BC_Prevalence_Male_n[, c("5-9", "10-14", "15-19")]
GBD_Global_Male_n0    <- GBD_Global_Male_n[,    c("5-9", "10-14", "15-19")]

Male_esoph <- APCList(BC_Prevalence_Male_n0, GBD_Global_Male_n0, gf = 5)

Male_bapc_result <- BAPC(
  Male_esoph,
  predict   = list(npredict = 14, retro = TRUE),
  secondDiff = FALSE,
  stdweight  = wstand[names(wstand) != "0-4"],
  verbose    = FALSE
)

# ---------------- Extract projected age-specific rates + 95% CI ----------------
extract_ci_gamma <- function(mean, sd) {
  shape <- (mean / sd)^2
  rate  <- mean / (sd^2)
  qgamma(c(0.025, 0.975), shape = shape, rate = rate)
}

# Female
years_f <- as.numeric(Female_bapc_result@periodlab)
ages_f  <- Female_bapc_result@agelab
pyrs_f  <- Female_bapc_result@pyrs
proj_f  <- Female_bapc_result@agespec.proj  # list of matrices, one per age

female_list <- lapply(seq_along(proj_f), function(i) {
  m  <- proj_f[[i]][, "mean"]
  s  <- proj_f[[i]][, "sd"]
  ci <- t(mapply(extract_ci_gamma, m, s))
  data.frame(
    year = years_f,
    age_group = ages_f[i],
    rate  = m  / pyrs_f[((i - 1) * length(years_f) + 1):(i * length(years_f))] * 1e5,
    lower = ci[, 1] / pyrs_f[((i - 1) * length(years_f) + 1):(i * length(years_f))] * 1e5,
    upper = ci[, 2] / pyrs_f[((i - 1) * length(years_f) + 1):(i * length(years_f))] * 1e5,
    sex = "Female"
  )
})
female_df <- do.call(rbind, female_list)

# Male
years_m <- as.numeric(Male_bapc_result@periodlab)
ages_m  <- Male_bapc_result@agelab
pyrs_m  <- Male_bapc_result@pyrs
proj_m  <- Male_bapc_result@agespec.proj

male_list <- lapply(seq_along(proj_m), function(i) {
  m  <- proj_m[[i]][, "mean"]
  s  <- proj_m[[i]][, "sd"]
  ci <- t(mapply(extract_ci_gamma, m, s))
  data.frame(
    year = years_m,
    age_group = ages_m[i],
    rate  = m  / pyrs_m[((i - 1) * length(years_m) + 1):(i * length(years_m))] * 1e5,
    lower = ci[, 1] / pyrs_m[((i - 1) * length(years_m) + 1):(i * length(years_m))] * 1e5,
    upper = ci[, 2] / pyrs_m[((i - 1) * length(years_m) + 1):(i * length(years_m))] * 1e5,
    sex = "Male"
  )
})
male_df <- do.call(rbind, male_list)

# ---------------- Combine & visualize ----------------
plot_data <- rbind(male_df, female_df) %>%
  mutate(
    age_group = factor(age_group, levels = c("0-4", "5-9", "10-14", "15-19")),
    is_proj   = ifelse(year >= 2022, "Projection", "Historical")
  )

f5_a <- ggplot(plot_data, aes(x = year, y = rate, color = sex, fill = sex)) +
  geom_line(aes(linetype = is_proj), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  facet_wrap(~ age_group, scales = "free_y", nrow = 1) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "gray50") +
  labs(
    x = "Year",
    y = "Prevalence rate (per 100,000)",
    title = "(A) Conduct disorder"
  ) +
  theme_classic() +
  scale_color_manual(values = c("Male" = "#2b8cbe", "Female" = "#fcae91")) +
  scale_fill_manual(values = c("Male" = "#2b8cbe", "Female" = "#fcae91")) +
  theme(
    plot.title = element_text(hjust = -0.15, face = "bold", size = 14),
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white"),
    legend.position = "right"
  ) +
  guides(linetype = "none")
f5_a

df_f5_a <- plot_data
df_f5_a$outcomes <- "Conduct disorder"

ggsave("Results/Figure5_projections_bapc_CD.tiff", f5_a, width = 8, height = 4, dpi = 300)

