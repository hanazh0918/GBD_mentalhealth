# =============================================================================
# Project: GBD MBD (<20 years) — Numbers (1990 vs 2021) and Rates (EAPC)
# File purpose:
#   1) Compute percent change (PC) in absolute numbers between 1990 and 2021
#      for Incidence / Prevalence / DALYs across the 27 GBD regions.
#   2) Summarize 1990 & 2021 rates and estimate EAPC (annual % change) from
#      1990–2021 via log-linear regression for the same measures.
#
# Inputs required (per disease dataset, e.g., conduct_27, ADHD_27, etc.):
#   - Columns: location, year, age, sex, metric, measure, val, upper, lower
#   - Age filter uses "<20 years"; Sex uses "Both"
#   - metric ∈ {"Number","Rate"}, measure ∈ {"Incidence","Prevalence","DALYs"}
#   - region_order: a data.frame with at least a "location" column to control
#     the display order / grouping (will be left-joined into outputs)
#
# Outputs:
#   - table1_pc.xlsx: Percent change in numbers (1990→2021) with CIs
#   - table1_eapc.xlsx: 1990 & 2021 rate summaries + EAPC with 95% CI
#
# Notes:
#   - EAPC is estimated as 100*(exp(beta)-1) from lm(log(rate) ~ year).
#   - This script assumes strictly positive rates for log-transform.
#   - Replace dataset objects (conduct_27, ADHD_27, Anxiety_27, Depressive_27)
#     with your in-memory data.frames as needed.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(writexl)
})

# Ensure output folder exists
dir.create("Results", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Step 1. Percent change in NUMBERS (1990 → 2021)
# -----------------------------------------------------------------------------
# Function: compute PC for absolute numbers with 95% UI presentation
hans_gbd_pc <- function(data, index_label, measure_filter, dataset_name) {
  # 2021 snapshot (Numbers)
  num_2021 <- data %>%
    dplyr::filter(
      year == 2021, age == "<20 years", sex == "Both",
      metric == "Number", measure == measure_filter
    ) %>%
    dplyr::select(location, val, upper, lower) %>%
    dplyr::mutate(
      val_2021   = round(val,   0),
      upper_2021 = round(upper, 0),
      lower_2021 = round(lower, 0),
      v_2021_show = paste0(val_2021, " (", lower_2021, ", ", upper_2021, ")")
    ) %>%
    dplyr::select(location, val_2021, upper_2021, lower_2021, v_2021_show)
  
  # 1990 snapshot (Numbers)
  num_1990 <- data %>%
    dplyr::filter(
      year == 1990, age == "<20 years", sex == "Both",
      metric == "Number", measure == measure_filter
    ) %>%
    dplyr::select(location, val, upper, lower) %>%
    dplyr::mutate(
      val_1990   = round(val,   0),
      upper_1990 = round(upper, 0),
      lower_1990 = round(lower, 0),
      v_1990_show = paste0(val_1990, " (", lower_1990, ", ", upper_1990, ")")
    ) %>%
    dplyr::select(location, val_1990, upper_1990, lower_1990, v_1990_show)
  
  # Merge and compute percent change
  merged_data <- dplyr::left_join(num_1990, num_2021, by = "location") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(val_1990, upper_1990, lower_1990,
                  val_2021, upper_2021, lower_2021),
        as.numeric
      ),
      pc_v = round((val_2021  - val_1990 ) / val_1990,  4),
      pc_u = round((upper_2021 - upper_1990) / upper_1990, 4),
      pc_l = round((lower_2021 - lower_1990) / lower_1990, 4),
      pc_show = paste0(pc_v, " (", pc_l, ", ", pc_u, ")"),
      index  = index_label,
      measure = measure_filter,
      metric  = "number",
      dataset = dataset_name
    )
  
  # Join region order (if provided globally)
  merged_data <- dplyr::left_join(region_order, merged_data, by = "location")
  return(merged_data)
}

# Datasets list (replace with your objects as needed)
datasets <- list(
  conduct   = conduct_27,
  adhd      = ADHD_27,
  anxiety   = Anxiety_27,
  depression= Depressive_27
)

# Iterate over datasets and measures
results <- list()
for (name in names(datasets)) {
  data <- datasets[[name]]
  results[[paste0(name, "_incidence")]]  <- hans_gbd_pc(data, "Incidence_Number",  "Incidence",  name)
  results[[paste0(name, "_dalys")]]      <- hans_gbd_pc(data, "DALYs_Number",      "DALYs",      name)
  results[[paste0(name, "_prevalence")]] <- hans_gbd_pc(data, "Prevalence_Number", "Prevalence", name)
}

# Bind all into one table and save
table1_pc <- dplyr::bind_rows(results)
writexl::write_xlsx(table1_pc, path = "Results/table1_pc.xlsx")

# -----------------------------------------------------------------------------
# Step 2. 2021 RATE snapshots + EAPC (1990–2021) for the 27 regions
# -----------------------------------------------------------------------------
# Function: compute EAPC based on log-linear regression on rates
hans_gbd_eapc <- function(data, index_label, measure_filter, region_order, dataset_name) {
  # (A) 2021 snapshot (Rates)
  rate_2021 <- data %>%
    dplyr::filter(
      year == 2021, age == "<20 years", sex == "Both",
      metric == "Rate", measure == measure_filter
    ) %>%
    dplyr::select(location, val, upper, lower) %>%
    dplyr::mutate(
      val_2021   = round(val,   4),
      upper_2021 = round(upper, 4),
      lower_2021 = round(lower, 4),
      v_2021_show = paste0(val_2021, " (", lower_2021, ", ", upper_2021, ")")
    ) %>%
    dplyr::select(location, val_2021, upper_2021, lower_2021, v_2021_show)
  
  # (B) 1990 snapshot (Rates)
  rate_1990 <- data %>%
    dplyr::filter(
      year == 1990, age == "<20 years", sex == "Both",
      metric == "Rate", measure == measure_filter
    ) %>%
    dplyr::select(location, val, upper, lower) %>%
    dplyr::mutate(
      val_1990   = round(val,   4),
      upper_1990 = round(upper, 4),
      lower_1990 = round(lower, 4),
      v_1990_show = paste0(val_1990, " (", lower_1990, ", ", upper_1990, ")")
    ) %>%
    dplyr::select(location, val_1990, upper_1990, lower_1990, v_1990_show)
  
  merged_data <- dplyr::left_join(rate_1990, rate_2021, by = "location") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(val_1990, upper_1990, lower_1990,
                  val_2021, upper_2021, lower_2021),
        as.numeric
      )
    )
  
  # Add region ordering
  merged_data <- dplyr::left_join(region_order, merged_data, by = "location")
  
  # (C) EAPC calculation on Rates (1990–2021)
  eapc_data <- data %>%
    dplyr::filter(
      age == "<20 years", sex == "Both",
      metric == "Rate", measure == measure_filter
    ) %>%
    dplyr::select(location, year, val)
  
  country <- rate_1990$location
  EAPC_cal <- data.frame(
    location = country,
    EAPC = 0, UCI = 0, LCI = 0
  )
  
  for (i in seq_along(country)) {
    country_name <- as.character(EAPC_cal[i, "location"])
    sub_data <- eapc_data[eapc_data$location == country_name, ]
    sub_data$y <- log(sub_data$val)             # requires positive rates
    model <- lm(y ~ year, data = sub_data)
    beta <- summary(model)[["coefficients"]][2, ]   # slope & SE
    EAPC_cal[i, "EAPC"] <- round(100 * (exp(beta[1]) - 1), 4)
    EAPC_cal[i, "LCI"]  <- round(100 * (exp(beta[1] - 1.96 * beta[2]) - 1), 4)
    EAPC_cal[i, "UCI"]  <- round(100 * (exp(beta[1] + 1.96 * beta[2]) - 1), 4)
  }
  EAPC_cal$EAPC_cal <- paste0(EAPC_cal$EAPC, " (", EAPC_cal$LCI, ", ", EAPC_cal$UCI, ")")
  
  # Merge EAPC back
  result <- dplyr::left_join(merged_data, EAPC_cal, by = "location") %>%
    dplyr::mutate(
      index   = index_label,
      measure = measure_filter,
      metric  = "Rate",
      dataset = dataset_name
    )
  
  return(result)
}

# Run for each dataset × measure
rate_results <- list()
for (name in names(datasets)) {
  data <- datasets[[name]]
  rate_results[[paste0(name, "_incidence")]]  <- hans_gbd_eapc(data, "Incidence_Rate",  "Incidence",  region_order, name)
  rate_results[[paste0(name, "_dalys")]]      <- hans_gbd_eapc(data, "DALYs_Rate",      "DALYs",      region_order, name)
  rate_results[[paste0(name, "_prevalence")]] <- hans_gbd_eapc(data, "Prevalence_Rate", "Prevalence", region_order, name)
}

table1_eapc <- dplyr::bind_rows(rate_results)
writexl::write_xlsx(table1_eapc, path = "Results/table1_eapc.xlsx")
