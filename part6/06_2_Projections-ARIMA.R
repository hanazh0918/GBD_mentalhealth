
# =============================================================================
# File: arima_age_sex_rate_projections.R
# Title: ARIMA projections of age- and sex-specific prevalence rates (1990–2035)
# Purpose:
#   - Build year×age matrices (1990–2035) for prevalence counts and population
#   - Convert counts to rates per 100,000 using aligned populations
#   - Fit BoxCox-transformed ARIMA on rates (train: 1990–2021), forecast to 2035
#   - Return historical + projected means and 95% prediction intervals
#
# Inputs (prepared upstream in your workflow):
#   - BC_Prevalence_Female, BC_Prevalence_Male   : long tables of prevalence *counts*
#       columns include at least: year, age, val
#   - GBD_Global_Female, GBD_Global_Male         : long tables of *population*
#       columns include at least: year, age, val
#   - ages_group                                 : character vector of ages (e.g., c("0-4","5-9","10-14","15-19"))
#
# Outputs (in-memory):
#   - female_arima_df, male_arima_df : tidy data frames with columns
#       year, mean, lower, upper, age_group, sex
#
# Notes:
#   - Forecast horizon is derived from years_all (default 1990–2035) and train_end = 2021.
#   - Rates are forced positive for BoxCox/log transforms (a tiny offset may be added).
#   - Ensure age labels in counts and population match exactly (case and dashes).
# =============================================================================

# ---------------- Dependencies ----------------
library(reshape2)
library(dplyr)
library(tidyr)
library(purrr)
library(forecast)
library(ggplot2)

# ---------------- Common settings ----------------
years_all <- 1990:2035

# Helper: long -> wide (year × age), align to `years_all`, add missing years/ages as NA,
# and order columns by `ages_keep`.
to_year_age_matrix <- function(df_long, ages_keep, value = "val", years_all = 1990:2035){
  wide <- reshape2::dcast(df_long, year ~ age, value.var = value)
  # Use year as rownames and drop year column
  rownames(wide) <- as.character(wide$year)
  wide <- wide[, setdiff(colnames(wide), "year"), drop = FALSE]
  
  # Add missing age columns if necessary
  miss_cols <- setdiff(ages_keep, colnames(wide))
  if (length(miss_cols) > 0) wide[, miss_cols] <- NA_real_
  
  # Reorder age columns
  wide <- wide[, ages_keep, drop = FALSE]
  
  # Add missing years as NA rows
  miss_years <- setdiff(as.character(years_all), rownames(wide))
  if (length(miss_years) > 0){
    add <- matrix(NA_real_, nrow = length(miss_years), ncol = ncol(wide),
                  dimnames = list(miss_years, colnames(wide)))
    wide <- rbind(wide, add)
  }
  
  # Sort rows by years_all
  wide <- wide[as.character(years_all), , drop = FALSE]
  return(as.matrix(wide))
}

# ------------------------------------------------------------------
# Assumed available from your data-prep stage:
#   BC_Prevalence_Female, BC_Prevalence_Male     (long counts)
#   GBD_Global_Female,   GBD_Global_Male         (long populations)
#   ages_group <- c("0-4","5-9","10-14","15-19")
# ------------------------------------------------------------------

# ---- Build count matrices (Number): rows 1990–2035; 2022–2035 likely NA (counts) ----
BC_Prevalence_Female_n0 <- to_year_age_matrix(
  BC_Prevalence_Female, ages_keep = ages_group, value = "val", years_all = years_all
)
BC_Prevalence_Male_n0 <- to_year_age_matrix(
  BC_Prevalence_Male, ages_keep = ages_group, value = "val", years_all = years_all
)

# ---- Build population matrices (observed+forecast combined upstream) ----
GBD_Global_Female_n0 <- to_year_age_matrix(
  GBD_Global_Female, ages_keep = ages_group, value = "val", years_all = years_all
)
GBD_Global_Male_n0 <- to_year_age_matrix(
  GBD_Global_Male, ages_keep = ages_group, value = "val", years_all = years_all
)

# ================== (II) ARIMA on rates (per 100,000) ==================
ages_mod  <- c("5-9","10-14","15-19")  # avoid potential structural gaps for "0-4"
train_end <- 2021
h         <- length((train_end + 1):max(years_all))  # forecast horizon (to 2035)
eps       <- 1e-6

# Fit ARIMA for a single age×sex series:
# - Convert counts to rates per 100,000 using aligned population
# - BoxCox transform + auto.arima + forecast(h)
fit_arima_rate <- function(counts_vec, pop_vec,
                           start_year = 1990,
                           train_end_year = 2021,
                           horizon = 14){
  yrs_train <- start_year:train_end_year
  
  # Ensure names are years for safe indexing
  if (is.null(names(counts_vec))) names(counts_vec) <- as.character(start_year:(start_year + length(counts_vec) - 1))
  if (is.null(names(pop_vec)))    names(pop_vec)    <- names(counts_vec)
  
  # Training-period rate per 100k
  y_rate <- (counts_vec[as.character(yrs_train)] / pop_vec[as.character(yrs_train)]) * 1e5
  y_rate <- as.numeric(y_rate)
  
  # Force strictly positive for BoxCox/log if needed
  y_rate_pos <- if (any(y_rate <= 0)) y_rate + (abs(min(y_rate)) + eps) else y_rate
  y_ts <- ts(y_rate_pos, start = start_year, frequency = 1)
  
  # Estimate lambda (allow λ=0)
  lam <- BoxCox.lambda(y_ts, lower = 0)
  
  # Fit ARIMA on BoxCox-transformed series
  fit <- auto.arima(
    y_ts,
    lambda = lam,
    biasadj = TRUE,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  fc  <- forecast(fit, h = horizon, level = 95)
  
  # Assemble output: historical (no PI) + future (95% PI)
  df_hist <- tibble(
    year  = yrs_train,
    mean  = as.numeric(y_ts),
    lower = NA_real_,
    upper = NA_real_
  )
  years_future <- (train_end_year + 1):(train_end_year + horizon)
  df_fc <- tibble(
    year  = years_future,
    mean  = as.numeric(fc$mean),
    lower = as.numeric(fc$lower[, "95%"]),
    upper = as.numeric(fc$upper[, "95%"])
  )
  bind_rows(df_hist, df_fc)
}

# Run ARIMA for all modeled ages of one sex
arima_by_sex <- function(counts_mat, pop_mat, sex_label){
  stopifnot(identical(rownames(counts_mat), rownames(pop_mat)))
  map_dfr(ages_mod, function(ag){
    counts_vec <- counts_mat[, ag, drop = TRUE]
    pop_vec    <- pop_mat[,    ag, drop = TRUE]
    names(counts_vec) <- rownames(counts_mat)
    names(pop_vec)    <- rownames(pop_mat)
    
    fit_arima_rate(
      counts_vec,
      pop_vec,
      start_year     = min(years_all),
      train_end_year = train_end,
      horizon        = h
    ) %>%
      mutate(age_group = ag, sex = sex_label)
  })
}

# ---- Execute ----
female_arima_df <- arima_by_sex(BC_Prevalence_Female_n0, GBD_Global_Female_n0, "Female")
male_arima_df   <- arima_by_sex(BC_Prevalence_Male_n0,   GBD_Global_Male_n0,   "Male")



