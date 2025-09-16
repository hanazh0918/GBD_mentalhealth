
# =============================================================================
# File: Prophet_age_sex_rate_projections.R
# Title: Prophet projections of age- and sex-specific prevalence rates (1990–2035)
# Purpose:
#   - Build year×age matrices (1990–2035) for prevalence counts and population
#   - Convert counts to rates per 100,000 using aligned populations
#   - Fit Prophet models on annual rates (train: 1990–2021), forecast to 2035
#   - Return historical + projected means and 95% prediction intervals
#
# Inputs (prepared upstream in your workflow):
#   - BC_Prevalence_Female, BC_Prevalence_Male   : long tables of prevalence counts
#       (columns include at least: year, age, val)
#   - GBD_Global_Female, GBD_Global_Male         : long tables of population
#       (columns include at least: year, age, val)
#   - ages_group                                 : character vector, e.g. c("0-4","5-9","10-14","15-19")
#
# Outputs (in-memory):
#   - female_prophet_df, male_prophet_df : tidy data frames with columns
#       year, mean, lower, upper, age_group, sex
#
# Notes:
#   - Annual data: all Prophet seasonalities are disabled (yearly/weekly/daily = FALSE).
#   - Rates are forced positive for stability (tiny offset if min<=0).
#   - Forecast horizon is derived from years_all (default 1990–2035) and train_end = 2021.
# =============================================================================

# ---------------- Dependencies ----------------
library(prophet)
library(purrr)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# ---------------- Common settings ----------------
years_all <- 1990:2035

# Helper: long -> wide (year × age), align to `years_all`, add missing years/ages as NA,
# and order columns by `ages_keep`.
to_year_age_matrix <- function(df_long, ages_keep, value = "val", years_all = 1990:2035){
  wide <- reshape2::dcast(df_long, year ~ age, value.var = value)
  # Use year as row names and drop the 'year' column
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

# ---------------- Prepare matrices ----------------
# Counts (Number): rows 1990–2035; 2022–2035 will typically be NA (counts only go to 2021)
BC_Prevalence_Female_n0 <- to_year_age_matrix(BC_Prevalence_Female, ages_keep = ages_group, value = "val", years_all = years_all)
BC_Prevalence_Male_n0   <- to_year_age_matrix(BC_Prevalence_Male,   ages_keep = ages_group, value = "val", years_all = years_all)

# Population (observed + forecast already combined upstream)
GBD_Global_Female_n0 <- to_year_age_matrix(GBD_Global_Female, ages_keep = ages_group, value = "val", years_all = years_all)
GBD_Global_Male_n0   <- to_year_age_matrix(GBD_Global_Male,   ages_keep = ages_group, value = "val", years_all = years_all)

# ================== Prophet on rates (per 100,000) ==================
ages_mod  <- c("5-9","10-14","15-19")   # avoid potential structural gaps for "0-4"
train_end <- 2021
h         <- length((train_end + 1):max(years_all))
eps       <- 1e-6

# Fit Prophet for a single age×sex series:
# - Convert counts to rates per 100,000 using aligned population
# - Prophet with no seasonality (annual data)
fit_prophet_rate <- function(counts_vec, pop_vec,
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
  
  # Prophet expects positive targets; add tiny offset if needed
  y_pos <- if (any(y_rate <= 0)) y_rate + (abs(min(y_rate)) + eps) else y_rate
  
  # Prophet input format: ds (Date), y (numeric)
  df_prophet <- tibble::tibble(
    ds = as.Date(paste0(yrs_train, "-01-01")),
    y  = y_pos
  )
  
  # Fit Prophet (no seasonalities for annual data)
  m <- prophet::prophet(
    df_prophet,
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality  = FALSE,
    changepoint.prior.scale = 0.05
  )
  
  # Future dates: annual steps for the required horizon
  future_dates <- tibble::tibble(
    ds = seq.Date(from = as.Date(paste0(train_end_year + 1, "-01-01")),
                  to   = as.Date(paste0(train_end_year + horizon, "-01-01")),
                  by   = "year")
  )
  
  # Forecast
  fc <- predict(m, future_dates)
  
  # Assemble output: historical (no PI) + future (with PI)
  df_hist <- tibble::tibble(
    year  = yrs_train,
    mean  = df_prophet$y,
    lower = NA_real_,
    upper = NA_real_
  )
  
  df_fc <- tibble::tibble(
    year  = (train_end_year + 1):(train_end_year + horizon),
    mean  = fc$yhat,
    lower = fc$yhat_lower,
    upper = fc$yhat_upper
  )
  
  dplyr::bind_rows(df_hist, df_fc)
}

# Run Prophet for all modeled ages of one sex
prophet_by_sex <- function(counts_mat, pop_mat, sex_label){
  stopifnot(identical(rownames(counts_mat), rownames(pop_mat)))
  purrr::map_dfr(ages_mod, function(ag){
    counts_vec <- counts_mat[, ag, drop = TRUE]
    pop_vec    <- pop_mat[,    ag, drop = TRUE]
    names(counts_vec) <- rownames(counts_mat)
    names(pop_vec)    <- rownames(pop_mat)
    
    fit_prophet_rate(
      counts_vec,
      pop_vec,
      start_year     = min(years_all),
      train_end_year = train_end,
      horizon        = h
    ) %>%
      dplyr::mutate(age_group = ag, sex = sex_label)
  })
}

# ---------------- Execute ----------------
female_prophet_df <- prophet_by_sex(BC_Prevalence_Female_n0, GBD_Global_Female_n0, "Female")
male_prophet_df   <- prophet_by_sex(BC_Prevalence_Male_n0,   GBD_Global_Male_n0,   "Male")





