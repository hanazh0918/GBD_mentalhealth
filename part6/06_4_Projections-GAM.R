# =============================================================================
# File: GAM_age_sex_rate_projections.R
# Title: GAM projections of age- and sex-specific prevalence rates (1990–2035)
# Purpose:
#   - Build year×age matrices (1990–2035) for prevalence counts and population
#   - Convert counts to rates per 100,000 using aligned populations
#   - Fit GAM (on rates) with smooth trend over year (train: 1990–2021), forecast to 2035
#   - Return historical + projected means and 95% intervals (Gaussian on rate scale)
#
# Inputs (prepared upstream in your workflow):
#   - BC_Prevalence_Female, BC_Prevalence_Male   : long tables of prevalence *counts*
#       (columns include at least: year, age, val)
#   - GBD_Global_Female, GBD_Global_Male         : long tables of *population*
#       (columns include at least: year, age, val)
#   - ages_group                                 : character vector (e.g., c("0-4","5-9","10-14","15-19"))
#
# Outputs (in-memory):
#   - female_gam_df, male_gam_df : tidy data frames with columns
#       year, mean, lower, upper, age_group, sex
#
# Notes:
#   - The default family is Gaussian on the rate scale; if rates are highly skewed or zero-inflated,
#     consider Gamma(link="log") (see commented option).
#   - k (basis dimension) is set adaptively with a conservative cap to avoid overfitting.
#   - Forecast horizon is derived from years_all (default 1990–2035) and train_end = 2021.
# =============================================================================

# ---------------- Dependencies ----------------
suppressPackageStartupMessages({
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(mgcv)
})

# ---------------- Common settings ----------------
years_all <- 1990:2035

# Helper: long -> wide (year × age), align to `years_all`, add missing years/ages as NA,
# and order columns by `ages_keep`.
to_year_age_matrix <- function(df_long, ages_keep, value = "val", years_all = 1990:2035){
  wide <- reshape2::dcast(df_long, year ~ age, value.var = value)
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
BC_Prevalence_Female_n0 <- to_year_age_matrix(BC_Prevalence_Female, ages_keep = ages_group, value = "val", years_all = years_all)
BC_Prevalence_Male_n0   <- to_year_age_matrix(BC_Prevalence_Male,   ages_keep = ages_group, value = "val", years_all = years_all)
GBD_Global_Female_n0    <- to_year_age_matrix(GBD_Global_Female,    ages_keep = ages_group, value = "val", years_all = years_all)
GBD_Global_Male_n0      <- to_year_age_matrix(GBD_Global_Male,      ages_keep = ages_group, value = "val", years_all = years_all)

# ================== GAM on rates (per 100,000) ==================
ages_mod  <- c("0-4", "5-9","10-14","15-19")
train_end <- 2021
h         <- length((train_end + 1):max(years_all))

# Fit GAM for a single age×sex series:
# - Convert counts to rates per 100,000 using aligned population
# - GAM with smooth over year on the rate scale
fit_gam_rate <- function(counts_vec, pop_vec,
                         start_year = 1990,
                         train_end_year = 2021,
                         horizon = 14){
  yrs_train <- start_year:train_end_year
  
  # Ensure names are years for safe indexing
  if (is.null(names(counts_vec))) names(counts_vec) <- as.character(start_year:(start_year + length(counts_vec) - 1))
  if (is.null(names(pop_vec)))    names(pop_vec)    <- names(counts_vec)
  
  # Build training data (rate per 100k)
  train_data <- data.frame(
    year  = yrs_train,
    count = as.numeric(counts_vec[as.character(yrs_train)]),
    pop   = as.numeric(pop_vec[as.character(yrs_train)])
  )
  train_data$rate <- (train_data$count / train_data$pop) * 1e5
  train_data <- na.omit(train_data)
  
  # Minimal data guard
  if (nrow(train_data) < 5) {
    warning("Not enough data for modeling")
    return(data.frame(year = numeric(), mean = numeric(), lower = numeric(), upper = numeric()))
  }
  
  # Conservative basis dimension to reduce overfit risk
  k <- min(8, floor(nrow(train_data) / 3))
  
  tryCatch({
    # Gaussian on rate scale
    fit <- mgcv::gam(rate ~ s(year, k = k, bs = "cr"),
                     data = train_data,
                     method = "REML",
                     family = gaussian())
    
    # Future years (include historical years to return a full series)
    pred_years <- data.frame(year = start_year:(train_end_year + horizon))
    
    # Predict on rate scale
    pred <- predict(fit, newdata = pred_years, type = "response", se.fit = TRUE)
    
    mean_pred  <- as.numeric(pred$fit)
    se_pred    <- as.numeric(pred$se.fit)
    lower_pred <- pmax(mean_pred - 1.96 * se_pred, 0)  # keep non-negative
    upper_pred <- mean_pred + 1.96 * se_pred
    
    data.frame(
      year  = pred_years$year,
      mean  = mean_pred,
      lower = lower_pred,
      upper = upper_pred
    )
  }, error = function(e) {
    warning(paste("GAM fitting failed:", e$message, "Falling back to linear model."))
    lm_fit    <- lm(rate ~ year, data = train_data)
    pred_years <- data.frame(year = start_year:(train_end_year + horizon))
    pred       <- predict(lm_fit, newdata = pred_years, se.fit = TRUE)
    
    data.frame(
      year  = pred_years$year,
      mean  = as.numeric(pred$fit),
      lower = as.numeric(pred$fit - 1.96 * pred$se.fit),
      upper = as.numeric(pred$fit + 1.96 * pred$se.fit)
    )
  })
}

# Run GAM for all modeled ages of one sex
gam_by_sex <- function(counts_mat, pop_mat, sex_label){
  stopifnot(identical(rownames(counts_mat), rownames(pop_mat)))
  purrr::map_dfr(ages_mod, function(ag){
    counts_vec <- counts_mat[, ag, drop = TRUE]
    pop_vec    <- pop_mat[,    ag, drop = TRUE]
    names(counts_vec) <- rownames(counts_mat)
    names(pop_vec)    <- rownames(pop_mat)
    
    fit_gam_rate(
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
female_gam_df <- gam_by_sex(BC_Prevalence_Female_n0, GBD_Global_Female_n0, "Female")
male_gam_df   <- gam_by_sex(BC_Prevalence_Male_n0,   GBD_Global_Male_n0,   "Male")
