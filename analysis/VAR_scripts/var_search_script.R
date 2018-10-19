source('./R/VAR_functions.R')

country <- "Argentina"
sizes <- c(2, 3, 4, 5)
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 7
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
vec_lags <-  c(1, 2, 3, 4, 5, 6) # default value, can be omitted for shorter code
vec_freq_limit <- c("none", "none", 10, 10) # default value, can be omitted
t_tresh <- c(2, 2, 2, 2) # default value, can be omitted for shorter code
number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- 25 # default value, can be omitted for shorter code
max_rank_some_h <- 50 # default value, can be omitted for shorter code

var_result <- var_search(
  country = country,
  sizes = sizes,
  forecast_exercise_year = forecast_exercise_year, 
  forecast_exercise_number = forecast_exercise_number,
  fc_horizon = fc_horizon, 
  add_aic_bic_hq_fpe_lags = add_aic_bic_hq_fpe_lags, 
  vec_lags = vec_lags, 
  vec_freq_limit = vec_freq_limit, 
  t_tresh = t_tresh, 
  number_of_cv = number_of_cv, 
  train_span = train_span, 
  max_rank_some_h = max_rank_some_h
  ) 











