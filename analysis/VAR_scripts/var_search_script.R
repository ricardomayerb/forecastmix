source('./R/VAR_functions.R')

country <- "Brasil"
print(paste0("this country: ", country))
sizes <- c(2, 3, 4, 5)
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 8
prechosen_variables_at_each_size <-  list(c(""), c(""), c(""), c("")) # default value, can be omitted for shorter code
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
vec_lags <-  c(1, 2, 3, 4, 5, 6) # default value, can be omitted for shorter code
# vec_freq_limit <- list("none", "none", 10, 10) # default value, can be omitted
vec_freq_limit <- list("none", "none", 20, 15) 
t_tresh <- c(1.65, 1.65, 1.65, 1.65) # default value, can be omitted for shorter code
number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- 40
max_rank_some_h <- 50 # default value, can be omitted for shorter code
results_file_name <-  NULL
return_cv <-  TRUE


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
  max_rank_some_h = max_rank_some_h,
  other_prechosen_variables = prechosen_variables_at_each_size,
  results_file_name = results_file_name, 
  ret_cv = return_cv
) 



