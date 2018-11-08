source('./R/VAR_functions.R')

country <- "Chile"
print(paste0("this country: ", country))
names_exogenous <- c("ip_us", "ip_ue", "ip_asia")
# names_exogenous <- c("emae_arg", "act_eco_bra", "ip_bra", 
#                      "ip_us", "ip_ue", "ip_asia")
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 8
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
# vec_lags <-  c(5) # default value, can be omitted for shorter code

default_vec_lag <-  c(3:5)
default_treshold <- 1.65

Search_step_1 <- list(size = 2, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_2 <- list(size = 3, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_3 <- list(size = 4, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag)
Search_step_4 <- list(size = 5, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 3, lags = default_vec_lag)
search_plan <- list(Search_step_1, Search_step_2, Search_step_3, Search_step_4)

number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- "common_max"
max_rank_some_h <- 50 # default value, can be omitted for shorter code
results_file_name <-  paste0(country, "_auto_2s4_3s5.rds")
return_cv <-  TRUE



var_result <- var_search(
  country = country,
  search_plan = search_plan,
  forecast_exercise_year = forecast_exercise_year, 
  forecast_exercise_number = forecast_exercise_number,
  fc_horizon = fc_horizon, 
  add_aic_bic_hq_fpe_lags = add_aic_bic_hq_fpe_lags, 
  number_of_cv = number_of_cv, 
  train_span = train_span, 
  max_rank_some_h = max_rank_some_h,
  results_file_name = results_file_name, 
  ret_cv = return_cv, 
  max_small_rank = 3, 
  max_rank_some_h_for_freq = 30,
  names_exogenous = names_exogenous
) 

