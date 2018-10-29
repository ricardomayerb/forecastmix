source('./R/VAR_functions.R')

country <- "Chile"
print(paste0("this country: ", country))
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 8
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
vec_lags <-  c(5) # default value, can be omitted for shorter code

default_vec_lag <-  c(5)
default_treshold <- 1.65

Search_step_1 <- list(size = 2, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_2 <- list(size = 3, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_3 <- list(size = 4, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag)
Search_step_4 <- list(size = 5, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag)
search_plan <- list(Search_step_1, Search_step_2, Search_step_3, Search_step_4)

number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- "common_max"
max_rank_some_h <- 40 # default value, can be omitted for shorter code
results_file_name <-  NULL
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
  max_rank_some_h_for_freq = 30
) 



# sizes <- c(2, 3, 4, 4, 5)
# manually_prechosen_variables_at_each_size <-  list(c(""), c(""), c(""), c(""), c("")) # default value, can be omitted for shorter code
# vec_freq_limit <- list(list(type = "none"), 
#                        list(type = "none"),
#                        list(type = "cummulative-preselection", nth = 1),
#                        list(type = "cummulative-preselection", nth = 2),
#                        list(type = "cummulative-preselection", nth = 2)) 
# t_tresh <- c(1.65, 1.65, 1.65, 1.65, 1.65) # default value, can be omitted for shorter code

# var_result <- var_search_semi_old(
#   country = country,
#   sizes = sizes,
#   forecast_exercise_year = forecast_exercise_year, 
#   forecast_exercise_number = forecast_exercise_number,
#   fc_horizon = fc_horizon, 
#   add_aic_bic_hq_fpe_lags = add_aic_bic_hq_fpe_lags, 
#   vec_lags = vec_lags, 
#   vec_freq_limit = vec_freq_limit, 
#   t_tresh = t_tresh, 
#   number_of_cv = number_of_cv, 
#   train_span = train_span, 
#   max_rank_some_h = max_rank_some_h,
#   other_prechosen_variables = manually_prechosen_variables_at_each_size,
#   results_file_name = results_file_name, 
#   ret_cv = return_cv, 
#   max_small_rank = 3, 
#   max_rank_some_h_for_freq = 30
# ) 



