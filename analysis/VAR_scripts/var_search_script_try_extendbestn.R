source('./R/VAR_functions.R')

country <- "Chile"
print(paste0("this country: ", country))
names_exogenous <- c("ip_us", "ip_ue", "ip_asia")
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 8
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
# vec_lags <-  c(5) # default value, can be omitted for shorter code

default_vec_lag <-  c(4)
default_treshold <- 1.65

Search_step_1 <- list(size = 2, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_2 <- list(size = 3, vbl_selection_type = "none", lags = default_vec_lag)
# Search_step_3 <- list(size = 4, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag)
# Search_step_3 <- list(size = 4, vbl_selection_type = "manually_prechosen_variables", manually_prechosen = list(c("primario"), c("imp_capital")),  lags = default_vec_lag)
Search_step_3 <- list(size = 4, vbl_selection_type = "manually_prechosen_variables", manually_prechosen = list(c("primario"), c("imp_capital")),
                      add_augmented_models = "TRUE",  lags = default_vec_lag)


# Search_step_4 <- list(size = 5, vbl_selection_type = "manually_prechosen_variables", manually_prechosen = list(c("primario" , "imp_capital"), c("imp_capital", "imp")), lags = default_vec_lag)
# search_plan <- list(Search_step_1, Search_step_2, Search_step_3, Search_step_4)
# search_plan <- list(Search_step_2)
search_plan <- list(Search_step_1, Search_step_2, Search_step_3)


number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- "common_max"
max_rank_some_h <- 40 # default value, can be omitted for shorter code
results_file_name <-  paste0(country, "_manual.rds")
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


li1 <- list(c("rgdp", "cred"), c("rgdp", "ip_us"), c("rgdp", "ip_ue"), c("rgdp", "ip_asia"))

li2 <- list(c("rgdp", "imp", "imp_capital"), c("rgdp", "imp", "electricity"),
            c("rgdp", "imp", "vtas_superm"), c("rgdp", "imp", "copper_output"))
li3 <- c(li1, li2)

v1 <- c("rgdp", "ip_ue")
v2 <- c("rgdp", "ri")
v3 <- c("rgdp", "imp", "electricity")
v4 <- c("rgdp", "imp", "rpc")


any(map_lgl(li1,  ~ identical(.x, v1)))
any(map_lgl(li2,  ~ identical(.x, v1)))
any(map_lgl(li3,  ~ identical(.x, v1)))

any(map_lgl(li1,  ~ identical(.x, v2)))
any(map_lgl(li2,  ~ identical(.x, v2)))
any(map_lgl(li3,  ~ identical(.x, v2)))

any(map_lgl(li1,  ~ identical(.x, v3)))
any(map_lgl(li2,  ~ identical(.x, v3)))
any(map_lgl(li3,  ~ identical(.x, v3)))

any(map_lgl(li1,  ~ identical(.x, v4)))
any(map_lgl(li2,  ~ identical(.x, v4)))
any(map_lgl(li3,  ~ identical(.x, v4)))



