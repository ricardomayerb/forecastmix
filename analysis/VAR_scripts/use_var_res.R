source('./R/VAR_functions.R')

country <- "Argentina"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

# Option 1: build filename programmatically from parameter values
# Also some of these parameters are used below in other tasks
sizes <- c(2, 3, 4, 5)
fc_horizon <- 7
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
vec_lags <-  c(1, 2, 3, 4, 5, 6) # default value, can be omitted for shorter code
vec_freq_limit <- c("none", "none", 15, 10) # default value, can be omitted
t_tresh <- c(2, 2, 2, 2) # default value, can be omitted for shorter code
number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- 25 # default value, can be omitted for shorter code
max_rank_some_h <- 50 # default value, can be omitted for shorter code

allsizes <- paste(sizes, collapse = "")
allthresh <- paste(t_tresh, collapse = "")
allfqlim <- paste(vec_freq_limit, collapse = "")
file_suffix_all_sizes <-  paste0("_sizes_", allsizes, "_fqlims_", allfqlim,
                                 "_t_", allthresh, ".rds")
filename <- paste0("var_results_", country, file_suffix_all_sizes)

# # Option 2: give the filename directly
# filename <- "var_results_Argentina_sizes_2345_fqlims_nonenone1510_t_2222.rds"

# Load the VAR results of interest
arg_var_res <- readRDS(paste0(output_path, filename))
arg_var_res_old <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Argentina_by_step_12345.rds") 

arg_var_res_tmp <- arg_var_res %>% 
  mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
         model_type = "new") %>% 
  dplyr::select(-t_treshold) %>% 
  dplyr::select(-lag_sel_method)

arg_var_res_old_tmp <- arg_var_res_old %>% 
  mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
         model_type = "old",
         var_size = map_dbl(variables, length)) %>% 
  dplyr::select(-rmse_8)
  

sort(names(arg_var_res))
sort(names(arg_var_res_old))




