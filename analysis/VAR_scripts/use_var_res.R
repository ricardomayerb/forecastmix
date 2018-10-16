source('./R/VAR_functions.R')

country <- "Argentina"
sizes <- c(2, 3, 4, 5)
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
fc_horizon <- 7
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
vec_lags <-  c(1, 2, 3, 4, 5, 6) # default value, can be omitted for shorter code
vec_freq_limit <- c("none", "none", 15, 10) # default value, can be omitted
t_tresh <- c(2, 2, 2, 2) # default value, can be omitted for shorter code
number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- 25 # default value, can be omitted for shorter code
max_rank_some_h <- 50 # default value, can be omitted for shorter code

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

allsizes <- paste(sizes, collapse = "")
allthresh <- paste(t_tresh, collapse = "")
allfqlim <- paste(vec_freq_limit, collapse = "")

file_suffix_all_sizes <-  paste0("_sizes_", allsizes, "_fqlims_", allfqlim,
                                 "_t_", allthresh, ".rds")

filename <- paste0("var_results_", country, file_suffix_all_sizes)


var_res <- readRDS(paste0(output_path, filename))
