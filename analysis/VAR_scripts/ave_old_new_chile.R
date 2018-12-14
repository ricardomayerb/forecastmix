source('./R/combinations_functions.R')

forecast_exercise_year <- 2018
forecast_exercise_number <- 3
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")
country <- "Chile"
bra_search_results_name <- "vr_Chile_auto_3s4_2s5_norest" 
bra_search_results <- readRDS(paste0(output_path, bra_search_results_name, ".rds"))

VAR_data_for_estimation <- bra_search_results$var_data

bra_models_tbl <-  bra_search_results$consolidated_var_res

max_VAR_models_per_h <- bra_search_results$max_rank_some_h
print(paste0("max_VAR_models_per_h = ", max_VAR_models_per_h))

n_cv <- bra_search_results$number_of_cv
print(paste0("n_cv = ", n_cv))

training_length <- bra_search_results$train_span
print(paste0("training_length = ", training_length))

names_exogenous <- bra_search_results$names_exogenous
print("names_exogenous = ")
print(names_exogenous)

fc_horizon <- bra_search_results$fc_horizon
print(paste0("fc_horizon = ", fc_horizon))

rgdp_transformation <- bra_search_results$target_variable_transform
print(paste0("rgdp_transformation = ", rgdp_transformation))

data_ts <- get_raw_data_ts(country, data_path = "./data/edd_exercises/2018_exercise_3/")
rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

smaller_max_VAR_models_per_h <- 30

bra_models_tbl_smaller <- as_tibble(bra_models_tbl) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 

exodata_fullsample <- VAR_data_for_estimation[,names_exogenous]
target_used_in_VAR <- VAR_data_for_estimation[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))

tic()
extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
                                        endo_end = end_target_in_VAR)
toc()


tic()
cv_extension_of_exo <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = 8, endo_end = end_target_in_VAR, 
  n_cv = n_cv, same_model_across_cv = FALSE)
toc()

bra_no_t <- bra_models_tbl_smaller %>% dplyr::select(-t_treshold)

tic()
cv_less <- cv_var_from_model_tbl(
  h = fc_horizon,
  n_cv = n_cv,
  training_length = training_length,
  models_tbl = bra_models_tbl_smaller,
  var_data = VAR_data_for_estimation,
  new_t_threshold = c(0, 1.65, 2),
  target_level_ts = rgdp_level_ts,
  target_transform = rgdp_transformation,
  names_exogenous = names_exogenous,
  future_exo = extension_of_exo[["future_exo"]],
  future_exo_cv = cv_extension_of_exo[["future_exo_cv"]],
  keep_fc_objects = TRUE, do_full_sample_fcs = TRUE, 
  extended_exo_mts = extension_of_exo[["extended_exo"]])
toc()

quasi_ave_5 <- ave_fc_from_cv(cv_less, best_n_to_keep = 5)
quasi_ave_10 <- ave_fc_from_cv(cv_less, best_n_to_keep = 10)
quasi_ave_20 <- ave_fc_from_cv(cv_less, best_n_to_keep = 20)
quasi_ave_30 <- ave_fc_from_cv(cv_less, best_n_to_keep = 30)

print(quasi_ave_5$ave_by_h_fc)
print(quasi_ave_10$ave_by_h_fc)
print(quasi_ave_20$ave_by_h_fc)
print(quasi_ave_30$ave_by_h_fc)


target_yoy_realized_and_fc <- ts(c(rgdp_yoy_ts, quasi_ave_20$ave_by_h_fc), 
                                 frequency = frequency(rgdp_yoy_ts), 
                                 start = start(rgdp_yoy_ts))

year_2018 <- window(target_yoy_realized_and_fc, start = c(2018,1), end = c(2018,4))

year_2019 <- window(target_yoy_realized_and_fc, start = c(2019,1), end = c(2019,4))
year_2018
year_2019
mean(year_2018)
mean(year_2019)

saveRDS(list(target_yoy_realized_and_fc, quasi_ave_10, quasi_ave_20, quasi_ave_30), file = paste0(country, "_quasi_ave.rds"))




old_search_results_name <- paste0(output_path, "from_older_code/", country,"_by_step_12345.rds")
old_search_results <- readRDS(old_search_results_name)
names(old_search_results)

old_models_tbl_smaller <- as_tibble(old_search_results) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 

tic()
cv_less_old <- cv_var_from_model_tbl(
  h = fc_horizon,
  n_cv = n_cv,
  training_length = training_length,
  models_tbl = old_models_tbl_smaller,
  var_data = VAR_data_for_estimation,
  new_t_threshold = c(0, 1.65, 2),
  target_level_ts = rgdp_level_ts,
  target_transform = rgdp_transformation,
  names_exogenous = names_exogenous,
  future_exo = extension_of_exo[["future_exo"]],
  future_exo_cv = cv_extension_of_exo[["future_exo_cv"]],
  keep_fc_objects = TRUE, do_full_sample_fcs = TRUE, 
  extended_exo_mts = extension_of_exo[["extended_exo"]])
toc()


quasi_ave_5_old <- ave_fc_from_cv(cv_less_old, best_n_to_keep = 5)
quasi_ave_10_old <- ave_fc_from_cv(cv_less_old, best_n_to_keep = 10)
quasi_ave_20_old <- ave_fc_from_cv(cv_less_old, best_n_to_keep = 20)
quasi_ave_30_old <- ave_fc_from_cv(cv_less_old, best_n_to_keep = 30)

print(quasi_ave_5_old$ave_by_h_fc)
print(quasi_ave_10_old$ave_by_h_fc)
print(quasi_ave_20_old$ave_by_h_fc)
print(quasi_ave_30_old$ave_by_h_fc)


target_yoy_realized_and_fc_old <- ts(c(rgdp_yoy_ts, quasi_ave_20_old$ave_by_h_fc), 
                                     frequency = frequency(rgdp_yoy_ts), 
                                     start = start(rgdp_yoy_ts))

year_2018_old <- window(target_yoy_realized_and_fc_old, start = c(2018,1), end = c(2018,4))

year_2019_old <- window(target_yoy_realized_and_fc_old, start = c(2019,1), end = c(2019,4))
year_2018_old
year_2019_old
mean(year_2018_old)
mean(year_2019_old)


saveRDS(list(target_yoy_realized_and_fc_old, quasi_ave_10_old, quasi_ave_20_old,
             quasi_ave_30_old), file = paste0(country, "_quasi_ave_om.rds"))
