source('./R/combinations_functions.R')

smaller_max_VAR_models_per_h <- 30
forecast_exercise_year <- 2018
forecast_exercise_number <- 3
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")
country <- "Brasil"
bra_search_results_name <- "vr_Brasil_auto_3s4_2s5_norest" 
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

# extension_of_exo[["future_exo"]]
# extension_of_exo[["extended_exo"]]
# extension_of_exo[["arima_models"]]

tic()
shoo_cv_rw <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = 8, endo_end = end_target_in_VAR, 
  n_cv = n_cv, same_model_across_cv = FALSE)
toc()

foo <- bra_models_tbl_smaller %>% dplyr::select(-t_treshold)

tic()
fc_from_cv_tbl_nott <- forecast_var_from_model_tbl(
  models_tbl = foo, 
  var_data = VAR_data_for_estimation,
  fc_horizon = fc_horizon, 
  new_t_threshold = c(0, 1.65, 2),
  target_transform = rgdp_transformation, 
  target_level_ts = rgdp_level_ts, 
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo[["extended_exo"]])
toc()


tic()
fc_from_cv_tbl <- forecast_var_from_model_tbl(
  models_tbl = bra_models_tbl_smaller, 
  var_data = VAR_data_for_estimation,
  fc_horizon = fc_horizon, 
  new_t_threshold = c(0, 1.65, 2),
  target_transform = rgdp_transformation, 
  target_level_ts = rgdp_level_ts, 
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo[["extended_exo"]])
toc()



tic()
cv_less_nott <- cv_var_from_model_tbl(
  h = fc_horizon,
  n_cv = n_cv,
  training_length = training_length,
  models_tbl = foo,
  var_data = VAR_data_for_estimation,
  new_t_threshold = c(0, 1.65, 2),
  target_level_ts = rgdp_level_ts,
  target_transform = rgdp_transformation,
  names_exogenous = names_exogenous,
  future_exo = extension_of_exo[["future_exo"]],
  future_exo_cv = shoo_cv_rw[["future_exo_cv"]],
  keep_fc_objects = TRUE, do_full_sample_fcs = TRUE, 
  extended_exo_mts = extension_of_exo[["extended_exo"]])
toc()


ave_fc_from_cv <- function(cv_tbl, best_n_to_keep = "all", is_wide = TRUE) {
  
  if (is_wide) {
    
    rmse_names <- names(cv_tbl)[str_detect(names(cv_tbl), "rmse")]

    cv_tbl <- cv_tbl %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>%
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
      group_by(rmse_h) %>%
      arrange(rmse_h, rmse) %>%
      mutate(rank_h = rank(rmse)) %>%
      ungroup() %>%
      mutate(lags = unlist(lags),
             model_type = "VAR")
  }

  if (best_n_to_keep == "all") {
    cv_tbl <- cv_tbl
  }

  if (is.numeric(best_n_to_keep)) {
    cv_tbl <- cv_tbl %>%
      arrange(rmse_h, rmse) %>%
      group_by(rmse_h) %>%
      mutate(rank_h = rank(rmse)) %>%
      filter(rank_h <= best_n_to_keep) %>%
      mutate(inv_mse = 1/(rmse*rmse),
             model_weight_h = inv_mse/sum(inv_mse),
             weighted_fc_h = map2(target_mean_fc_yoy, model_weight_h, ~ .x * .y)
      ) %>%
      ungroup()
  }

  
  # ave_fc <- cv_tbl %>%
  #   group_by(rmse_h) %>%
  #   summarise(ave_fc_h = colSums(reduce(weighted_fc_h, rbind)))
  # return(list(ave_fc = ave_fc, cv_tbl = cv_tbl))
  return(cv_tbl)
  
}


quasi_ave <- ave_fc_from_cv(cv_less_nott, best_n_to_keep = 20)

# quasi_ave %>% group_by(rmse_h) %>% 
#   summarise(mats = map(weighted_fc_h, ~ reduce(.x, rbind) ))

yoo <- quasi_ave$weighted_fc_h 
yoo[[1]]
unlist(yoo[[1]])
yoo[[1]]
yoo[[2]]
yoo[[1]] + yoo[[2]]
loo <- list(yoo[[1]], yoo[[2]])
reduce(loo, sum)
map(loo, sum)
reduce(loo, rbind)
colSums(reduce(loo, rbind))



# tic()
# cv_less <- cv_var_from_model_tbl(
#   h = fc_horizon,
#   n_cv = n_cv,
#   training_length = training_length,
#   models_tbl = bra_models_tbl_smaller,
#   var_data = VAR_data_for_estimation, 
#   new_t_threshold = c(0, 1.65, 2), 
#   target_level_ts = rgdp_level_ts,
#   target_transform = rgdp_transformation, 
#   names_exogenous = names_exogenous,
#   future_exo = extension_of_exo[["future_exo"]],
#   future_exo_cv = shoo_cv_rw[["future_exo_cv"]],
#   keep_fc_objects = TRUE)
# toc()
# 
# 
# 
# 
# 

# 
# 
# 
# 
# 
# 
# 
# # forecast_var_from_model_tbl(
# #   models_tbl = mt_with_fit_new, var_data = VAR_data_for_estimation, 
# #   new_t_threshold = c(0, 1.65, 2), fc_horizon = 8, fit_column = "fit",
# #   target_transform = rgdp_transformation, 
# #   target_level_ts = rgdp_level_ts, names_exogenous = names_exogenous,
# #   extended_exo_mts = extension_of_exo[["extended_exo"]])
# # toc()
# 
# bra_var_20 <- ave_fc_from_cv(bra_models_tbl, best_n_to_keep = 20)
# 

# ave_fc_from_cv <- function(cv_tbl, best_n_to_keep = "all", is_wide = FALSE) {
#   
#   if (is_wide) {
#     rmse_names <- names(cv_tbl)[str_detect(names(cv_tbl), "rmse")]
#     
#     cv_tbl <- cv_tbl %>% 
#       gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
#       dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
#       group_by(rmse_h) %>% 
#       arrange(rmse_h, rmse) %>% 
#       mutate(rank_h = rank(rmse)) %>% 
#       ungroup() %>% 
#       mutate(lags = unlist(lags),
#              model_type = "VAR")
#   }
#   
#   if (best_n_to_keep == "all") {
#     cv_tbl <- cv_tbl
#   }
#   
#   if (is.numeric(best_n_to_keep)) {
#     cv_tbl <- cv_tbl %>% 
#       arrange(rmse_h, rmse) %>%
#       group_by(rmse_h) %>% 
#       mutate(rank_h = rank(rmse)) %>% 
#       filter(rank_h <= best_n_to_keep) %>% 
#       mutate(inv_mse = 1/(rmse*rmse),
#              model_weight_h = inv_mse/sum(inv_mse),
#              weighted_fc_h = fc_yoy*model_weight_h,
#              average_fc_h = sum(weighted_fc_h)
#       ) %>% 
#       ungroup()
#   }
#   
#   ave_fc <- cv_tbl %>% 
#     group_by(rmse_h) %>% 
#     summarise(ave_fc_h = sum(weighted_fc_h))
#   
#   return(ave_fc)
# }
# 
# 





# 
# VAR_data_for_estimation <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/VAR_data_Uruguay.rds")
# 
# target_used_in_VAR <- VAR_data_for_estimation[, "rgdp"]
# start_target_in_VAR <- start(na.omit(target_used_in_VAR))
# end_target_in_VAR <- end(na.omit(target_used_in_VAR))
