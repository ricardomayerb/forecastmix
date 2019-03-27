source('./R/combinations_functions.R')

# data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
data_object_per_new <- readRDS("./data/VAR_data_Chile.rds")
print(colnames(data_object_per_new))
target_transformation <- readRDS("./data/target_transformation/target_transformation_Chile.rds")
target_transformation <- target_transformation$target_transformation
# country <- data_object_ury$country_name
# target_transformation <- data_object_ury$target_transformation
# raw_data <- data_object_ury$raw_data
# var_data <- data_object_ury$transformed_data
raw_data <- readRDS("./data/raw_VAR_data/raw_VAR_data_Chile.rds")
var_data <- data_object_per_new

# exclude "exp_tradicional" and keep "exp" and "exp_notradicional" 
var_data <- var_data[, ! colnames(var_data) == "exp_tradicional"]

target_variable <- "rgdp"
print(target_transformation)
n_cv <- 8
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])
threshold <- 1.65

# Extend exogenous variables
exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")

names_exogenous <- exogenous_variables 

# Forecast the exogenous variables with Arima models. These are used later on in the VAR forecasts and cv with exo variables
exodata_fullsample <- var_data[,exogenous_variables] # note that exogenous_variables is specified at the start of the scirpt and contains all exogenous variables to Uruguay's economic activity.
target_used_in_VAR <- var_data[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))


# tic()
# extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
#                                         endo_end = end_target_in_VAR)
# toc()
# tic()
# cv_extension_of_exo <- extending_exogenous_for_cv(
#   exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR, 
#   n_cv = n_cv, same_model_across_cv = FALSE)
# toc()
# saveRDS(extension_of_exo, file = "./data/extension_of_exo_us_ue_asia.rds")
# saveRDS(cv_extension_of_exo, file = "./data/cv_extension_of_exo_us_ue_asia.rds")

extension_of_exo <- readRDS(file = "./data/extension_of_exo_us_ue_asia.rds")
cv_extension_of_exo <- readRDS(file = "./data/cv_extension_of_exo_us_ue_asia.rds")

names_all <- colnames(var_data)
names_all

models_from_search_1 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_1.rds")
models_from_search_2 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_2.rds")
models_from_search_3 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_3.rds")
models_from_search_4 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_4.rds")
models_from_search_5 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_5.rds")
models_from_search_6 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_6.rds")

models_from_search <- rbind(models_from_search_1$all_passing_models_2345,
                            models_from_search_2$all_passing_models_2345,
                            models_from_search_3$all_passing_models_2345,
                            models_from_search_4$all_passing_models_2345,
                            models_from_search_5$all_passing_models_2345,
                            models_from_search_6$all_passing_models_2345)

nt <- nrow(models_from_search)
nt
models_per_h_to_work <- 50
working_models <- discard_by_rank(models_tbl = models_from_search, max_rank_h = models_per_h_to_work, is_wide = TRUE)

# to free some memory
rm(models_from_search, models_from_search_1, models_from_search_2, models_from_search_3, models_from_search_4, models_from_search_5, models_from_search_6)


####

tic()
fc_individual_and_ensemble <- fc_models_and_ensemble_by_row(
  var_data = var_data,
  working_models = working_models,
  extension_of_exo = extension_of_exo,
  cv_extension_of_exo = cv_extension_of_exo,
  fc_horizon = fc_horizon,
  target_level_ts = target_level_ts,
  target_transform = target_transform,
  n_cv = n_cv,
  training_length = training_length,
  names_exogenous = names_exogenous,
  max_rank_h = models_per_h_to_work)
toc()

fc_individual_and_ensemble %>%
  filter(short_name == "ensemble") %>%
  dplyr::select(short_name, ranking) %>% print()


tic()
fc_individual_and_ensemble_40 <- fc_models_and_ensemble_by_row(
  var_data = var_data,
  working_models = working_models,
  extension_of_exo = extension_of_exo,
  cv_extension_of_exo = cv_extension_of_exo,
  fc_horizon = fc_horizon,
  target_level_ts = target_level_ts,
  target_transform = target_transform,
  n_cv = n_cv,
  training_length = training_length,
  names_exogenous = names_exogenous,
  max_rank_h = 40)
toc()

fc_individual_and_ensemble_40 %>%
  filter(short_name == "ensemble") %>%
  dplyr::select(short_name, ranking) %>% print()



tic()
fc_individual_and_ensemble_30 <- fc_models_and_ensemble_by_row(
  var_data = var_data,
  working_models = working_models,
  extension_of_exo = extension_of_exo,
  cv_extension_of_exo = cv_extension_of_exo,
  fc_horizon = fc_horizon,
  target_level_ts = target_level_ts,
  target_transform = target_transform,
  n_cv = n_cv,
  training_length = training_length,
  names_exogenous = names_exogenous,
  max_rank_h = 30)
toc()

fc_individual_and_ensemble_30 %>%
  filter(short_name == "ensemble") %>%
  dplyr::select(short_name, ranking) %>% print()



tic()
fc_individual_and_ensemble_20 <- fc_models_and_ensemble_by_row(
  var_data = var_data,
  working_models = working_models,
  extension_of_exo = extension_of_exo,
  cv_extension_of_exo = cv_extension_of_exo,
  fc_horizon = fc_horizon,
  target_level_ts = target_level_ts,
  target_transform = target_transform,
  n_cv = n_cv,
  training_length = training_length,
  names_exogenous = names_exogenous,
  max_rank_h = 20)
toc()

fc_individual_and_ensemble_20 %>%
  filter(short_name == "ensemble") %>%
  dplyr::select(short_name, ranking) %>% print()



tic()
fc_individual_and_ensemble_10 <- fc_models_and_ensemble_by_row(
  var_data = var_data,
  working_models = working_models,
  extension_of_exo = extension_of_exo,
  cv_extension_of_exo = cv_extension_of_exo,
  fc_horizon = fc_horizon,
  target_level_ts = target_level_ts,
  target_transform = target_transform,
  n_cv = n_cv,
  training_length = training_length,
  names_exogenous = names_exogenous,
  max_rank_h = 10)
toc()

fc_individual_and_ensemble_10 %>%
  filter(short_name == "ensemble") %>%
  dplyr::select(short_name, ranking) %>% print()






tic()
ensemble_fc_list_10 <- ensemble_fc_by_row(var_data = var_data,
                                         models_tbl = working_models,
                                         extended_exo_mts =  extension_of_exo$extended_exo,
                                         fc_horizon = fc_horizon,
                                         target_level_ts = target_level_ts,
                                         target_transform = target_transform,
                                         names_exogenous = names_exogenous,
                                         max_rank_h = 10)
toc()

tic()
ensemble_fc_list_5 <- ensemble_fc_by_row(var_data = var_data,
                                         models_tbl = working_models,
                                         extended_exo_mts =  extension_of_exo$extended_exo,
                                         fc_horizon = fc_horizon,
                                         target_level_ts = target_level_ts,
                                         target_transform = target_transform,
                                         names_exogenous = names_exogenous,
                                         max_rank_h = 5)
toc()


tic()
ensemble_fc_list_5_ss <- ensemble_fc_by_row(var_data = var_data,
                                         models_tbl = working_models,
                                         extended_exo_mts =  extension_of_exo$extended_exo,
                                         fc_horizon = fc_horizon,
                                         target_level_ts = target_level_ts,
                                         target_transform = target_transform,
                                         names_exogenous = names_exogenous,
                                         max_rank_h = 5, superset_fcs = )
toc()

moo5 <- filter(ensemble_fc_list_10$model_and_fcs, rank_h <= 5) %>% 
  arrange(short_name)

eoo5 <- ensemble_fc_list_5$model_and_fcs %>% 
  arrange(short_name)

moo5
eoo5
identical(moo5$short_name, eoo5$short_name)
identical(moo5$rmse, eoo5$rmse)



cv_of_ensamble_list_10 <- cv_of_VAR_ensemble_by_row(var_data = var_data,
                                                 used_cv_models = working_models,
                                                 fc_horizon = fc_horizon,
                                                 n_cv = n_cv,
                                                 training_length = training_length,
                                                 cv_extension_of_exo = cv_extension_of_exo$future_exo_cv,
                                                 extension_of_exo = extension_of_exo$extended_exo,
                                                 names_exogenous = names_exogenous,
                                                 target_transform = target_transform,
                                                 target_level_ts = target_level_ts,
                                                 max_rank_h = 10,
                                                 full_cv_output = TRUE)





# foo <- ensemble_fc_list$model_and_fcs %>%  arrange(rmse_h, rmse)
# 
# foo_1_5 <- foo[1:5,]
# r_1_5 <- foo_1_5$rmse
# i_1_5 <- 1/(r_1_5*r_1_5)
# s_1_5 <- sum(i_1_5)
# w_1_5 <- i_1_5/s_1_5
# w_1_5
# foo_1_5$model_weight
# 
# 
# 
# 
# cv_of_ensamble_list <- cv_of_VAR_ensemble_by_row(var_data = var_data,
#                                                  used_cv_models = working_models,
#                                                  fc_horizon = fc_horizon,
#                                                  n_cv = n_cv,
#                                                  training_length = training_length,
#                                                  cv_extension_of_exo = cv_extension_of_exo$future_exo_cv,
#                                                  extension_of_exo = extension_of_exo$extended_exo,
#                                                  names_exogenous = names_exogenous,
#                                                  target_transform = target_transform,
#                                                  target_level_ts = target_level_ts,
#                                                  max_rank_h = models_per_h_to_work,
#                                                  full_cv_output = FALSE)
# 
# 
# 
# 
# ensemble_fc_and_rmse <- ensemble_fc_list$ensemble_tbl %>%
#   mutate(rmse_h = paste0("rmse_", 1:n()),
#          rmse = cv_of_ensamble_list$ensemble_rmse)
# 
# fcs_models_to_bind <- models_fc_list$model_and_fcs %>%
#   mutate(lags = list(lags)) %>%
#   dplyr::select(names(ensemble_fc_and_rmse))
# 
# models_and_ensemble_fcs <- rbind(ensemble_fc_and_rmse,
#                                  fcs_models_to_bind)

  

