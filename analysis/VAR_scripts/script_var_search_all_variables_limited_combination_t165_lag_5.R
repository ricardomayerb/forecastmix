source('./R/combinations_functions.R')
library(openxlsx)

# data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
data_object_per_new <- readRDS("./data/VAR_data_Brasil.rds")
print(colnames(data_object_per_new))
target_transformation <- readRDS("./data/target_transformation/target_transformation_Brasil.rds")
target_transformation <- target_transformation$target_transformation
# country <- data_object_ury$country_name
# target_transformation <- data_object_ury$target_transformation
# raw_data <- data_object_ury$raw_data
# var_data <- data_object_ury$transformed_data
raw_data <- readRDS("./data/raw_VAR_data/raw_VAR_data_Brasil.rds")
var_data <- data_object_per_new

target_variable <- "rgdp"
print(target_transformation)
n_cv <- 8
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])
threshold <- 1.65
# Extend exogenous variables
# names_exogenous <- c("ip_us","ip_asia","ip_ue","ip_bra","act_eco_bra","emae_arg")

exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")
names_exogenous <- exogenous_variables 

# Forecast the exogenous variables with Arima models. These are used later on in the VAR forecasts and cv with exo variables
exodata_fullsample <- var_data[,exogenous_variables] # note that exogenous_variables is specified at the start of the scirpt and contains all exogenous variables to Uruguay's economic activity.
target_used_in_VAR <- var_data[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))

tic()
extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
                                        endo_end = end_target_in_VAR)
toc()

# extended_exo_mts <- extension_of_exo$extended_exo

# extension_of_exo[["future_exo"]]
# extension_of_exo[["extended_exo"]]
# extension_of_exo[["arima_models"]]

tic()
cv_extension_of_exo <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR, 
  n_cv = n_cv, same_model_across_cv = FALSE)
toc()
# cv_extended_exo_mts <- cv_extension_of_exo
# extension_of_exo <- readRDS(file = "./data/examples/example_extension_of_exo.rds")
# cv_extension_of_exo <- readRDS(file = "./data/examples/example_cv_extension_of_exo.rds")


# Number of combinations with all variables
ncombs_33 <- map(2:7, ~ count_combn(var_size = .x, n_total = 33, n_exo = 3, n_fixed = 1))
ntable_33 <- as_tibble(cbind(n = 33, n_fixed = 1, size = 2:7, reduce(ncombs_33, rbind)))
print(ntable_33)


# ncombs_15 <- map(2:7, ~ count_combn(var_size = .x, n_total = 15, n_exo = 6, n_fixed = 1))
# ntable_15 <- as_tibble(cbind(n = 15, n_fixed = 1, size = 2:7, reduce(ncombs_15, rbind)))
# print(ntable_15)
# 
# 
# ncombs_12 <- map(2:7, ~ count_combn(var_size = .x, n_total = 12, n_exo = 6, n_fixed = 1))
# ntable_12 <- as_tibble(cbind(n = 12, n_fixed = 1, size = 2:7, reduce(ncombs_12, rbind)))
# print(ntable_12)

names_all <- colnames(var_data)
names_all
# names_12 <- names_all[c(1,2,3,4,5,11,12,16,17,23,25,29)]
# names_15 <- c(names_12, names_all[c(15, 19, 20)])
# names_20 <- c(names_15, names_all[c(7, 8, 13, 28, 30)])
# names_25 <- c(names_20, names_all[c(6, 9, 31, 18, 27)])
# length(names_25)
# names_25



# so with 15 variables, we have, summing sizes 2 to 5, 1470 (or 1414 if we dicard all-exogenous VARs)
# that mea exploring the performance of 5880 VARs, tow-third of them restricted vars (or 5656)
# With 12 variables the 1470 go down to 561 and 5880 to 2244 (505 and 2020)



# var_size <- 3 
# all_variables <- names_15
# target_variable <- "rgdp"
# non_target_fixed <- c("")
lag_choices <- c(5)
# var_data_25 <- var_data[, names_25]


accumulated_tried_models <- tibble()
accumulated_passing_models <- tibble()

tic()
specs_size_2_u <- all_specifications(
  var_size = 2,
  all_variables = names_all,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data,
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)
toc()

# tic()
# ftmt_size_2 <- fit_tests_models_table(specs_size_2_u, 
#                                       var_data = var_data_25,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_2 <- ftmt_size_2[["passing_models"]]
# 
# 
# tic()
# ftmt_size_3 <- fit_tests_models_table(specs_size_3_u, 
#                                          var_data = var_data_25,
#                                          names_exogenous = names_exogenous
# )
# toc()
# pm_size_3 <- ftmt_size_3[["passing_models"]]
# 
# 
# tic()
# ftmt_size_4 <- fit_tests_models_table(specs_size_4_u, 
#                                       var_data = var_data_25,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_4 <- ftmt_size_4[["passing_models"]]
# 
# 
# tic()
# ftmt_size_5 <- fit_tests_models_table(specs_size_5_u, 
#                                       var_data = var_data_25,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_5 <- ftmt_size_5[["passing_models"]]


tic()
cv_size_2 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_2_u, 
                                   var_data = var_data, 
                                   fit_column = NULL, 
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts, 
                                   names_exogenous = exogenous_variables, 
                                   future_exo = extension_of_exo, 
                                   extended_exo_mts = cv_extension_of_exo,
                                   keep_varest_obj = FALSE,
                                   do_tests = TRUE
)
toc()
tried_models_size_2_by_col <- cv_size_2$tried_models
passing_models_size_2_by_col <- cv_size_2$passing_models

# accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_2)
# accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_2)

tic()
cv_size_2_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                              training_length = training_length, 
                                              models_tbl = specs_size_2_u, 
                                              var_data = var_data,
                                              target_transform = target_transform, 
                                              target_level_ts = target_level_ts, 
                                              names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()

tried_models_size_2 <- cv_size_2_per_row$tried_models_tbl
passing_models_size_2 <- cv_size_2_per_row$passing_models_tbl
accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_2)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_2)



# saveRDS(list(cv_size_2 = cv_size_2),
#         file = "./data/forecast_models/all_ury_models_25_variables_new_data_all_variables_restricted_combos_s2.rds")

### generate all size 3 and do cv
tic()
specs_size_3_u <- all_specifications(
  var_size = 3,
  all_variables = names_all,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data,
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)
toc()


# tic()
# cv_size_3 <- cv_var_from_model_tbl(h = fc_horizon,
#                                    training_length = training_length, 
#                                    n_cv = n_cv,
#                                    models_tbl = specs_size_3_u, 
#                                    var_data = var_data, 
#                                    fit_column = NULL, 
#                                    target_transform = target_transform,
#                                    target_level_ts = target_level_ts, 
#                                    names_exogenous = exogenous_variables, 
#                                    future_exo = extended_exo_mts, 
#                                    extended_exo_mts = cv_extended_exo_mts,
#                                    keep_varest_obj = FALSE, 
#                                    do_tests = TRUE
# )
# toc()
# 
# tried_models_size_3_by_col <- cv_size_3$tried_models
# passing_models_size_3_by_col <- cv_size_3$passing_models

tic()
cv_size_3_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_3_u, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()

tried_models_size_3 <- cv_size_3_per_row$tried_models_tbl
passing_models_size_3 <- cv_size_3_per_row$passing_models_tbl

# roo <- passing_models_size_3 %>% arrange(rmse_1)
# coo <- passing_models_size_3_by_col %>% arrange(rmse_1)
# noo <- cbind(roo$short_name, coo$short_name, roo$short_name == coo$short_name)
# View(noo)
# 
# roo_50 <- discard_by_rank(passing_models_size_3, 50) %>% arrange(rmse_1) 
# coo_50 <- discard_by_rank(passing_models_size_3_by_col, 50) %>% arrange(rmse_1)
# noo_50 <- cbind(roo_50$short_name[1:50], coo_50$short_name[1:50], roo_50$short_name[1:50] == coo_50$short_name[1:50])
# View(noo_50)
# soo_50 <- cbind(sort(roo_50$short_name[1:50]), sort(coo_50$short_name[1:50]), sort(roo_50$short_name[1:50]) == sort(coo_50$short_name[1:50]))
# View(soo_50)


# Add all new passing models to the passing models of size 2
accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_3)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_3)

# # Make sure that we only retain unique models
# accumulated_tried_models <- mutate(
#   accumulated_tried_models, 
#   short_name = pmap(list(variables, lags, t_threshold),
#                     ~ make_model_name(variables = ..1, 
#                                       lags = ..2,
#                                       t_threshold = ..3)),
#   short_name = unlist(short_name)
# )

# saveRDS(list(cv_size_3 = cv_size_3),
#              file = "./data/forecast_models/all_ury_models_25_variables_new_data_all_variables_restricted_combos_s3.rds")

###### start size 4 choosing specs and variables
# We know from print(ntable_31) that the number of specifications gets really large from size 4 on
# Therefore, we tried to come up with strategies to reduce the number of recommendations
# We combine two strategies: 
# (1) At each h we collect the best 10 models in the accumulated models tibble. Then at each h for each of these 10 models
# we augment it by increasing its size by any combination possible given the model specification and the variables not used
# in that specification yet. 
# (2) try all models but with reduced number of variables. We try all models at the new size for only those variables 
# that are among the best 10 ranked in terms of frequency in the accumulated models tibble.

# Strategy 1
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)

# All combination of the unique best 10 models at each h extended with all the other variables
in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_all)

# Elminate models that have been already tried
not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]

# Stategy 2: try all models but with reduced number of variables. max_small_rank = 3 means the top 3 models per h
f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 3)

f_vbls$vbl_freqs_by_h

new_names_by_freq <- sort(f_vbls[["variables_in_top_small"]])

specs_size_4_freq <- all_specifications(
  var_size = 4,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data[, new_names_by_freq],
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)

specs_size_4_freq <- mutate(specs_size_4_freq,
                            short_name = pmap(list(variables, lags, t_threshold),
                                              ~ make_model_name(variables = ..1, 
                                                                lags = ..2,
                                                                t_threshold = ..3)),
                            short_name = unlist(short_name))

not_in_best_10 <- !specs_size_4_freq$short_name %in% in_best_10_augmented_not_tried$short_name
sum(not_in_best_10)
specs_size_4_freq_not_in_best_10 <- specs_size_4_freq[not_in_best_10, ]
not_tried <- !specs_size_4_freq_not_in_best_10$short_name %in% accumulated_tried_models$short_name
specs_size_4_freq_proposed <- specs_size_4_freq_not_in_best_10[not_tried, ]


proposed_specs_s4 <- rbind(dplyr::select(in_best_10_augmented_not_tried, 
                                         names(specs_size_4_freq_proposed)),
                           specs_size_4_freq_proposed)

nrow(distinct(proposed_specs_s4, short_name))

# tic()
# cv_proposed_size_4 <- cv_var_from_model_tbl(
#   h = fc_horizon,
#   training_length = training_length, 
#   n_cv = n_cv,
#   models_tbl = proposed_specs_s4, 
#   var_data = var_data, 
#   fit_column = NULL, 
#   target_transform = target_transform,
#   target_level_ts = target_level_ts, 
#   names_exogenous = exogenous_variables, 
#   future_exo = extension_of_exo, 
#   extended_exo_mts = cv_extension_of_exo,
#   keep_varest_obj = FALSE, 
#   do_tests = TRUE
# )
# toc()

tic()
cv_size_4_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = proposed_specs_s4, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()


## update passing and tried models
tried_models_size_4_pr <- cv_size_4_per_row$tried_models_tbl
passing_models_size_4_pr <- cv_size_4_per_row$passing_models_tbl

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_4_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_4_pr)



# tried_models_size_4 <- cv_size_4$tried_models
# passing_models_size_4 <- cv_size_4$passing_models


# saveRDS(list(cv_size_4 = cv_proposed_size_4),
#              file = "./data/forecast_models/all_ury_models_25_variables_new_data_all_variables_restricted_combos_s4.rds")

######  start size 5 choosing specs and variables
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)

# All combination of the unique best 10 models at each h extended with all the other variables
in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_all)

# Elminate models that have been already tried
not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]

# Stategy 2: try all models but with reduced number of variables
f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 2)

f_vbls$vbl_freqs_by_h

new_names_by_freq <- sort(f_vbls[["variables_in_top_small"]])

specs_size_5_freq <- all_specifications(
  var_size = 5,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20[, new_names_by_freq],
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)

specs_size_5_freq <- mutate(specs_size_5_freq,
                            short_name = pmap(list(variables, lags, t_threshold),
                                              ~ make_model_name(variables = ..1, 
                                                                lags = ..2,
                                                                t_threshold = ..3)),
                            short_name = unlist(short_name))

not_in_best_10 <- !specs_size_5_freq$short_name %in% in_best_10_augmented_not_tried$short_name
sum(not_in_best_10)
specs_size_5_freq_not_in_best_10 <- specs_size_5_freq[not_in_best_10, ]
not_tried <- !specs_size_5_freq_not_in_best_10$short_name %in% accumulated_tried_models$short_name
specs_size_5_freq_proposed <- specs_size_5_freq_not_in_best_10[not_tried, ]


proposed_specs_s5 <- rbind(dplyr::select(in_best_10_augmented_not_tried, 
                                         names(specs_size_5_freq_proposed)),
                           specs_size_5_freq_proposed)

nrow(distinct(proposed_specs_s5, short_name))

# tic()
# cv_proposed_size_5 <- cv_var_from_model_tbl(
#   h = fc_horizon,
#   training_length = training_length, 
#   n_cv = n_cv,
#   models_tbl = proposed_specs_s5, 
#   var_data = var_data, 
#   fit_column = NULL, 
#   target_transform = target_transform,
#   target_level_ts = target_level_ts, 
#   names_exogenous = exogenous_variables, 
#   future_exo = extension_of_exo, 
#   extended_exo_mts = cv_extension_of_exo,
#   keep_varest_obj = FALSE, 
#   do_tests = TRUE
# )
# toc()

tic()
cv_size_5_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = proposed_specs_s5, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()





## update passing and tried models
tried_models_size_5_pr <- cv_size_5_per_row$tried_models_tbl
passing_models_size_5_pr <- cv_size_5_per_row$passing_models_tbl

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_5_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_5_pr)


accumulated_passing_models <- distinct(accumulated_passing_models, short_name, .keep_all = TRUE)
accumulated_tried_models <- distinct(accumulated_tried_models, short_name, .keep_all = TRUE)

write.xlsx(accumulated_passing_models, file = "./Excel_Output/Brasil/All_accumulated_passing_models_Brasil_t165_lag_5.xlsx")

  
# saveRDS(list(cv_size_5 = cv_proposed_size_5),
#              file = "./data/forecast_models/all_ury_models_25_variables_new_data_all_variables_restricted_combos_s5.rds")

# all_passing_models_2345 <- rbind(passing_models_size_2, passing_models_size_3,
#                                  passing_models_size_4, passing_models_size_5)
# 
# all_tried_models_2345 <- rbind(tried_models_size_2, tried_models_size_3,
#                                  tried_models_size_4, tried_models_size_5)
# 
# n_total_passing <- nrow(all_passing_models_2345)
# n_total_tried <- nrow(all_tried_models_2345)


saveRDS(list(cv_size_2 = cv_size_2, cv_size_3 = cv_size_3, 
             cv_size_4 = cv_proposed_size_4, cv_size_5 = cv_proposed_size_5, 
             all_tried_models_2345 = accumulated_tried_models,
             all_passing_models_2345 = accumulated_passing_models),
        file = "./data/forecast_models/all_brasil_models_new_data_all_variables_restricted_combos_t165_lag_5.rds")

