source('./R/combinations_functions.R')

data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
print(names(data_object_ury))
country <- data_object_ury$country_name
target_transformation <- data_object_ury$target_transformation
raw_data <- data_object_ury$raw_data
var_data <- data_object_ury$transformed_data
target_variable <- "rgdp"
print(target_transformation)
n_cv <- 10
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])


names_exogenous <- c("ip_us","ip_asia","ip_ue","ip_bra","act_eco_bra","emae_arg")

extension_of_exo <- readRDS(file = "./data/examples/example_extension_of_exo.rds")
cv_extension_of_exo <- readRDS(file = "./data/examples/example_cv_extension_of_exo.rds")



ncombs_31 <- map(2:7, ~ count_combn(var_size = .x, n_total = 31, n_exo = 6, n_fixed = 1))
ntable_31 <- as_tibble(cbind(n = 31, n_fixed = 1, size = 2:7, reduce(ncombs_31, rbind)))
print(ntable_31)


ncombs_15 <- map(2:7, ~ count_combn(var_size = .x, n_total = 15, n_exo = 6, n_fixed = 1))
ntable_15 <- as_tibble(cbind(n = 15, n_fixed = 1, size = 2:7, reduce(ncombs_15, rbind)))
print(ntable_15)


ncombs_12 <- map(2:7, ~ count_combn(var_size = .x, n_total = 12, n_exo = 6, n_fixed = 1))
ntable_12 <- as_tibble(cbind(n = 12, n_fixed = 1, size = 2:7, reduce(ncombs_12, rbind)))
print(ntable_12)

names_all <- colnames(var_data)
names_all
names_12 <- names_all[c(1,2,3,4,5,11,12,16,17,23,25,29)]
length(names_12)
names_12
names_15 <- c(names_12, names_all[c(15, 19, 20)])
length(names_15)
names_15
names_20 <- c(names_15, names_all[c(7, 8, 13, 28, 30)])
length(names_20)
names_20



# so with 15 variables, we have, summing sizes 2 to 5, 1470 (or 1414 if we dicard all-exogenous VARs)
# that mea exploring the performance of 5880 VARs, tow-third of them restricted vars (or 5656)
# With 12 variables the 1470 go down to 561 and 5880 to 2244 (505 and 2020)



var_size <- 3 
all_variables <- names_15
target_variable <- "rgdp"
non_target_fixed <- c("")
lag_choices <- c(3, 5)
var_data_20 <- var_data[, names_20]
this_t_thresholds <- c(1.65)


tic()
specs_size_2_u <- all_specifications(
  var_size = 2,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = this_t_thresholds,
  names_exogenous = names_exogenous)
toc()


tic()
specs_size_3_u <- all_specifications(
  var_size = 3,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = this_t_thresholds,
  names_exogenous = names_exogenous)
toc()

tic()
specs_size_4_u <- all_specifications(
  var_size = 4,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = this_t_thresholds,
  names_exogenous = names_exogenous)
toc()

tic()
specs_size_5_u <- all_specifications(
  var_size = 5,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = this_t_thresholds,
  names_exogenous = names_exogenous)
toc()





# tic()
# ftmt_size_2 <- fit_tests_models_table(specs_size_2_u, 
#                                       var_data = var_data_20,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_2 <- ftmt_size_2[["passing_models"]]
# 
# 
# tic()
# ftmt_size_3 <- fit_tests_models_table(specs_size_3_u, 
#                                          var_data = var_data_20,
#                                          names_exogenous = names_exogenous
# )
# toc()
# pm_size_3 <- ftmt_size_3[["passing_models"]]
# 
# 
# tic()
# ftmt_size_4 <- fit_tests_models_table(specs_size_4_u, 
#                                       var_data = var_data_20,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_4 <- ftmt_size_4[["passing_models"]]
# 
# 
# tic()
# ftmt_size_5 <- fit_tests_models_table(specs_size_5_u, 
#                                       var_data = var_data_20,
#                                       names_exogenous = names_exogenous
# )
# toc()
# pm_size_5 <- ftmt_size_5[["passing_models"]]


tic()
cv_size_2 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_2_u, 
                                   var_data = var_data_20, 
                                   fit_column = NULL, 
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts, 
                                   names_exogenous = names_exogenous, 
                                   future_exo = extension_of_exo, 
                                   extended_exo_mts = cv_extension_of_exo,
                                   keep_varest_obj = FALSE,
                                   do_tests = TRUE
)
toc()
tried_models_size_2 <- cv_size_2$tried_models
passing_models_size_2 <- cv_size_2$passing_models


tic()
cv_size_3 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_3_u, 
                                   var_data = var_data_20, 
                                   fit_column = NULL, 
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts, 
                                   names_exogenous = names_exogenous, 
                                   future_exo = extension_of_exo, 
                                   extended_exo_mts = cv_extension_of_exo,
                                   keep_varest_obj = FALSE, 
                                   do_tests = TRUE
)
toc()
tried_models_size_3 <- cv_size_3$tried_models
passing_models_size_3 <- cv_size_3$passing_models


tic()
cv_size_4 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_4_u, 
                                   var_data = var_data_20, 
                                   fit_column = NULL, 
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts, 
                                   names_exogenous = names_exogenous, 
                                   future_exo = extension_of_exo, 
                                   extended_exo_mts = cv_extension_of_exo,
                                   keep_varest_obj = FALSE,
                                   do_tests = TRUE
)
toc()
tried_models_size_4 <- cv_size_4$tried_models
passing_models_size_4 <- cv_size_4$passing_models



tic()
cv_size_5 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_5_u, 
                                   var_data = var_data_20, 
                                   fit_column = NULL, 
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts, 
                                   names_exogenous = names_exogenous, 
                                   future_exo = extension_of_exo, 
                                   extended_exo_mts = cv_extension_of_exo,
                                   keep_varest_obj = FALSE,
                                   do_tests = TRUE
)

toc()
tried_models_size_5 <- cv_size_5$tried_models
passing_models_size_5 <- cv_size_5$passing_models

all_passing_models_2345 <- rbind(passing_models_size_2, passing_models_size_3,
                                 passing_models_size_4, passing_models_size_5)

all_tried_models_2345 <- rbind(tried_models_size_2, tried_models_size_3,
                                 tried_models_size_4, tried_models_size_5)

n_total_passing <- nrow(all_passing_models_2345)
n_total_tried <- nrow(all_tried_models_2345)


saveRDS(list(cv_size_2 = cv_size_2, cv_size_3 = cv_size_3, 
             cv_size_4 = cv_size_4, cv_size_5 = cv_size_5, 
             all_tried_models_2345 = all_tried_models_2345,
             all_passing_models_2345 = all_passing_models_2345),
        file = "./data/examples/all_ury_models_20_variables_restr.rds")



