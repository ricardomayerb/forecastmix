source('./R/combinations_functions.R')

all_specs_objects <- readRDS("./data/examples/all_ury_models_20_variables.rds")
all_passing_models <- all_specs_objects[[6]]

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

names_all <- colnames(var_data)
names_all
names_12 <- names_all[c(1,2,3,4,5,11,12,16,17,23,25,29)]
length(names_12)

names_15 <- c(names_12, names_all[c(15, 19, 20)])
length(names_15)
names_15
names_20 <- c(names_15, names_all[c(7, 8, 13, 28, 30)])
length(names_20)
names_20


var_size <- 3 
all_variables <- names_20
target_variable <- "rgdp"
non_target_fixed <- c("")
lag_choices <- c(3, 5)
var_data_20 <- var_data[, names_20]

accumulated_tried_models <- tibble()
accumulated_passing_models <- tibble()

### generate all size 2 and do cv
specs_size_2_u <- all_specifications(
  var_size = 2,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = 0,
  names_exogenous = names_exogenous)


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

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_2)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_2)



### generate all size 3 and do cv
tic()
specs_size_3_u <- all_specifications(
  var_size = 3,
  all_variables = names_20,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()


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

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_3)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_3)

accumulated_tried_models <- mutate(
  accumulated_tried_models, 
  short_name = pmap(list(variables, lags, t_threshold),
                    ~ make_model_name(variables = ..1, 
                                      lags = ..2,
                                      t_threshold = ..3)),
  short_name = unlist(short_name)
)

###### start size 4 choosing specs and variables
  
in_best_2_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 2, is_wide = TRUE)
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)
in_best_20_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 20, is_wide = TRUE)

nrow(in_best_10_some_h)
nrow(in_best_20_some_h)

in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_20)

not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]


f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 3)


new_names_by_freq <- sort(f_vbls$variables_in_top_small)

names_in_best_2 <- unique(unlist(in_best_2_some_h$variables))

new_names_by_freq

new_names_by_freq_and_best_2 <- sort(unique(c(new_names_by_freq, names_in_best_2)))


specs_size_4_freq <- all_specifications(
  var_size = 4,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20[, new_names_by_freq],
  t_thresholds = 0,
  names_exogenous = names_exogenous)

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

tic()
cv_proposed_size_4 <- cv_var_from_model_tbl(
  h = fc_horizon,
  training_length = training_length, 
  n_cv = n_cv,
  models_tbl = proposed_specs_s4, 
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

## update passing and tried models
tried_models_size_4_pr <- cv_proposed_size_4$tried_models
passing_models_size_4_pr <- cv_proposed_size_4$passing_models

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_4_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_4_pr)

accumulated_tried_models <- mutate(
  accumulated_tried_models, 
  short_name = pmap(list(variables, lags, t_threshold),
                    ~ make_model_name(variables = ..1, 
                                      lags = ..2,
                                      t_threshold = ..3)),
  short_name = unlist(short_name)
)


######  start size 5 choosing specs and variables

in_best_2_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 2, is_wide = TRUE)
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)
in_best_20_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 20, is_wide = TRUE)

nrow(in_best_10_some_h)
nrow(in_best_20_some_h)

in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_20)

not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]


f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 3)


new_names_by_freq <- sort(f_vbls$variables_in_top_small)

names_in_best_2 <- unique(unlist(in_best_2_some_h$variables))

new_names_by_freq

new_names_by_freq_and_best_2 <- sort(unique(c(new_names_by_freq, names_in_best_2)))


specs_size_5_freq <- all_specifications(
  var_size = 5,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20[, new_names_by_freq],
  t_thresholds = 0,
  names_exogenous = names_exogenous)

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

tic()
cv_proposed_size_5 <- cv_var_from_model_tbl(
  h = fc_horizon,
  training_length = training_length, 
  n_cv = n_cv,
  models_tbl = proposed_specs_s5, 
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

## update passing and tried models
tried_models_size_5_pr <- cv_proposed_size_5$tried_models
passing_models_size_5_pr <- cv_proposed_size_5$passing_models

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_5_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_5_pr)

accumulated_tried_models <- mutate(
  accumulated_tried_models, 
  short_name = pmap(list(variables, lags, t_threshold),
                    ~ make_model_name(variables = ..1, 
                                      lags = ..2,
                                      t_threshold = ..3)),
  short_name = unlist(short_name)
)

accumulated_passing_models <- distinct(accumulated_passing_models, short_name, .keep_all = TRUE)
accumulated_tried_models <- distinct(accumulated_tried_models, short_name, .keep_all = TRUE)



end_of_search_models <- accumulated_passing_models


true_best_20_models_h1 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_1)) %>% 
  filter(ranking <= 20) 

true_best_20_models_h2 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_2)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h3 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_3)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h4 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_4)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h5 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_5)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h6 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_6)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h7 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_7)) %>% 
  filter(ranking <= 20) 


true_best_20_models_h8 <- all_passing_models %>% 
  mutate(ranking = rank(rmse_8)) %>% 
  filter(ranking <= 20) 





search_best_20_models_h1 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_1)) %>% 
  filter(ranking <= 20) 


search_best_20_models_h2 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_2)) %>% 
  filter(ranking <= 20) 


search_best_20_models_h3 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_3)) %>% 
  filter(ranking <= 20) 


search_best_20_models_h4 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_4)) %>% 
  filter(ranking <= 20)


search_best_20_models_h5 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_5)) %>% 
  filter(ranking <= 20) 


search_best_20_models_h6 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_6)) %>% 
  filter(ranking <= 20) 


search_best_20_models_h7 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_7)) %>% 
  filter(ranking <= 20)


search_best_20_models_h8 <- end_of_search_models %>% 
  mutate(ranking = rank(rmse_8)) %>% 
  filter(ranking <= 20)

sort(true_best_20_models_h1$short_name)
sort(search_best_20_models_h1$short_name)

100*sum(true_best_20_models_h1$short_name %in% search_best_20_models_h1$short_name)/length(true_best_20_models_h1$short_name)
100*sum(true_best_20_models_h2$short_name %in% search_best_20_models_h2$short_name)/length(true_best_20_models_h2$short_name)
100*sum(true_best_20_models_h3$short_name %in% search_best_20_models_h3$short_name)/length(true_best_20_models_h3$short_name)
100*sum(true_best_20_models_h4$short_name %in% search_best_20_models_h4$short_name)/length(true_best_20_models_h4$short_name)
100*sum(true_best_20_models_h5$short_name %in% search_best_20_models_h5$short_name)/length(true_best_20_models_h5$short_name)
100*sum(true_best_20_models_h6$short_name %in% search_best_20_models_h6$short_name)/length(true_best_20_models_h6$short_name)
100*sum(true_best_20_models_h7$short_name %in% search_best_20_models_h7$short_name)/length(true_best_20_models_h7$short_name)
100*sum(true_best_20_models_h8$short_name %in% search_best_20_models_h8$short_name)/length(true_best_20_models_h8$short_name)






