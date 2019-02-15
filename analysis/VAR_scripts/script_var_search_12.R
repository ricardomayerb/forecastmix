
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

source('./R/combinations_functions.R')

all_specs_objects <- readRDS("./data/examples/all_ury_models_12_variables.rds")
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
names_12


var_size <- 3 
all_variables <- names_12
target_variable <- "rgdp"
non_target_fixed <- c("")
lag_choices <- c(3, 5)
var_data_12 <- var_data[, names_12]

accumulated_tried_models <- tibble()
accumulated_passing_models <- tibble()

### generate all size 2 and do cv
specs_size_2_u <- all_specifications(
  var_size = 2,
  all_variables = names_12,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_12,
  t_thresholds = 0,
  names_exogenous = names_exogenous)


tic()
cv_size_2 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_2_u, 
                                   var_data = var_data_12, 
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
  all_variables = names_12,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_12,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()


tic()
cv_size_3 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_3_u, 
                                   var_data = var_data_12, 
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

in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_12)

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
  var_data = var_data_12[, new_names_by_freq],
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
  var_data = var_data_12, 
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

in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_12)

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
  var_data = var_data_12[, new_names_by_freq],
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
  var_data = var_data_12, 
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








# search_plan <- 1:4
# n_steps <- length(search_plan)
# per_size_results <- list_along(1:n_steps)
# f_vbls_list <- list_along(1:n_steps)
# current_consolidated_models_list <- list_along(1:n_steps)
# cv_objects_list <- list_along(1:n_steps)
# prechosen_variables_at_each_step <- list_along(1:n_steps)
# all_prechosen_variables_at_each_step <- list_along(seq(1, n_steps))
# 


# new_var_search <- function(country, 
#                        search_plan,
#                        forecast_exercise_year, 
#                        forecast_exercise_number,
#                        fc_horizon,
#                        target_variable = c("rgdp"),
#                        default_t_treshold = 1.65,
#                        default_lags = c(2, 3, 4, 5),
#                        add_aic_bic_hq_fpe_lags =  FALSE,
#                        restrict_by_signif = TRUE,
#                        number_of_cv = 8,
#                        train_span = 25,
#                        ret_cv = TRUE,
#                        max_rank_some_h =50,
#                        max_rank_some_h_for_freq = 50,
#                        max_small_rank = 3,
#                        results_file_name = NULL,
#                        names_exogenous = c(""),
#                        exo_lag = NULL,
#                        combn_already_tried = NULL
# ) {
#   
#   initial_time <- Sys.time()
#   tic(msg = "Total time for this country")
#   
#   
#   n_steps <- length(search_plan)
#   per_size_results <- list_along(1:n_steps)
#   f_vbls_list <- list_along(1:n_steps)
#   current_consolidated_models_list <- list_along(1:n_steps)
#   cv_objects_list <- list_along(1:n_steps)
#   prechosen_variables_at_each_step <- list_along(1:n_steps)
#   all_prechosen_variables_at_each_step <- list_along(seq(1, n_steps))
#   
#   tic(msg = "Finish var search")
#   
#   for (i in seq(1, n_steps)) {
#     
#     set_of_prechosen_to_use <- NULL
#     
#     n_searches_for_this_size <- 0
#     this_search_step <- search_plan[[i]]
#     this_size <- this_search_step[["size"]]
#     this_selection_type <- this_search_step[["vbl_selection_type"]]
#     
#     print("")
#     print("--------------------------------------")
#     print("")
#     print(paste0("Starting the estimation of VAR with ", this_size," vbls"))
#     print(paste0("Variable selection type for this size: ", this_selection_type))
#     
#     if (is.null(this_search_step$lags)) {
#       this_lags <- default_lags
#     } else 
#     {
#       this_lags <- this_search_step[["lags"]]
#     }
#     
#     # print("This lags = ")
#     # print(this_lags)
#     
#     
#     if (is.null(this_search_step$t_treshold)) {
#       this_t_tresh <- default_t_treshold
#     } else {
#       this_t_tresh <- this_search_step[["t_treshold"]]
#     }
#     
#     # print("This t tresh = ")
#     # print(this_t_tresh)
#     
#     if (this_selection_type == "none") {
#       print("Using all variables without pre-chosen variables")
#       this_VAR_data <- VAR_data_for_estimation
#       this_prechosen_variables <- NULL
#       f_vbls <- NULL
#       new_select_vbls <- colnames(VAR_data_for_estimation) 
#       vbls_top_small <- NA
#       by_total_not_in_tsm <- NA
#     }
#     
#     
#     if (i > 1 & is.numeric(this_selection_type)) {
#       f_vbls <- variable_freq_by_n(current_consolidated_models, 
#                                    h_max = fc_horizon,
#                                    max_rank = max_rank_some_h_for_freq,
#                                    n_freq = this_selection_type, 
#                                    is_wide = TRUE,
#                                    mas_small_rank)
#       freq_sel_vbls_by_multi <- f_vbls$vbl_multi
#       vbls_top_small <- f_vbls$variables_in_top_small
#       
#       if(length(vbls_top_small) > this_selection_type) {
#         print(paste0("Number of best-n-VAR variables (", length(vbls_top_small), 
#                      "exceeds next_freq_limit (",  this_selection_type, "). We will preserve 
#         the integrity of best VARs and use those",  length(vbls_top_small), " variables in next size." )  )
#         
#         print(paste0("If you want to decrease the number of variables, reduce the mas_small_rank 
#                      parameter to some value lower than ", max_small_rank))
#         
#         vbls_top_small <- vbls_top_small
#       }
#       
#       by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
#       
#       by_total_na <- is.na(by_total_not_in_tsm)
#       
#       by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
#       
#       n_gap_vbls <- this_selection_type - length(vbls_top_small)
#       
#       if (n_gap_vbls > 0) {
#         extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
#       } else {
#         extra_vbls <- c()
#       }
#       
#       new_select_vbls <- c(vbls_top_small, extra_vbls)
#       
#       print("Using this subset of variables: ")
#       print(new_select_vbls)
#       
#       this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
#     }
#     
#     if (this_selection_type == "manually_prechosen_variables") {
#       print("Using automatic incrementally added pre-chosen variables")
#       print("This option does not automatically inherits prechosen variables from previous steps")
#       
#       current_consolidated_models <- current_consolidated_models_list[[i-1]]
#       
#       
#       # print("before addig this step manual variables, we have:")
#       # print(all_prechosen_variables_at_each_step)
#       
#       updated_list_of_prechosen <- add_prechosen_for_this_step(
#         search_plan = search_plan, step_index = i, 
#         prechosen_so_far = all_prechosen_variables_at_each_step,
#         max_rank_some_h_for_freq = max_rank_some_h_for_freq,
#         models_table = current_consolidated_models)
#       
#       # print("And after add_prechosen_for_this_step, the updated version of it is")
#       # print(updated_list_of_prechosen)
#       all_prechosen_variables_at_each_step <- updated_list_of_prechosen
#       
#       set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
#       
#       # print("And in this step we will add the following variables as prechosen, one at the time:")
#       # print(set_of_prechosen_to_use)
#       
#     }
#     
#     if (this_selection_type == "incremental_auto_prechosen") {
#       
#       print("Using automatic incrementally added pre-chosen variables")
#       
#       print("Inherits from previous step, the following prechosen variables:")
#       print(all_prechosen_variables_at_each_step[[i - 1]])
#       
#       current_consolidated_models <- current_consolidated_models_list[[i-1]]
#       
#       updated_list_of_prechosen <- add_prechosen_for_this_step(
#         search_plan = search_plan, step_index = i, 
#         prechosen_so_far = all_prechosen_variables_at_each_step,
#         max_rank_some_h_for_freq = max_rank_some_h_for_freq,
#         models_table = current_consolidated_models)
#       
#       all_prechosen_variables_at_each_step <- updated_list_of_prechosen
#       
#       set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
#       
#       print("And in this step we will add the following variables as prechosen, one at the time:")
#       print(set_of_prechosen_to_use)
#       
#       # print("all_prechosen_variables_at_each_step")
#       # print(all_prechosen_variables_at_each_step)
#       
#       # print("set_of_prechosen_to_use")
#       # print(set_of_prechosen_to_use)
#     }
#     
#     add_augmented_models <- this_search_step[["add_augmented_models"]]
#     
#     if (is.null(add_augmented_models)) {
#       add_augmented_models <- FALSE
#     }
#     
#     if (add_augmented_models) {
#       
#       n_best_per_h <- 2
#       rmse_names <- paste("rmse", seq(fc_horizon), sep = "_")
#       
#       print(paste0(
#         "Also including one-extra-variable augmented versions of the best ",
#         n_best_per_h, " size-",search_plan[[i-1]]$size, "-VAR of each horizon",
#         " (including ties).")
#       )
#       
#       potential_models <- current_consolidated_models_list[[i-1]]
#       
#       potential_models <- potential_models %>% 
#         gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
#         dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
#         group_by(rmse_h) %>% 
#         arrange(rmse_h, rmse) %>% 
#         mutate(rank_h = rank(rmse),
#                nth_rmse = nth(rmse, n_best_per_h)) %>% 
#         ungroup()
#       
#       print("potential_models")
#       print(potential_models)
#       
#       vec_of_rmse_h <- sort(unique(potential_models$rmse_h))
#       
#       print("vec_of_rmse_h")
#       print(vec_of_rmse_h)
#       
#       list_best <- map(vec_of_rmse_h, 
#                        ~ potential_models %>% 
#                          filter(rmse_h == .x, rmse <= nth_rmse)
#       ) 
#       
#       print("list_best")
#       print(list_best)
#       
#       break
#       
#     }
#     
#     tic(msg = paste0("Finished VARs with ", this_size, " variables"))
#     
#     if (!is.null(set_of_prechosen_to_use)) {
#       # print("Inside the prechose vbls loop:")
#       # print("set_of_prechosen_to_use")
#       # print(set_of_prechosen_to_use)
#       
#       var_res_each_prechosen <- list_along(seq(1, length(set_of_prechosen_to_use)))
#       
#       for (ptu in seq(1, length(set_of_prechosen_to_use))) {
#         print(paste0("new prechosen ", ptu, " of ", length(set_of_prechosen_to_use)))
#         
#         this_prechosen_variables <- set_of_prechosen_to_use[ptu][[1]]
#         
#         print("pre-chosen variables to be use in the coming VAR search:")
#         print(this_prechosen_variables)
#         
#         print("is.list(this_prechosen_variables)")
#         print(is.list(this_prechosen_variables))
#         
#         var_res <- search_var_one_size(
#           var_size = this_size,
#           vec_lags = this_lags,
#           var_data = this_VAR_data,
#           rgdp_level_ts = rgdp_level_ts,
#           rgdp_yoy_ts = rgdp_yoy_ts,
#           target_v = target_variable,
#           pre_selected_v = this_prechosen_variables,
#           is_cv = TRUE,
#           training_length = train_span,
#           h_max = fc_horizon,
#           n_cv = number_of_cv,
#           return_cv = ret_cv,
#           rgdp_current_form = rgdp_rec,
#           max_rank = max_rank_some_h,
#           check_residuals_cv = TRUE,
#           check_residuals_full_sample = TRUE,
#           restrict_by_signif = restrict_by_signif,
#           t_tresh = this_t_tresh,
#           max_p_for_estimation = 12,
#           add_info_based_lags = add_aic_bic_hq_fpe_lags,
#           names_exogenous = names_exogenous,
#           exo_lag = exo_lag)
#         
#         # print("names(var_res)")
#         # 
#         # print(names(var_res))
#         
#         var_res[["explored_size"]] <- this_size
#         var_res[["used_prechosen"]] <- this_prechosen_variables
#         
#         var_res_each_prechosen[[ptu]] <- var_res
#         
#         n_searches_for_this_size <- n_searches_for_this_size + 1
#         print("N of searches for this size:")
#         print(n_searches_for_this_size)
#       }
#       
#       all_models <- map(var_res_each_prechosen, "accu_rankings_models")
#       all_models <- reduce(all_models, rbind)
#       
#       all_cv_obj <- map(var_res_each_prechosen, "cv_objects")
#       all_cv_obj <- reduce(all_cv_obj, rbind)
#       
#       var_res <- list(accu_rankings_models = all_models,
#                       cv_objects = all_cv_obj)
#       
#     }
#     
#     if (is.null(set_of_prechosen_to_use)) {
#       var_res <- search_var_one_size(
#         var_size = this_size,
#         vec_lags = this_lags,
#         var_data = this_VAR_data,
#         rgdp_level_ts = rgdp_level_ts,
#         rgdp_yoy_ts = rgdp_yoy_ts,
#         target_v = target_variable,
#         pre_selected_v = this_prechosen_variables,
#         is_cv = TRUE,
#         training_length = train_span,
#         h_max = fc_horizon,
#         n_cv = number_of_cv,
#         return_cv = ret_cv,
#         rgdp_current_form = rgdp_rec,
#         max_rank = max_rank_some_h,
#         check_residuals_cv = TRUE,
#         check_residuals_full_sample = TRUE,
#         restrict_by_signif = restrict_by_signif,
#         t_tresh = this_t_tresh,
#         max_p_for_estimation = 12,
#         add_info_based_lags = add_aic_bic_hq_fpe_lags, 
#         names_exogenous = names_exogenous, 
#         exo_lag = exo_lag)
#       
#       n_searches_for_this_size <- n_searches_for_this_size + 1
#       print("N of searches for this size:")
#       print(n_searches_for_this_size)
#       
#       var_res[["explored_size"]] <- this_size
#       var_res[["used_prechosen"]] <- this_prechosen_variables
#     }
#     
#     per_size_results[[i]] <- var_res
#     
#     if (i == 1) {
#       current_consolidated_models <- stack_models(
#         list(var_res[["accu_rankings_models"]])
#       ) 
#     } else {
#       current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
#     }
#     
#     combn_already_tried <- c(combn_already_tried, 
#                              var_res[["combinations_of_variables_considered"]])
#     
#     file_suffix <- paste0("_size_", this_size,
#                           "_t_", this_t_tresh, "mr", max_rank_some_h,
#                           "_mrfq", max_rank_some_h_for_freq, ".rds")
#     
#     filename <- paste0("var_results_", country, file_suffix)
#     
#     saveRDS(var_res, paste0(output_path, filename))
#     
#     per_size_results[[i]] <- var_res
#     f_vbls_list[[i]] <- f_vbls
#     
#     prechosen_variables_at_each_step[[i]] <- this_prechosen_variables
#     current_consolidated_models_list[[i]] <- current_consolidated_models
#     cv_objects_list[[i]] <- var_res[["cv_objects"]]
#     
#     toc()
#   }
#   
#   toc()
#   
#   bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
#   
#   consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
#   
#   final_time <- Sys.time()
#   
#   elapsed_time <- final_time - initial_time
#   
#   if (ret_cv) {
#     res_and_info <- list(consolidated_var_res = consolidated_var_res,
#                          f_vbls_all_sizes = f_vbls_list,
#                          var_data = VAR_data_for_estimation,
#                          elapsed_time = elapsed_time, 
#                          prechosen = all_prechosen_variables_at_each_step,
#                          cv_objects = cv_objects_list,
#                          target_variable_transform = rgdp_rec,
#                          names_exogenous = names_exogenous,
#                          fc_horizon = fc_horizon,
#                          train_span = train_span,
#                          number_of_cv = number_of_cv,
#                          max_rank_some_h = max_rank_some_h)
#     
#   } else {
#     res_and_info <- list(consolidated_var_res = consolidated_var_res,
#                          f_vbls_all_sizes = f_vbls_list,
#                          var_data = VAR_data_for_estimation,
#                          prechosen = all_prechosen_variables_at_each_step,
#                          elapsed_time = elapsed_time,
#                          target_variable_transform = rgdp_rec,
#                          names_exogenous,
#                          fc_horizon = fc_horizon,
#                          train_span = train_span,
#                          number_of_cv = number_of_cv,
#                          max_rank_some_h = max_rank_some_h)
#   }
#   
#   allsizes <- paste(n_steps, collapse = "")
#   allthresh <- "foo"
#   # allthresh <- paste(t_tresh, collapse = "")
#   allfqlim <- paste(c(9,6,6), collapse = "")
#   
#   file_suffix_all_sizes <-  paste0("_s", allsizes,
#                                    "_t", allthresh, "_mr", max_rank_some_h,
#                                    "_mrfq", max_rank_some_h_for_freq,
#                                    "_cv",number_of_cv,"_tspan", train_span,
#                                    "_h", fc_horizon,".rds")
#   
#   
#   if(is.null(results_file_name)) {
#     filename <- paste0("vr_", country, file_suffix_all_sizes)
#   } else {
#     filename <- results_file_name
#   }
#   
#   print("filename")
#   print(filename)
#   
#   saveRDS(res_and_info, paste0(output_path, filename))
#   
#   return(res_and_info)
# }
# 
# 







# So how many VARs are "all" VARs?
# A better defined quantity is how many combinations of variables which contains the target variables
# and then there is the number of variations given by your choices of max lag and restrictions
# So suppose there are 1000 combiantions of variables that include rgdp, and you want to try VAR(p) specifications with p euqal 3 and p equal 5. 
# Furthermore for each VAR(p) you want to see how the unrestricted models performs but also a more parsimonious version where
# all coefficietes with t tests lower than 1.65 are set to zero. That is two version of each VAR(p) and two choices of p, 
# given 4 VARs per each variable combination. And that could escalate quickly to 6 if we eanted to explore an additional p, and to 9 if on
# top of that we want aditional t test value to filter coefficients. So you can go from 1000 models to 9000 thousand models very easily


# so with 15 variables, we have, summing sizes 2 to 5, 1470 (or 1414 if we dicard all-exogenous VARs)
# that mea exploring the performance of 5880 VARs, tow-third of them restricted vars (or 5656)
# With 12 variables the 1470 go down to 561 and 5880 to 2244 (505 and 2020)




