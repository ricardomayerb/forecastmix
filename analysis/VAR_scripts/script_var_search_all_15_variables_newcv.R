
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

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


# So how many VARs are "all" VARs?
# A better defined quantity is how many combinations of variables which contains the target variables
# and then there is the number of variations given by your choices of max lag and restrictions
# So suppose there are 1000 combiantions of variables that include rgdp, and you want to try VAR(p) specifications with p euqal 3 and p equal 5. 
# Furthermore for each VAR(p) you want to see how the unrestricted models performs but also a more parsimonious version where
# all coefficietes with t tests lower than 1.65 are set to zero. That is two version of each VAR(p) and two choices of p, 
# given 4 VARs per each variable combination. And that could escalate quickly to 6 if we eanted to explore an additional p, and to 9 if on
# top of that we want aditional t test value to filter coefficients. So you can go from 1000 models to 9000 thousand models very easily


#' ## Counting specifications
#' 
#' The total number of potential specifications depends on a number of factors:
#'  - number of variable combinations. Which in turn depends on:
#'     - the total number of variables in the data set
#'     - the number of variables in the VAR (the "size" of the VAR: 2, 3, 4 ...)
#'     - the number of exogenous variables in the data set. This is because we generally choose leave out VARs where there is only one endogenous variable and all the rest are exogenous. That's more properly called an ARIMAX model. The default is to ignore such models when they show up, but it can be changed.
#'  - number of maximum lags to consider: e.g. 3, 4, 5 and 6
#'  - number of restricted version to consider: unrestricted, t = 1.65 and t = 2
#'  
#'  With two restricted version, plus the unrestricted one and four possible lag choices, we generate 12 specification per each variable combination we submit. A more modest inquiry may examine only unrestricted models for two lag choices, in which case ge only generate 2 specifications per tuple of variables. Say we have 500 combinations of variables to try out, that would tipically imply between 2x500 = 1000 and 12x500 = 6000 specifications to estimate, test and do cross-validation. 
#'  
#' A formula of the number of combinations, ignoring the distinction between endogenous and exogenpus variables, can be written as:
#' 
#' $$ncomb = \frac{(n - n_f)!}{(n - n_f - s - n_f)! ~ (s-n_f)!} = \frac{(n_a)!}{(n_a - s_a)! ~ (s_a)!}$$
#' 
#' Where $n$ is the total number of variables in the data set, $s$ is the number of distinct variables in the VAR (the "size") and $n_f$ is the number of *fixed variables*, i.e. those that need to be in any VAR (tipically $rgdp$ in our examples but it is *always* an endogenous variable and since we have at least one target variable it is at least equal to one). It can be more succintly expressed in the number of adjusted (for combinatorial purposes) numbers of variables to choose from,  $n_a := n -n_f$, and the adjusted  numbers of slots to fill, $s_a := s - n_f$. 
#' 
#' If we wanted to exclude those VAR with just one endogenous variables, then we can adjust $ncomb$ above by this quantity (notice that this case can only happen when we have only one endogenous variable acting as fixed variable):
#' 
#' $$ncomb_x = \frac{n_x!}{(n_x - (s-1))! ~ (s-1)!}$$
#' Where $n_x$ is the number of variables that we consider as exogenous. Notice that this number is zero in whenever there is less objects to choose from than the number of slots to be fill i.e when $n_x < s-1$.
#' 
#' Finally, we could define
#' 
#' $$ncomb^* = ncomb - ncomb_x$$
#' 
#' as the number of combinations, adjusted by ignoring VARs with only one endogenous variables. The table below shows, however that for the case of Uruguay (and it will be the case for the rest of our countries) it makes very little difference in the final number of variable combinations.
#'  
#' 
## ----countingcombinations------------------------------------------------


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


# so with 15 variables, we have, summing sizes 2 to 5, 1470 (or 1414 if we dicard all-exogenous VARs)
# that mea exploring the performance of 5880 VARs, tow-third of them restricted vars (or 5656)
# With 12 variables the 1470 go down to 561 and 5880 to 2244 (505 and 2020)



var_size <- 3 
all_variables <- names_15
target_variable <- "rgdp"
non_target_fixed <- c("")
lag_choices <- c(3, 5)
var_data_15 <- var_data[, names_15]



tic()
specs_size_2_u <- all_specifications(
  var_size = 2,
  all_variables = names_15,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_15,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()



tic()
specs_size_2_r <- all_specifications(
  var_size = 2,
  all_variables = names_15,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_15,
  t_thresholds = c(1.65),
  names_exogenous = names_exogenous)
toc()


tic()
specs_size_3_u <- all_specifications(
  var_size = 3,
  all_variables = names_15,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_15,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()

tic()
specs_size_4_u <- all_specifications(
  var_size = 4,
  all_variables = names_15,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_15,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()

tic()
specs_size_5_u <- all_specifications(
  var_size = 5,
  all_variables = names_15,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_15,
  t_thresholds = 0,
  names_exogenous = names_exogenous)
toc()




foo_size_2_r <- mutate(specs_size_2_r,
                       this_foo = pmap(list(variables, lags, t_threshold), 
                                       ~ specs_to_rmse(var_data = var_data_15,variables = ..1,
                                                       lags = ..2, t_thresholds = ..3, 
                                                       future_exo_cv = future_exo_cv, 
                                                       training_length = training_length, 
                                                       h = fc_horizon, n_cv = n_cv,
                                                       target_transform = target_transform, 
                                                       target_level_ts = target_level_ts, 
                                                       names_exogenous = names_exogenous)))


foo_size_2 <- mutate(specs_size_2_u,
                     this_foo = pmap(list(variables, lags, t_threshold), 
                         ~ specs_to_rmse(var_data = var_data_15,variables = ..1,
                                         lags = ..2, t_thresholds = ..3, 
                                         future_exo_cv = future_exo_cv, 
                                         training_length = training_length, 
                                         h = fc_horizon, n_cv = n_cv,
                                         target_transform = target_transform, 
                                         target_level_ts = target_level_ts, 
                                         names_exogenous = names_exogenous)
                         )
  )


foo_size_2 <- unnest(foo_size_2, this_foo) 


foo_size_2_r <- unnest(foo_size_2_r, this_foo) 

tic()
cv_size_2_per_row <- cv_var_from_tbl_by_row(h = fc_horizon, n_cv = n_cv, 
                                training_length = training_length, 
                                models_tbl = specs_size_2_u, 
                                var_data = var_data_15,
                                target_transform = target_transform, 
                                target_level_ts = target_level_ts, 
                                names_exogenous = names_exogenous, 
                                extended_exo_mts = extended_exo_mts)
toc()

tic()
cv_size_2 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_2_u, 
                                   var_data = var_data_15, 
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


foo <- arrange(passing_models_size_2, short_name)
moo <- arrange(cv_size_2_per_row$models_tbl, short_name) 

identical(moo$short_name, foo$short_name)
identical(moo$rmse_1, foo$rmse_1)
identical(moo$rmse_8, foo$rmse_8)



tic()
cv_size_3 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_3_u, 
                                   var_data = var_data_15, 
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
cv_size_3_per_row <- cv_var_from_tbl_by_row(h = fc_horizon, n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_3_u, 
                                            var_data = var_data_15,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            extended_exo_mts = extended_exo_mts)
toc()

tic()
cv_size_4 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_4_u, 
                                   var_data = var_data_15, 
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
cv_size_4_per_row <- cv_var_from_tbl_by_row(h = fc_horizon, n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_4_u, 
                                            var_data = var_data_15,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            extended_exo_mts = extended_exo_mts)
toc()



tic()
cv_size_5 <- cv_var_from_model_tbl(h = fc_horizon,
                                   training_length = training_length, 
                                   n_cv = n_cv,
                                   models_tbl = specs_size_5_u, 
                                   var_data = var_data_15, 
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




tic()
cv_size_5_per_row <- cv_var_from_tbl_by_row(h = fc_horizon, n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_5_u, 
                                            var_data = var_data_15,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            extended_exo_mts = extended_exo_mts)
toc()




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
        file = "./data/examples/all_ury_models_15_variables.rds")




# specs_to_rmse_old <- function(var_data, variables, lags, h, n_cv, training_length, 
#                               future_exo_cv, target_transform, target_level_ts,
#                               t_thresholds = 0, 
#                               do_tests = TRUE, names_exogenous = c("")) {
#   pass_tests <- TRUE
#   
#   if (length(t_thresholds) == 1) {
#     if (t_thresholds == 0) {
#       is_unrestricted <- TRUE
#     } else {
#       is_unrestricted <- FALSE
#     }
#   } else {
#     is_unrestricted <- FALSE
#   }
#   
#   # do the unrestricted even if it is restricted
#   
#   fit_u <- try(fit_VAR_rest(var_data = var_data, variables = variables, p = lags, 
#                             t_thresh = 0, names_exogenous = names_exogenous),
#                silent = TRUE)
#   
#   fit_u_class <- class(fit_u)[[1]]
#   
#   if (fit_u_class != "varest") {
#     do_tests <- FALSE
#     pass_tests <- FALSE
#     tested <- FALSE
#     fit_r <- NA
#   }
#   
#   if (do_tests) {
#     tested <- TRUE
#     is_stable <-  all(vars::roots(fit_u) < 1)
#     is_white_noise <-  check_resid_VAR(fit_u)
#     pass_tests <- is_stable & is_white_noise
#     # print(pass_tests)
#   }
#   
#   names_rmses <- paste0("rmse_", seq(1, h))
#   
#   rmse_yoy_all_h <- rep(NA, h)
#   names(rmse_yoy_all_h) <- names_rmses
#   
#   if (pass_tests) {
#     cv_obj <- cv_var_from_one_row(fit = fit_u, var_data = var_data, 
#                                   variables = variables, lags = lags, h = h, 
#                                   n_cv = n_cv, training_length = training_length, 
#                                   names_exogenous = names_exogenous, 
#                                   this_type = "const",
#                                   this_thresh = t_thresholds, 
#                                   future_exo_cv = future_exo_cv)
#     
#     full_sample_resmat = cv_obj[["full_sample_resmat"]]
#     # print("transform to yoy")
#     
#     if (target_transform != "yoy") {
#       
#       if (target_transform == "diff_yoy") {
#         
#         # print("from diff_yoy to yoy")
#         
#         cv_obj_diff_yoy <-  cv_obj
#         
#         cv_obj_yoy = transform_all_cv(cv_obj_diff_yoy,
#                                       current_form = target_transform,
#                                       target_level_ts =  target_level_ts,
#                                       n_cv = n_cv)
#       }
#       
#       if (target_transform == "diff") {
#         # print("from diff to yoy")
#         auxiliary_ts <-  target_level_ts
#         
#         models_tbl <- models_tbl %>%
#           rename(cv_obj_diff = cv_obj)
#         
#         results_all_models <- results_all_models %>%
#           mutate(cv_obj_yoy = map(cv_obj_diff,
#                                   ~ transform_all_cv(cv_object  = .,
#                                                      current_form = target_transformation,
#                                                      auxiliary_ts = target_level_ts,
#                                                      n_cv = n_cv)
#           )
#           )
#       }
#       
#     }
#     
#     if (target_transform == "yoy") {
#       # print("Already in yoy form")
#       cv_obj_yoy <- cv_obj
#     }
#     
#     # print("done transforming")
#     
#     
#     rmse_yoy_all_h <-  all_rmse_from_cv_obj(cv_obj_yoy)
#     names(rmse_yoy_all_h) <- names_rmses
#     
#     
#   }
#   
#   tibble_to_return <- tibble(tested = tested, pass_tests = pass_tests)
#   
#   tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_h))
#   # print(tibble_to_return)
#   
#   if (is_unrestricted) {
#     return(tibble_to_return)
#   }
#   
#   # it only executes this part if its a restricted VAR
#   print("is restricted")
#   fit_r <- try(fit_VAR_rest(var_data = var_data, variables = variables, p = lags, 
#                             t_thresh = t_thresholds,
#                             names_exogenous = names_exogenous),
#                silent = TRUE)
#   
#   print("fit_r")
#   print(fit_r)
#   
#   # print("fit_r$fit")
#   # print(fit_r$fit)
#   
#   fit_r_class <- class(fit_r$fit[[2]])[[1]]
#   
#   if (fit_r_class != "varest") {
#     do_tests <- FALSE
#     pass_tests <- FALSE
#     tested <- FALSE
#     fit_r <- NA
#   }
#   
#   if (do_tests) {
#     tested <- TRUE
#     is_stable <-  all(vars::roots(fit_r$fit[[2]]) < 1)
#     is_white_noise <-  check_resid_VAR(fit_r$fit[[2]])
#     pass_tests <- is_stable & is_white_noise
#     print("pass_tests")
#     print(pass_tests)
#   }
#   
#   rmse_yoy_all_h <- rep(NA, h)
#   names(rmse_yoy_all_h) <- names_rmses
#   
#   if (pass_tests) {
#     cv_obj <- cv_var_from_one_row(fit = fit_r, var_data = var_data, 
#                                   variables = variables, lags = lags, h = h, 
#                                   n_cv = n_cv, training_length = training_length, 
#                                   names_exogenous = names_exogenous, 
#                                   this_type = "const",
#                                   this_thresh = t_thresholds, 
#                                   future_exo_cv = future_exo_cv)
#     
#     full_sample_resmat = cv_obj[["full_sample_resmat"]]
#     
#     if (target_transform != "yoy") {
#       
#       if (target_transform == "diff_yoy") {
#         
#         cv_obj_diff_yoy <-  cv_obj
#         
#         cv_obj_yoy = transform_all_cv(cv_obj_diff_yoy,
#                                       current_form = target_transform,
#                                       target_level_ts =  target_level_ts,
#                                       n_cv = n_cv)
#       }
#       
#       if (target_transform == "diff") {
#         # print("from diff to yoy")
#         auxiliary_ts <-  target_level_ts
#         
#         models_tbl <- models_tbl %>%
#           rename(cv_obj_diff = cv_obj)
#         
#         results_all_models <- results_all_models %>%
#           mutate(cv_obj_yoy = map(cv_obj_diff,
#                                   ~ transform_all_cv(cv_object  = .,
#                                                      current_form = target_transformation,
#                                                      auxiliary_ts = target_level_ts,
#                                                      n_cv = n_cv)
#           )
#           )
#       }
#       
#     }
#     
#     if (target_transform == "yoy") {
#       # print("Already in yoy form")
#       cv_obj_yoy <- cv_obj
#     }
#     
#     rmse_yoy_all_h <-  all_rmse_from_cv_obj(cv_obj_yoy)
#     names(rmse_yoy_all_h) <- names_rmses
#   }
#   
#   tibble_to_return_r <- tibble(tested = tested, pass_tests = pass_tests)
#   
#   tibble_to_return_r <- as_tibble(c(tibble_to_return_r, rmse_yoy_all_h))
#   # print(tibble_to_return_r)
#   
#   return(tibble_to_return_r)
# }
