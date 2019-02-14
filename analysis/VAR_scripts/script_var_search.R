
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

source('./R/combinations_functions.R')

data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
print(names(data_object_ury))
country <- data_object_ury$country_name
target_transformation <- data_object_ury$target_transformation
raw_data <- data_object_ury$raw_data
var_data <- data_object_ury$transformed_data
print(target_transformation)

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

# so with 15 variables, we have, summing sizes 2 to 5, 1470 (or 1414 if we dicard all-exogenous VARs)
# that mea exploring the performance of 5880 VARs, tow-third of them restricted vars (or 5656)
# With 12 variables the 1470 go down to 561 and 5880 to 2244 (505 and 2020)


# ncombs41 <- map(2:7, ~ count_combn(var_size = .x, n_total = 41, n_exo = 6, n_fixed = 1))
# ntable41 <- as_tibble(cbind(n = ncol(var_data), n_fixed = 1, size = 2:7, reduce(ncombs41, rbind)))
# print(ntable41)
# 
# ncombs10 <- map(2:7, ~ count_combn(var_size = .x, n_total = 10, n_exo = 6, n_fixed = 1))
# ntable10 <- as_tibble(cbind(n = ncol(var_data), n_fixed = 1, size = 2:7, reduce(ncombs10, rbind)))
# print(ntable10)

#' 
#'  
#' Notice how rapidly the number of combinations increases with VAR size. Since $comb_x$ is relatively small here, for the next table we will consider $ncomb$ alone. Depending on the number of different lags and restrictions the final number of specification will tipically  be from two (no restrictions and two lag choices) to twelve times (two restrictions and four lag choices) the number of variable combinations. In the following table we consider the resulting number of specification under those two scenarios plus the number of estimations made taking into account ten rounds of cross validations in addition to the full sample original estimate. Since at least some models will fail the tests, only a fraction of them will qualify to be passed to cross-validation. The table below assume that between 20 to 90 percent of the models will pass. So the minimum number of VAR estimation to do happens in the no-restriction, two lag choices and 20% of qulifiying, whereas the maximum number of estimation would happens if 90% of the models pass the test and we are trying four lag choices and two restrictions.
#' 
#' 
## ----nspecifications-----------------------------------------------------

nspectable <- ntable %>% dplyr::select(size, ncomb)

nspectable <- nspectable %>% 
  mutate(nVAR_2 = ncomb*2, nVAR_12 = ncomb*12, cv_min = nVAR_2*0.2*10, cv_max = nVAR_12*0.9*10)

print(nspectable)



#' 
#' Even with a modest size of 4, we can be dealing with 50,000 specification (and tests) and almost half a million estimations. Projecting how long those estimatiation would take, needs to consider that restricted estimations are slower than unrestrited ones and we will leave that for later. By this time we just want to notice that in order to use VAR of size 5 we either confine ourselves to unrestricted models and few lag choices or we need to find a strategy to select variables that lowers the number of possible combinations to be explored.
#' 
#' To expede wthing up in this document we will restrict oour attention to size-3 VARs, usig at first two variable combinations and at the end using all of them.
#' 
#' 
#' 
#' ## Generate all specifications for a given size
#' 
#' 
#' 
## ----tuplesofvbls, cache=TRUE--------------------------------------------

var_size <- 3 
all_variables <- colnames(var_data)
target_variable <- "rgdp"
non_target_fixed <- c("")


# 1742
tic()
specifications_size_3_all_u <- all_specifications(
  var_size = 3, all_variables = colnames(var_data),
  lag_choices = c(3,4,5), use_info_lags = TRUE,
  var_data = var_data, t_thresholds = 0, names_exogenous = names_exogenous)
toc()

# 1742
tic()
specifications_size_3_all_u_noinfolags <- all_specifications(
  var_size = 3, all_variables = colnames(var_data),
  lag_choices = c(3,4,5), use_info_lags = FALSE,
  var_data = var_data, t_thresholds = 0, names_exogenous = names_exogenous)
toc()

# 1742
tic()
specifications_size_3_all <- all_specifications(
  var_size = 3, all_variables = colnames(var_data),
  lag_choices = c(3,4,5), use_info_lags = TRUE,
  var_data = var_data, t_thresholds = c(1.65, 2), names_exogenous = names_exogenous)
toc()

# 1305
tic()
specifications_size_3_all_noinfolag <- all_specifications(
  var_size = 3, all_variables = colnames(var_data), 
  lag_choices = c(3,4,5), use_info_lags = FALSE, 
  var_data = var_data, t_thresholds = c(1.65, 2), names_exogenous = names_exogenous)
toc()




#' 
#' 
#' 
#' 
#' 
#' ### Select a subset of specification for expediency in this document
## ----s3smmd--------------------------------------------------------------

specifications_size_3_17 <- specifications_size_3_all[1:17, ] 
specifications_size_3_170 <- specifications_size_3_all[1:170, ] 

specifications_size_3_17_u <- specifications_size_3_all_u[1:17, ] 
specifications_size_3_170_u <- specifications_size_3_all_u[1:170, ] 


#' 
#' 
#' ## Fit all specifications and keep only acceptable ones
#' 
## ----passing_models_size3small, cache=TRUE-------------------------------
ftmt_size_3_17 <- fit_tests_models_table(specifications_size_3_17, 
                                         var_data = var_data,
                                         names_exogenous = names_exogenous
                                         )
pm_size_3_17 <- ftmt_size_3_17[["passing_models"]]
print(head(pm_size_3_17))

ftmt_size_3_17_u <- fit_tests_models_table(specifications_size_3_17_u, 
                                           var_data = var_data, 
                                           names_exogenous = names_exogenous)
pm_size_3_17_u <- ftmt_size_3_17_u[["passing_models"]]
print(head(pm_size_3_17_u))

pm_3specs <- pm_size_3_17[c(1, 11, 21), ]



#' 
#' 
#' ## Time Series Cross Validation on valid specifications
#' 
#' 
#' ### Preparation: forecast exogenous values in advance
#' 
#' ### Cross Validation
#' 
#' 
#' 
#' 
#' 
## ----tscv_s3small, cache=TRUE--------------------------------------------
n_cv <- 10
fc_horizon <- 8
training_length <- "per_cv_maxs" 
fit_column <- "fit"
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])

tic()
cv_size_3_17_use <- cv_var_from_model_tbl(h = fc_horizon,
                                               training_length = training_length, 
                                               n_cv = n_cv,
                                               models_tbl = pm_size_3_17, 
                                               var_data = var_data, 
                                               fit_column = "fit", 
                                               target_transform = target_transform,
                                               target_level_ts = target_level_ts, 
                                               names_exogenous = names_exogenous, 
                                               future_exo = extension_of_exo, 
                                               extended_exo_mts = cv_extension_of_exo,
                                               keep_varest_obj = FALSE
)
toc()


tic()
cv_size_3_17_estimate <- cv_var_from_model_tbl(h = fc_horizon,
                             training_length = training_length, 
                             n_cv = n_cv,
                             models_tbl = pm_size_3_17, 
                             var_data = var_data, 
                             fit_column = NULL, 
                             target_transform = target_transform,
                             target_level_ts = target_level_ts, 
                             names_exogenous = names_exogenous, 
                             future_exo = extension_of_exo, 
                             extended_exo_mts = cv_extension_of_exo,
                             keep_varest_obj = FALSE
                             )
toc()



tic()
cv_size_3_17_use_and_keep <- cv_var_from_model_tbl(h = fc_horizon,
                             training_length = training_length, 
                             n_cv = n_cv,
                             models_tbl = pm_size_3_17, 
                             var_data = var_data, 
                             fit_column = "fit", 
                             target_transform = target_transform,
                             target_level_ts = target_level_ts, 
                             names_exogenous = names_exogenous, 
                             future_exo = extension_of_exo, 
                             extended_exo_mts = cv_extension_of_exo,
                             keep_varest_obj = TRUE
                             )
toc()


tic()
cv_size_3_17_estimate_and_keep <- cv_var_from_model_tbl(h = fc_horizon,
                                               training_length = training_length, 
                                               n_cv = n_cv,
                                               models_tbl = pm_size_3_17, 
                                               var_data = var_data, 
                                               fit_column = NULL, 
                                               target_transform = target_transform,
                                               target_level_ts = target_level_ts, 
                                               names_exogenous = names_exogenous, 
                                               future_exo = extension_of_exo, 
                                               extended_exo_mts = cv_extension_of_exo,
                                               keep_varest_obj = TRUE
)
toc()


tic()
cv_3s_use <- cv_var_from_model_tbl(h = fc_horizon,
                             training_length = training_length, 
                             n_cv = n_cv,
                             models_tbl = pm_3specs, 
                             var_data = var_data, 
                             fit_column = "fit", 
                             target_transform = target_transform,
                             target_level_ts = target_level_ts, 
                             names_exogenous = names_exogenous, 
                             future_exo = extension_of_exo, 
                             extended_exo_mts = cv_extension_of_exo,
                             keep_varest_obj = FALSE
                             )
toc()

tic()
cv_3s_use_and_keep <- cv_var_from_model_tbl(h = fc_horizon,
                             training_length = training_length, 
                             n_cv = n_cv,
                             models_tbl = pm_3specs, 
                             var_data = var_data, 
                             fit_column = "fit", 
                             target_transform = target_transform,
                             target_level_ts = target_level_ts, 
                             names_exogenous = names_exogenous, 
                             future_exo = extension_of_exo, 
                             extended_exo_mts = cv_extension_of_exo,
                             keep_varest_obj = TRUE
                             )
toc()




## ----forecast_using_cv---------------------------------------------------

# forecast all 21 models
fcs_from_all_eak <- forecast_var_from_model_tbl(
  cv_size_3_17_estimate_and_keep,
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  keep_wide_tbl = TRUE
)

mod1 <- fcs_from_all_eak$models_tbl
mod1i <- fcs_from_all_eak$models_info_per_h
mod1w <- fcs_from_all_eak$models_tbl_wide

fcs_from_all_uak <- forecast_var_from_model_tbl(
  cv_size_3_17_use_and_keep, 
  fit_column = "fit",
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  keep_wide_tbl = TRUE
)

fcs_from_all_e <- forecast_var_from_model_tbl(
  cv_size_3_17_estimate,
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  keep_wide_tbl = TRUE
)

fcs_from_all_u <- forecast_var_from_model_tbl(
  cv_size_3_17_use, 
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  keep_wide_tbl = TRUE
)


fcs_from_all_e_10 <- forecast_var_from_model_tbl(
  models_tbl = cv_size_3_17_estimate,
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  keep_wide_tbl = TRUE,
  max_rank_h = 10
)

# fc_rmse_weighted <- fc_best_per_h$fcs_wavg
# models_tbl_max_rank <- fc_best_per_h$models_tbl
# models_tbl_max_rank
# snames_best <-  models_tbl_max_rank %>% 
#   dplyr::select(short_name) %>% distinct()
# snames_best
# 
# models_tbl_used_fc <- semi_join(cv_size_3_17, snames_best, by = "short_name")
# models_tbl_not_used_fc <- anti_join(cv_size_3_17, snames_best, by = "short_name")

models_smaller1 <- cv_size_3_17_estimate[1, ]
models_smaller2 <- cv_size_3_17_estimate[c(1, 11), ]
models_smaller3 <- cv_size_3_17_estimate[c(1, 11, 21), ]

h_max <- fc_horizon

training_length <- "per_cv_maxs"

#' 
#' 
#' 
## ----cv_of_ensemble------------------------------------------------------



# models_tbl_sel <- dplyr::select(boo, variables,  short_name, horizon, 
#                                 this_h_fc_yoy, rmse_h, rmse)



ensemble_fc_list <- fc_mean_of_VAR_ensemble(models_tbl = fcs_from_all_e$models_tbl)
ensemble_fc_list_10 <- fc_mean_of_VAR_ensemble(models_tbl = fcs_from_all_e$models_tbl, max_rank_h = 10)

ensemble_fc_tbl <- ensemble_fc_list$ensemble_tbl
ensemble_fc_ts <- ensemble_fc_list$weighted_avg_fc_yoy

ensemble_fc_tbl_10 <- ensemble_fc_list_10$ensemble_tbl
ensemble_fc_ts_10 <- ensemble_fc_list_10$weighted_avg_fc_yoy


ensemble_cv <- cv_of_VAR_ensemble(var_data = var_data,
                                  used_cv_models = fcs_from_all_e$models_tbl_wide,
                                  fc_horizon = fc_horizon,
                                  n_cv = n_cv,
                                  training_length = training_length,
                                  cv_extension_of_exo = cv_extension_of_exo,
                                  names_exogenous = names_exogenous,
                                  max_rank_h = NULL,
                                  target_transform = target_transform,
                                  target_level_ts = target_level_ts)


ensemble_cv_10 <- cv_of_VAR_ensemble(var_data = var_data,
                                  used_cv_models = fcs_from_all_e$models_tbl_wide,
                                  fc_horizon = fc_horizon,
                                  n_cv = n_cv,
                                  training_length = training_length,
                                  cv_extension_of_exo = cv_extension_of_exo,
                                  names_exogenous = names_exogenous,
                                  max_rank_h = 10,
                                  target_transform = target_transform,
                                  target_level_ts = target_level_ts)



ensemble_fc_and_rmse <- ensemble_fc_tbl %>% 
  mutate(rmse_h = paste0("rmse_", 1:n()),
         rmse = ensemble_cv$ensemble_rmse,
         rank = -1)

fcs_models_to_bind <- fcs_from_all_e$models_tbl %>% 
  mutate(lags = list(lags)) %>% 
  dplyr::select(names(ensemble_fc_and_rmse))


models_and_ensemble_fcs <- rbind(ensemble_fc_and_rmse, 
                                 fcs_models_to_bind)


foo_long <- discard_by_rank(fcs_from_all_e$models_tbl, max_rank_h = 3, is_wide = FALSE)

foo_wide <- discard_by_rank(fcs_from_all_e$models_tbl_wide, max_rank_h = 3, is_wide = TRUE)

sort(unique(foo_long$short_name))
sort(unique(foo_wide$short_name))

identical(sort(unique(foo_long$short_name)), sort(unique(foo_wide$short_name)))


fc10 <- discard_by_rank(fcs_from_all_e$models_tbl, max_rank_h = 8, is_wide = FALSE)


ensemble_fc_from_models_rmse(models_tbl_with_rmse = cv_size_3_17_estimate,
                             var_data = var_data, 
                             n_cv = n_cv, 
                             training_length = training_length, 
                             max_rank_h = 10, 
                             fc_horizon = fc_horizon,
                             names_exogenous = names_exogenous, 
                             target_transform = target_transform,
                             target_level_ts = target_level_ts,
                             extension_of_exo = extension_of_exo, 
                             cv_extension_of_exo = cv_extension_of_exo, 
                             fit_column = NULL)


# ensemble_all_h <- ensemble_all_h %>% 
#   mutate(rmse_h = paste0("rmse_", 1:fc_horizon),
#          rmse = ensemble_rmse)
# 
# models_tbl_sel <- dplyr::select(models_tbl, variables,  short_name, horizon, 
#                                 this_h_fc_yoy, rmse_h, rmse)
# 
# models_and_ensemble <- bind_rows(dplyr::select(ensemble_all_h, -lags), models_tbl_sel)
# }


# 
# 
# p <- ggplot(data = models_and_ensemble,
#             aes(x = horizon, y = this_h_fc_yoy, color = short_name)) + 
#   geom_line()
# 
# p
