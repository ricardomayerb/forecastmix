#' ---
#' title: 'Multiple models: a two-models speedy case'
#' author: "Ricardo Mayer"
#' date: "12/6/2018"
#' output:
#'   html_document: default
#'   pdf_document: default
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#' 
## ----source-and-lib, message=FALSE, warning=FALSE------------------------
source('./R/combinations_functions.R')

#' 
#' We will start with a ready to use data set with quarterly, stationary series than can be used in an ordinary VAR, and we will not explain how the data munging is done. In particular, we will *not* discuss the following important points:
#'     - how the data was obtained
#'     - how monthly data was converted to quaterly frequency
#'     - how monthly data was *extended* to complete its current final quarter
#'     - how (potentially) exogenous data was forecasted in order to make it available to produce conditional forecasts
#'     - how each series was transformed using seasonal and oridinary differeces to render them stationary
#'     
#' All those points are discussed in the data preparation document, see *here*
#' 
#' ## VAR-ready data set
#'     
#' We will use the example dataset with domestic series from Uruguay and few external series. The rds file contains the country's name, the transformation applied to rgdp to render it stationary and two data sets: the original or raw data and one ready to used in VAR estimation,  containing only stationary versions of the original series. 
#' 
#' 
## ----loading_data--------------------------------------------------------
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

# exodata_fullsample <- var_data[,names_exogenous]
# target_used_in_VAR <- var_data[, target_variable]
# start_target_in_VAR <- start(na.omit(target_used_in_VAR))
# end_target_in_VAR <- end(na.omit(target_used_in_VAR))
# fc_horizon <- 8
# n_cv <- 10
# 
# tic()
# extension_of_exo <- extending_exogenous(exodata = exodata_fullsample,
#                                         h = fc_horizon,
#                                         endo_end = end_target_in_VAR)
# toc()
# 
# tic()
# cv_extension_of_exo <- extending_exogenous_for_cv(
#   exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR, 
#   n_cv = n_cv, same_model_across_cv = FALSE)
# toc()
# 



#' 
#' 
#' 
#' 
#' 
#' In this case, all VARs will use a  "diff-yoy" transformation of the real GDP series (i.e. first, take seasonal differences on the quarterly series and then ordinary differences on the result).    
#' 
#' 
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


ncombs <- map(2:7, ~ count_combn(var_size = .x, n_total = 31, n_exo = 6, n_fixed = 1))
ntable <- as_tibble(cbind(n = ncol(var_data), n_fixed = 1, size = 2:7, reduce(ncombs, rbind)))
print(ntable)

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
ftmt_size_3_17 <- fit_tests_models_table(specifications_size_3_17, var_data = var_data, names_exogenous = names_exogenous)
pm_size_3_17 <- ftmt_size_3_17[["passing_models"]]
print(head(pm_size_3_17))

ftmt_size_3_17_u <- fit_tests_models_table(specifications_size_3_17_u, var_data = var_data, names_exogenous = names_exogenous)
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
cv_size_3_17 <- cv_var_from_model_tbl(h = fc_horizon,
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
cv_size_3_17_tra28 <- cv_var_from_model_tbl(h = fc_horizon,
                             training_length = 28, 
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
cv_size_3_17_with_varest <- cv_var_from_model_tbl(h = fc_horizon,
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
cv_3s <- cv_var_from_model_tbl(h = fc_horizon,
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
cv_3s_with_varest <- cv_var_from_model_tbl(h = fc_horizon,
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

# tic()
# cv_size_3_170 <- cv_var_from_model_tbl(h = fc_horizon,
#                              training_length = training_length, 
#                              n_cv = n_cv,
#                              models_tbl = pm_size_3_170, 
#                              var_data = var_data, 
#                              fit_column = "fit", 
#                              target_transform = target_transform,
#                              target_level_ts = target_level_ts,
#                              )
# toc() 


#' 
#' 
#' 
#' ## Combine forecasts based on performance measure
#' 
#' 
#' 
#' The function forecast_var_from_model_tbl by default, it just take all specifications in given tibble and produce forecasts of the target variable, more precisely it creates a list-column containing mean forecasts of YoY growth of the target variable over the specified forecast horizon. Some options include to keep varest or forecast objects (not advised with very long tibbles) and more importantly it can restrict the sample of models by keeping only the best  models at each horizon and computing a (rmse-)weighted average mean forecast.
#' 
#' In this case we take our models from the cv-exercises, thus all models where already tested for stability, restrictions and white noise residuals and therefore we can spare those test here, which is the default behaviour.
#' 
#' 
## ----forecast_using_cv---------------------------------------------------

# forecast all 21 models
fcs_from_all <- forecast_var_from_model_tbl(
  cv_size_3_17,
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo
)

fcs_from_all_ve <- forecast_var_from_model_tbl(
  cv_size_3_17_with_varest, 
  fit_column = "fit",
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo
)

max_rank_h <- 4

fc_best_per_h <- forecast_var_from_model_tbl(
  cv_size_3_17,
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo, 
  do_weigthed_average_fc = TRUE,
  max_rank_h = max_rank_h, 
  filter_by_rank = TRUE,
  remove_aux_unrest = TRUE) 

fc_rmse_weighted <- fc_best_per_h$fcs_wavg
models_tbl_max_rank <- fc_best_per_h$models_tbl
models_tbl_max_rank
snames_best <-  models_tbl_max_rank %>% 
  dplyr::select(short_name) %>% distinct()
snames_best

models_tbl_used_fc <- semi_join(cv_size_3_17, snames_best, by = "short_name")
models_tbl_not_used_fc <- anti_join(cv_size_3_17, snames_best, by = "short_name")


fc_best_per_h_ve <- forecast_var_from_model_tbl(
  cv_size_3_17_with_varest,
  fit_column = "fit",
  var_data, 
  fc_horizon, 
  target_transform = target_transform,
  target_level_ts = target_level_ts,
  names_exogenous = names_exogenous, 
  extended_exo_mts = extension_of_exo,
  do_weigthed_average_fc = TRUE,
  max_rank_h = max_rank_h, 
  filter_by_rank = TRUE) 

fc_rmse_weighted_ve <- fc_best_per_h_ve$fcs_wavg
models_tbl_max_rank_ve <- fc_best_per_h_ve$models_tbl
models_tbl_max_rank_ve
snames_best_ve <-  models_tbl_max_rank_ve %>% 
  dplyr::select(short_name) %>% distinct()
snames_best_ve

models_tbl_used_fc_ve <- semi_join(cv_size_3_17_with_varest, snames_best_ve, by = "short_name")
models_tbl_not_used_fc_ve <- anti_join(cv_size_3_17_with_varest, snames_best_ve, by = "short_name")

moove <- fc_best_per_h_ve$stmod
moo <- fc_best_per_h$stmod



foo <- fc_best_per_h$models_tbl
foove <- fc_best_per_h_ve$models_tbl

fooved <- foove %>% distinct(short_name, .keep_all = TRUE)

food <- foo %>% distinct(short_name, .keep_all = TRUE)


#' 
#' 
#' 
## ----cv_of_ensemble------------------------------------------------------

variables_used <- fc_best_per_h$models_tbl %>% 
  dplyr::select(variables) %>% unlist() %>% unique()
var_data_best_fc <- var_data[, variables_used]

variables_used_ve <- fc_best_per_h$models_tbl %>% 
  dplyr::select(variables) %>% unlist() %>% unique()
var_data_best_fc <- var_data[, variables_used]


models_smaller <- cv_size_3_17[1, ]
models_smallerdsd <- cv_size_3_17[c(1, 11), ]
models_smaller_28 <- cv_size_3_17_tra28[1, ]
models_smaller_28_2 <- cv_size_3_17_tra28[c(1, 11), ]

h_max <- fc_horizon
  
training_length <- "per_cv_maxs"
# training_length <- "common_max"
training_length <- 28

if (training_length == "common_max") {
    total_obs <- nrow( na.omit(var_data[, variables_used]))
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
}

train_test_dates <- make_test_dates_list(
  ts_data = na.omit(var_data[, variables_used]), 
  type = "tscv",
  n = n_cv, 
  h_max = fc_horizon, 
  training_length = training_length)

# tail(var_data_train)

cv_ensemble_fcs <- list_along(seq(1, n_cv))
cv_ensemble_test_data <- list_along(seq(1, n_cv))
cv_ensemble_errors <- list_along(seq(1, n_cv))

for (i in seq(1, 10)) {
  
  print(paste0("This is cv round ", i))
  
  train_test_yq <- train_test_dates[["list_of_year_quarter"]]

  this_tra_s <- train_test_yq[[i]]$tra_s
  this_tra_e <- train_test_yq[[i]]$tra_e
  
  this_tes_s <- train_test_yq[[i]]$tes_s
  this_tes_e <- train_test_yq[[i]]$tes_e
  
  print(this_tra_e)
  print(this_tra_s)
  
  var_data_train <- window(var_data, start = this_tra_s, end = this_tra_e)

  this_fc_tbl <- forecast_var_from_model_tbl(
    models_tbl = models_smaller_28_2, 
    fc_horizon = fc_horizon, 
    var_data = var_data_train,
    target_transform = target_transform,
    target_level_ts = target_level_ts,
    names_exogenous = names_exogenous, 
    extended_exo_mts = cv_extension_of_exo[[i]],
    do_weigthed_average_fc = TRUE,
    max_rank_h = 5,
    filter_by_rank = TRUE, 
    use_resmat = TRUE) 

  ensemble_fc_rmse_weighted <- this_fc_tbl$fcs_wavg
  
  print("ensemble_fc_rmse_weighted")
  print(ensemble_fc_rmse_weighted)
  
  test_target_yoy <- window(make_yoy_ts(target_level_ts), 
                           start = this_tes_s,
                           end = this_tes_e)
  
  ensemble_cv_error_yoy <- test_target_yoy - ensemble_fc_rmse_weighted
  
  cv_ensemble_fcs[[i]] <- ensemble_fc_rmse_weighted
  cv_ensemble_test_data[[i]] <- test_target_yoy
  cv_ensemble_errors[[i]] <- ensemble_cv_error_yoy
}

mat_cv_errors_ensemble <- matrix(
    reduce(cv_ensemble_errors, rbind), 
    nrow = n_cv)

rownames(mat_cv_errors_ensemble) <- NULL

ensemble_rmse <- sqrt(colMeans(mat_cv_errors_ensemble^2, na.rm = TRUE))

ensemble_rmse

boo <- this_fc_tbl[["models_tbl"]]
soo <- spread(boo, key = "rmse_h", value = "rmse")

models_info_per_h <- boo %>% group_by(rmse_h) %>% 
  summarise(unique_vbl_per_h = list(unique(unlist(variables))),
            freq_vbl_per_h = list(table(unlist(variables))),
            unique_maxlag_per_h = list(unique(unlist(lags))),
            freq_lags_per_h = list(table(unlist(lags)))
            )

models_tbl_sel <- dplyr::select(boo, variables,  short_name, horizon, 
                                this_h_fc_yoy, rmse_h, rmse)

ensemble_all_h <- tibble(variables = list(table(unlist(boo$variables))),
                         lags = list(table(unlist(boo$lags))),
                         short_name = "ensemble",
                         horizon = sort(unique(models_tbl_sel$horizon)),
                         this_h_fc_yoy = as.numeric(ensemble_fc_rmse_weighted),
                         rmse_h = paste0("rmse_", 1:length(ensemble_fc_rmse_weighted)),
                         rmse = ensemble_rmse)


models_and_ensemble <- bind_rows(dplyr::select(ensemble_all_h, -lags), models_tbl_sel)



cv_of_VAR_ensemble <- function(var_data, used_cv_models, fc_horizon, n_cv,
                               training_length, cv_extension_of_exo, names_exogenous, 
                               max_rank_h, target_transform, target_level_ts) {
  
  print("inside cv of VAR ensemble")
  
  print("used_cv_models")
  print(used_cv_models)
  
  
  variables_used <- used_cv_models %>% 
    dplyr::select(variables) %>% unlist() %>% unique()
  
  if (training_length == "common_max") {
    total_obs <- nrow( na.omit(var_data[, variables_used]))
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  train_test_dates <- make_test_dates_list(
    ts_data = na.omit(var_data[, variables_used]), 
    type = "tscv",
    n = n_cv, 
    h_max = fc_horizon, 
    training_length = training_length)
  
  cv_ensemble_fcs <- list_along(seq(1, n_cv))
  cv_ensemble_test_data <- list_along(seq(1, n_cv))
  cv_ensemble_errors <- list_along(seq(1, n_cv))
  
  
  for (i in seq(1, n_cv)) {
    
    print(paste0("This is cv round ", i))
    
    train_test_yq <- train_test_dates[["list_of_year_quarter"]]
    
    this_tra_s <- train_test_yq[[i]]$tra_s
    this_tra_e <- train_test_yq[[i]]$tra_e
    
    this_tes_s <- train_test_yq[[i]]$tes_s
    this_tes_e <- train_test_yq[[i]]$tes_e
    
    print(this_tra_e)
    print(this_tra_s)
    
    var_data_train <- window(var_data, start = this_tra_s, end = this_tra_e)
    
    this_fc_tbl <- forecast_var_from_model_tbl(
      models_tbl = used_cv_models, 
      fc_horizon = fc_horizon, 
      var_data = var_data_train,
      target_transform = target_transform,
      target_level_ts = target_level_ts,
      names_exogenous = names_exogenous, 
      extended_exo_mts = cv_extension_of_exo[[i]],
      do_weigthed_average_fc = TRUE,
      max_rank_h = max_rank_h,
      filter_by_rank = TRUE, 
      use_resmat = TRUE) 
    
    ensemble_fc_rmse_weighted <- this_fc_tbl$fcs_wavg

    test_target_yoy <- window(make_yoy_ts(target_level_ts), 
                              start = this_tes_s,
                              end = this_tes_e)
    
    ensemble_cv_error_yoy <- test_target_yoy - ensemble_fc_rmse_weighted
    
    print("ensemble_fc_rmse_weighted")
    print(ensemble_fc_rmse_weighted)
    
    print("test_target_yoy ")
    print(test_target_yoy)
    
    print("ensemble_cv_error_yoy")
    print(ensemble_cv_error_yoy)
    
    
    
    
    cv_ensemble_fcs[[i]] <- ensemble_fc_rmse_weighted
    cv_ensemble_test_data[[i]] <- test_target_yoy
    cv_ensemble_errors[[i]] <- ensemble_cv_error_yoy
  }
  
  mat_cv_errors_ensemble <- matrix(
    reduce(cv_ensemble_errors, rbind), 
    nrow = n_cv)
  
  rownames(mat_cv_errors_ensemble) <- NULL
  
  ensemble_rmse <- sqrt(colMeans(mat_cv_errors_ensemble^2, na.rm = TRUE))
  
  print("cv_ensemble_errors")
  print(cv_ensemble_errors)
  
  print("mat_cv_errors_ensemble")
  print(mat_cv_errors_ensemble)
  
  print("ensemble_rmse")
  print(ensemble_rmse)
  
  return(ensemble_rmse)
}


foo <- forecast_var_from_model_tbl(models_tbl = cv_size_3_17_tra28,
                                   var_data = var_data,
                                   fc_horizon = fc_horizon,
                                   new_t_threshold = NULL, 
                                   fit_column = NULL,
                                   target_transform = target_transform,
                                   target_level_ts = target_level_ts,
                                   keep_fc_obj = FALSE,
                                   keep_varest_obj = FALSE,
                                   names_exogenous = names_exogenous,
                                   extended_exo_mts = extension_of_exo,
                                   do_tests = FALSE,
                                   filter_by_rank = TRUE,
                                   max_rank_h = max_rank_h, 
                                   remove_aux_unrest = TRUE,
                                   use_resmat = FALSE,
                                   do_weigthed_average_fc = TRUE,
                                   do_ensemble_cv = TRUE,
                                   n_cv = n_cv,
                                   training_length = training_length,
                                   cv_extension_of_exo = cv_extension_of_exo)






p <- ggplot(data = models_and_ensemble,
            aes(x = horizon, y = this_h_fc_yoy, color = short_name)) + 
  geom_line()

p
