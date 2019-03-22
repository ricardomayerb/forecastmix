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
models_per_h_to_work <- 20
working_models <- discard_by_rank(models_tbl = models_from_search, max_rank_h = models_per_h_to_work, is_wide = TRUE)

# to free some memory
rm(models_from_search, models_from_search_1, models_from_search_2, models_from_search_3, models_from_search_4, models_from_search_5, models_from_search_6)


####

# if (target_transform == "yoy") {
#   print("Target variable already in YoY form, so no transformation is needed")
# } 
# if (target_transform != "yoy") {
#   print(paste0("Target variable is in ", target_transform,
#                " form. Forecasts will be transformed to YoY."))
# }


specs_to_fc <- function(var_data, 
                        variables, 
                        lags,
                        h, 
                        target_transform, 
                        target_level_ts,
                        extended_exo_mts,
                        t_thresholds = 0, 
                        do_tests = FALSE,
                        names_exogenous = c("")){
  pass_tests <- TRUE
  
  
  
  if (length(t_thresholds) == 1) {
    if (t_thresholds == 0) {
      is_unrestricted <- TRUE
    } else {
      is_unrestricted <- FALSE
    }
  } else {
    is_unrestricted <- FALSE
  }
  
  fit <- try(fit_VAR_rest(var_data = var_data, variables = variables,
                          p = lags, t_thresh = t_thresholds,
                          names_exogenous = names_exogenous),
             silent = TRUE)

  if (is_unrestricted) {
    thresh_fit_tbl <- tibble(t_threshold = t_thresholds, fit = list(fit))

  } else {
    thresh_fit_tbl <- fit
  }
  
  nfits <- nrow(thresh_fit_tbl)
  all_fits_list <- list_along(seq(1, nfits))
  
  for (f in seq(1, nfits)) {
    this_row <- thresh_fit_tbl[f, ]
    this_fit <- this_row[["fit"]][[1]]
    this_thresh <- this_row[["t_threshold"]]
    fit_class <- class(this_fit)[[1]]

    if (fit_class != "varest") {
      do_tests <- FALSE
      pass_tests <- FALSE
      tested <- FALSE
    }
    
    msg <- "ok"
    
    if (this_thresh > 0 & fit_class != "varest") {
      msg <- "restr_fail"
    }
    
    if (this_thresh == 0 & fit_class != "varest") {
      msg <- "unrestr_fail"
    }
    
    tested <- FALSE

    if (do_tests) {
      tested <- TRUE
      is_stable <-  try(all(vars::roots(this_fit) < 1))
      if(class(is_stable) == "try-error") {
        print("problem with var roots. Current variables are")
        print(variables)
        is_stable <- FALSE 
      }
      is_white_noise <-  check_resid_VAR(this_fit)
      pass_tests <- is_stable & is_white_noise

    }
    
    if (tested) {
      if (!is_stable) {
        msg <- "unstable"
      } 
      
      if (is_stable & !is_white_noise) {
        msg <- "not_white_noise"
      }
    }
    
    if (!tested) {
      is_stable <- NA
      is_white_noise <- NA
      pass_tests <- NA
    }
    
    if (is.na(pass_tests)) {
      do_fc <- fit_class == "varest"
    } else {
      do_fc <- pass_tests
    }
    
    this_fc <- forecast_VAR_one_row(this_fit, h, variables, extended_exo_mts, 
                                     names_exogenous = c(""), exo_lag = NULL) 
    if (target_transform == "yoy") {
     target_mean_fc_yoy <- this_fc[["forecast"]][["rgdp"]][["mean"]]
     target_mean_fc  <- target_mean_fc_yoy
    }
    
    if (target_transform != "yoy") {
      target_mean_fc <- this_fc[["forecast"]][["rgdp"]][["mean"]]
      target_mean_fc_yoy = any_fc_2_fc_yoy(current_fc = target_mean_fc, 
                                          rgdp_transformation = target_transform,
                                          rgdp_level_ts = target_level_ts)
      }
    

    tibble_to_return <- tibble(msg=msg, tested=tested, pass_tests=pass_tests,
                               t_threshold=this_thresh, variables=list(variables),
                               fc_obj=list(this_fc), mean_fc=list(target_mean_fc), 
                               mean_fc_yoy=list(target_mean_fc_yoy))
    
    # tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_h))
    
    all_fits_list[[f]] <- tibble_to_return
  }
  
  all_fits_list <- reduce(all_fits_list, rbind)
  
  return(all_fits_list)

}

# 
# if (!keep_fc_obj) {
#   models_tbl <- models_tbl %>% 
#     dplyr::select(-fc_object_raw)
# }
# 
# if(keep_wide_tbl) {
#   models_tbl_wide <- models_tbl
# } 
# 
# rmse_names <- paste0("rmse_", 1:fc_horizon)
# 
# models_tbl <- models_tbl %>% 
#   gather(key = "rmse_h", value = "rmse", rmse_names)
# 
# models_tbl <- models_tbl %>% 
#   mutate(horizon = as.numeric(substr(rmse_h, 6, 6))
#   ) 
# 
# models_tbl <- models_tbl %>% 
#   mutate(this_h_fc_yoy = map2_dbl(target_mean_fc_yoy, horizon, ~ .x[.y])
#   ) 
# 
# models_tbl <- models_tbl %>% 
#   group_by(horizon) %>% 
#   mutate(rank = rank(rmse))  
# 
# if (!is.null(max_rank_h)) {
#   models_tbl <- discard_by_rank(models_tbl, max_rank_h = max_rank_h,
#                                 is_wide =FALSE)
# }


variables <- working_models[1,]$variables[[1]]
lags <- 1
t_threshold <- working_models[1,]$t_threshold

foo <- specs_to_fc(var_data = var_data,
                   variables = variables,
                   lags = lags,
                   h = fc_horizon,
                   target_transform = target_transform,
                   target_level_ts = target_level_ts,
                   t_thresholds = t_threshold, 
                   extended_exo_mts = extension_of_exo$extended_exo,
                   do_tests = FALSE,
                   names_exogenous = names_exogenous)


foo  


  

ensemble_fc_by <- function(models_tbl_with_rmse, 
                            var_data,
                            n_cv, 
                            training_length,
                            max_rank_h,
                            fc_horizon,
                            names_exogenous,
                            target_transform, 
                            target_level_ts, 
                            extension_of_exo, 
                            cv_extension_of_exo, 
                            fit_column = NULL,
                            keep_wide_tbl = TRUE, 
                            full_cv_output = FALSE){
  
  fcs_list <- forecast_var_from_model_tbl(
    models_tbl = models_tbl_with_rmse,
    fit_column = fit_column,
    var_data = var_data, 
    fc_horizon = fc_horizon, 
    target_transform = target_transform,
    target_level_ts = target_level_ts,
    names_exogenous = names_exogenous, 
    extended_exo_mts = extension_of_exo, 
    keep_wide_tbl = keep_wide_tbl, 
    max_rank_h = max_rank_h
  )
  
}



final_tbl_10 <- new_ensemble_fc(
  models_tbl_with_rmse = working_models,
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






