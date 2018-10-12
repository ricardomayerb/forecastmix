source('./R/VAR_functions.R')


##### data selection part -----
# arguments
country_name <- "Bolivia"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

# file paths
excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")


##### data pre-processing part -----

country_data_ts <- get_raw_data_ts(country_name, excel_data_path)
external_data_ts <- get_raw_external_data_ts(excel_data_path)

data_ts <- country_data_ts

rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

print(paste0("This country: ", country_name))
print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
print("Names of variables: ")
print(colnames(data_ts))

tic()
print("Finding and applying stationary transformations to all variables")
reco_all_variables <- find_statio_diffs(data_ts, country_name)
country_transformed_data <- follow_rec(data_ts, reco_all_variables)
print("Done.")
toc()

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
print(paste0("Stationary transformation for rgdp: ", rgdp_rec))

VAR_data_for_estimation  <- country_transformed_data

print(paste0("rgdp obs. after transformation: ", 
             length(na.omit(VAR_data_for_estimation[ , "rgdp"]))
             )
      )

print(paste0("rgdp obs. before transformation: ", length(rgdp_level_ts)))


variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

saveRDS(VAR_data_for_estimation, 
        paste0(output_path, "VAR_data_",country_name,".rds"))


##### VAR selection part -----
# arguments
## target, pre-chosen and exogenous variables
target_variable <- c("rgdp")
other_prechosen_variables <- c("")

## VAR sizes (how many variables per VAR) to explore:
# vec_var_sizes <- c(2, 3, 4, 5)
vec_var_sizes <- c(2, 3, 4, 2)
vec_freq_limit <- c("none", "none", 15, 10)
freq_sel_vbls <- colnames(VAR_data_for_estimation) # same as freq_limit = 'none'

## VAR lags to explore
vec_lags <- c(1, 2, 3, 4, 5, 6)
add_aic_bic_hq_fpe_lags <-  FALSE

## VAR restrictions
restrict_by_signif <- TRUE
t_tresh <- c(2, 2, 2, 2)

## forecast horizon
fc_horizon <- 7

## Cross-validation parameters
number_of_cv <- 8
train_span <- 25
ret_cv = TRUE

## model retention parameters
max_rank_some_h <- 50



if (train_span + fc_horizon + number_of_cv > nrow(VAR_data_for_estimation)) {
  print("not enough obs")
  stop()
}

per_size_results <- list_along(vec_var_sizes)



for (i in seq(length(vec_var_sizes))) {

  this_size <- vec_var_sizes[i]
  this_t_tresh <- t_tresh[i]
  this_freq_limit <- vec_freq_limit[i]
  
  if(i < length(vec_var_sizes)){
    next_freq_limit <- vec_freq_limit[i + 1]
  }
  
  if (this_freq_limit == "none") {
    this_VAR_data <- VAR_data_for_estimation
  }
  
  print(paste0("Starting the estimation of VAR with ", this_size," variables"))

  if (is.numeric(this_freq_limit)) {
    print("Using this subset of variables: ")
    print(freq_sel_vbls)
    
    this_VAR_data <- VAR_data_for_estimation[, freq_sel_vbls]
  }
  
  
  
  
  tic(msg = paste0("Finished VARs with ", this_size, " variables"))
  
  
  var_res <- search_var_one_size(
    var_size = this_size,
    vec_lags = vec_lags,
    var_data = this_VAR_data,
    rgdp_level_ts = rgdp_level_ts,
    rgdp_yoy_ts = rgdp_yoy_ts,
    target_v = target_variable,
    pre_selected_v = other_prechosen_variables,
    is_cv = TRUE,
    training_length = train_span,
    h_max = fc_horizon,
    n_cv = number_of_cv,
    return_cv = ret_cv,
    rgdp_current_form = rgdp_rec,
    max_rank = max_rank_some_h,
    check_residuals_cv = TRUE,
    check_residuals_full_sample = TRUE,
    restrict_by_signif = restrict_by_signif,
    t_tresh = this_t_tresh,
    max_p_for_estimation = 12,
    add_info_based_lags = add_aic_bic_hq_fpe_lags)
  
  if (i < length(vec_var_sizes)) {
    if(this_freq_limit == "none") {
      f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                   h_max = fc_horizon, max_rank = 10,
                                   n_freq = 50, is_wide = TRUE)
    }
    
    if(is.numeric(this_freq_limit)) {
      f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                   h_max = fc_horizon, max_rank = 10,
                                   n_freq = next_freq_limit, is_wide = TRUE)
      freq_sel_vbls <- f_vbls$vbl_multi
    }
  } 
  
  if (i == length(vec_var_sizes)) {
    f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                 h_max = fc_horizon, max_rank = 10,
                                 n_freq = next_freq_limit, is_wide = TRUE)
  }
  
  
  file_suffix <- paste0("_size_", this_size, "_fqlim_", this_freq_limit, "_t_", this_t_tresh, ".rds")
  filename <- paste0("var_results_", country_name, file_suffix)
  saveRDS(var_res, paste0(output_path, filename))
  
  per_size_results[i] <- var_res
  
  toc()
}

var_res_all_sizes <- reduce(per_size_results, stack_models)

allsizes <- paste(vec_var_sizes, collapse = "")
allthresh <- paste(t_tresh, collapse = "")
allfqlim <- paste(vec_freq_limit, collapse = "")

file_suffix_all_sizes <-  paste0("_sizes_", allsizes, "_fqlims_", allfqlim,
                                 "_t_", allthresh, ".rds")



print(paste0("Starting the estimation of VAR with 2 variables"))
tic()
var_res_s2_123456_t2 <- search_var_one_size(var_size = 2,
                               vec_lags = vec_lags_123456,
                               var_data = VAR_data_for_estimation,
                               rgdp_level_ts = rgdp_level_ts,
                               rgdp_yoy_ts = rgdp_yoy_ts,
                               target_v = target_variable,
                               pre_selected_v = c(""),
                               is_cv = TRUE,
                               training_length = train_span,
                               h_max = fc_horizon,
                               n_cv = number_of_cv,
                               return_cv = ret_cv,
                               rgdp_current_form = rgdp_rec,
                               max_rank = max_rank_some_h,
                               check_residuals_cv = TRUE,
                               check_residuals_full_sample = TRUE,
                               restrict_by_signif = TRUE,
                               t_tresh = 2,
                               max_p_for_estimation = 12, 
                               add_info_based_lags = add_aic_bic_hq_fpe_lags)

toc()
models_and_accu_s2_123456_t2 <- var_res_s2_123456_t2[["accu_rankings_models"]]
cv_objects_s2_123456_t2 <- var_res_s2_123456_t2[["cv_objects"]]

models_and_accu_s2_t2 <- stack_models(list(models_and_accu_s2_123456_t2))


saveRDS(models_and_accu_s2_123456_t2,
        paste0(output_path, country_name,"_s2_123456_t2.rds"))
saveRDS(models_and_accu_s2_t2,
        paste0(output_path, country_name, "_s2_t2.rds"))

saveRDS(cv_objects_s2_123456_t2,
        paste0(output_path, country_name,"_cvobj_s2_123456_t2.rds"))

f_s2_t2 <- variable_freq_by_n(models_and_accu_s2_t2, h_max = 7, max_rank = 20,
                              n_freq = 4, is_wide = TRUE)

format(object.size(models_and_accu_s2_t2), units = "auto")
rm(models_and_accu_s2_123456_t2)



tic()
var_res_s3_123456_t2 <- search_var_one_size(var_size = 3,
                               vec_lags = vec_lags_123456,
                               var_data = VAR_data_for_estimation,
                               rgdp_level_ts = rgdp_level_ts,
                               rgdp_yoy_ts = rgdp_yoy_ts,
                               target_v = target_variable,
                               pre_selected_v = c(""),
                               is_cv = TRUE,
                               training_length = train_span,
                               h_max = fc_horizon,
                               n_cv = number_of_cv,
                               return_cv = ret_cv,
                               rgdp_current_form = rgdp_rec,
                               max_rank = max_rank_some_h,
                               check_residuals_cv = FALSE,
                               check_residuals_full_sample = TRUE,
                               max_p_for_estimation = 12,
                               restrict_by_signif = TRUE,
                               t_tresh = 2, 
                               add_info_based_lags = add_aic_bic_hq_fpe_lags)

toc()
models_and_accu_s3_123456_t2 <- var_res_s3_123456_t2[["accu_rankings_models"]]
cv_objects_s3_123456_t2 <- var_res_s3_123456_t2[["cv_objects"]]

models_and_accu_s3_t2 <- stack_models(list(models_and_accu_s3_123456_t2))

saveRDS(models_and_accu_s3_123456_t2,
        paste0(output_path, country_name,"_s3_123456_t2.rds"))
saveRDS(models_and_accu_s3_t2,
        paste0(output_path, country_name, "_s3_t2.rds"))

saveRDS(cv_objects_s3_123456_t2,
        paste0(output_path, country_name,"_cvobj_s3_123456_t2.rds"))

f_s3_123456_t2 <- variable_freq_by_n(models_and_accu_s3_123456_t2,
                                     h_max = 7, max_rank = 20, n_freq = 4,
                                     is_wide = TRUE)

f_s3_t2 <- variable_freq_by_n(models_and_accu_s3_t2, h_max = 7, max_rank = 10,
                              n_freq = 15, is_wide = TRUE)

vbl_most_freq_multi <- f_s3_t2$vbl_multi
vbl_most_freq_by_h1 <- f_s3_t2$vbl_by_h1
vbl_most_freq_by_hlast <- f_s3_t2$vbl_by_hlast
vbl_most_freq_by_total <- f_s3_t2$vbl_by_total

lags_most_freq_multi <- f_s3_t2$lags_multi
lags_most_freq_by_h1 <- f_s3_t2$lags_by_h1
lags_most_freq_by_hlast <- f_s3_t2$lags_by_hlast
lags_most_freq_by_total <- f_s3_t2$lags_by_total

vbl_most_freq_multi
lags_most_freq_multi

format(object.size(models_and_accu_s3_t2), units = "auto")
rm(models_and_accu_s3_123456_t2)

VAR_data_for_estimation_for_s4 <- VAR_data_for_estimation[, vbl_most_freq_multi]




tic()
var_res_s4_123456_t2 <- search_var_one_size(var_size = 4,
                               vec_lags = vec_lags_123456,
                               var_data = VAR_data_for_estimation_for_s4,
                               rgdp_level_ts = rgdp_level_ts,
                               rgdp_yoy_ts = rgdp_yoy_ts,
                               target_v = target_variable,
                               pre_selected_v = c(""),
                               is_cv = TRUE,
                               training_length = train_span,
                               h_max = fc_horizon,
                               n_cv = number_of_cv,
                               return_cv = ret_cv,
                               rgdp_current_form = rgdp_rec,
                               max_rank = max_rank_some_h,
                               check_residuals_cv = FALSE,
                               check_residuals_full_sample = TRUE,
                               max_p_for_estimation = 12,
                               restrict_by_signif = TRUE,
                               t_tresh = 2, 
                               add_info_based_lags = add_aic_bic_hq_fpe_lags)

toc()
models_and_accu_s4_123456_t2 <- var_res_s4_123456_t2[["accu_rankings_models"]]
cv_objects_s4_123456_t2 <- var_res_s4_123456_t2[["cv_objects"]]

models_and_accu_s4_t2 <- stack_models(list(models_and_accu_s4_123456_t2))

saveRDS(models_and_accu_s4_123456_t2,
        paste0(output_path, country_name,"_s4_123456_t2.rds"))
saveRDS(cv_objects_s4_123456_t2,
        paste0(output_path, country_name,"_cvobj_s4_123456_t2.rds"))

saveRDS(models_and_accu_s4_t2,
        paste0(output_path, country_name,"_s4_t2.rds"))


f_s4_t2 <- variable_freq_by_n(models_and_accu_s4_t2, h_max = 7, max_rank = 20,
                              n_freq = 10, is_wide = TRUE)

s4_vbl_most_freq_multi <- f_s4_t2$vbl_multi
s4_vbl_most_freq_by_h1 <- f_s4_t2$vbl_by_h1
s4_vbl_most_freq_by_hlast <- f_s4_t2$vbl_by_hlast
s4_vbl_most_freq_by_total <- f_s4_t2$vbl_by_total

s4_lags_most_freq_multi <- f_s4_t2$lags_multi
s4_lags_most_freq_by_h1 <- f_s4_t2$lags_by_h1
s4_lags_most_freq_by_hlast <- f_s4_t2$lags_by_hlast
s4_lags_most_freq_by_total <- f_s4_t2$lags_by_total

s4_vbl_most_freq_multi
s4_lags_most_freq_multi

format(object.size(models_and_accu_s4_t2), units = "auto")
rm(models_and_accu_s4_123456_t2)



VAR_data_for_estimation_for_s5 <- VAR_data_for_estimation[, s4_vbl_most_freq_multi]
tic()
var_res_s5_123456_t2 <- search_var_one_size(var_size = 5,
                                            vec_lags = vec_lags_123456,
                                            var_data = VAR_data_for_estimation_for_s5,
                                            rgdp_level_ts = rgdp_level_ts,
                                            rgdp_yoy_ts = rgdp_yoy_ts,
                                            target_v = target_variable,
                                            pre_selected_v = c(""),
                                            is_cv = TRUE,
                                            training_length = train_span,
                                            h_max = fc_horizon,
                                            n_cv = number_of_cv,
                                            return_cv = ret_cv,
                                            rgdp_current_form = rgdp_rec,
                                            max_rank = max_rank_some_h,
                                            check_residuals_cv = FALSE,
                                            check_residuals_full_sample = TRUE,
                                            max_p_for_estimation = 12,
                                            restrict_by_signif = TRUE,
                                            t_tresh = 2, 
                                            add_info_based_lags = add_aic_bic_hq_fpe_lags)

toc()
models_and_accu_s5_123456_t2 <- var_res_s5_123456_t2[["accu_rankings_models"]]
cv_objects_s5_123456_t2 <- var_res_s5_123456_t2[["cv_objects"]]

models_and_accu_s5_t2 <- stack_models(list(models_and_accu_s5_123456_t2))

saveRDS(models_and_accu_s5_123456_t2,
        paste0(output_path, country_name,"_s5_123456_t2.rds"))
saveRDS(cv_objects_s5_123456_t2,
        paste0(output_path, country_name,"_cvobj_s5_123456_t2.rds"))

saveRDS(models_and_accu_s5_t2,
        paste0(output_path, country_name,"_s5_t2.rds"))

models_and_accu_s12345_t2 <- stack_models(list(models_and_accu_s2_t2,
                                              models_and_accu_s3_t2,
                                              models_and_accu_s4_t2,
                                              models_and_accu_s5_t2))

saveRDS(models_and_accu_s12345_t2,
        paste0(output_path, country_name,"_s12345_t2.rds"))
format(object.size(models_and_accu_s12345_t2), units = "auto")

