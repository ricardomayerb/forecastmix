source('./R/VAR_functions.R')

country_name <- "Colombia"
print(country_name)
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

country_data_ts <- get_raw_data_ts(country_name, excel_data_path)

external_data_ts <- get_raw_external_data_ts(excel_data_path)
# colnames(external_data_ts)

data_ts <- country_data_ts
rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)

rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)


# data_ts <- ts.union(country_data_ts, external_data_ts)
# colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))
colnames(data_ts)

reco_all_variables <- find_statio_diffs(data_ts, country_name)

country_transformed_data <- follow_rec(data_ts, reco_all_variables)

VAR_data_for_estimation  <- country_transformed_data
variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

saveRDS(VAR_data_for_estimation, 
        paste0(output_path, "VAR_data_",country_name,".rds"))

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
print(rgdp_rec)

n_best <- 50
number_of_cv <- 8
fc_horizon <- 7
# fc_horizon is set to 7 because 8 is too long for peru
train_span <- 25

if (train_span+fc_horizon+number_of_cv > nrow(VAR_data_for_estimation)) {
  
  print("not enough obs")
  
  stop()
  
}

target_variable <- c("rgdp")

add_aic_bic_hq_fpe_lags <-  FALSE


# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")
ret_cv = TRUE

# vec_lags_1 <- c(1,2,3)

vec_lags_fpe <- c("fpe")
vec_lags_hq_sc <- c("hq", "sc")
vec_lags_sc <- c("sc")
vec_lags_fpe_hq_sc <- c("fpe", "hq", "sc")
vec_lags_aic_fpe_hq_sc <- c("aic", "fpe", "hq", "sc")
vec_lags_12345 <- c(1,2,3,4,5)
vec_lags_123456 <- c(1,2,3,4,5,6)

max_rank_some_h <- 50



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

