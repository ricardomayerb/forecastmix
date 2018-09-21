# source('./R/utils_av_ricardo.R')
source('./R/VAR_functions.R')

country_name <- "Peru"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

country_data_level_ts <- get_raw_data_ts(country_name, excel_data_path)

reco_all_variables <- find_statio_diffs(country_data_level_ts, country_name)

country_transformed_data <- follow_rec(country_data_level_ts, 
                                       reco_all_variables)

VAR_data_for_estimation  <- na.omit(country_transformed_data)
variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)


saveRDS(VAR_data_for_estimation , 
        paste0("./analysis/VAR_output/VAR_data_",country_name,".rds"))

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
print(rgdp_rec)

n_best <- 50
number_of_cv <- 8
fc_horizon <- 7
# fc_horizon is set to 7 because 8 is too long for peru
train_span <- 20

if (train_span+fc_horizon+number_of_cv > nrow(VAR_data_for_estimation)) {
  
  print("not enough obs")
  
  stop()
  
}

target_variable <- c("rgdp")


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


tic()
var_res_s2_aic_fpe_hq_sc_t2  <- search_var(vec_size = 2,
                                           vec_lags = vec_lags_aic_fpe_hq_sc ,
                                           var_data = VAR_data_for_estimation,
                                           rgdp_level_ts = rgdp_level_ts, 
                                           target_v = target_variable,
                                           pre_selected_v = c(""), 
                                           is_cv = TRUE,
                                           training_length = train_span,
                                           h_max = fc_horizon, 
                                           n_cv = number_of_cv,
                                           return_cv = ret_cv,
                                           rgdp_current_form = rgdp_rec,
                                           max_rank = 50, 
                                           check_residuals_cv = FALSE,
                                           check_residuals_full_sample = TRUE, 
                                           max_p_for_estimation = 12, 
                                           restrict_by_signif = TRUE,
                                           t_tresh = 1.65)

toc()
models_and_accu_s2_aic_fpe_hq_sc_t2 <- var_res_s2_aic_fpe_hq_sc_t2[["accu_rankings_models"]]
cv_objects_s2_aic_fpe_hq_sc_t2 <- var_res_s2_aic_fpe_hq_sc_t2[["cv_objects"]]
saveRDS(models_and_accu_s2_aic_fpe_hq_sc_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s2_aic_fpe_hq_sc_t2.rds"))
saveRDS(cv_objects_s2_aic_fpe_hq_sc_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s2_aic_fpe_hq_sc_t2.rds"))



tic()
var_res_s3_12345_t2 <- search_var(vec_size = 2,
                                  vec_lags = vec_lags_12345,
                                  var_data = VAR_data_for_estimation,
                                  rgdp_level_ts = rgdp_level_ts, 
                                  target_v = target_variable,
                                  pre_selected_v = c(""), 
                                  is_cv = TRUE,
                                  training_length = train_span,
                                  h_max = fc_horizon, 
                                  n_cv = number_of_cv,
                                  return_cv = ret_cv,
                                  rgdp_current_form = rgdp_rec,
                                  max_rank = 10, 
                                  check_residuals_cv = FALSE,
                                  check_residuals_full_sample = TRUE, 
                                  max_p_for_estimation = 12, 
                                  restrict_by_signif = TRUE,
                                  t_tresh = 2)

toc()
models_and_accu_s3_12345_t2 <- var_res_s3_12345_t2[["accu_rankings_models"]]
cv_objects_s3_12345_t2 <- var_res_s3_12345_t2[["cv_objects"]]
saveRDS(models_and_accu_s3_12345_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s3_12345_t2.rds"))
saveRDS(cv_objects_s3_12345_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s3_12345_t2.rds"))



v2 <- c("rgdp", "tcr")
p <- 5
var1 <- vars::VAR(VAR_data_for_estimation[,v2], p = p, type = "const")
var1r <- vars::restrict(var1, method = "ser", thresh = 1.65)
var1r

var1cv <- vars::VAR(VAR_data_for_estimation[2:28,v2], p = p, type = "const")
var1rr <- vars::restrict(var1cv, method = "manual", resmat = var1r$restrictions)
var1rr





tic()
var_res_s3_aic_fpe_hq_sc_t2  <- search_var(vec_size = 3,
                                        vec_lags = vec_lags_aic_fpe_hq_sc ,
                                        var_data = VAR_data_for_estimation,
                                        rgdp_level_ts = rgdp_level_ts,
                                        target_v = target_variable,
                                        pre_selected_v = c(""),
                                        is_cv = TRUE,
                                        training_length = train_span,
                                        h_max = fc_horizon,
                                        n_cv = number_of_cv,
                                        return_cv = ret_cv,
                                        rgdp_current_form = rgdp_rec,
                                        max_rank = 50,
                                        check_residuals_cv = FALSE,
                                        check_residuals_full_sample = TRUE,
                                        max_p_for_estimation = 12,
                                        restrict_by_signif = TRUE,
                                        t_tresh = 2)

toc()
models_and_accu_s3_aic_fpe_hq_sc_t2 <- var_res_s3_aic_fpe_hq_sc_t2[["accu_rankings_models"]]
cv_objects_s3_aic_fpe_hq_sc_t2 <- var_res_s3_aic_fpe_hq_sc_t2[["cv_objects"]]
saveRDS(models_and_accu_s3_aic_fpe_hq_sc_t2,
        paste0("./analysis/VAR_output/",country_name,"_s3_aic_fpe_hq_sc_t2.rds"))
saveRDS(cv_objects_s3_aic_fpe_hq_sc_t2,
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s3_aic_fpe_hq_sc_t2.rds"))
# 
# 
# 
# tic()
# var_res_s3_12345_t2 <- search_var(vec_size = 3,
#                                vec_lags = vec_lags_12345,
#                                var_data = VAR_data_for_estimation,
#                                rgdp_level_ts = rgdp_level_ts, 
#                                target_v = target_variable,
#                                pre_selected_v = c(""), 
#                                is_cv = TRUE,
#                                training_length = train_span,
#                                h_max = fc_horizon, 
#                                n_cv = number_of_cv,
#                                return_cv = ret_cv,
#                                rgdp_current_form = rgdp_rec,
#                                max_rank = 10, 
#                                check_residuals_cv = FALSE,
#                                check_residuals_full_sample = TRUE, 
#                                max_p_for_estimation = 12, 
#                                restrict_by_signif = TRUE,
#                                t_tresh = 2)
# 
# toc()
# models_and_accu_s3_12345_t2 <- var_res_s3_12345_t2[["accu_rankings_models"]]
# cv_objects_s3_12345_t2 <- var_res_s3_12345_t2[["cv_objects"]]
# saveRDS(models_and_accu_s3_12345_t2, 
#         paste0("./analysis/VAR_output/",country_name,"_s3_12345_t2.rds"))
# saveRDS(cv_objects_s3_12345_t2, 
#         paste0("./analysis/VAR_output/",country_name,"_cvobj_s3_12345_t2.rds"))


models_and_accu_s3_12345_t2 %>% arrange(rmse_1)

