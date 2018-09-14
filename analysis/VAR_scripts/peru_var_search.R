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

n_best <- 5
number_of_cv <- 8
fc_horizon <- 7
# fc_horizon is set to 7 because 8 is too long for peru
train_span <- 25

if (train_span+fc_horizon+number_of_cv > nrow(VAR_data_for_estimation)) {
  
  print("not enough obs")
  
  stop()
  
}

target_variable <- c("rgdp")


# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")
ret_cv = TRUE

# vec_lags_1 <- c(1,2,3)

vec_lags_1 <- c("hq", "sc")
tic()
var_res_1_sc_hq <- search_var(vec_size = 2,
                        vec_lags = vec_lags_1,
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
                        max_rank = 5, 
                        check_residuals_cv = TRUE,
                        check_residuals_full_sample = TRUE, 
                        restrict_by_signif = TRUE, 
                        t_tresh = 1.8)

toc()

models_and_accu_1 <- var_res_1[["accu_rankings_models"]]
cv_objects_1 <- var_res_1[["cv_objects"]]




vec_lags_2 <- c("sc")
tic()
var_res_2 <- search_var(vec_size = 3,
                        vec_lags = vec_lags_2,
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
                        max_rank = 5, 
                        check_residuals_cv = FALSE,
                        check_residuals_full_sample = TRUE, 
                        max_p_for_estimation = 10, 
                        restrict_by_signif = TRUE,
                        t_tresh = 1.8)

toc()

models_and_accu_2 <- var_res_2[["accu_rankings_models"]]
cv_objects_2 <- var_res_2[["cv_objects"]]




vec_lags_3 <- c(1, 2, 3, 4, 5, 6)
tic()
var_res_3 <- search_var(vec_size = 4,
                        vec_lags = vec_lags_3,
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
                        max_rank = 30, 
                        check_residuals_cv = FALSE,
                        check_residuals_full_sample = TRUE, 
                        max_p_for_estimation = 6,
                        restrict_by_signif = TRUE, 
                        t_tresh = 2)

toc()

models_and_accu_3 <- var_res_3[["accu_rankings_models"]]
cv_objects_3 <- var_res_3[["cv_objects"]]

# saveRDS(models_and_accu_4, "./analysis/VAR_output/Peru_vs_4.rds")
# saveRDS(cv_objects_4, "./analysis/VAR_output/Peru_cv_obj_vs_4.rds")





vec_lags_size_4_sc <- c("sc")
tic()
var_res_size_4_sc <- search_var(vec_size = 4,
                        vec_lags = vec_lags_size_4_sc,
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
                        max_rank = 30, 
                        check_residuals_cv = FALSE,
                        check_residuals_full_sample = TRUE, 
                        max_p_for_estimation = 10,
                        restrict_by_signif = TRUE, 
                        t_tresh = 2)

toc()

models_and_accu_size_4_sc <- var_res_size_4_sc[["accu_rankings_models"]]
cv_objects_size_4_sc <- var_res_size_4_sc[["cv_objects"]]




vec_lags_size_4_sc_hq <- c("sc", "hq")
tic()
var_res_size_4_sc_hq <- search_var(vec_size = 4,
                                vec_lags = vec_lags_size_4_sc_hq,
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
                                max_rank = 30, 
                                check_residuals_cv = FALSE,
                                check_residuals_full_sample = TRUE, 
                                max_p_for_estimation = 10,
                                restrict_by_signif = TRUE, 
                                t_tresh = 1.8)

toc()

models_and_accu_size_4_sc_hq <- var_res_size_4_sc_hq[["accu_rankings_models"]]
cv_objects_size_4_sc_hq <- var_res_size_4_sc_hq[["cv_objects"]]





vec_lags_4 <- c("sc")
tic()
var_res_4 <- search_var(vec_size = 5,
                        vec_lags = vec_lags_4,
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
                        max_rank = 5, 
                        check_residuals_cv = FALSE,
                        check_residuals_full_sample = TRUE, 
                        max_p_for_estimation = 7, 
                        restrict_by_signif = FALSE)

toc()

models_and_accu_4 <- var_res_4[["accu_rankings_models"]]
cv_objects_4 <- var_res_4[["cv_objects"]]

