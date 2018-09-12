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
number_of_cv <- 3
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

vec_lags_1 <- c(1,2,3)

tic()
var_res_1 <- search_var(vec_size = 2,
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
                        check_residuals_full_sample = TRUE)

toc()

models_and_accu_1 <- var_res_1[["accu_rankings_models"]]
cv_objects_1 <- var_res_1[["cv_objects"]]

foo1 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_1, rank_1, wn_cv, wn_fs) %>% 
  arrange(rank_1)
foo1

foo2 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_2, rank_2, wn_cv, wn_fs) %>% 
  arrange(rank_2)
foo2

foo3 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_3, rank_3, wn_cv, wn_fs) %>% 
  arrange(rank_3)
foo3

foo4 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_4, rank_4, wn_cv, wn_fs) %>% 
  arrange(rank_4)
foo4

foo5 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_5, rank_5, wn_cv, wn_fs) %>% 
  arrange(rank_5)
foo5

foo6 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_6, rank_6, wn_cv, wn_fs) %>% 
  arrange(rank_6)
foo6

foo7 <- models_and_accu_1 %>% 
  dplyr::select(variables, lags, rmse_7, rank_7, wn_cv, wn_fs) %>% 
  arrange(rank_7)
foo7

