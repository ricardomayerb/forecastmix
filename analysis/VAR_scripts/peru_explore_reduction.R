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

# VAR_data_for_estimation  <- na.omit(country_transformed_data)
VAR_data_for_estimation  <- country_transformed_data


variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)
clean_rgdp <- na.omit(VAR_data_for_estimation[, "rgdp"])
start_rgdp <- start(clean_rgdp) 
end_rgdp <- end(clean_rgdp)
nobs_rgdp <- length(clean_rgdp)
VAR_data_rgdp_period <- window(VAR_data_for_estimation, start = start_rgdp,
                               end = end_rgdp)
first_half_row <-  floor(nrow(VAR_data_rgdp_period)/2)
second_half <- nrow(VAR_data_rgdp_period) - first_half_row

saveRDS(VAR_data_for_estimation , 
        paste0("./analysis/VAR_output/VAR_data_",country_name,".rds"))

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
print(rgdp_rec)

n_best <- 50
number_of_cv <- 8
fc_horizon <- 7
# fc_horizon is set to 7 because 8 is too long for peru
train_span <- 24
obs_used_in_cv <-  train_span + number_of_cv + fc_horizon
obs_to_spare <- nobs_rgdp - obs_used_in_cv

print(paste("Obs. used in cv:", obs_used_in_cv))
print(paste("(transformed) rgdp series has", nobs_rgdp, "observations"))
print(paste("Extra rgdp observations available:", obs_to_spare))
print(paste("Or, equiv., using any variable together with rgdp that forces us to loose more than", obs_to_spare,"observations, will result in an error when using fixed-length training window"))
print(paste("Worst case (shortest) n-combination --with rgdp-- reduce observations to", nrow(na.omit(VAR_data_for_estimation))))

if (train_span + fc_horizon + number_of_cv > nrow(na.omit(VAR_data_for_estimation))) {
  
  print("not enough obs for at least some combination")
  
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
vec_lags_123456 <- c(1,2,3,4,5,6)


tic()
var_res_s2_aic_fpe_hq_sc_t2  <- search_var_one_size(var_size = 2,
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
var_res_s2_123456_t2 <- search_var_one_size(var_size = 2,
                                  vec_lags = vec_lags_123456,
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
models_and_accu_s2_123456_t2 <- var_res_s2_123456_t2[["accu_rankings_models"]]
cv_objects_s2_123456_t2 <- var_res_s2_123456_t2[["cv_objects"]]
saveRDS(models_and_accu_s2_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s2_123456_t2.rds"))
saveRDS(cv_objects_s2_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s2_123456_t2.rds"))



tic()
var_res_s3_123456_t2 <- search_var_one_size(var_size = 3,
                                  vec_lags = vec_lags_123456,
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
models_and_accu_s3_123456_t2 <- var_res_s3_123456_t2[["accu_rankings_models"]]
cv_objects_s3_123456_t2 <- var_res_s3_123456_t2[["cv_objects"]]
saveRDS(models_and_accu_s3_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s3_123456_t2.rds"))
saveRDS(cv_objects_s3_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s3_123456_t2.rds"))





tic()
var_res_s3_aic_fpe_hq_sc_t2  <- search_var_one_size(var_size = 3,
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



tic()
var_res_s4_aic_fpe_hq_sc_t2  <- search_var_one_size(var_size = 4,
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
models_and_accu_s4_aic_fpe_hq_sc_t2 <- var_res_s4_aic_fpe_hq_sc_t2[["accu_rankings_models"]]
cv_objects_s4_aic_fpe_hq_sc_t2 <- var_res_s4_aic_fpe_hq_sc_t2[["cv_objects"]]
saveRDS(models_and_accu_s4_aic_fpe_hq_sc_t2,
        paste0("./analysis/VAR_output/",country_name,"_s4_aic_fpe_hq_sc_t2.rds"))
saveRDS(cv_objects_s4_aic_fpe_hq_sc_t2,
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s4_aic_fpe_hq_sc_t2.rds"))


tic()
var_res_s4_12345_t2  <- search_var_one_size(var_size = 4,
                                   vec_lags = vec_lags_12345 ,
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
                                   t_tresh = 2,
                                   keep_varest = TRUE)
toc()
models_and_accu_s4_12345_t2 <- var_res_s4_12345_t2[["accu_rankings_models"]]
cv_objects_s4_12345_t2 <- var_res_s4_12345_t2[["cv_objects"]]
saveRDS(models_and_accu_s4_12345_t2,
        paste0("./analysis/VAR_output/",country_name,"_s4_12345_t2.rds"))
saveRDS(cv_objects_s4_12345_t2,
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s4_12345_t2.rds"))



tic()
var_res_s4_6_t2  <- search_var_one_size(var_size = 4,
                                   vec_lags = c(6),
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
models_and_accu_s4_6_t2 <- var_res_s4_6_t2[["accu_rankings_models"]]
cv_objects_s4_6_t2 <- var_res_s4_6_t2[["cv_objects"]]
saveRDS(models_and_accu_s4_6_t2,
        paste0("./analysis/VAR_output/",country_name,"_s4_6_t2.rds"))
saveRDS(cv_objects_s4_6_t2,
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s4_6_t2.rds"))





# v2 <- c("rgdp", "tcr")
# p <- 8
# var1 <- vars::VAR(na.omit(VAR_data_for_estimation[,v2]), p = p, type = "const")
# var1
# var1r <- vars::restrict(var1, method = "ser", thresh = 2)
# var1r
# 
# var1cv <- vars::VAR(na.omit(VAR_data_for_estimation[,v2]), p = p, type = "const")
# var1rr <- vars::restrict(var1cv, method = "manual", resmat = var1r$restrictions)
# var1rr
# 
# foo <- names(var1r$varresult$rgdp$model)
# str_detect(foo, ".l")
# moo <- foo[ str_detect(foo, ".l")]
# doo <- c("rgdp.l1", "rgdp.l43", "rgdp.l5", "tcr.l7") 
# moo_num <-  as.numeric(map_chr(str_extract_all(moo, "\\d"), ~ paste(.x, collapse = "")))
# doo_num <-  as.numeric(map_chr(str_extract_all(doo, "\\d"), ~ paste(.x, collapse = "")))
# 
# moo_num
# doo_num
# 
# max(moo_num)
# max(doo_num)
# 
# vres <- var1r$restrictions
# vres
# vres[,1:(ncol(vres)-1)]
# colSums(vres[,1:(ncol(vres)-1)])
# hoo <- colSums(vres[,1:ncol(vres)])
# yoo <- names(hoo[hoo > 0])
# yoo_num <-  as.numeric(map_chr(str_extract_all(yoo, "\\d"), ~ paste(.x, collapse = "")))
# yoo_num
# max(yoo_num, na.rm = TRUE)
# 
# foo2 <- names(var1r$varresult$tcr$model)
# moo2 <- foo2[ str_detect(foo2, ".l")]
# moo2
# moo2_num <-  as.numeric(map_chr(str_extract_all(moo2, "\\d"), ~ paste(.x, collapse = "")))
# moo2_num
# max(moo2_num)
# 
# 
# max_effective_lag(var1r)
# max_effective_lag(var1)
