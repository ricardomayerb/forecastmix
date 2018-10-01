# source('./R/utils_av_ricardo.R')
source('./R/VAR_functions.R')

country_name <- "Brasil"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

country_data_ts <- get_raw_data_ts(country_name, excel_data_path)
# colnames(country_data_ts)
country_data_ts[is.nan(country_data_ts)] <- NA
country_data_ts[, "ibc"]


external_data_ts <- get_raw_external_data_ts(excel_data_path)
# colnames(external_data_ts)

data_ts <- country_data_ts
# data_ts <- ts.union(country_data_ts, external_data_ts)
# colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))

colnames(data_ts)

reco_all_variables <- find_statio_diffs(data_ts, country_name)

country_transformed_data <- follow_rec(data_ts, reco_all_variables)

VAR_data_for_estimation  <- country_transformed_data
variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

foo <- na.omit(VAR_data_for_estimation)
foo[, "ibc"]

saveRDS(VAR_data_for_estimation , 
        paste0("./analysis/VAR_output/VAR_data_",country_name,".rds"))

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
var_res_s2_aic_fpe_hq_sc_t2 <- search_var_one_size(var_size = 2,
                                       vec_lags = vec_lags_aic_fpe_hq_sc,
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
                                       max_rank = max_rank_some_h, 
                                       check_residuals_cv = TRUE,
                                       check_residuals_full_sample = TRUE, 
                                       restrict_by_signif = TRUE, 
                                       t_tresh = 2,
                                       max_p_for_estimation = 12)

toc()
models_and_accu_s2_aic_fpe_hq_sc_t2 <- var_res_s2_aic_fpe_hq_sc_t2[["accu_rankings_models"]]
cv_objects_s2_aic_fpe_hq_sc_t2 <- var_res_s2_aic_fpe_hq_sc_t2[["cv_objects"]]
saveRDS(models_and_accu_s2_aic_fpe_hq_sc_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s2_aic_fpe_hq_sc_t2.rds"))
saveRDS(cv_objects_s2_aic_fpe_hq_sc_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s2_aic_fpe_hq_sc_t2.rds"))


tic()
var_res_s2_123456_t2 <- search_var(vec_size = 2,
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
                               max_rank = max_rank_some_h, 
                               check_residuals_cv = TRUE,
                               check_residuals_full_sample = TRUE, 
                               restrict_by_signif = TRUE, 
                               t_tresh = 2,
                               max_p_for_estimation = 12)

toc()
models_and_accu_s2_123456_t2 <- var_res_s2_123456_t2[["accu_rankings_models"]]
cv_objects_s2_123456_t2 <- var_res_s2_123456_t2[["cv_objects"]]
saveRDS(models_and_accu_s2_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s2_123456_t2.rds"))
saveRDS(cv_objects_s2_123456_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s2_123456_t2.rds"))




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
                                        max_rank = max_rank_some_h, 
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
var_res_s3_123456_t2 <- search_var(vec_size = 3,
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
                               max_rank = max_rank_some_h, 
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
var_res_s4_6_t2 <- search_var(vec_size = 4,
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
                              max_rank = max_rank_some_h, 
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

tic()
var_res_s4_aic_fpe_hq_sc_t2 <- search_var(vec_size = 4,
                                       vec_lags = vec_lags_aic_fpe_hq_sc,
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
                                       max_rank = max_rank_some_h, 
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
models_and_accu_s4_aic_fpe_hq_sc_t2 %>% arrange(rmse_1, rmse_2, rmse_3, rmse_4)



tic()
var_res_s4_12345_t2 <- search_var(vec_size = 4,
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
                               max_rank = max_rank_some_h, 
                               check_residuals_cv = FALSE,
                               check_residuals_full_sample = TRUE, 
                               max_p_for_estimation = 12,
                               restrict_by_signif = TRUE, 
                               t_tresh = 2)

toc()

models_and_accu_s4_12345_t2 <- var_res_s4_12345_t2[["accu_rankings_models"]]
cv_objects_s4_12345_t2 <- var_res_s4_12345_t2[["cv_objects"]]
saveRDS(models_and_accu_s4_12345_t2, 
        paste0("./analysis/VAR_output/",country_name,"_s4_12345_t2.rds"))
saveRDS(cv_objects_s4_12345_t2, 
        paste0("./analysis/VAR_output/",country_name,"_cvobj_s4_12345_t2.rds"))


s2_123456 <- readRDS(file = "analysis/VAR_output/Brasil_s2_123456_t2.rds")
s2_ic <- readRDS(file = "analysis/VAR_output/Brasil_s2_aic_fpe_hq_sc_t2.rds")
s3_123456 <- readRDS(file = "analysis/VAR_output/Brasil_s3_123456_t2.rds")
s3_ic <- readRDS(file = "analysis/VAR_output/Brasil_s3_aic_fpe_hq_sc_t2.rds")
s4_12345 <- readRDS(file = "analysis/VAR_output/Brasil_s4_12345_t2.rds")
s4_6 <- readRDS(file = "analysis/VAR_output/Brasil_s4_6_t2.rds")
s4_ic <- readRDS(file = "analysis/VAR_output/Brasil_s4_aic_fpe_hq_sc_t2.rds")

all_models <- as_tibble(rbind(s2_123456, s2_ic, s3_123456, s3_ic, s4_12345, s4_6, s4_ic)) %>%
  dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
  mutate(lags = unlist(lags))

all_models <- all_models %>% 
  mutate(short_name = map2(variables, lags,
                           ~ make_model_name(variables = .x, lags = .y)),
         short_name = unlist(short_name),
         var_size = map_dbl(variables, length)
  )

all_models <- all_models %>% dplyr::distinct(short_name, .keep_all = TRUE)

all_models_ranked <- add_rmse_rankings(all_models)

all_models_ranked  <- all_models_ranked %>% 
  dplyr::select(short_name, var_size, lags, everything()) %>% 
  dplyr::select(everything(), -one_of(c("variables", "full_sample_varest")),
                one_of(c("variables", "full_sample_varest"))) 


wfoo <- variable_freq_by_n(all_models_ranked, h_max = 7, max_rank = 20, n_freq = 4, is_wide = TRUE)

wfoo


variable_freq_by_n <- function(tbl_of_models, h_max = 8, max_rank = 10, 
                               n_freq = 4, is_wide = FALSE) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if ("full_sample_varest" %in% names(tbl_of_models)) {
    tbl_of_models <-  tbl_of_models %>% 
      dplyr::select(-full_sample_varest)
  }
  
  if (is_wide) {
    tbl_of_models <- tbl_of_models %>% 
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse)) %>% 
      ungroup()
  }
  
  vec_of_rmse_h <- sort(unique(tbl_of_models$rmse_h))
  
  list_best <- map(vec_of_rmse_h, 
                   ~ tbl_of_models %>% 
                     filter(rmse_h == .x, rank_h < max_rank +1 ) %>% 
                     dplyr::select("variables") %>% 
                     unlist() %>% 
                     table() %>% 
                     as_tibble() %>% 
                     arrange(desc(n)) %>% 
                     rename(., vbl = .)
  ) 
  
  
  tbl_best <- reduce(list_best, left_join, by = c("vbl"))
  names(tbl_best) <- c("vbl", paste("h", seq(h_max), sep = "_"))
  
  tbl_best <- tbl_best %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE))
  
  by_h1 <- tbl_best %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_total <- tbl_best %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both <- unique(c(by_h1$vbl, by_total$vbl))
  
  list_best_lags <- map(vec_of_rmse_h, 
                        ~ tbl_of_models %>% 
                          filter(rmse_h == .x, rank_h < max_rank +1 ) %>% 
                          dplyr::select("lags") %>% 
                          unlist() %>% 
                          table() %>% 
                          as_tibble() %>% 
                          arrange(desc(n)) %>% 
                          rename(., max_lag = .)
  ) 
  
  
  tbl_best_lags <- reduce(list_best_lags, left_join, by = c("max_lag"))
  names(tbl_best_lags) <- c("max_lag", paste("h", seq(h_max), sep = "_"))
  
  tbl_best_lags <- tbl_best_lags %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE))
  
  by_h1_lags <- tbl_best_lags %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_total_lags <- tbl_best_lags %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both_lags <- unique(c(by_h1_lags$max_lag, by_total_lags$max_lag))
  
  return( list(freqs_by_h = tbl_best, top_h1_total = both,
               freqs_by_h_lags = tbl_best_lags, top_h1_total_lags = both_lags))
}



