source('./R/VAR_functions.R')

country <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

read_compare_var_res <- function(filename_new, filename_old, h_max = 7, 
                                 rank_h_max = 30) {
  
  
  if (is.character(filename_new)) {
    var_res_new <- readRDS(filename_new)
    var_res_old <- readRDS(filename_old)
  } else {
    var_res_new <- filename_new
    var_res_old <- filename_old
  }
  

  if ("f_vbls_all_sizes" %in% names(var_res_new)) {
    var_res_new <- var_res_new[["consolidated_var_res"]]
  }
  
  if ("f_vbls_all_sizes" %in% names(var_res_old)) {
    var_res_old <- var_res_old[["consolidated_var_res"]]
  }
  
  var_res_new <- var_res_new %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "new") %>% 
    dplyr::select(-t_treshold) %>% 
    dplyr::select(-lag_sel_method) %>% 
    dplyr::select(-rmse_8) %>% 
    dplyr::select(-rank_8)
  
  var_res_old <- var_res_old %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "old",
           var_size = map_dbl(variables, length)) %>% 
    dplyr::select(-rmse_8) %>% 
    dplyr::select(-rank_8)
  
  old_and_new <- stack_models(list(var_res_new, var_res_old))
  
  plot_best_consolidated <- single_plot_rmse_all_h(old_and_new, is_wide = TRUE, 
                                        h_max = h_max, rank_h_max = rank_h_max)
  
  plot_best_each <- each_plot_rmse_all_h(selected_one = var_res_new,
                                         selected_two = var_res_old,
                                         is_wide = TRUE, 
                                         h_max = h_max,
                                         rank_h_max = rank_h_max)
  
  size4_vbls_new <-  var_res_new %>% 
    filter(var_size == 4) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique()
  
  size4_vbls_old <-  var_res_old %>% 
    filter(var_size == 4) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique()
  
  size5_vbls_new <-  var_res_new %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique()
  
  size5_vbls_old <-  var_res_old %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique()
  
  print("Size 4 VARs: variables in new that are not in old:")
  print(size4_vbls_new[!size4_vbls_new %in% size4_vbls_old])
  
  print("Size 4 VARs: variables in old that are not in new:")
  print(size4_vbls_old[!size4_vbls_old %in% size4_vbls_new])
  

  
  print("Size 5 VARs: variables in new that are not in old:")
  print(size5_vbls_new[!size5_vbls_new %in% size5_vbls_old])
  
  print("Size 5 VARs: variables in old that are not in new:")
  print(size5_vbls_old[!size5_vbls_old %in% size5_vbls_new])
  
  return(list(size4_vbls_new = size4_vbls_new, size4_vbls_old = size4_vbls_old,
              size5_vbls_new = size5_vbls_new, size5_vbls_old = size5_vbls_old,
              var_res_old_and_new = old_and_new, var_res_new = var_res_new,
              var_res_old = var_res_old, 
              plot_best_consolidated  = plot_best_consolidated,
              plot_best_each = plot_best_each))
  
} 


chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"
chl_old <- readRDS(chl_filename_old)

chl_2_2_165_partial_filename_new <- "Chile_two4_two5.rds"
chl_2_2_165_filename_new <- paste0(output_path, chl_2_2_165_partial_filename_new)
chl_2_2_165 <- read_compare_var_res(chl_2_2_165_filename_new, chl_filename_old)

chl_1_2_165_partial_filename_new <- "Chile_one4_two5.rds"
chl_1_2_165_filename_new <- paste0(output_path, chl_1_2_165_partial_filename_new)
chl_1_2_165 <- read_compare_var_res(chl_1_2_165_filename_new, chl_filename_old)

chl_1_3_165_partial_filename_new <- "Chile_one4_three5.rds"
chl_1_3_165_filename_new <- paste0(output_path, chl_1_3_165_partial_filename_new)
chl_1_3_165 <- read_compare_var_res(chl_1_3_165_filename_new, chl_filename_old)

chl_1_3_0_partial_filename_new <- "Chile_one4_three5_t0.rds"
chl_1_3_0_filename_new <- paste0(output_path, chl_1_3_0_partial_filename_new)
chl_1_3_0 <- read_compare_var_res(chl_1_3_0_filename_new, chl_filename_old)

chl_manual1_partial_filename_new <- "Chile_manual1.rds"
chl_manual1_filename_new <- paste0(output_path, chl_manual1_partial_filename_new)
chl_manual1 <- read_compare_var_res(chl_manual1_filename_new, chl_filename_old)


chl_2_2_rds <- readRDS(chl_2_2_165_filename_new)
chl_2_2_mr <- chl_2_2_rds$consolidated_var_res

chl_1_2_rds <- readRDS(chl_1_2_165_filename_new)
chl_1_2_mr <- chl_1_2_rds$consolidated_var_res

chl_1_3_rds <- readRDS(chl_1_3_165_filename_new)
chl_1_3_mr <- chl_1_3_rds$consolidated_var_res

chl_1_3_0_rds <- readRDS(chl_1_3_0_filename_new)
chl_1_3_0_mr <- chl_1_3_0_rds$consolidated_var_res

chl_manual1_rds <- readRDS(chl_manual1_filename_new)
chl_manual1_mr <- chl_manual1_rds$consolidated_var_res

# best_old_h7 <- chl_old %>% filter(rank_7 == 1)
# sort(best_old_h7$variables[[1]])
# best_old_h7$lags[[1]]
# best_old_h7
# 
# this_short_name <- "copper_output___imp___imp_capital___ipec___rgdp__3"
# 
# manualfoo <- chl_manual1_mr %>% mutate(short_name = unlist(short_name))
# 
# manual_same_short <- manualfoo %>% filter(short_name == this_short_name)
# manual_same_short$t_treshold
# 
# 
# 
# 
# max_VAR_models_per_h <- 100
# models_and_accu_reasonable <- as.tibble(models_and_accu) %>% 
#   filter(rank_1 <= max_VAR_models_per_h | rank_2 <= max_VAR_models_per_h | 
#            rank_3 <= max_VAR_models_per_h | rank_4 <= max_VAR_models_per_h |
#            rank_5 <= max_VAR_models_per_h | rank_6 <= max_VAR_models_per_h | 
#            rank_7 <= max_VAR_models_per_h | rank_8 <= max_VAR_models_per_h) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # best old model rmse 7
# selected_variables <- c("rgdp", "ipec", "imp", "imp_capital", "copper_output")	
# selected_lags <- 3
# # rmse_1 0.002612981 
# VAR_data_for_estimation <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/VAR_data_Chile.rds")
# var_data <- na.omit(VAR_data_for_estimation)
# var_data <- var_data[, selected_variables]
# 
# 
# fit <- vars::VAR(y = var_data, p = selected_lags, type = "const")
# all(roots(fit) < 1)
# vars::serial.test(fit)
# vars::serial.test(fit, lags.pt = 12)
# vars::serial.test(fit, lags.pt = 8)
# 
# fr0  <- vars::restrict(fit, method = "ser", thresh = 0)
# fr2 <- vars::restrict(fit, method = "ser", thresh = 2)
# fr165 <- vars::restrict(fit, method = "ser", thresh = 1.65)
# 
# cv_r0 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
#                 h_max = 8, training_length = 32, full_sample_resmat = fr0$restrictions)
# 
# cv_r2 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
#                 h_max = 8, training_length = 32, full_sample_resmat = fr2$restrictions)
# 
# cv_r165 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
#                   h_max = 8, training_length = 32, full_sample_resmat = fr165$restrictions)
# 
# results_r0 <- cv_r0
# column_names <- names(results_r0)
# results_r0 <- as_tibble(results_r0)
# names(results_r0) <- column_names
# cv_errors0 <- results_r0[["cv_errors"]]
# mat_error0 <- reduce(cv_errors0, rbind)
# mse_all_h0 <- colMeans(mat_error0^2)
# rmse_all_h0 <- sqrt(mse_all_h0)
# rmse_all_h0[7]
# 
# results_r2 <- cv_r2
# results_r2 <- as_tibble(results_r2)
# names(results_r2) <- column_names
# cv_errors2 <- results_r2[["cv_errors"]]
# mat_error2 <- reduce(cv_errors2, rbind)
# mse_all_h2 <- colMeans(mat_error2^2)
# rmse_all_h2 <- sqrt(mse_all_h2)
# rmse_all_h2[7]
# 
# results_r165 <- cv_r165
# results_r165 <- as_tibble(results_r165)
# names(results_r165) <- column_names
# cv_errors165 <- results_r165[["cv_errors"]]
# mat_error165 <- reduce(cv_errors165, rbind)
# mse_all_h165 <- colMeans(mat_error165^2)
# rmse_all_h165 <- sqrt(mse_all_h165)
# rmse_all_h165[7]
# 
# rmse_all_h0
# rmse_all_h165
# rmse_all_h2
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# chl_2_2_165_plot <- chl_2_2_165$plot_best_consolidated + ggtitle("Chile_2_2_165")
# print(chl_2_2_165_plot)
# 
# chl_1_2_165_plot <- chl_1_2_165$plot_best_consolidated + ggtitle("Chile_1_2_165")
# print(chl_1_2_165_plot)
# 
# chl_1_3_165_plot <- chl_1_3_165$plot_best_consolidated + ggtitle("Chile_1_3_165")
# print(chl_1_3_165_plot)
# 
# chl_1_3_0_plot <- chl_1_3_0$plot_best_consolidated + ggtitle("Chile_1_3_0")
# print(chl_1_3_0_plot)
# 
# chl_manual1_plot <- chl_manual1$plot_best_consolidated + ggtitle("Chile_manual1")
# print(chl_manual1_plot)
# 
# 
# 
# 
# chl_2_2_165_plot2 <- chl_2_2_165$plot_best_each + ggtitle("Chile_2_2_165")
# print(chl_2_2_165_plot2)
# 
# chl_1_2_165_plot2 <- chl_1_2_165$plot_best_each + ggtitle("Chile_1_2_165")
# print(chl_1_2_165_plot2)
# 
# chl_1_3_165_plot2 <- chl_1_3_165$plot_best_each + ggtitle("Chile_1_3_165")
# print(chl_1_3_165_plot2)
# 
# chl_1_3_0_plot2 <- chl_1_3_0$plot_best_each + ggtitle("Chile_1_3_0")
# print(chl_1_3_0_plot2)
# 
# chl_manual1_plot2 <- chl_manual1$plot_best_each + ggtitle("Chile_manual1")
# print(chl_manual1_plot2)
# 
# 
# 
# # > foobest7
# # # A tibble: 1 x 19
# # variables lags  rmse_1 rmse_2 rmse_3 rmse_4 rmse_5 rmse_6  rmse_7 rank_1 rank_2 rank_3 rank_4 rank_5 rank_6 rank_7 short_name
# # <list>    <lis>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <chr>     
# #   1 <chr [5]> <dbl~ 0.0191 0.0325 0.0357 0.0351 0.0286 0.0158 0.00380   2153   2169   2165   2150   2060    820      1 copper_ou~
# 
# 
# 
