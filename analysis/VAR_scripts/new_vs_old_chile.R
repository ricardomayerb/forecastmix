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

chl2_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t2222_mr50_mrfq50_cv8_tspan39_h8.rds"
chl2_filename_new <- paste0(output_path, chl2_partial_filename_new)
chl2 <- read_compare_var_res(chl2_filename_new, chl_filename_old)

chl0_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t0000_mr50_mrfq50_cv8_tspan32_h8.rds"
chl0_filename_new <- paste0(output_path, chl0_partial_filename_new)
chl0 <- read_compare_var_res(chl0_filename_new, chl_filename_old)

chl165_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t165165165165_mr50_mrfq50_cv8_tspan32_h8.rds"
chl165_filename_new <- paste0(output_path, chl165_partial_filename_new)
chl165 <- read_compare_var_res(chl165_filename_new, chl_filename_old)

chl2_rds <- readRDS(chl2_filename_new)
chl2_mr <- chl2_rds$consolidated_var_res

chl0_rds <- readRDS(chl0_filename_new)
chl0_mr <- chl0_rds$consolidated_var_res

chl165_rds <- readRDS(chl165_filename_new)
chl165_mr <- chl165_rds$consolidated_var_res

chl2_165_mr <- rbind(chl2_mr , chl165_mr ) 
chl2165 <- read_compare_var_res(chl2_165_mr, chl_old)

chl2_plot <- chl2$plot_best_consolidated + ggtitle("Chile2")
print(chl2_plot)

chl0_plot <- chl0$plot_best_consolidated + ggtitle("Chile0")
print(chl0_plot)

chl165_plot <- chl165$plot_best_consolidated + ggtitle("Chile165")
print(chl165_plot)

chl2165_plot <- chl2165$plot_best_consolidated + ggtitle("Chile2165")
print(chl2165_plot)



chl2_plot2 <- chl2$plot_best_each + ggtitle("Chile2")
print(chl2_plot2)

chl0_plot2 <- chl0$plot_best_each + ggtitle("Chile0")
print(chl0_plot2)

chl165_plot2 <- chl165$plot_best_each + ggtitle("Chile165")
print(chl165_plot2)

chl2165_plot2 <- chl2165$plot_best_each + ggtitle("Chile2165")
print(chl2165_plot2)
