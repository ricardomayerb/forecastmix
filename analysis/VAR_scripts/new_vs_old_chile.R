source('./R/VAR_functions.R')

country <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

read_compare_var_res <- function(filename_new, filename_old, h_max = 7, 
                                 rank_h_max = 30) {
  
  var_res_new <- readRDS(filename_new)
  var_res_old <- readRDS(filename_old)
  

  if ("f_vbls_all_sizes" %in% names(var_res_new)) {
    var_res_new <- var_res_new[["consolidated_var_res"]]
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



# chl_partial_filename_new <- "var_results_Chile_sizes_2345_fqlims_nonenone1510_t_2222.rds"
# chl_filename_new <- paste0(output_path, chl_partial_filename_new)
chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"
# chl <- read_compare_var_res(chl_filename_new, chl_filename_old)

# chl2_partial_filename_new <- "var_results_Chile_sizes_2345_fqlims_nonenone1510_t_2222_mr50_mrfq30.rds"
# chl2_filename_new <- paste0(output_path, chl2_partial_filename_new)
# chl2 <- read_compare_var_res(chl2_filename_new, chl_filename_old)
# 
# chl3_partial_filename_new <- "var_results3_Chile_sizes_2345_fqlims_nonenone1510_t_2222_mr50_mrfq30.rds"
# chl3_filename_new <- paste0(output_path, chl3_partial_filename_new)
# chl3 <- read_compare_var_res(chl3_filename_new, chl_filename_old)
# 
# chl4_partial_filename_new <- "var_results4_Chile_sizes_2345_fqlims_nonenone1515_t_2222_mr50_mrfq30.rds"
# chl4_filename_new <- paste0(output_path, chl4_partial_filename_new)
# chl4 <- read_compare_var_res(chl4_filename_new, chl_filename_old)
# 
# chl5_partial_filename_new <- "var_results4_Chile_sizes_2345_fqlims_nonenone2015_t_2222_mr50_mrfq50.rds"
# chl5_filename_new <- paste0(output_path, chl5_partial_filename_new)
# chl5 <- read_compare_var_res(chl5_filename_new, chl_filename_old)

chl6_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t165165165165_mr50_mrfq50_cv8_tspan25_h8.rds"
chl6_filename_new <- paste0(output_path, chl6_partial_filename_new)
chl6 <- read_compare_var_res(chl6_filename_new, chl_filename_old)

chl7_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t0000_mr50_mrfq50_cv8_tspan25_h8.rds"
chl7_filename_new <- paste0(output_path, chl7_partial_filename_new)
chl7 <- read_compare_var_res(chl7_filename_new, chl_filename_old)

chl8_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t2222_mr50_mrfq50_cv8_tspan25_h8.rds"
chl8_filename_new <- paste0(output_path, chl8_partial_filename_new)
chl8 <- read_compare_var_res(chl8_filename_new, chl_filename_old)

# chl_rds <- readRDS(chl_filename_old)
# chl_mr <- chl_rds

# chl5_rds <- readRDS(chl5_filename_new)
# chl5_mr <- chl5_rds$consolidated_var_res

chl6_rds <- readRDS(chl6_filename_new)
chl6_mr <- chl6_rds$consolidated_var_res

chl7_rds <- readRDS(chl7_filename_new)
chl7_mr <- chl7_rds$consolidated_var_res

chl8_rds <- readRDS(chl8_filename_new)
chl8_mr <- chl8_rds$consolidated_var_res

# chl_plot <- chl$plot_best_consolidated + ggtitle("Chile")
# print(chl_plot)

# chl2_plot <- chl2$plot_best_consolidated + ggtitle("Chile2")
# print(chl2_plot)
# 
# chl3_plot <- chl3$plot_best_consolidated + ggtitle("Chile3")
# print(chl3_plot)
# 
# chl4_plot <- chl4$plot_best_consolidated + ggtitle("Chile4")
# print(chl4_plot)
# 

# chl5_plot <- chl5$plot_best_consolidated + ggtitle("Chile5")
# print(chl5_plot)

chl6_plot <- chl6$plot_best_consolidated + ggtitle("Chile6")
print(chl6_plot)

chl7_plot <- chl7$plot_best_consolidated + ggtitle("Chile7")
print(chl7_plot)

chl8_plot <- chl8$plot_best_consolidated + ggtitle("Chile7")
print(chl8_plot)



# chl_plot2 <- chl$plot_best_each + ggtitle("Chile")
# print(chl_plot2)
# 
# chl2_plot2 <- chl2$plot_best_each + ggtitle("Chile2")
# print(chl2_plot2)
# 
# chl3_plot2 <- chl3$plot_best_each + ggtitle("Chile3")
# print(chl3_plot2)
# 
# chl4_plot2 <- chl4$plot_best_each + ggtitle("Chile4")
# print(chl4_plot2)

# chl5_plot2 <- chl5$plot_best_each + ggtitle("Chile5")
# print(chl5_plot2)

chl6_plot2 <- chl6$plot_best_each + ggtitle("Chile6")
print(chl6_plot2)

chl7_plot2 <- chl7$plot_best_each + ggtitle("Chile7")
print(chl7_plot2)

chl8_plot2 <- chl8$plot_best_each + ggtitle("Chile7")
print(chl8_plot2)
