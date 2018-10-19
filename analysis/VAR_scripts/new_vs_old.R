source('./R/VAR_functions.R')

country <- "Argentina"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

read_compare_var_res <- function(filename_new, filename_old, h_max = 7, 
                                 rank_h_max = 30) {
  
  var_res_new <- readRDS(filename_new)
  var_res_old <- readRDS(filename_old)
  
  var_res_new <- var_res_new %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "new") %>% 
    dplyr::select(-t_treshold) %>% 
    dplyr::select(-lag_sel_method)
  
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


arg_partial_filename_new <- "var_results_Argentina_sizes_2345_fqlims_nonenone1510_t_2222.rds"
arg_filename_new <- paste0(output_path, arg_partial_filename_new)
arg_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Argentina_by_step_12345.rds"
arg <- read_compare_var_res(arg_filename_new, arg_filename_old)

bol_partial_filename_new <- "var_results_Bolivia_sizes_2345_fqlims_nonenone1510_t_2222.rds"
bol_filename_new <- paste0(output_path, bol_partial_filename_new)
bol_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Bolivia_by_step_12345.rds"
bol <- read_compare_var_res(bol_filename_new, bol_filename_old)

bra_partial_filename_new <- "var_results_Brasil_sizes_2345_fqlims_nonenone1510_t_2222.rds"
bra_filename_new <- paste0(output_path, bra_partial_filename_new)
bra_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Brasil_by_step_12345.rds"
bra <- read_compare_var_res(bra_filename_new, bra_filename_old)

chl_partial_filename_new <- "var_results_Chile_sizes_2345_fqlims_nonenone1510_t_2222.rds"
chl_filename_new <- paste0(output_path, chl_partial_filename_new)
chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"
chl <- read_compare_var_res(chl_filename_new, chl_filename_old)

col_partial_filename_new <- "var_results_Colombia_sizes_2345_fqlims_nonenone1510_t_2222.rds"
col_filename_new <- paste0(output_path, col_partial_filename_new)
col_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Colombia_by_step_12345.rds"
col <- read_compare_var_res(col_filename_new, col_filename_old)

ecu_partial_filename_new <- "var_results_Ecuador_sizes_2345_fqlims_nonenone1510_t_2222.rds"
ecu_filename_new <- paste0(output_path, ecu_partial_filename_new)
ecu_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Ecuador_by_step_12345.rds"
ecu <- read_compare_var_res(ecu_filename_new, ecu_filename_old)

par_partial_filename_new <- "var_results_Paraguay_sizes_2345_fqlims_nonenone1510_t_2222.rds"
par_filename_new <- paste0(output_path, par_partial_filename_new)
par_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Paraguay_by_step_12345.rds"
par <- read_compare_var_res(par_filename_new, par_filename_old)

per_partial_filename_new <- "var_results_Peru_sizes_2345_fqlims_nonenone1510_t_2222.rds"
per_filename_new <- paste0(output_path, per_partial_filename_new)
per_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Peru_by_step_12345.rds"
per <- read_compare_var_res(per_filename_new, per_filename_old)

ury_partial_filename_new <- "var_results_Uruguay_sizes_2345_fqlims_nonenone1510_t_2222.rds"
ury_filename_new <- paste0(output_path, ury_partial_filename_new)
ury_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Uruguay_by_step_12345.rds"
ury <- read_compare_var_res(ury_filename_new, ury_filename_old)


arg_plot <- arg$plot_best_consolidated + ggtitle("Argentina")
print(arg_plot)

bra_plot <- bra$plot_best_consolidated + ggtitle("Brasil")
print(bra_plot)

bol_plot <- bol$plot_best_consolidated + ggtitle("Bolivia")
print(bol_plot)

chl_plot <- chl$plot_best_consolidated + ggtitle("Chile")
print(chl_plot)

col_plot <- col$plot_best_consolidated + ggtitle("Colombia")
print(col_plot)

ecu_plot <- ecu$plot_best_consolidated + ggtitle("Ecuador")
print(ecu_plot)

par_plot <- par$plot_best_consolidated + ggtitle("Paraguay")
print(par_plot)

per_plot <- per$plot_best_consolidated + ggtitle("Peru")
print(per_plot)

ury_plot <- ury$plot_best_consolidated + ggtitle("Uruguay")
print(ury_plot)





arg_plot2 <- arg$plot_best_each+ ggtitle("Argentina")
print(arg_plot2)

bra_plot2 <- bra$plot_best_each+ ggtitle("Brasil")
print(bra_plot2)

bol_plot2 <- bol$plot_best_each + ggtitle("Bolivia")
print(bol_plot2)

chl_plot2 <- chl$plot_best_each + ggtitle("Chile")
print(chl_plot2)

col_plot2 <- col$plot_best_each + ggtitle("Colombia")
print(col_plot2)

ecu_plot2 <- ecu$plot_best_each + ggtitle("Ecuador")
print(ecu_plot2)

par_plot2 <- par$plot_best_each + ggtitle("Paraguay")
print(par_plot2)

per_plot2 <- per$plot_best_each + ggtitle("Peru")
print(per_plot2)

ury_plot2 <- ury$plot_best_each + ggtitle("Uruguay")
print(ury_plot2)

