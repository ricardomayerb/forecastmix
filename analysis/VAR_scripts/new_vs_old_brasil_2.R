source('./R/combinations_functions.R')

country <- "Brasil"
forecast_exercise_year <- 2018
forecast_exercise_number <- 3
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")
new_search_results_name <- paste0("vr_", country, "_auto_3s4_2s5_norest") 
new_search_results <- readRDS(paste0(output_path, new_search_results_name, ".rds"))

new_search_results_3t <- readRDS(paste0("./", country, "_quasi_ave.rds"))
new_search_results_3t_tbl <- new_search_results_3t[[4]][["cv_tbl"]]

  
  
old_search_results_name <- paste0(output_path, "from_older_code/", country,"_by_step_12345.rds")
old_search_results <- readRDS(old_search_results_name)

var_res_new <- new_search_results[["consolidated_var_res"]]
var_res_old <- old_search_results
newall_oldall <- read_compare_var_res(var_res_new, var_res_old)


var_res_new_3t <- new_search_results_3t_tbl %>% 
  dplyr::select(-c(model_type, inv_mse, model_weight_h, weighted_fc_h, 
                   target_mean_fc_yoy, short_name_t, var_size)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  spread(key = rmse_h, value = rmse) %>% 
  mutate(var_size = length(variables)) %>% 
  mutate(rank_1 = 0, rank_2 = 0, rank_3 = 0, rank_4 = 0, 
         rank_5 = 0, rank_6 = 0, rank_7 = 0, rank_8 = 0) %>% 
  rename(t_treshold = t_threshold)
new3t_oldall <- read_compare_var_res(var_res_new_3t, var_res_old)



print(newall_oldall$plot_best_each)
# print(new3t_oldall$plot_best_each)


print(newall_oldall$plot_best_consolidated)
print(new3t_oldall$plot_best_consolidated)



