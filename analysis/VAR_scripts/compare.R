source('./R/combinations_functions.R')
library(openxlsx)

rmse_names <- paste0("rmse_", 1:8)
exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")

new_result <- readRDS("./data/forecast_models/all_brasil_models_new_data_all_variables_restricted_combos_t165_lag_4.rds")
old_result <- readRDS("./data/forecast_models/oc_all_brasil_models_new_data_all_variables_restricted_combos_t165_lag_4.rds")


pm2_new <- filter(new_result$all_passing_models_2345, size == 2) %>% arrange(short_name)
pm2_old <- filter(old_result$all_passing_models_2345, size == 2) %>% arrange(short_name)
identical(pm2_old$short_name, pm2_new$short_name)
rmse2_old <- dplyr::select(pm2_old, c(short_name, rmse_names))
rmse2_new <- dplyr::select(pm2_new, c(short_name, rmse_names))
rmse2_new[, rmse_names]-rmse2_old[, rmse_names]
colSums(rmse2_new[, rmse_names]-rmse2_old[, rmse_names])

pm3_new <- filter(new_result$all_passing_models_2345, size == 3) %>% arrange(short_name)
pm3_old <- filter(old_result$all_passing_models_2345, size == 3) %>% arrange(short_name)
identical(pm3_old$short_name, pm3_new$short_name)
rmse3_old <- dplyr::select(pm3_old, c(short_name, rmse_names))
rmse3_new <- dplyr::select(pm3_new, c(short_name, rmse_names))
rmse3_new[, rmse_names]-rmse3_old[, rmse_names]
colSums(rmse3_new[, rmse_names]-rmse3_old[, rmse_names])
# rowSums(rmse3_new[, rmse_names]-rmse3_old[, rmse_names])
discrepant_rows3 <- rowSums(rmse3_new[, rmse_names]-rmse3_old[, rmse_names]) != 0
congruent_rows3 <-  rowSums(rmse3_new[, rmse_names]-rmse3_old[, rmse_names]) == 0

discrepant_s3 <- pm3_old[discrepant_rows3, ]
congruent_s3 <- pm3_old[congruent_rows3, ]

exo_in_dis <- discrepant_s3 %>% transmute(exo_present = map_lgl(variables, ~ any(exogenous_variables %in% .))) 
mean(exo_in_dis$exo_present)
exo_in_con <- congruent_s3 %>% transmute(exo_present = map_lgl(variables, ~ any(exogenous_variables %in% .))) 
mean(exo_in_con$exo_present)



# pm4_new <- filter(new_result$all_passing_models_2345, size == 4) %>% arrange(short_name)
# pm4_old <- filter(old_result$all_passing_models_2345, size == 4) %>% arrange(short_name)
# identical(pm4_old$short_name, pm4_new$short_name)
# rmse4_old <- dplyr::select(pm4_old, c(short_name, rmse_names))
# rmse4_new <- dplyr::select(pm4_new, c(short_name, rmse_names))
# # rmse4_new[, rmse_names]-rmse4_old[, rmse_names]
# # colSums(rmse4_new[, rmse_names]-rmse4_old[, rmse_names])





cvs2_new <- new_result$cv_size_2
cvs2_old <- old_result$cv_size_2


