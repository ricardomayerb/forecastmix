source('./R/VAR_functions.R')

forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")
chl2_partial_filename_new <- "var_results_Chile_sizes_2345_fqlims_nonenone1510_t_2222_mr50_mrfq30.rds"
chl2_filename_new <- paste0(output_path, chl2_partial_filename_new)
chl2 <- readRDS(chl2_filename_new)

chl3_partial_filename_new <- "var_results3_Chile_sizes_2345_fqlims_nonenone1510_t_2222_mr50_mrfq30.rds"
chl3_filename_new <- paste0(output_path, chl3_partial_filename_new)
chl3 <- readRDS(chl3_filename_new)


selected_vbls <- chl2$selected_for_next_size
freq_info <- chl2$f_vbls_all_sizes
all_vbls <- selected_vbls[[1]]
sel_for_4 <- selected_vbls[[2]]
sel_for_5 <- selected_vbls[[3]]

sel_for_4_by_total <- freq_info[[2]][["vbl_by_total"]]
sel_for_5_by_total <- freq_info[[3]][["vbl_by_total"]]


print(all_vbls)
print(sel_for_4)
print(sel_for_5)
f_after_s2 <- freq_info[[1]]
f_after_s3 <- freq_info[[2]]
f_after_s4 <- freq_info[[3]]
f_after_s5 <- freq_info[[4]]

selected_vbls3 <- chl3$selected_for_next_size
freq_info3 <- chl3$f_vbls_all_sizes
all_vbls3 <- selected_vbls3[[1]]
sel_for_43 <- selected_vbls3[[2]]
sel_for_53 <- selected_vbls3[[3]]

sel_for_43_by_total <- freq_info3[[2]][["vbl_by_total"]]
sel_for_53_by_total <- freq_info3[[3]][["vbl_by_total"]]

print(all_vbls3)
print(sel_for_43)
print(sel_for_53)
f_after_s23 <- freq_info3[[1]]
f_after_s33 <- freq_info3[[2]]
f_after_s43 <- freq_info3[[3]]
f_after_s53 <- freq_info3[[4]]




print(sort(sel_for_4_by_total))
print(sort(sel_for_5_by_total))


