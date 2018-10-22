source('./R/VAR_functions.R')

forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")
chl0_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t0000_mr50_mrfq50_cv8_tspan25_h8.rds"
chl0_filename_new <- paste0(output_path, chl0_partial_filename_new)
chl0 <- readRDS(chl0_filename_new)

chl2_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t2222_mr50_mrfq50_cv8_tspan25_h8.rds"
chl2_filename_new <- paste0(output_path, chl2_partial_filename_new)
chl2 <- readRDS(chl2_filename_new)

chl165_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t165165165165_mr50_mrfq50_cv8_tspan25_h8.rds"
chl165_filename_new <- paste0(output_path, chl165_partial_filename_new)
chl165 <- readRDS(chl165_filename_new)


selected_vbls_2 <- chl2$selected_for_next_size
freq_info_2 <- chl2$f_vbls_all_sizes
all_vbls_2 <- selected_vbls_2[[1]]
sel_for_4_2 <- selected_vbls_2[[2]]
sel_for_5_2 <- selected_vbls_2[[3]]

sel_for_4_by_total_2 <- freq_info_2[[2]][["vbl_by_total"]]
sel_for_5_by_total_2 <- freq_info_2[[3]][["vbl_by_total"]]

print(all_vbls_2)
print(sel_for_4_2)
print(sel_for_5_2)
f_after_s2_2 <- freq_info_2[[1]]
f_after_s3_2 <- freq_info_2[[2]]
f_after_s4_2 <- freq_info_2[[3]]
f_after_s5_2 <- freq_info_2[[4]]





selected_vbls_0 <- chl0$selected_for_next_size
freq_info_0 <- chl0$f_vbls_all_sizes
all_vbls_0 <- selected_vbls_0[[1]]
sel_for_4_0 <- selected_vbls_0[[2]]
sel_for_5_0 <- selected_vbls_0[[3]]

sel_for_4_by_total_0 <- freq_info_0[[2]][["vbl_by_total"]]
sel_for_5_by_total_0 <- freq_info_0[[3]][["vbl_by_total"]]

print(all_vbls_0)
print(sel_for_4_0)
print(sel_for_5_0)
f_after_s2_0 <- freq_info_0[[1]]
f_after_s3_0 <- freq_info_0[[2]]
f_after_s4_0 <- freq_info_0[[3]]
f_after_s5_0 <- freq_info_0[[4]]





selected_vbls_165 <- chl165$selected_for_next_size
freq_info_165 <- chl165$f_vbls_all_sizes
all_vbls_165 <- selected_vbls_165[[1]]
sel_for_4_165 <- selected_vbls_165[[2]]
sel_for_5_165 <- selected_vbls_165[[3]]

sel_for_4_by_total_165 <- freq_info_165[[2]][["vbl_by_total"]]
sel_for_5_by_total_165 <- freq_info_165[[3]][["vbl_by_total"]]

print(all_vbls_165)
print(sel_for_4_165)
print(sel_for_5_165)
f_after_s2_165 <- freq_info_165[[1]]
f_after_s3_165 <- freq_info_165[[2]]
f_after_s4_165 <- freq_info_165[[3]]
f_after_s5_165 <- freq_info_165[[4]]


