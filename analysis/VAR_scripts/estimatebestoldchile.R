source('./R/VAR_functions.R')

initial_time <- Sys.time()

tic(msg = "Total time for this country")
##### data selection part -----
# arguments
country_name <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

# file paths
excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")


##### data pre-processing part -----

country_data_ts <- get_raw_data_ts(country_name, excel_data_path)
external_data_ts <- get_raw_external_data_ts(excel_data_path)

data_ts <- country_data_ts

rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

print(paste0("This country: ", country_name))
print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
print("Names of variables: ")
print(colnames(data_ts))

tic()
print("Finding and applying stationary transformations to all variables")
reco_all_variables <- find_statio_diffs(data_ts, country_name)
country_transformed_data <- follow_rec(data_ts, reco_all_variables)
print("Done.")
toc()

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
print(paste0("Stationary transformation for rgdp: ", rgdp_rec))

VAR_data_for_estimation  <- country_transformed_data

print(paste0("rgdp obs. after transformation: ", 
             length(na.omit(VAR_data_for_estimation[ , "rgdp"]))
)
)

print(paste0("rgdp obs. before transformation: ", length(rgdp_level_ts)))


variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)


chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"
chlold <- readRDS(chl_filename_old)

chl0_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t0000_mr50_mrfq50_cv8_tspan25_h8.rds"
chl0_filename_new <- paste0(output_path, chl0_partial_filename_new)
chl0 <- readRDS(chl0_filename_new)[["consolidated_var_res"]]

chl2_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t2222_mr50_mrfq50_cv8_tspan25_h8.rds"
chl2_filename_new <- paste0(output_path, chl2_partial_filename_new)
chl2 <- readRDS(chl2_filename_new)[["consolidated_var_res"]]

chl165_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t165165165165_mr50_mrfq50_cv8_tspan25_h8.rds"
chl165_filename_new <- paste0(output_path, chl165_partial_filename_new)
chl165 <- readRDS(chl165_filename_new)[["consolidated_var_res"]]



best_old_h1 <- chlold  %>% top_n(1, -rmse_1) 
best_old_h2 <- chlold  %>% top_n(1, -rmse_2) 
best_old_h3 <- chlold  %>% top_n(1, -rmse_3) 
best_old_h4 <- chlold  %>% top_n(1, -rmse_4) 
best_old_h5 <- chlold  %>% top_n(1, -rmse_5) 
best_old_h6 <- chlold  %>% top_n(1, -rmse_6) 
best_old_h7 <- chlold  %>% top_n(1, -rmse_7) 
best_old_h8 <- chlold  %>% top_n(1, -rmse_8) 

best_new_0_h1 <- chl0  %>% top_n(1, -rmse_1) 
best_new_0_h2 <- chl0  %>% top_n(1, -rmse_2) 
best_new_0_h3 <- chl0  %>% top_n(1, -rmse_3) 
best_new_0_h4 <- chl0  %>% top_n(1, -rmse_4) 
best_new_0_h5 <- chl0  %>% top_n(1, -rmse_5) 
best_new_0_h6 <- chl0  %>% top_n(1, -rmse_6) 
best_new_0_h7 <- chl0 %>% top_n(1, -rmse_7) 
best_new_0_h8 <- chl0 %>% top_n(1, -rmse_8)

best_new_165_h1 <- chl165  %>% top_n(1, -rmse_1) 
best_new_165_h2 <- chl165  %>% top_n(1, -rmse_2) 
best_new_165_h3 <- chl165  %>% top_n(1, -rmse_3) 
best_new_165_h4 <- chl165  %>% top_n(1, -rmse_4) 
best_new_165_h5 <- chl165  %>% top_n(1, -rmse_5) 
best_new_165_h6 <- chl165  %>% top_n(1, -rmse_6) 
best_new_165_h7 <- chl165 %>% top_n(1, -rmse_7) 
best_new_165_h8 <- chl165 %>% top_n(1, -rmse_8)

best_new_2_h1 <- chl2  %>% top_n(1, -rmse_1) 
best_new_2_h2 <- chl2  %>% top_n(1, -rmse_2) 
best_new_2_h3 <- chl2  %>% top_n(1, -rmse_3) 
best_new_2_h4 <- chl2  %>% top_n(1, -rmse_4) 
best_new_2_h5 <- chl2  %>% top_n(1, -rmse_5) 
best_new_2_h6 <- chl2  %>% top_n(1, -rmse_6) 
best_new_2_h7 <- chl2 %>% top_n(1, -rmse_7) 
best_new_2_h8 <- chl2 %>% top_n(1, -rmse_8)



vb_1_old <- c(best_old_h1$variables[[1]], best_old_h1$lags[[1]], best_old_h1$rmse_1[[1]])
vb_1_n0 <- c(best_new_0_h1$variables[[1]], best_new_0_h1$lags[[1]], best_new_0_h1$rmse_1[[1]])
vb_1_n165 <- c(best_new_165_h1$variables[[1]], best_new_165_h1$lags[[1]], best_new_165_h1$rmse_1[[1]])
vb_1_n2 <- c(best_new_2_h1$variables[[1]], best_new_2_h1$lags[[1]], best_new_2_h1$rmse_1[[1]])
all_1 <- rbind(vb_1_old, vb_1_n0, vb_1_n165, vb_1_n2)
vb_1_old
vb_1_n0
vb_1_n165
vb_1_n2

vb_2_old <- c(best_old_h2$variables[[1]], best_old_h2$lags[[1]], best_old_h2$rmse_2[[1]])
vb_2_n0 <- c(best_new_0_h2$variables[[1]], best_new_0_h2$lags[[1]], best_new_0_h2$rmse_2[[1]])
vb_2_n165 <- c(best_new_165_h2$variables[[1]], best_new_165_h2$lags[[1]], best_new_165_h2$rmse_2[[1]])
vb_2_n2 <- c(best_new_2_h2$variables[[1]], best_new_2_h2$lags[[1]], best_new_2_h2$rmse_2[[1]])
vb_2_old
vb_2_n0
vb_2_n165
vb_2_n2

vb_3_old <- c(best_old_h3$variables[[1]], best_old_h3$lags[[1]], best_old_h3$rmse_3[[1]])
vb_3_n0 <- c(best_new_0_h3$variables[[1]], best_new_0_h3$lags[[1]], best_new_0_h3$rmse_3[[1]])
vb_3_n165 <- c(best_new_165_h3$variables[[1]], best_new_165_h3$lags[[1]], best_new_165_h3$rmse_3[[1]])
vb_3_n2 <- c(best_new_2_h3$variables[[1]], best_new_2_h3$lags[[1]], best_new_2_h3$rmse_3[[1]])
vb_3_old
vb_3_n0
vb_3_n165
vb_3_n2


vb_4_old <- c(best_old_h4$variables[[1]], best_old_h4$lags[[1]], best_old_h4$rmse_4[[1]])
vb_4_n0 <- c(best_new_0_h4$variables[[1]], best_new_0_h4$lags[[1]], best_new_0_h4$rmse_4[[1]])
vb_4_n165 <- c(best_new_165_h4$variables[[1]], best_new_165_h4$lags[[1]], best_new_165_h4$rmse_4[[1]])
vb_4_n2 <- c(best_new_2_h4$variables[[1]], best_new_2_h4$lags[[1]], best_new_2_h4$rmse_4[[1]])
vb_4_old
vb_4_n0
vb_4_n165
vb_4_n2


vb_5_old <- c(best_old_h5$variables[[1]], best_old_h5$lags[[1]], best_old_h5$rmse_5[[1]])
vb_5_n0 <- c(best_new_0_h5$variables[[1]], best_new_0_h5$lags[[1]], best_new_0_h5$rmse_5[[1]])
vb_5_n165 <- c(best_new_165_h5$variables[[1]], best_new_165_h5$lags[[1]], best_new_165_h5$rmse_5[[1]])
vb_5_n2 <- c(best_new_2_h5$variables[[1]], best_new_2_h5$lags[[1]], best_new_2_h5$rmse_5[[1]])
vb_5_old
vb_5_n0
vb_5_n165
vb_5_n2

vb_6_old <- c(best_old_h6$variables[[1]], best_old_h6$lags[[1]], best_old_h6$rmse_6[[1]])
vb_6_n0 <- c(best_new_0_h6$variables[[1]], best_new_0_h6$lags[[1]], best_new_0_h6$rmse_6[[1]])
vb_6_n165 <- c(best_new_165_h6$variables[[1]], best_new_165_h6$lags[[1]], best_new_165_h6$rmse_6[[1]])
vb_6_n2 <- c(best_new_2_h6$variables[[1]], best_new_2_h6$lags[[1]], best_new_2_h6$rmse_6[[1]])
vb_6_old
vb_6_n0
vb_6_n165
vb_6_n2


vb_7_old <- c(best_old_h7$variables[[1]], best_old_h7$lags[[1]], best_old_h7$rmse_7[[1]])
vb_7_n0 <- c(best_new_0_h7$variables[[1]], best_new_0_h7$lags[[1]], best_new_0_h7$rmse_7[[1]])
vb_7_n165 <- c(best_new_165_h7$variables[[1]], best_new_165_h7$lags[[1]], best_new_165_h7$rmse_7[[1]])
vb_7_n2 <- c(best_new_2_h7$variables[[1]], best_new_2_h7$lags[[1]], best_new_2_h7$rmse_7[[1]])
vb_7_old
vb_7_n0
vb_7_n165
vb_7_n2


vb_8_old <- c(best_old_h8$variables[[1]], best_old_h8$lags[[1]], best_old_h8$rmse_8[[1]])
vb_8_n0 <- c(best_new_0_h8$variables[[1]], best_new_0_h8$lags[[1]], best_new_0_h8$rmse_8[[1]])
vb_8_n165 <- c(best_new_165_h8$variables[[1]], best_new_165_h8$lags[[1]], best_new_165_h8$rmse_8[[1]])
vb_8_n2 <- c(best_new_2_h8$variables[[1]], best_new_2_h8$lags[[1]], best_new_2_h8$rmse_8[[1]])
vb_8_old
vb_8_n0
vb_8_n165
vb_8_n2




selected_variables <- c("rgdp", "ipec", "ri", "primario", "imp_capital")
selected_lags <- 3
var_data <- na.omit(VAR_data_for_estimation[, selected_variables])

fit <- vars::VAR(y = var_data, p = selected_lags, type = "const")
all(roots(fit) < 1)
vars::serial.test(fit)
vars::serial.test(fit, lags.pt = 12)
vars::serial.test(fit, lags.pt = 8)

fr0  <- vars::restrict(fit, method = "ser", thresh = 0)
fr2 <- vars::restrict(fit, method = "ser", thresh = 2)
fr165 <- vars::restrict(fit, method = "ser", thresh = 1.65)

cv_r0 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
              h_max = 7, training_length = 25, full_sample_resmat = fr0$restrictions)

cv_r2 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
                h_max = 7, training_length = 25, full_sample_resmat = fr2$restrictions)

cv_r165 <- var_cv(var_data = var_data, this_p = 3, n_cv = 8,
                h_max = 7, training_length = 25, full_sample_resmat = fr165$restrictions)

results_r0 <- cv_r0
column_names <- names(results_r0)
results_r0 <- as_tibble(results_r0)
names(results_r0) <- column_names
cv_errors0 <- results_r0[["cv_errors"]]
mat_error0 <- reduce(cv_errors0, rbind)
mse_all_h0 <- colMeans(mat_error0^2)
rmse_all_h0 <- sqrt(mse_all_h0)

results_r2 <- cv_r2
results_r2 <- as_tibble(results_r2)
names(results_r2) <- column_names
cv_errors2 <- results_r2[["cv_errors"]]
mat_error2 <- reduce(cv_errors2, rbind)
mse_all_h2 <- colMeans(mat_error2^2)
rmse_all_h2 <- sqrt(mse_all_h2)


results_r165 <- cv_r165
results_r165 <- as_tibble(results_r165)
names(results_r165) <- column_names
cv_errors165 <- results_r165[["cv_errors"]]
mat_error165 <- reduce(cv_errors165, rbind)
mse_all_h165 <- colMeans(mat_error165^2)
rmse_all_h165 <- sqrt(mse_all_h165)


rmse_all_h0
rmse_all_h165
rmse_all_h2

