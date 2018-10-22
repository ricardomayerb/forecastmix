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

