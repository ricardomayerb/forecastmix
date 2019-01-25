source('./R/combinations_functions.R')

data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
print(names(data_object_ury))
country <- data_object_ury$country_name
target_transformation <- data_object_ury$target_transformation
raw_data <- data_object_ury$raw_data
var_data <- data_object_ury$transformed_data
print(target_transformation)
names_exogenous <- c("ip_us","ip_asia","ip_ue","ip_bra","act_eco_bra","emae_arg")

this_target_ts <- na.omit(raw_data[,"rgdp"])

tic()
this_t_100_u_lags135 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                       reps = 10, tosample = 100, lags = c(1, 3, 5))
toc()

tic()
this_t_100_r1_lags135 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                    reps = 10, tosample = 100, lags = c(1, 3, 5), t_thresholds = 1.65)
toc()

tic()
this_t_100_r2_lags135 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                    reps = 10, tosample = 100, lags = c(1, 3, 5), t_thresholds = c(1.65, 2))
toc()


tic()
this_t_100_u_lags15 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                    reps = 10, tosample = 100, lags = c(1,  5))
toc()

tic()
this_t_100_r1_lags15 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                     reps = 10, tosample = 100, lags = c(1,  5), t_thresholds = 1.65)
toc()

tic()
this_t_100_r2_lags15 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                     reps = 10, tosample = 100, lags = c(1,  5), t_thresholds = c(1.65, 2))
toc()




tic()
this_t_100_u_lags5_withexo <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                  reps = 10, tosample = 100, lags = c(  5), 
                                  names_exogenous = names_exogenous)
toc()


tic()
this_t_100_u_lags5 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                   reps = 10, tosample = 100, lags = c(  5))
toc()

tic()
this_t_100_r1_lags5 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                    reps = 10, tosample = 100, lags = c( 5), t_thresholds = 1.65)
toc()

tic()
this_t_100_r2_lags5 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                                    reps = 10, tosample = 100, lags = c( 5), t_thresholds = c(1.65, 2))
toc()

ten_reps_timings <- list(this_t_100_u_lags135 = this_t_100_u_lags135, this_t_100_r1_lags135 = this_t_100_r1_lags135, this_t_100_r2_lags135 = this_t_100_r2_lags135,
                         this_t_100_u_lags15 = this_t_100_u_lags15, this_t_100_r1_lags15 = this_t_100_r1_lags15, this_t_100_r2_lags15 = this_t_100_r2_lags15,
                         this_t_100_u_lags5 = this_t_100_u_lags5, this_t_100_r1_lags5 = this_t_100_r1_lags5, this_t_100_r2_lags5 = this_t_100_r2_lags5)

saveRDS(ten_reps_timings, file = "ten_reps_timings.rds")
