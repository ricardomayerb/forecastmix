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

var_data_path <- paste0(output_path, "VAR_data_", country_name)

partial_filename <- "vr_Chile_s2345_fqnonenone2015_t2222_mr50_mrfq50_cv8_tspan25_h8.rds"
filename_new <- paste0(output_path, partial_filename)

var_models <-  readRDS(filename_new)[["consolidated_var_res"]]



# make_models_tbl <- function(arima_res, var_models_and_rmse, VAR_data, h_max, 
#                             force.constant, ave_rmse_sel = FALSE, pval_arima = 0.05) {
  
  
rmse_yoy_sarimax <- arima_res$compare_rmse_yoy %>% mutate(id = 1:n())
rmse_level_sarimax <- arima_res$compare_rmse %>% mutate(id = 1:n())
v_lags_order_season <- arima_res$var_lag_order_season 
extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima


rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
  left_join(v_lags_order_season, by = c("variable", "lag"))


each_h_just_model_and_ave_rmse_var <- var_models_and_rmse %>% 
  mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
  dplyr::select(- starts_with("rank"))


each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
  mutate(model_function = "Arima") %>% 
  dplyr::select(variable, lag, id, starts_with("yoy"), arima_order, arima_seasonal, 
                model_function) %>% 
  rename(variables = variable, lags = lag) %>% 
  rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))


if (ave_rmse_sel) {
  models_rmse_at_each_h_arima  <- as_tibble(
    each_h_just_model_and_ave_rmse_sarimax) %>% 
    mutate(ave_rmse = rowMeans(select(., starts_with("rmse")))) %>% 
    group_by(variables) %>%
    mutate(min_ave_per_variable = min(ave_rmse)) %>% 
    filter(ave_rmse == min_ave_per_variable) %>% 
    ungroup() %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
    ungroup() %>% 
    group_by(rmse_h) %>% 
    mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
    filter(rmse <= rgdp_rmse) %>% 
    ungroup() %>% 
    select(-c(ave_rmse, rgdp_rmse, min_ave_per_variable)) %>% 
    arrange(rmse_h, variables)
  
} else {
  models_rmse_at_each_h_arima <- as_tibble(
    each_h_just_model_and_ave_rmse_sarimax) %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
    arrange(variables) %>% 
    group_by(rmse_h, variables) %>% 
    mutate(min_per_variable_and_h = min(rmse)) %>% 
    filter(rmse == min_per_variable_and_h) %>% 
    select(-min_per_variable_and_h ) %>%  
    ungroup() %>% 
    group_by(rmse_h) %>% 
    mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
    filter(rmse <= rgdp_rmse) %>% 
    ungroup() %>% 
    select(-rgdp_rmse) %>% 
    arrange(rmse_h, rmse)
}


models_rmse_at_each_h_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse"))

models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                               models_rmse_at_each_h_arima) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  arrange(rmse_h, rank_h)


models_rmse_at_each_h <- models_rmse_at_each_h %>%
  mutate(short_name = map2(variables, lags,
                           ~ make_model_name(variables = .x, lags = .y)),
         long_name = pmap(list(variables, lags, model_function),
                          ~ make_model_name(variables = ..1, lags = ..2,
                                            model_function = ..3)),
         short_name = as_factor(unlist(short_name)),
         long_name = as_factor(unlist(long_name))
  ) 

my_stability_fun <- function(model_type, model_object) {
  
  # print(model_type)
  # print(model_object)
  
  if (model_type == "Arima") {
    is.stable <- TRUE
    
  }
  if (model_type == "VAR"){
    is.stable <- all(roots(model_object) < 1)
  }
  
  if(!is.stable) {
    print("Ooops, not stable")
  }
  
  return(is.stable)
}

tic()
models_rmse_at_each_h <- models_rmse_at_each_h %>%
  dplyr::distinct(long_name, .keep_all = TRUE) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse),
         fit = pmap(list(model_function, variables, lags, arima_order, 
                         arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5,
                                    extended_x_data_ts = extended_x_data_ts,
                                    arima_rgdp_ts = rgdp_ts_in_arima,
                                    force.constant = force.constant,
                                    var_data = VAR_data)),
         is_stable = map2(model_function, fit, 
                          ~my_stability_fun(model_type = .x, model_object = .y)),
         is_white_noise = map2(model_function, fit, 
                               ~check_resid_VAR_Arima(model_function = .x, 
                                                      fit = .y,
                                                      pval_arima = pval_arima))
  ) %>% 
  ungroup() %>% filter(is_stable == TRUE, is_white_noise == TRUE) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  select(-fit)
toc()
  




# models_tbl <- make_models_tbl(
#   arima_res = arima_res, var_models_and_rmse = models_and_accu_reasonable, 
#   VAR_data = VAR_data, h_max = h_max,
#   force.constant = do.force.constant, pval_arima = 0.05)
