source('./R/combinations_functions.R')

country <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")


data_ts <- get_raw_data_ts(country, data_path = "./data/edd_exercises/2018_exercise_2/")
rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

read_compare_var_res <- function(filename_new, filename_old, h_max = 7, 
                                 rank_h_max = 30) {
  
  
  if (is.character(filename_new)) {
    var_res_new <- readRDS(filename_new)
    var_res_old <- readRDS(filename_old)
  } else {
    var_res_new <- filename_new
    var_res_old <- filename_old
  }
  

  if ("f_vbls_all_sizes" %in% names(var_res_new)) {
    var_res_new <- var_res_new[["consolidated_var_res"]]
  }
  
  if ("f_vbls_all_sizes" %in% names(var_res_old)) {
    var_res_old <- var_res_old[["consolidated_var_res"]]
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

VAR_data_for_estimation <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/VAR_data_Chile.rds")


chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"

chl_2_2_165_partial_filename_new <- "Chile_two4_two5.rds"
chl_1_2_165_partial_filename_new <- "Chile_one4_two5.rds"
chl_1_3_165_partial_filename_new <- "Chile_one4_three5.rds"
chl_manual1_partial_filename_new <- "Chile_manual1.rds"

chl_2_2_165_filename_new <- paste0(output_path, chl_2_2_165_partial_filename_new)
chl_1_2_165_filename_new <- paste0(output_path, chl_1_2_165_partial_filename_new)
chl_1_3_165_filename_new <- paste0(output_path, chl_1_3_165_partial_filename_new)
chl_manual1_filename_new <- paste0(output_path, chl_manual1_partial_filename_new)

chl_2_2_rds <- readRDS(chl_2_2_165_filename_new)
chl_1_2_rds <- readRDS(chl_1_2_165_filename_new)
chl_1_3_rds <- readRDS(chl_1_3_165_filename_new)
chl_manual1_rds <- readRDS(chl_manual1_filename_new)

chl_old <- readRDS(chl_filename_old)
chl_2_2_mr <- chl_2_2_rds$consolidated_var_res
chl_1_2_mr <- chl_1_2_rds$consolidated_var_res
chl_1_3_mr <- chl_1_3_rds$consolidated_var_res
chl_manual1_mr <- chl_manual1_rds$consolidated_var_res

max_VAR_models_per_h <- 50

oldless <- as_tibble(chl_old) %>% 
  filter(rank_1 <= max_VAR_models_per_h | rank_2 <= max_VAR_models_per_h | 
           rank_3 <= max_VAR_models_per_h | rank_4 <= max_VAR_models_per_h |
           rank_5 <= max_VAR_models_per_h | rank_6 <= max_VAR_models_per_h | 
           rank_7 <= max_VAR_models_per_h | rank_8 <= max_VAR_models_per_h) 

auto13less <- as_tibble(chl_1_3_mr) %>% 
  filter(rank_1 <= max_VAR_models_per_h | rank_2 <= max_VAR_models_per_h | 
           rank_3 <= max_VAR_models_per_h | rank_4 <= max_VAR_models_per_h |
           rank_5 <= max_VAR_models_per_h | rank_6 <= max_VAR_models_per_h | 
           rank_7 <= max_VAR_models_per_h | rank_8 <= max_VAR_models_per_h) 

manual1less <- as_tibble(chl_manual1_mr) %>% 
  filter(rank_1 <= max_VAR_models_per_h | rank_2 <= max_VAR_models_per_h | 
           rank_3 <= max_VAR_models_per_h | rank_4 <= max_VAR_models_per_h |
           rank_5 <= max_VAR_models_per_h | rank_6 <= max_VAR_models_per_h | 
           rank_7 <= max_VAR_models_per_h | rank_8 <= max_VAR_models_per_h) 

names(manual1less)
names(auto13less)
names(oldless)


auto_tbl <- auto13less
rmse_names <- names(auto_tbl)[str_detect(names(auto_tbl), "rmse")]
rmse_names 
auto_tbl <- auto_tbl %>% 
  gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  group_by(rmse_h) %>% 
  arrange(rmse_h, rmse) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup()
auto_tbl


old_tbl <- oldless
old_tbl <- old_tbl %>% 
  gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  group_by(rmse_h) %>% 
  arrange(rmse_h, rmse) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup()
old_tbl


fit_VAR_rest <- function(var_data, variables, p,
                         t_tresh = FALSE, type = "const")  {
  
  this_var_data <- var_data[, variables]
  this_var_data <- na.omit(this_var_data)
  
  this_fit <- vars::VAR(y = this_var_data, p = p, type = type)
  
  if (is.numeric(t_tresh)) {
    this_fit <- try(vars::restrict(this_fit, method = "ser", 
                   thresh = t_tresh), silent = TRUE)
    
    if (class(this_fit) == "try-error") {
      this_fit <- "one_or_more_eqn_drops"
    }
  }
  return(this_fit)
}

forecast_VAR_one_row <- function(fit, h)  {
  
  if (class(fit) == "varest") {
    this_fc <- forecast(fit, h = h)
  }
  
  if (!  class(fit) == "varest") {
    this_fc <- NULL
  }
  
  return(this_fc)
}


estimate_var_from_model_tbl <- function(models_tbl, var_data, new_t_treshold = NULL) {
  
  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names
  has_t_treshold <- "t_treshold" %in% starting_names
  
  if (!has_t_treshold) {
    models_tbl <- models_tbl %>% mutate(t_treshold = FALSE)
  }
  
  rmse_names <- names(models_tbl)[str_detect(names(models_tbl), "rmse")]
  
  models_tbl <- models_tbl %>% 
    gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
    dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
    group_by(rmse_h) %>% 
    arrange(rmse_h, rmse) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    ungroup() %>% 
    mutate(lags = unlist(lags),
           t_treshold = unlist(t_treshold),
           model_type = "VAR")
  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  one_model_per_row <- models_tbl %>% 
    dplyr::select(-c(rmse, rmse_h, rank_h)) %>% 
    distinct(short_name, .keep_all = TRUE)
  
  if(is.null(new_t_treshold)) {
    one_model_per_row <- one_model_per_row %>%
      mutate(fit = pmap(list(variables, lags, t_treshold),
                        ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                       p = ..2, t_tresh = ..3))
      )
  } 
  
  if(!is.null(new_t_treshold)) {
    
    all_one_model_per_row <- list_along( seq(1, length(new_t_treshold))  )
    
    for (i in seq(1, length(new_t_treshold))) {
      
      this_tresh <- new_t_treshold[i]
      
      this_one_model_per_row <- one_model_per_row %>%
        mutate(t_treshold = this_tresh,
               short_name_t = map_chr(short_name, ~ paste0(.x, "_t", this_tresh*100)),
               fit = pmap(list(variables, lags, t_treshold),
                          ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                         p = ..2, t_tresh = ..3))
        )

      all_one_model_per_row[[i]] <- this_one_model_per_row
    }
    one_model_per_row <- reduce(all_one_model_per_row, rbind)
  }
  
  return(one_model_per_row)
}

forecast_var_from_model_tbl <- function(models_tbl, var_data, fc_horizon, 
                                        new_t_treshold = NULL, fit_column = NULL,
                                        target_transform = "yoy", target_level_ts = NULL,
                                        keep_fc_obj = FALSE, keep_varest_obj = FALSE) {
  
  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names
  has_t_treshold <- "t_treshold" %in% starting_names
  
  if (!has_t_treshold) {
    models_tbl <- models_tbl %>% mutate(t_treshold = FALSE)
  }
  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  if (is.null(fit_column)) {
    print("There is no column with fit varest objects, so we will estimate all VARs now")
    models_tbl <- estimate_var_from_model_tbl(
      models_tbl = models_tbl, var_data = var_data, new_t_treshold = new_t_treshold)
    
    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }
  
  models_tbl <- models_tbl %>% 
    mutate(fc_object_raw = map(fit, ~ forecast_VAR_one_row(fit = .x, h = fc_horizon))
           )
  
  if (target_transform == "yoy") {
    print("Target variable already in YoY form, so no transformation is needed")
    models_tbl <- models_tbl %>% 
      mutate(target_mean_fc_yoy = map(fc_object_raw,
                                      ~ .x[["forecast"]][["rgdp"]][["mean"]]))
  }

  if (target_transform != "yoy") {
    
    print(paste0("Target variable is in ", target_transform, " form. Forecasts will be transformed to YoY."))
    
    models_tbl <- models_tbl %>% 
      mutate(target_mean_fc = map(fc_object_raw,
                                  ~ .x[["forecast"]][["rgdp"]][["mean"]]),
             target_mean_fc_yoy = map(target_mean_fc, 
                                      ~ any_fc_2_fc_yoy(
                                        current_fc = .x, 
                                        rgdp_transformation = target_transform,
                                        rgdp_level_ts = rgdp_level_ts)
                                      )
             )
  }
  
  if (!keep_varest_obj) {
    models_tbl <- models_tbl %>% 
      dplyr::select(-fit)
  }
  
  
  if (!keep_fc_obj) {
    models_tbl <- models_tbl %>% 
      dplyr::select(-fc_object_raw)
  }
  
  return(models_tbl)
}

cv_var_from_model_tbl <- function(h, n_cv, training_span, models_tbl, var_data, 
                                  fc_horizon, new_t_treshold = NULL, 
                                  fit_column = NULL, target_transform = "yoy", 
                                  target_level_ts = NULL) {
  
  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names
  has_t_treshold <- "t_treshold" %in% starting_names
  
  if (!has_t_treshold) {
    models_tbl <- models_tbl %>% mutate(t_treshold = FALSE)
  }
  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y)),
             short_name = unlist(short_name)) %>% 
      dplyr::select(short_name, everything())
  }
  
  
  
  
} 


tic()
fcold_3t_from_scratch <- forecast_var_from_model_tbl(oldless, var_data = VAR_data_for_estimation,
                                                     new_t_treshold = c(0, 1.65, 2), fc_horizon = 8)
toc()

print(object.size(fcold_3t_from_scratch), units = "auto")


# foo <- forecast_var_from_model_tbl(oldless, var_data = VAR_data_for_estimation,
#                                    new_t_treshold = c(0, 1.65, 2), fc_horizon = 8,
#                                    target_transform = "diff", target_level_ts = rgdp_level_ts)

# tic()
# fitold <- estimate_var_from_model_tbl(oldless, var_data = VAR_data_for_estimation)
# toc()
# 
# tic()
# fitold_3t <- estimate_var_from_model_tbl(oldless, var_data = VAR_data_for_estimation, 
#                                          new_t_treshold = c(0, 1.65, 2))
# toc()
# 
# 
# tic()
# fcold_3t <- forecast_var_from_model_tbl(fitold_3t, var_data = VAR_data_for_estimation,
#                                         fit_column = "fit", fc_horizon = 8)
# toc()
# 
# 
# 
# 
# 
# 
# 
# 
# tic()
# fitauto <- estimate_var_from_model_tbl(auto13less, var_data = VAR_data_for_estimation)
# toc()
# 
# tic()
# fitauto_3t <- estimate_var_from_model_tbl(auto13less, var_data = VAR_data_for_estimation,
#                                           new_t_treshold = c(0, 1.65, 2))
# toc()
# 
# 
# print(object.size(fitold), units = "auto")
# print(object.size(fitauto), units = "auto")
# print(object.size(fitold_3t), units = "auto")
# print(object.size(fitauto_3t), units = "auto")

# 
# 
# 
# best_old_h7 <- chl_old %>% filter(rank_7 == 1)
# sort(best_old_h7$variables[[1]])
# best_old_h7$lags[[1]]
# best_old_h7
# 
# this_short_name <- "copper_output___imp___imp_capital___ipec___rgdp__3"
# 
# manualfoo <- chl_manual1_mr %>% mutate(short_name = unlist(short_name))
# 
# manual_same_short <- manualfoo %>% filter(short_name == this_short_name)
# manual_same_short$t_treshold
# 
