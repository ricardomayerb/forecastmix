source('./R/combinations_functions.R')

country <- "Uruguay"
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

VAR_data_for_estimation <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/VAR_data_Uruguay.rds")


ury_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Uruguay_by_step_12345.rds"

ury_auto_1s4_3s5_partial_filename_new <- "Uruguay_auto_1s4_3s5.rds"
ury_auto_2s4_3s5_partial_filename_new <- "Uruguay_auto_2s4_3s5.rds"

ury_auto_1s4_3s5_filename_new <- paste0(output_path, ury_auto_1s4_3s5_partial_filename_new)
ury_auto_2s4_3s5_filename_new <- paste0(output_path, ury_auto_2s4_3s5_partial_filename_new)

ury_auto_1s4_3s5 <- readRDS(ury_auto_1s4_3s5_filename_new)
ury_auto_2s4_3s5 <- readRDS(ury_auto_2s4_3s5_filename_new)
ury_old <- readRDS(ury_filename_old)

ury_auto_1s4_3s5_mr <- ury_auto_1s4_3s5$consolidated_var_res
ury_auto_2s4_3s5_mr <- ury_auto_2s4_3s5$consolidated_var_res
ury_old_mr <- ury_old

max_VAR_models_per_h <- ury_auto_1s4_3s5$max_rank_some_h
print(paste0("max_VAR_models_per_h = ", max_VAR_models_per_h))

n_cv <- ury_auto_1s4_3s5$number_of_cv
print(paste0("n_cv = ", n_cv))

training_length <- ury_auto_1s4_3s5$train_span
print(paste0("training_length = ", training_length))

names_exogenous <- ury_auto_2s4_3s5$names_exogenous
print("names_exogenous = ")
print(names_exogenous)

fc_horizon <- ury_auto_2s4_3s5$fc_horizon
print(paste0("fc_horizon = ", fc_horizon))

rgdp_transformation <- ury_auto_2s4_3s5$target_variable_transform
print(paste0("rgdp_transformation = ", rgdp_transformation))


smaller_max_VAR_models_per_h <- 20

oldless <- as_tibble(ury_old) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 

auto13less <- as_tibble(ury_auto_1s4_3s5_mr) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 

auto23less <- as_tibble(ury_auto_2s4_3s5_mr) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 

names(auto13less)
names(auto23less)
names(oldless)


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
  
  if (!class(fit) == "varest") {
    this_fc <- NA
  }
  # print(this_fc)
  return(this_fc)
}

cv_var_from_one_row <- function(var_data, fit, variables, lags, h, 
                                names_exogenous, training_length, 
                                n_cv, this_type = "const") {
  
  
  # print(fit)
 

  this_restriction_mat <- try(fit$restrictions, silent = TRUE) 
  
  if (class(this_restriction_mat) == "try-error") {
    this_restriction_mat <-  NULL
  }
  
  # print("this_restriction_mat")
  # print(this_restriction_mat)
  
  sub_data <- na.omit(var_data[, variables])
  
  # print(sub_data)
  # print("variables")
  # print(variables)
  
  # print("colnames(sub_data)")
  # print(colnames(sub_data))
  
  sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
  
  this_cv <- var_cv(var_data = sub_data,  h_max = h,
                    n_cv = n_cv, this_p = lags,  external_idx = sub_data_tk_index,
                    full_sample_resmat = this_restriction_mat,
                    names_exogenous = names_exogenous,
                    training_length = training_length,
                    this_type = this_type)
  
  return(this_cv)
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

cv_var_from_model_tbl <- function(h, n_cv, training_length, 
                                  models_tbl, var_data, 
                                  new_t_treshold = NULL, 
                                  fit_column = NULL, 
                                  target_transform = "yoy", 
                                  target_level_ts = NULL,
                                  keep_varest_obj = FALSE,
                                  keep_cv_objects = FALSE,
                                  keep_fc_objects = FALSE) {
  
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
  
  if (is.null(fit_column)) {
    print("There is no column with fit varest objects, so we will estimate all VARs now")
    models_tbl <- estimate_var_from_model_tbl(
      models_tbl = models_tbl, var_data = var_data, new_t_treshold = new_t_treshold)
    
    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }
  
  models_tbl <- models_tbl %>% 
    mutate(fc_obj = map(fit, ~ forecast_VAR_one_row(fit = .x, h = h)),
           fc_target_mean = map(fc_obj, c("forecast", "rgdp", "mean")),
           fc_target_mean_yoy = map(fc_target_mean,
                                    ~ any_fc_2_fc_yoy(
                                      current_fc = .x,
                                      rgdp_transformation = target_transform,
                                      rgdp_level_ts = rgdp_level_ts)
                                    )
           )
    
  print("Starting cv")

  models_tbl <-  models_tbl %>% 
    mutate(cv_obj = pmap(list(fit, variables, lags),
                         ~ cv_var_from_one_row(var_data = var_data, fit = ..1, 
                                               variables = ..2, lags = ..3,
                                               h = h, n_cv = n_cv,
                                               names_exogenous = names_exogenous,
                                               training_length = training_length,
                                               this_type = "const")
                         )
           )
  
  
  if (target_transform != "yoy") {
    
    # rgdp_yoy_ts <- make_yoy_ts(target_level_ts)
    
    if (target_transform == "diff_yoy") {

      models_tbl <- models_tbl %>% 
        rename(cv_obj_diff_yoy = cv_obj)
      
      models_tbl <- models_tbl %>% 
        mutate(cv_obj_yoy = map(cv_obj_diff_yoy,
                                ~ transform_all_cv( ., 
                                                current_form = rgdp_transformation,
                                                target_level_ts =  rgdp_level_ts,
                                                n_cv = n_cv) 
                                )
        )
    }
    
    if (target_transform == "diff") {
      auxiliary_ts <-  target_level_ts
      
      models_tbl <- models_tbl %>% 
        rename(cv_obj_diff = cv_obj)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_obj_yoy = map(cv_obj_diff,
                                ~ transform_all_cv(cv_object  = .,
                                               current_form = rgdp_transformation,
                                               auxiliary_ts = target_level_ts,
                                               n_cv = n_cv)
                                )
               )
    }
    
  }
  
  if (target_transform == "yoy") {
    models_tbl <- models_tbl %>% 
      rename(cv_obj_yoy = cv_obj)
  }
  
  models_tbl <- models_tbl %>% 
    mutate(rmse_yoy_all_h = map(cv_obj_yoy, all_rmse_from_cv_obj))
  
  rmse_tibble <- as_tibble(reduce(models_tbl$rmse_yoy_all_h, rbind))
  names(rmse_tibble) <- paste0("rmse_", seq(1, ncol(rmse_tibble)))
  
  models_tbl <- models_tbl %>% 
    dplyr::select(-rmse_yoy_all_h) %>% 
    cbind(rmse_tibble)
  
  
  if (!keep_varest_obj) {
    models_tbl <- models_tbl %>% 
      dplyr::select(-fit)
  }

  if (!keep_fc_objects) {
    models_tbl <- models_tbl %>% 
      dplyr::select(vars_select(names(.), -starts_with("fc_ob")))
  }
  
  if (!keep_cv_objects) {
    models_tbl <- models_tbl %>% 
      dplyr::select(vars_select(names(.), -starts_with("cv_ob")))
  }
  
  
  mean_yoy <- models_tbl$fc_target_mean_yoy
  mean_yoy[map_lgl(mean_yoy, is.null)] <- NA 
  fc_tibble <- as_tibble(reduce(mean_yoy, rbind))
  names(fc_tibble) <- paste0("fc_yoy_", seq(1, ncol(fc_tibble)))
  # fc_tibble
  
  fc_tibble_long <- fc_tibble %>% 
    gather(key = "fc_yoy_h", value = "fc_yoy")
  
  models_tbl <- models_tbl %>% 
    dplyr::select(-c(fc_target_mean, fc_target_mean_yoy)) %>% 
    gather(key = "rmse_h", value = "rmse", vars_select(names(.), starts_with("rmse")))
  
  models_tbl <- models_tbl %>% cbind(fc_tibble_long)
  
  models_tbl <- models_tbl %>% 
    filter(!is.na(fc_yoy)) %>% 
    mutate(mse = rmse*rmse) %>% 
    group_by(rmse_h) %>% 
    mutate(inv_mse = 1/mse,
           model_weight_h = inv_mse/sum(inv_mse),
           weighted_fc_h = fc_yoy*model_weight_h,
           average_fc_h = sum(weighted_fc_h)
    ) %>% 
    ungroup()
  
  return(models_tbl)
} 


all_rmse_from_cv_obj <- function(cv_obj) {
  cv_errors <- cv_obj[["cv_errors"]]
  # print(cv_errors)
  n_cv <- length(cv_errors)
  # print(n_cv)
  # print(cv_errors[[1]])
  t_periods <-  length(cv_errors[[1]])
  # print(t_periods)
  
  # matrix is n_cv x t_periods, i.e. a column represent fixed period, varying cv
  matrix_errors <- reduce(cv_errors, rbind) 
  rownames(matrix_errors) <- NULL
  rmse <- sqrt(colMeans(matrix_errors^2))

  return(rmse)
}



transform_all_cv <- function(cv_object, current_form,
                         target_level_ts, n_cv) {
  
  if (current_form == "yoy") {
    #noting to transform
    return(cv_object)
  }
  
  old_test_data_list <- cv_object[["cv_test_data"]]
  old_fcs_list <- cv_object[["cv_fcs"]]
  
  new_test_data_list <- list_along(1:n_cv)
  new_fcs_list <- list_along(1:n_cv)
  new_fcs_errors_list  <- list_along(1:n_cv)
  
  if (current_form == "diff_yoy") {
    
    len_initial_cond <- 1
    auxiliary_ts <- make_yoy_ts(target_level_ts)
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- old_test_data_list[[td]]
      this_fcs <- old_fcs_list[[td]]
      
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      
      new_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      new_fcs <- un_diff_ts(initial_cond_ts, this_fcs)
      
      new_fcs_errors <-  new_test_data - new_fcs
      new_fcs_errors_list[[td]] <- new_fcs_errors
      
      new_test_data_list[[td]] <- new_test_data
      new_fcs_list[[td]] <- new_fcs
    }
    
  }
  
  if (current_form == "diff") {
    len_initial_cond <- 1
    auxiliary_ts <- target_level_ts
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- old_test_data_list[[td]]
      this_fcs <- old_fcs_list[[td]]
      
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      
      level_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      level_fcs <- un_diff_ts(initial_cond_ts, this_fcs)
      
      pre_test_level_data <- window(auxiliary_ts, end = end_initial_cond_y_q)

      data_and_test_level <- ts(c(pre_test_level_data, level_test_data),
                                frequency = 4, start = start(auxiliary_ts))
      data_and_fcs_level <- ts(c(pre_test_level_data, level_fcs),
                                frequency = 4, start = start(auxiliary_ts))
      
      data_and_test_yoy <- make_yoy_ts(data_and_test_level, freq = 4, 
                                       is_log = FALSE)
      data_and_fcs_yoy <- make_yoy_ts(data_and_fcs_level, freq = 4, 
                                       is_log = FALSE)
      
      new_test_data <- window(data_and_test_yoy, start = start(this_test_data),
                              end = end(this_test_data))
      new_fcs <- window(data_and_fcs_yoy, start = start(this_fcs),
                              end = end(this_fcs))
      
      new_fcs_errors <-  new_test_data - new_fcs
      new_fcs_errors_list[[td]] <- new_fcs_errors
      
      new_fcs_list[[td]] <- new_fcs
      new_test_data_list[[td]] <- new_test_data
      
    }
    
  }
  
  new_cv_object = list(cv_test_data = new_test_data_list, 
                       cv_fcs = new_fcs_list,
                       cv_errors = new_fcs_errors_list)
  
  return(new_cv_object)
}

# return(list(cv_errors = cv_errors,
#             cv_test_data = cv_test_data,
#             cv_fcs = cv_fcs,
#             mean_cv_rmse = mean_cv_rmse,
#             cv_vbl_names = cv_vbl_names,
#             cv_lag = cv_lag,
#             cv_is_white_noise = cv_is_white_noise))


# cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
#                                       series_name = "cv_test_data",
#                                       current_form = rgdp_current_form,
#                                       auxiliary_ts = auxiliary_ts,
#                                       n_cv = n_cv) ),
# cv_fcs = map(
#   cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
#                                    series_name = "cv_fcs",
#                                    current_form = rgdp_current_form,
#                                    auxiliary_ts = auxiliary_ts,
#                                    n_cv = n_cv) ),


tic()
fcold_3t_from_scratch <- forecast_var_from_model_tbl(oldless, var_data = VAR_data_for_estimation,
                                                     new_t_treshold = c(0, 1.65, 2),
                                                     fc_horizon = 8, 
                                                     target_transform = rgdp_transformation)
toc()

print(object.size(fcold_3t_from_scratch), units = "auto")



tic()
fc13_3t_from_scratch <- forecast_var_from_model_tbl(auto13less, var_data = VAR_data_for_estimation,
                                                     new_t_treshold = c(0, 1.65, 2), 
                                                    fc_horizon = 8, target_transform = rgdp_transformation)
toc()

print(object.size(fc13_3t_from_scratch), units = "auto")


tic()
cv_oldless_3t_from_scratch <- cv_var_from_model_tbl(h = fc_horizon, 
                                                n_cv = n_cv, 
                                                training_length = training_length,
                                                models_tbl = oldless, 
                                                var_data = VAR_data_for_estimation, 
                                                new_t_treshold = c(0, 1.65, 2), 
                                                target_level_ts = rgdp_level_ts,
                                                target_transform = rgdp_transformation)
toc()

print(object.size(cv_oldless_3t_from_scratch), units = "auto")


smaller_max_VAR_models_per_h <- 30

old30 <- as_tibble(ury_old) %>% 
  filter(rank_1 <= smaller_max_VAR_models_per_h | rank_2 <= smaller_max_VAR_models_per_h | 
           rank_3 <= smaller_max_VAR_models_per_h | rank_4 <= smaller_max_VAR_models_per_h |
           rank_5 <= smaller_max_VAR_models_per_h | rank_6 <= smaller_max_VAR_models_per_h | 
           rank_7 <= smaller_max_VAR_models_per_h | rank_8 <= smaller_max_VAR_models_per_h) 


tic()
cv_old30_3t_from_scratch <- cv_var_from_model_tbl(h = fc_horizon, 
                                                n_cv = n_cv, 
                                                training_length = training_length,
                                                models_tbl = old30, 
                                                var_data = VAR_data_for_estimation, 
                                                new_t_treshold = c(0, 1.65, 2), 
                                                target_level_ts = rgdp_level_ts,
                                                target_transform = rgdp_transformation)
toc()



tic()
cv_old_3t_from_scratch <- cv_var_from_model_tbl(h = fc_horizon, 
                                                n_cv = n_cv, 
                                                training_length = training_length,
                                                models_tbl = ury_old, 
                                                var_data = VAR_data_for_estimation, 
                                                new_t_treshold = c(0, 1.65, 2), 
                                                target_level_ts = rgdp_level_ts,
                                                target_transform = rgdp_transformation)
toc()

print(object.size(cv_old_3t_from_scratch), units = "auto")


ave_fc_from_cv <- function(cv_tbl, best_n_to_keep = "all") {
  
  if (best_n_to_keep == "all") {
    cv_tbl <- cv_tbl
  }

  if (is.numeric(best_n_to_keep)) {
    cv_tbl <- cv_tbl %>% 
      arrange(rmse_h, rmse) %>%
      group_by(rmse_h) %>% 
      mutate(rank_h = rank(rmse)) %>% 
      filter(rank_h <= best_n_to_keep) %>% 
      mutate(inv_mse = 1/(rmse*rmse),
             model_weight_h = inv_mse/sum(inv_mse),
             weighted_fc_h = fc_yoy*model_weight_h,
             average_fc_h = sum(weighted_fc_h)
      ) %>% 
      ungroup()
  }
  
  ave_fc <- cv_tbl %>% 
    group_by(rmse_h) %>% 
    summarise(ave_fc_h = sum(weighted_fc_h))
  
  return(ave_fc)
}

ave_fc_from_cv(cv_tbl = cv_oldless_3t_from_scratch)

ave_fc_from_cv(cv_tbl = cv_old30_3t_from_scratch)

ave_fc_from_cv(cv_tbl = cv_old30_3t_from_scratch, best_n_to_keep = 15)


# rmseall <- cv_old_3t_from_scratch$rmse_yoy_all_h

# rmse_tibble <- as_tibble(reduce(rmseall, rbind))
# names(rmse_tibble) <- paste0("rmse_", seq(1, ncol(rmse_tibble)))
# rmse_tibble
# remoo <- cv_old_3t_from_scratch[1,]




# 
# troo <- new_transform_cv(list_series = moo[["cv_test_data"]], series_name = "cv_test_data", 
#                          current_form = "diff_yoy", auxiliary_ts = rgdp_yoy_ts, n_cv = n_cv)

# cv_var_from_model_tbl <- function(h, n_cv, training_length, models_tbl, var_data, 
#                                   fc_horizon, new_t_treshold = NULL, 
#                                   fit_column = NULL, target_transform = "yoy", 
#                                   target_level_ts = NULL,
#                                   keep_fc_obj = FALSE, keep_varest_obj = FALSE)

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
