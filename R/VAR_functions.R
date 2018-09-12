source(file = "./R/utils.R")
library(vars)


get_rmses_h_rankings_h <- function(data = cv_objects, h_max = 6){
  cv_errors <- data[["cv_errors"]]
  
  all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
  all_rmses_tbl <- reduce(all_rmses, rbind)
  rmse_names <- paste0("rmse_", 1:h_max)
  colnames(all_rmses_tbl) <- rmse_names
  row.names(all_rmses_tbl) <- NULL
  
  
  for (r in seq_along(rmse_names)) {
    this_rmse <- rmse_names[r]
    rmse_vec <- all_rmses_tbl[, this_rmse]
    this_rank <- rank(rmse_vec)
    all_rmses_tbl <- cbind(all_rmses_tbl, this_rank)
    
  }
  
  ranking_names <- paste0("rank_", 1:h_max)
  rmse_and_rank_names <- c(rmse_names, ranking_names)
  colnames(all_rmses_tbl) <- rmse_and_rank_names
  
  rmse_each_h <- cbind(data, all_rmses_tbl)
  
  return(rmse_each_h)
  
}


check_resid_VAR <- function(fit_VAR, type = "PT.adjusted", lags.pt = 12,
                            pval_ref = 0.05) {
  
  test_object <- serial.test(fit_VAR)
  
  pval <- test_object[["serial"]][["p.value"]]
  
  pval <- unname(pval)
  
  # print("pval")
  # print(pval)
  
  is_white_noise <- pval > pval_ref

  return(is_white_noise)
}


get_sets_of_variables <- function(df, this_size, all_variables, already_chosen){
  
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- this_size - len_already_chosen
  
  passing_variables <- all_variables

  passing_not_alr_chosen <- passing_variables[!passing_variables %in% already_chosen]
  
  n_passing_vbls <- length(passing_not_alr_chosen)
  
  print(paste("We have", n_passing_vbls, "variables, to fill", len_other_vbls,
              "slots in the VAR.Total possible combinations :",
              choose(n_passing_vbls, len_other_vbls)))
  
  combinations <- combn(passing_not_alr_chosen, len_other_vbls)
}


# search var formerly known as try_sizes_vbls_lags
search_var <- function(var_data, rgdp_yoy_ts, rgdp_level_ts, target_v, 
                       vec_size = c(3,4,5), 
                       vec_lags = c(1,2,3,4), pre_selected_v = "",
                       is_cv = FALSE, h_max = 5, n_cv = 8,
                       training_length = 16,
                       return_cv = TRUE,
                       max_rank = 30,
                       rgdp_current_form = "yoy",
                       check_residuals_full_sample = TRUE,
                       check_residuals_cv = TRUE,
                       white_noise_target_ratio = 1,
                       keep_only_white_noise_fs = TRUE) {
  
  # print("in try_sizes_vbls_lags, has_timetk_idx(var_data)")
  # print(has_timetk_idx(var_data))
  
  len_size <-  length(vec_size)
  len_lag <- length(vec_lags)
  
  all_names <- colnames(var_data)
  
  # i, outer most loop: var size (number of edogenous variables), e.g. 3, then 4, then 5 variables
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  # I considered including a loop between i and j, loopig through several
  # choices of fixed or preselected variables but I think that makes the code less intuitive and 
  # is not a frequently used feature, so I discarded it. 
  
  results_all_models <- list_along(seq.int(1, len_size))
  fcs_var_all_sizes <- list_along(seq.int(1, len_size))
  
  var_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  fcs_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  
  model_number <- 0
  
  for (i in seq.int(1, len_size)) {
    this_size <- vec_size[i]
    
    already_chosen <- c(target_v, pre_selected_v)
    already_chosen <- already_chosen[already_chosen != ""]
    len_already_chosen <- length(already_chosen)
    len_other_vbls <- this_size - len_already_chosen
    
    
    sets_of_other_variables <- get_sets_of_variables(
      df = var_data, this_size = this_size, all_variables = all_names, 
      already_chosen = already_chosen)

    len_sets_of_vars <- ncol(sets_of_other_variables)
    
    var_fixed_size_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      # vec_of_other_vbls <- sets_of_other_variables[[j]]
      vec_of_other_vbls <- sets_of_other_variables[,j]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      sub_data = var_data[, vbls_for_var]
      sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
      
      sel <- vars::VARselect(sub_data, type = "const")
      
      sel_criteria <- sel$selection
      
      print(sel_criteria)
      
      for (k in seq.int(1, len_lag)) {
        
        model_number <- model_number + 1
        
        this_lag <- vec_lags[k]
        
        full_sample_var <- vars::VAR(sub_data, type = "const", p = this_lag)
        
        is_stable <- all(vars::roots(full_sample_var) < 1)
        
        if(!is_stable) {
          print("Current VAR not stable. No CV analysis will be done")
          }
        
        if (check_residuals_full_sample) {
          
          is_white_noise_fs <- check_resid_VAR(full_sample_var)

          
        } else {
          is_white_noise_fs <- TRUE
        }
        
        if (is_white_noise_fs & is_stable) {
          this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                            external_idx = sub_data_tk_index, this_p = this_lag,
                            this_type = "const", h_max = h_max,
                            n_cv = n_cv, training_length = training_length, 
                            test_residuals = check_residuals_cv)
          
          cv_num_of_white_noises <- sum(this_cv[["cv_is_white_noise"]])
          
          ratio_of_white_noises <- cv_num_of_white_noises/n_cv
          
          overall_cv_white_noise <- ratio_of_white_noises >= white_noise_target_ratio
          
          this_cv[["overall_cv_white_noise"]] <- overall_cv_white_noise
          this_cv[["is_white_noise_fse"]] <- is_white_noise_fs
          this_cv[["is_stable"]] <- TRUE
        }
        
        if ( (!is_white_noise_fs) | (!is_stable) ) {
       
          this_cv <- list(cv_errors = list(NULL),
                          cv_test_data = list(NULL),
                          cv_fcs = list(NULL),
                          mean_cv_rmse = list(NULL),
                          cv_vbl_names = list(colnames(sub_data)),
                          cv_lag = list(this_lag),
                          cv_is_white_noise = list(NULL))
          
          this_cv[["overall_cv_white_noise"]] <- list(NULL)
          this_cv[["is_white_noise_fse"]] <- list(FALSE)
          this_cv[["is_stable"]] <- is_stable
          
        }
        
        var_fixed_size_fixed_vset_all_lags[[k]] <- this_cv
        
      }

      est_var_this_vset <- var_fixed_size_fixed_vset_all_lags
      var_fixed_size_all_vset_all_lags[[j]] <- est_var_this_vset
      
    }
    
    est_var_this_size <- var_fixed_size_all_vset_all_lags
    results_all_models[[i]] <- est_var_this_size 

  }

  results_all_models <- flatten(flatten(results_all_models))

  column_names <- names(results_all_models[[1]])

  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)
  
  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  names(results_all_models) <- column_names
  
  
  if (rgdp_current_form != "yoy") {
    if (rgdp_current_form == "diff_yoy") {
      
      auxiliary_ts <-  rgdp_yoy_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff_yoy = cv_test_data,
               cv_fcs_diff_yoy = cv_fcs)
      
      # print(cv_objects[["cv_test_data_diff_yoy"]])
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_rec,
                                                auxiliary_ts = auxiliary_ts) ),
          cv_fcs = map(
            cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_rec,
                                             auxiliary_ts = auxiliary_ts) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
    }
    ##### ESTA PARTE HAY QUE CAMBIAR: DIFF
    if (rgdp_current_form == "diff") {
      auxiliary_ts <-  rgdp_level_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff = cv_test_data,
               cv_fcs_diff = cv_fcs)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff, ~ transform_cv_new(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_rec,
                                                auxiliary_ts = auxiliary_ts) ),
          cv_fcs = map(
            cv_fcs_diff,  ~ transform_cv_new(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_rec,
                                             auxiliary_ts = auxiliary_ts) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
      
    }
    
  }

  if(keep_only_white_noise_fs){

    results_all_models <- results_all_models %>% 
      filter(unlist(is_white_noise_fse))

  }
  
  results_all_models <- get_rmses_h_rankings_h(data = results_all_models,
                                              h_max = h_max)
  
  results_all_models <- results_all_models %>% 
    filter_at( vars(starts_with("rank")), any_vars(. <= max_rank)) %>% 
    mutate(cv_vbl_names = map(cv_vbl_names, 1),
           cv_lag = map(cv_lag, 1))
  
  print(paste("Tried", len_lag, "different choices of lags per each combination"))
  print(paste("Number of models analyzed:", model_number))
  print(paste("CV repetitions:", number_of_cv))
  print(paste("Total estimations and fcs:", number_of_cv*model_number))
  
  cv_objects <- results_all_models %>% dplyr::select(cv_vbl_names, cv_lag, cv_errors, cv_test_data,
                                                     cv_fcs) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)

  accu_rankings_models <- results_all_models %>% 
    dplyr::select(cv_vbl_names, cv_lag, 
                  starts_with("rmse"), starts_with("rank"), 
                  overall_cv_white_noise, is_white_noise_fse) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag, 
           wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)

  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects))
  } else {
    return(list(accu_rankings_models = accu_rankings_models))
    
  }
  
}



var_cv <- function(var_data, this_p, this_type = "const", n_cv = 8, h_max = 6, 
                   train_test_marks = NULL,
                   training_length = 20, timetk_idx = TRUE,
                   external_idx = NULL, test_residuals = TRUE) {
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                                             type = "tscv", n = n_cv, h_max = h_max, 
                                             training_length = training_length, 
                                             timetk_idx = timetk_idx, 
                                             external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
  }
  
  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  cv_vbl_names <- list_along(1:n_cv)
  cv_lag <- list_along(1:n_cv)
  cv_is_white_noise <- vector(mode = "logical", length = n_cv)
  
  for (i in seq_along(1:n_cv)) {
    
    this_tra_s <- train_test_dates[[i]]$tra_s
    this_tra_e <- train_test_dates[[i]]$tra_e
    
    this_tes_s <- train_test_dates[[i]]$tes_s
    this_tes_e <- train_test_dates[[i]]$tes_e
    
    training_y <- window(var_data, 
                         start = this_tra_s,
                         end = this_tra_e)

    training_rgdp <- training_y[ , "rgdp"]
    
    test_y <- window(var_data, 
                     start = this_tes_s,
                     end = this_tes_e)
    
    test_rgdp <- test_y[ , "rgdp"]

    this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
    
    if (test_residuals) {
      resid_result <- check_resid_VAR(this_var)
      is_white_noise <- resid_result
    } else {
      is_white_noise <- TRUE
    }
    
    this_fc <- forecast(this_var, h = h_max)
    
    this_rgdp_fc_mean <- this_fc[["forecast"]][["rgdp"]][["mean"]]
    
    fc_error <- test_rgdp - this_rgdp_fc_mean
    
    vbl_names <- colnames(training_y)
    
    lag <- this_p
    
    cv_vbl_names[[i]] <- vbl_names
    cv_lag[[i]] <- lag
    cv_errors[[i]] <- fc_error
    cv_test_data[[i]] <- test_rgdp
    cv_fcs[[i]] <- this_rgdp_fc_mean
    cv_is_white_noise[[i]] <- is_white_noise
    
  }
  
  cv_test_data_mat <- reduce(cv_test_data, rbind)
  cv_fcs_mat <- reduce(cv_fcs, rbind)
  
  # eliminate pesky "out" of it
  dimnames(cv_test_data_mat) <- NULL
  dimnames(cv_fcs_mat) <- NULL
  
  mean_cv_rmse <- fcs_accu(cv_fcs_mat, cv_test_data_mat)
  
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag,
              cv_is_white_noise = cv_is_white_noise))
}
