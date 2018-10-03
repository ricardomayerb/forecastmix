source(file = "./R/utils.R")
library(MTS)
library(vars)


add_rmse_rankings <- function(tbl_with_rmses) {
  rmse_names <- names(tbl_with_rmses)
  rmse_names <- vars_select(names(tbl_with_rmses), starts_with("rmse"))
  rmse_names <- unname(rmse_names)
  
  rankings_as_list <- list_along(rmse_names)
  
  for (i in seq_along(rmse_names)) {
    this_rmse <- paste0("rmse_", i)
    this_rmse_data <- as.matrix(tbl_with_rmses[, this_rmse])
    this_rank <- rank(this_rmse_data)
    rankings_as_list[[i]] <- this_rank
  }
  
  rankings <- as_tibble(reduce(rankings_as_list, cbind))
  names(rankings) <- paste0("rank_", seq_along(rmse_names))
  new_tbl <- as_tibble(cbind(tbl_with_rmses, rankings))
  return(new_tbl)
}



get_rmses_h_rankings_h <- function(data = cv_objects, h_max = 6){
  cv_errors <- data[["cv_errors"]]
  
  all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
  print(is.null(all_rmses))
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
  
  test_object <- try(serial.test(fit_VAR), silent = TRUE)
  # print(class(test_object))
  
  if (class(test_object) == "try-error") {
    # print("Running serial.test threw an error.")
    is_white_noise <- FALSE
  } else {
    pval <- test_object[["serial"]][["p.value"]]
    pval <- unname(pval)
    is_white_noise <- pval > pval_ref
  }

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



max_effective_lag <- function(var_obj) {
  
  vres <- var_obj$restrictions
  
  if (is.null(vres)) {
    # print("VAR does nor have restriction matrix")
    nominal_lag <- var_obj$p
    return(nominal_lag)
  }
  
  
  csum <- colSums(vres[,1:ncol(vres)])
  names_unrest <- names(csum[csum > 0])
  names_unrest_num <-  as.numeric(map_chr(str_extract_all(names_unrest, "\\d"),
                                          ~ paste(.x, collapse = "")))
  max_lag_unrest <- max(names_unrest_num, na.rm = TRUE)
  return(max_lag_unrest)
}


# search var formerly known as try_sizes_vbls_lags
# then it was modified to work on single size choice

search_var_one_size <- function(var_data, rgdp_yoy_ts, rgdp_level_ts, target_v, 
                       var_size, 
                       vec_lags = c(1,2,3,4), pre_selected_v = "",
                       is_cv = FALSE, h_max = 5, 
                       n_cv = 8,
                       training_length = 24,
                       return_cv = TRUE,
                       max_rank = 30,
                       rgdp_current_form = "yoy",
                       check_residuals_full_sample = TRUE,
                       check_residuals_cv = TRUE,
                       white_noise_target_ratio = 1,
                       keep_only_white_noise_fs = TRUE,
                       max_p_for_estimation = 7,
                       restrict_by_signif = TRUE,
                       t_tresh = 1.65,
                       keep_varest = FALSE) {

  all_names <- colnames(var_data)
  models_with_cv_excercises <- 0
  models_with_eqn_dropping <- 0
  binding_max_p <- 0
  
  if (!restrict_by_signif) {
    t_tresh <- NA
  }
  
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags

  model_number <- 0
  models_unstable <- 0
  models_non_white_fs <- 0
    
  already_chosen <- c(target_v, pre_selected_v)
  already_chosen <- already_chosen[already_chosen != ""]
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- var_size - len_already_chosen
    
  sets_of_other_variables <- get_sets_of_variables(
      df = var_data, this_size = var_size, all_variables = all_names, 
      already_chosen = already_chosen)
    
  len_sets_of_vars <- ncol(sets_of_other_variables)
    
  var_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
  for (j in seq.int(1, len_sets_of_vars)) {
      
      # vec_of_other_vbls <- sets_of_other_variables[[j]]
      vec_of_other_vbls <- sets_of_other_variables[,j]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      sub_data = var_data[, vbls_for_var]
      sub_data = na.omit(sub_data)
      sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
      
      if (is.numeric(vec_lags)) {
        p_for_estimation <- unique(vec_lags)
      }
      
      if (is.character(vec_lags)) {
        lag_sel_method <- "info"
        sel <- vars::VARselect(sub_data, type = "const", lag.max = 16)
        sel_criteria <- sel$selection
        # print("sel_criteria")
        # print(sel_criteria)
        cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                       fpe = "FPE(n)")
        this_cri <- cri_names[vec_lags]
        named_lags <- sel_criteria[this_cri]
        # print("named_lags")
        # print(named_lags)
        p_for_estimation <- unique(unname(named_lags))
        max_found_p <- max(p_for_estimation)
        too_high_p <- p_for_estimation > max_p_for_estimation
        # print(any(too_high_p))
        p_for_estimation[too_high_p] <- max_p_for_estimation 
        # print(p_for_estimation)
        
        if(any(too_high_p)) {
          
          # print(paste0("One or more lags found larger than max_p_for_estimation (",
          #              max_p_for_estimation,"). Replace them with max instead"))
          
          binding_max_p <- binding_max_p + 1
        }
        
      }
      
      if (is.numeric(vec_lags)) {
        lag_sel_method <- "manual"
      }
      
      len_lag <- length(p_for_estimation)
      var_fixed_vset_all_lags <- list_along(seq(len_lag))
      fcs_fixed_vset_all_lags <- list_along(seq(len_lag))

      for (k in seq.int(1, len_lag)) {
        
        this_lag <- p_for_estimation[k]
        
        full_sample_var <- vars::VAR(sub_data, type = "const", p = this_lag)
        model_number <- model_number + 1
        
        if(restrict_by_signif){
          full_sample_var <- try(vars::restrict(full_sample_var, method = "ser", 
                                                thresh = t_tresh), silent = TRUE)
        }
        
        if (class(full_sample_var) == "try-error") {
          # print(paste("One or more equations in", paste(colnames(sub_data), collapse = " "),  
          #             ",have no coefficients passing t-treshold =", t_tresh))
          some_eqn_drop <- TRUE
          models_with_eqn_dropping <- models_with_eqn_dropping + 1
          is_stable <- FALSE
          is_white_noise_fs <- FALSE
          this_cv[["t_treshold"]] <-  t_tresh
          this_cv[["lag_sel_method"]] <- lag_sel_method
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
        } 
        
        if (!class(full_sample_var) == "try-error") {
          var_restrictions <- full_sample_var$restrictions
          some_eqn_drop <- FALSE
          this_root <- vars::roots(full_sample_var)
          is_stable <- all(this_root < 1)
          if (!is_stable) {
            # print("Current VAR not stable. No CV analysis will be done")
            # print(paste("Roots are", paste(this_root, collapse = ", ")))
            models_unstable <- models_unstable + 1 
            # print(paste("Unstable models so far:", models_unstable))
          }
          if (is_stable & check_residuals_full_sample) {
            is_white_noise_fs <- check_resid_VAR(full_sample_var)
            # print("is_white_noise_fs")
            # print(is_white_noise_fs)
            if (!is_white_noise_fs) {
              # print("foo")
              models_non_white_fs <- models_non_white_fs + 1
            }
          } else {
            is_white_noise_fs <- TRUE
          }
          
          if (is_white_noise_fs & is_stable) {
            models_with_cv_excercises <- models_with_cv_excercises + 1
            # print("var_restrictions")
            # print(var_restrictions)
            this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                              external_idx = sub_data_tk_index, this_p = this_lag,
                              this_type = "const", h_max = h_max,
                              n_cv = n_cv, training_length = training_length, 
                              test_residuals = check_residuals_cv,
                              full_sample_resmat = var_restrictions)
            cv_num_of_white_noises <- sum(this_cv[["cv_is_white_noise"]])
            ratio_of_white_noises <- cv_num_of_white_noises/n_cv
            overall_cv_white_noise <- ratio_of_white_noises >= white_noise_target_ratio
            this_cv[["overall_cv_white_noise"]] <- overall_cv_white_noise
            this_cv[["is_white_noise_fse"]] <- TRUE
            this_cv[["is_stable"]] <- TRUE
            this_cv[["t_treshold"]] <- t_tresh
            this_cv[["lag_sel_method"]] <- lag_sel_method
            if (keep_varest) {
              this_cv[["full_sample_varest"]] <- full_sample_var
            }
            
          }
        }
        
        if ( (!is_white_noise_fs) | (!is_stable) | some_eqn_drop) {
          
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
          this_cv[["t_treshold"]] <- t_tresh
          this_cv[["lag_sel_method"]] <- lag_sel_method
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
        }
        
        this_cv[["some_eqn_drop"]] <- some_eqn_drop
        
        # print(paste0("j = ", j, ", k = ", k))
        # print("names(this_cv)")
        # print(names(this_cv))
        # print("this_cv")
        # print(this_cv)
        
        var_fixed_vset_all_lags[[k]] <- this_cv
        
      }
      
      # print("1")
      var_all_vset_all_lags[[j]] <- var_fixed_vset_all_lags
      
  }
  # print("2")
    
  results_all_models <- flatten(var_all_vset_all_lags)
  
  # print("3")
  
  
  
  column_names <- names(results_all_models[[1]])

  
  # print("names(results_all_models)")
  # print(names(results_all_models))
  # 
  # print("length(results_all_models)")
  # print(length(results_all_models))
  # 
  # print("column_names")
  # print(column_names)
  # 
  # print("4")
  
  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)
  
  # print("5")
  
  
  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  
  # print("6")
  
  names(results_all_models) <- column_names
  
  # print("7")
  
  
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
  
  
  
  print(paste("Number of models analyzed:", model_number))
  print(paste("Total models dropped after significance restrictions applied:", 
              models_with_eqn_dropping, "out of", model_number))
  print(paste("Total significant models unstable:", 
              models_unstable, "out of", model_number - models_with_eqn_dropping))
  print(paste("Total significant stable models, but with non-white residuals:", 
              models_non_white_fs, "out of", model_number -
                models_with_eqn_dropping - models_unstable ))
  print(paste("As a result,  performed CV on", models_with_cv_excercises, "of them"))
  print(paste("CV repetitions:", number_of_cv))
  print(paste("Total estimations (full sample + cv rounds):", 
              number_of_cv*models_with_cv_excercises + model_number))
  print(paste("Total times p exceeded max_p_for_e:", binding_max_p))
  
  # print("results_all_models")
  # print(results_all_models)
  # print("length(results_all_models)")
  # print(length(results_all_models))
  # print("nrow(results_all_models)")
  # print(nrow(results_all_models))
  
  if(nrow(results_all_models) > 0) {
    
    # print("names(results_all_models)")
    # print(names(results_all_models))
    
    results_all_models <- get_rmses_h_rankings_h(data = results_all_models,
                                                 h_max = h_max)
    
    results_all_models <- results_all_models %>% 
      filter_at( vars(starts_with("rank")), any_vars(. <= max_rank)) %>% 
      mutate(cv_vbl_names = map(cv_vbl_names, 1),
             cv_lag = map(cv_lag, 1))
    

    cv_objects <- results_all_models %>% 
      dplyr::select(cv_vbl_names, cv_lag, 
                    cv_errors, cv_test_data, cv_fcs) %>% 
      rename(variables = cv_vbl_names, lags = cv_lag)
    
    
    if (keep_varest) {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse,
                      full_sample_varest) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    } else {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    }
    
    accu_rankings_models <- accu_rankings_models %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y))
      )
    
    accu_rankings_models <- accu_rankings_models %>% 
      dplyr::select(short_name, everything())
  }
  
  if(nrow(results_all_models) == 0){
    print("No models passed all criteria. Will return an empty list or lists.")
    results_all_models <- list()
    accu_rankings_models <- list()
    cv_objects <- list()
  }
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects))
  } else {
    return(list(accu_rankings_models = accu_rankings_models))
  }
}



stack_models <- function(models_list) {
  
  all_models <- as_tibble(reduce(models_list, rbind)) %>%
    dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
    dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
    mutate(lags = unlist(lags))
  
  all_models <- all_models %>% 
    mutate(short_name = map2(variables, lags,
                             ~ make_model_name(variables = .x, lags = .y)),
           short_name = unlist(short_name),
           var_size = map_dbl(variables, length)
    )
  
  all_models <- all_models %>% dplyr::distinct(short_name, .keep_all = TRUE)
  all_models_ranked <- add_rmse_rankings(all_models)
  
  return(all_models_ranked)
}




var_cv <- function(var_data, this_p, this_type = "const", 
                   n_cv = 8, h_max = 6, 
                   train_test_marks = NULL,
                   training_length = 20, timetk_idx = TRUE,
                   external_idx = NULL, test_residuals = TRUE,
                   full_sample_resmat = NULL) {
  
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
  
  # print("VAR data before cv")
  # print(var_data)
  
  total_obs <- nrow(var_data)
  cv_obs_used <- n_cv + training_length + h_max
  
  if (total_obs < cv_obs_used) {
    print(paste("Warning: For selected variables, balanced sample has only", 
                total_obs, "obs. Fixed-length cv needs", cv_obs_used, " obs."))
    
    print(paste0("Forecast length: ", h_max, ". Training length: ", 
                 training_length, ". CV rounds: ", n_cv, ". Total: ", 
                 n_cv + training_length + h_max))
  }
  
  
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
    this_var <- vars::restrict(this_var, method = "manual", 
                               resmat = full_sample_resmat)
    this_effective_lag <- max_effective_lag(this_var)
    
    # print(paste("nrow(training_y):", nrow(training_y), ", nom lag:", this_p, 
    #             ", eff lag:", this_effective_lag ,
    #             ". Start:", paste(start(training_y), collapse = "_"),
    #             ". End:", paste(end(training_y), collapse = "_")))
    # 
    # print(paste("nrow(test_y):", nrow(test_y),
    #             ". Start:", paste(start(test_y), collapse = "_"),
    #             ". End:", paste(end(test_y), collapse = "_")))
    
    
    # print(this_var)
    
    
    if (test_residuals) {
      resid_result <- check_resid_VAR(this_var)
      if (is.na(resid_result)) {
        print(paste("Error in resid test. Lag is", this_p, ", variables are", 
              paste(colnames(var_data), collapse = "_")))
      }
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
  # if(is.na(mean_cv_rmse)) {mean_cv_rmse <- Inf}
  
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag,
              cv_is_white_noise = cv_is_white_noise))
}




variable_freq_by_n <- function(tbl_of_models, h_max = 8, max_rank = 10, 
                               n_freq = 4, is_wide = FALSE) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if ("full_sample_varest" %in% names(tbl_of_models)) {
    tbl_of_models <-  tbl_of_models %>% 
      dplyr::select(-full_sample_varest)
  }
  
  if (is_wide) {
    tbl_of_models <- tbl_of_models %>% 
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse)) %>% 
      ungroup()
  }
  
  vec_of_rmse_h <- sort(unique(tbl_of_models$rmse_h))
  
  list_best <- map(vec_of_rmse_h, 
                   ~ tbl_of_models %>% 
                     filter(rmse_h == .x, rank_h < max_rank +1 ) %>% 
                     dplyr::select("variables") %>% 
                     unlist() %>% 
                     table() %>% 
                     as_tibble() %>% 
                     arrange(desc(n)) %>% 
                     rename(., vbl = .)
  ) 
  
  
  tbl_best <- reduce(list_best, left_join, by = c("vbl"))
  names(tbl_best) <- c("vbl", paste("h", seq(h_max), sep = "_"))
  
  tbl_best <- tbl_best %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE))
  
  by_total <- tbl_best %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1 <- tbl_best %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both <- unique(c(by_h1$vbl, by_total$vbl))
  
  list_best_lags <- map(vec_of_rmse_h, 
                        ~ tbl_of_models %>% 
                          filter(rmse_h == .x, rank_h < max_rank +1 ) %>% 
                          dplyr::select("lags") %>% 
                          unlist() %>% 
                          table() %>% 
                          as_tibble() %>% 
                          arrange(desc(n)) %>% 
                          rename(., max_lag = .)
  ) 
  
  
  tbl_best_lags <- reduce(list_best_lags, left_join, by = c("max_lag"))
  names(tbl_best_lags) <- c("max_lag", paste("h", seq(h_max), sep = "_"))
  
  tbl_best_lags <- tbl_best_lags %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE))
  
  by_total_lags <- tbl_best_lags %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1_lags <- tbl_best_lags %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both_lags <- unique(c(by_h1_lags$max_lag, by_total_lags$max_lag))
  
  return( list(vbl_freqs_by_h = tbl_best, vbl_top_h1_total = both,
               lags_freqs_by_h = tbl_best_lags, lags_top_h1_total = both_lags))
}







