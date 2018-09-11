source(file = "./R/utils.R")

# search var formerly known as try_sizes_vbls_lags
search_var <- function(var_data, rgdp_yoy_ts, rgdp_level_ts, target_v, vec_size = c(3,4,5), 
                       vec_lags = c(1,2,3,4), pre_selected_v = "",
                       is_cv = FALSE, h_max = 5, n_cv = 8,
                       training_length = 16, maxlag_ccm = 8,
                       bt_factor = 1.4, return_cv = TRUE,
                       max_rank = 30,
                       rgdp_current_form = "yoy") {
  
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
      already_chosen = already_chosen, bt_factor = bt_factor,
      maxlag_ccm = maxlag_ccm)
    
    # print(class("sets_of_other_variables"))
    # print(class(sets_of_other_variables))
    # 
    # print("sets_of_other_variables")
    # print(sets_of_other_variables)
    
    
    
    # 
    #     if (this_size == 3) {
    #       sets_of_other_variables <- list(c("tot"), c("imp"), c("exp"))
    #     }
    #     
    #     if (this_size == 4) {
    #       sets_of_other_variables <- list(c("tot", "ip"), c("imp", "m1"))
    #     }
    
    # len_sets_of_vars <- length(sets_of_other_variables)
    len_sets_of_vars <- ncol(sets_of_other_variables)
    
    var_fixed_size_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      # vec_of_other_vbls <- sets_of_other_variables[[j]]
      vec_of_other_vbls <- sets_of_other_variables[,j]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      for (k in seq.int(1, len_lag)) {
        
        model_number <- model_number + 1
        this_lag <- vec_lags[k]
        
        sub_data = var_data[, vbls_for_var]
        
        sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
        
        this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                          external_idx = sub_data_tk_index, this_p = this_lag,
                          this_type = "const", h_max = h_max,
                          n_cv = n_cv, training_length = training_length)
        
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
  
  
  
  
  results_all_models <- get_rmses_h_rakings_h(data = results_all_models,
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
                  starts_with("rmse"), starts_with("rank")) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)
  
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects))
  } else {
    return(list(accu_rankings_models = accu_rankings_models))
    
  }
  
}



