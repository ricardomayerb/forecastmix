source('./R/VAR_functions.R')

var_search <- function(country, 
                       sizes, 
                       forecast_exercise_year, 
                       forecast_exercise_number,
                       fc_horizon,
                       target_variable = c("rgdp"),
                       other_prechosen_variables = c(""),
                       vec_lags = c(1, 2, 3, 4, 5, 6),
                       add_aic_bic_hq_fpe_lags =  FALSE,
                       vec_freq_limit = list("none", "none", 15, 10),
                       restrict_by_signif = TRUE,
                       t_tresh = c(2, 2, 2, 2),
                       number_of_cv = 8,
                       train_span = 25,
                       ret_cv = TRUE,
                       max_rank_some_h =50
                       ) {
  
  tic(msg = "Total time for this country")
  
  # file paths
  excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                            "_exercise_", forecast_exercise_number, "/")
  
  output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                        forecast_exercise_year, 
                        "_exercise_", forecast_exercise_number, "/")
  
  country_data_ts <- get_raw_data_ts(country = country, data_path = excel_data_path)
  external_data_ts <- get_raw_external_data_ts(data_path = excel_data_path)
  data_ts <- country_data_ts
  
  rgdp_level_ts <- data_ts[, "rgdp"]
  rgdp_level_ts <- na.omit(rgdp_level_ts)
  rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)
  
  print(paste0("This country: ", country))
  print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
  print("Names of variables: ")
  print(colnames(data_ts))
  
  tic()
  print("Finding and applying stationary transformations to all variables")
  reco_all_variables <- find_statio_diffs(data_ts, country)
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
  
  saveRDS(VAR_data_for_estimation, 
          paste0(output_path, "VAR_data_", country, ".rds"))
  
  
  freq_sel_vbls <- colnames(VAR_data_for_estimation) # same as freq_limit = 'none'
  
  
  if (train_span + fc_horizon + number_of_cv > nrow(VAR_data_for_estimation)) {
    print("not enough obs")
    stop()
  }
  
  per_size_results <- list_along(sizes)
  
  tic(msg = "Finish var search")
  
  for (i in seq(length(sizes))) {
    
    this_size <- sizes[i]
    this_t_tresh <- t_tresh[i]
    this_freq_limit <- vec_freq_limit[[i]]
    
    if (i < length(sizes)) {
      next_freq_limit <- vec_freq_limit[[i + 1]]
    }
    
    if (this_freq_limit == "none") {
      print("Using all variables")
      
      this_VAR_data <- VAR_data_for_estimation
    }
    
    print(paste0("Starting the estimation of VAR with ", this_size," variables"))
    
    if (is.numeric(this_freq_limit)) {
      print("Using this subset of variables: ")
      print(freq_sel_vbls)
      
      this_VAR_data <- VAR_data_for_estimation[, freq_sel_vbls]
    }
    
    tic(msg = paste0("Finished VARs with ", this_size, " variables"))
    
    var_res <- search_var_one_size(
      var_size = this_size,
      vec_lags = vec_lags,
      var_data = this_VAR_data,
      rgdp_level_ts = rgdp_level_ts,
      rgdp_yoy_ts = rgdp_yoy_ts,
      target_v = target_variable,
      pre_selected_v = other_prechosen_variables,
      is_cv = TRUE,
      training_length = train_span,
      h_max = fc_horizon,
      n_cv = number_of_cv,
      return_cv = ret_cv,
      rgdp_current_form = rgdp_rec,
      max_rank = max_rank_some_h,
      check_residuals_cv = TRUE,
      check_residuals_full_sample = TRUE,
      restrict_by_signif = restrict_by_signif,
      t_tresh = this_t_tresh,
      max_p_for_estimation = 12,
      add_info_based_lags = add_aic_bic_hq_fpe_lags)
    
    if (i < length(sizes)) {
      if(this_freq_limit == "none") {
        f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                     h_max = fc_horizon, max_rank = 10,
                                     n_freq = 50, is_wide = TRUE)
      }
      
      if (is.numeric(next_freq_limit)) {
        f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                     h_max = fc_horizon, max_rank = 10,
                                     n_freq = next_freq_limit, is_wide = TRUE)
        freq_sel_vbls <- f_vbls$vbl_multi
      }
    } 
    
    if (i == length(sizes)) {
      f_vbls <- variable_freq_by_n(var_res[["accu_rankings_models"]], 
                                   h_max = fc_horizon, max_rank = 10,
                                   n_freq = next_freq_limit, is_wide = TRUE)
    }
    
    
    file_suffix <- paste0("_size_", this_size, "_fqlim_", this_freq_limit, "_t_", this_t_tresh, ".rds")
    filename <- paste0("var_results_", country, file_suffix)
    saveRDS(var_res, paste0(output_path, filename))
    
    per_size_results[[i]] <- var_res
    
    toc()
  }
  
  toc()
  
  bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
  
  consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
  
  allsizes <- paste(sizes, collapse = "")
  allthresh <- paste(t_tresh, collapse = "")
  allfqlim <- paste(vec_freq_limit, collapse = "")
  
  file_suffix_all_sizes <-  paste0("_sizes_", allsizes, "_fqlims_", allfqlim,
                                   "_t_", allthresh, ".rds")
  
  filename <- paste0("var_results_", country, file_suffix_all_sizes)
  saveRDS(consolidated_var_res, paste0(output_path, filename))
  
  return(consolidated_var_res)
}


foo <- var_search(country = "Bolivia",
                  sizes = c(2, 3, 4, 5),
                  forecast_exercise_year = 2018, 
                  forecast_exercise_number = 2,
                  fc_horizon = 7) 











