source(file = "./R/utils.R")
library(MTS)
library(vars)

add_prechosen_for_this_step <- function(search_plan, step_index, prechosen_so_far, 
                                        models_table, 
                                        max_rank_some_h_for_freq = 50,
                                        discard_previous = FALSE,
                                        best_n_VAR_for_preselecting = 10) {
  
  this_search_step <- search_plan[[step_index]]
  # print("In addprechosen, this_search_step:")
  # print(this_search_step)
  
  this_size <- this_search_step[["size"]]
  
  # print("Preparing possible pre-chosen variables (other than the target variable)")
  
  if (is.null(this_search_step$manually_prechosen)) {
    this_manually_prechosen <- c("")
  } else {
    this_manually_prechosen <- this_search_step[["manually_prechosen"]]
  }
  
  if (this_manually_prechosen == c("")) {
    # print("No manually pre-chosen variables for this step")
  }
  
  all_prechosen_previous_step <- prechosen_so_far[[step_index - 1]]
  
  if (is.null(all_prechosen_previous_step)) {
    # print("No pre-chosen variables from previous step")
    all_prechosen_previous_step <- c("")
  }
  
  previous_and_manual <- c(this_manually_prechosen, all_prechosen_previous_step)

  
  if (all(previous_and_manual == "")) {
    previous_and_manual <- c("")
  } else {
    previous_and_manual <- unique(previous_and_manual)
    previous_and_manual <- previous_and_manual[previous_and_manual != ""]
  }
  
  if ((!is.null(all_prechosen_previous_step))  & (this_manually_prechosen != c("")) ) {
    all_prechosen_previous_step <- map(all_prechosen_previous_step, ~ c(.x, this_manually_prechosen))
  }
  
  # print("Prechosen, including manually specified and from previous steps:")
  # print(all_prechosen_previous_step)
  
  
  if (is.null(this_search_step$n_new_prechosen)) {
    n_new_prechosen <- 0
    auto_prechosen_at_this_step <- c("")
    all_prechosen_this_step <- this_manually_prechosen
    updated_prechosen_so_far <- prechosen_so_far
    updated_prechosen_so_far[[step_index]] <- all_prechosen_this_step 
    return(updated_prechosen_so_far)
  } else {
    n_new_prechosen <- this_search_step[["n_new_prechosen"]]
  }
  
  n_sets_of_previous_prechosen <- length(all_prechosen_previous_step)
  
  updated_prechosen_so_far <- prechosen_so_far
  
  apc <- 1
  
  # print("unlist version of all_prechosen_previous_step")
  # print(unlist(all_prechosen_previous_step))
  
  vec_all_prechosen_previous_step <- all_prechosen_previous_step
  
  
  for (ppc in seq(1, n_sets_of_previous_prechosen)) {
    
    this_previous_prechosen <- all_prechosen_previous_step[ppc]
    this_previous_prechosen <- this_previous_prechosen[this_previous_prechosen != ""]
    
    new_prechosen_list <- list_along(seq(1, n_new_prechosen))
    
    for (new_pc in seq(1, n_new_prechosen)) {
      
      n_freq_for_preselecting <- 2*this_size
      
      print(paste0("step = ", step_index ,", previous = ", ppc, ", new = ", new_pc, " and this_previous_prechosen:"))
      print(this_previous_prechosen)
      
      position_of_new_prechosen <- 1 + new_pc + length(this_previous_prechosen)
      
      # print("position_of_new_prechosen")
      # print(position_of_new_prechosen)
      
      # print("models_table")
      # print(models_table)
      
      f_vbls <- variable_freq_by_n(models_table,
                                   h_max = fc_horizon,
                                   max_rank = max_rank_some_h_for_freq,
                                   n_freq = n_freq_for_preselecting,
                                   is_wide = TRUE,
                                   max_small_rank = best_n_VAR_for_preselecting)
      
      vbl_table <- f_vbls$vbl_freqs_by_h %>% arrange(desc(total_n))
      
      print("vbl_table")
      print(vbl_table)
      
      vbl_table_by_total <- vbl_table %>% 
        arrange(desc(total_n)) %>% 
        dplyr::select(vbl) %>% 
        dplyr::filter(row_number() <= n_freq_for_preselecting)
      
      vbl_by_total <-  vbl_table_by_total$vbl
      
      is_vbl_by_total_in_pc <- vbl_by_total %in% vec_all_prechosen_previous_step
      vbl_by_total_not_in_pc <- vbl_by_total[!is_vbl_by_total_in_pc]
      
      new_prechosen <- vbl_by_total_not_in_pc[new_pc + 1] 
      
      # print("vbl_by_total")
      # print(vbl_by_total)
      # print("vbl_by_total_not_in_pc")
      # print(vbl_by_total_not_in_pc)
      # print("new_prechosen")
      # print(new_prechosen)
      
      new_prechosen_list[[new_pc]] <- new_prechosen
      
      this_prechosen_variables <- c(this_previous_prechosen, new_prechosen)
      this_prechosen_variables <- this_prechosen_variables[this_prechosen_variables != ""]
      
      # print("this_prechosen_variables")
      # print(this_prechosen_variables)
      
      updated_prechosen_so_far[[step_index]][[apc]] <- this_prechosen_variables
      
      apc <- apc + 1
      
    }
    
  }
  
  return(updated_prechosen_so_far)
}


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
  
  # print(paste("We have", n_passing_vbls, "free variables, to fill the remaining", len_other_vbls,
  #             "variables in the VAR.Total possible combinations :",
  #             choose(n_passing_vbls, len_other_vbls)))
  
  combinations <- combn(passing_not_alr_chosen, len_other_vbls)
}


make_exomat <- function(exodata, exov, exo_lag) {
  
  n_exo <- length(exov)
  
  if (n_exo == 0) {
    exodata <- NULL
    return(exodata)
  }
  
  if (n_exo > 0) {
    exo_and_lags_list <- list_along(seq(1, n_exo))
    names_exo_and_lags_list <- list_along(seq(1, n_exo))
    for (ex in 1:n_exo) {
      this_exoname <- exov[ex]
      if (n_exo == 1) {
        this_exovar <- exodata
      } else {
        this_exovar <- exodata[, this_exoname]
      }
      # print(this_exoname)
      # print(this_exovar)
      
      one_exo_with_lags_list <- list_along(seq(0, exo_lag))
      # print("one_exo_with_lags_list")
      # print(one_exo_with_lags_list)
      for (exlag  in seq(0, exo_lag)) {
        # print(paste0("ex"))
        this_lag_exo <- lag.xts(this_exovar, k = exlag)
        one_exo_with_lags_list[[exlag + 1]] <- this_lag_exo
      }
      one_exo_with_lags <- reduce(one_exo_with_lags_list, ts.union)
      if (!is.null(dim(one_exo_with_lags))) {
        this_exolags_names <- paste(this_exoname, seq(0, exo_lag), sep = "_")
        colnames(one_exo_with_lags) <- this_exolags_names
      } else {
        this_exolags_names <- paste(this_exoname, seq(0, exo_lag), sep = "_")
        names(one_exo_with_lags) <- this_exolags_names
      }
      exo_and_lags_list[[ex]] <- one_exo_with_lags
      names_exo_and_lags_list[[ex]] <- this_exolags_names
    }
    exo_and_lags <- reduce(exo_and_lags_list, ts.union)
    names_exo_and_lags <- reduce(names_exo_and_lags_list, c)
    if(is.null(dim(exo_and_lags))){
      names(exo_and_lags) <- names_exo_and_lags
    } else {
      colnames(exo_and_lags) <- names_exo_and_lags
    }
  }
  
  return(exo_and_lags)
  
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



var_search <- function(country, 
                       search_plan,
                       forecast_exercise_year, 
                       forecast_exercise_number,
                       fc_horizon,
                       target_variable = c("rgdp"),
                       default_t_treshold = 1.65,
                       default_lags = c(2, 3, 4, 5),
                       add_aic_bic_hq_fpe_lags =  FALSE,
                       restrict_by_signif = TRUE,
                       number_of_cv = 8,
                       train_span = 25,
                       ret_cv = TRUE,
                       max_rank_some_h =50,
                       max_rank_some_h_for_freq = 50,
                       max_small_rank = 3,
                       results_file_name = NULL,
                       names_exogenous = c(""),
                       exo_lag = NULL,
                       combn_already_tried = NULL
) {
  
  initial_time <- Sys.time()
  tic(msg = "Total time for this country")
  
  
  # file paths
  excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                            "_exercise_", forecast_exercise_number, "/")
  
  output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                        forecast_exercise_year, 
                        "_exercise_", forecast_exercise_number, "/")
  
  country_data_ts <- get_raw_data_ts(country = country, data_path = excel_data_path)
  external_data_ts <- get_raw_external_data_ts(data_path = excel_data_path)
  # data_ts <- country_data_ts
  
  # print("country_data_ts")
  # print(country_data_ts)
  # print("external_data_ts")
  # print(external_data_ts)
  
  data_ts <- ts.union(country_data_ts, external_data_ts)
  colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))
  # print("data_ts")
  # print(data_ts)
  
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
  
  max_common_train_span_guaranted <- nrow(na.omit(VAR_data_for_estimation)) - fc_horizon - number_of_cv
  print(paste0("Taking all variables together, maximum common training span is ",
               max_common_train_span_guaranted))
  upper_bound_for_train_span <- length(na.omit(VAR_data_for_estimation[ , "rgdp"])) - fc_horizon - number_of_cv
  print(paste0("For variables encompasing rgdp extent, max training span is ",
               upper_bound_for_train_span))
  
  if (train_span == "common_max") {
    print(paste0("Using common_max span for training sets: ", max_common_train_span_guaranted))
    train_span <- max_common_train_span_guaranted
  }
  
  saveRDS(VAR_data_for_estimation, 
          paste0(output_path, "VAR_data_", country, ".rds"))
  
  n_steps <- length(search_plan)
  
  per_size_results <- list_along(1:n_steps)
  f_vbls_list <- list_along(1:n_steps)
  current_consolidated_models_list <- list_along(1:n_steps)
  cv_objects_list <- list_along(1:n_steps)
  prechosen_variables_at_each_step <- list_along(1:n_steps)
  all_prechosen_variables_at_each_step <- list_along(seq(1, n_steps))
  
  tic(msg = "Finish var search")
  
  for (i in seq(1, n_steps)) {
    
    set_of_prechosen_to_use <- NULL
    
    n_searches_for_this_size <- 0
    this_search_step <- search_plan[[i]]
    this_size <- this_search_step[["size"]]
    this_selection_type <- this_search_step[["vbl_selection_type"]]
    
    print("")
    print("--------------------------------------")
    print("")
    print(paste0("Starting the estimation of VAR with ", this_size," vbls"))
    print(paste0("Variable selection type for this size: ", this_selection_type))
    
    if (is.null(this_search_step$lags)) {
      this_lags <- default_lags
    } else 
    {
      this_lags <- this_search_step[["lags"]]
    }
    
    # print("This lags = ")
    # print(this_lags)
    
    
    if (is.null(this_search_step$t_treshold)) {
      this_t_tresh <- default_t_treshold
    } else {
      this_t_tresh <- this_search_step[["t_treshold"]]
    }
    
    # print("This t tresh = ")
    # print(this_t_tresh)

    if (this_selection_type == "none") {
      print("Using all variables without pre-chosen variables")
      this_VAR_data <- VAR_data_for_estimation
      this_prechosen_variables <- NULL
      f_vbls <- NULL
      new_select_vbls <- colnames(VAR_data_for_estimation) 
      vbls_top_small <- NA
      by_total_not_in_tsm <- NA
    }
    
    
    if (i > 1 & is.numeric(this_selection_type)) {
      f_vbls <- variable_freq_by_n(current_consolidated_models, 
                                   h_max = fc_horizon,
                                   max_rank = max_rank_some_h_for_freq,
                                   n_freq = this_selection_type, 
                                   is_wide = TRUE,
                                   mas_small_rank)
      freq_sel_vbls_by_multi <- f_vbls$vbl_multi
      vbls_top_small <- f_vbls$variables_in_top_small
      
      if(length(vbls_top_small) > this_selection_type) {
        print(paste0("Number of best-n-VAR variables (", length(vbls_top_small), 
                     "exceeds next_freq_limit (",  this_selection_type, "). We will preserve 
        the integrity of best VARs and use those",  length(vbls_top_small), " variables in next size." )  )
        
        print(paste0("If you want to decrease the number of variables, reduce the mas_small_rank 
                     parameter to some value lower than ", max_small_rank))
        
        vbls_top_small <- vbls_top_small
      }
      
      by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
      
      by_total_na <- is.na(by_total_not_in_tsm)
      
      by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
      
      n_gap_vbls <- this_selection_type - length(vbls_top_small)
      
      if (n_gap_vbls > 0) {
        extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
      } else {
        extra_vbls <- c()
      }
      
      new_select_vbls <- c(vbls_top_small, extra_vbls)
      
      print("Using this subset of variables: ")
      print(new_select_vbls)
      
      this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
    }
    
    if (this_selection_type == "manually_prechosen_variables") {
      print("Using automatic incrementally added pre-chosen variables")
      print("This option does not automatically inherits prechosen variables from previous steps")
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      

      # print("before addig this step manual variables, we have:")
      # print(all_prechosen_variables_at_each_step)

      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      # print("And after add_prechosen_for_this_step, the updated version of it is")
      # print(updated_list_of_prechosen)
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      # print("And in this step we will add the following variables as prechosen, one at the time:")
      # print(set_of_prechosen_to_use)

    }
    
    if (this_selection_type == "incremental_auto_prechosen") {
      
      print("Using automatic incrementally added pre-chosen variables")
      
      print("Inherits from previous step, the following prechosen variables:")
      print(all_prechosen_variables_at_each_step[[i - 1]])
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      
      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      print("And in this step we will add the following variables as prechosen, one at the time:")
      print(set_of_prechosen_to_use)
      
      # print("all_prechosen_variables_at_each_step")
      # print(all_prechosen_variables_at_each_step)
      
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
    }
    
    add_augmented_models <- this_search_step[["add_augmented_models"]]
    
    if (is.null(add_augmented_models)) {
      add_augmented_models <- FALSE
    }
    
    if (add_augmented_models) {
      
      n_best_per_h <- 2
      rmse_names <- paste("rmse", seq(fc_horizon), sep = "_")
      
      print(paste0(
        "Also including one-extra-variable augmented versions of the best ",
        n_best_per_h, " size-",search_plan[[i-1]]$size, "-VAR of each horizon",
        " (including ties).")
        )
      
      potential_models <- current_consolidated_models_list[[i-1]]
      
      potential_models <- potential_models %>% 
        gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
        dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
        group_by(rmse_h) %>% 
        arrange(rmse_h, rmse) %>% 
        mutate(rank_h = rank(rmse),
               nth_rmse = nth(rmse, n_best_per_h)) %>% 
        ungroup()
      
      print("potential_models")
      print(potential_models)
      
      vec_of_rmse_h <- sort(unique(potential_models$rmse_h))
      
      print("vec_of_rmse_h")
      print(vec_of_rmse_h)
      
      list_best <- map(vec_of_rmse_h, 
                       ~ potential_models %>% 
                         filter(rmse_h == .x, rmse <= nth_rmse)
                       ) 
      
      print("list_best")
      print(list_best)
      
      break
      
    }
    
    tic(msg = paste0("Finished VARs with ", this_size, " variables"))
    
    if (!is.null(set_of_prechosen_to_use)) {
      # print("Inside the prechose vbls loop:")
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
      
      var_res_each_prechosen <- list_along(seq(1, length(set_of_prechosen_to_use)))
      
      for (ptu in seq(1, length(set_of_prechosen_to_use))) {
        print(paste0("new prechosen ", ptu, " of ", length(set_of_prechosen_to_use)))
        
        this_prechosen_variables <- set_of_prechosen_to_use[ptu][[1]]
        
        print("pre-chosen variables to be use in the coming VAR search:")
        print(this_prechosen_variables)
        
        print("is.list(this_prechosen_variables)")
        print(is.list(this_prechosen_variables))
        
        var_res <- search_var_one_size(
          var_size = this_size,
          vec_lags = this_lags,
          var_data = this_VAR_data,
          rgdp_level_ts = rgdp_level_ts,
          rgdp_yoy_ts = rgdp_yoy_ts,
          target_v = target_variable,
          pre_selected_v = this_prechosen_variables,
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
          add_info_based_lags = add_aic_bic_hq_fpe_lags,
          names_exogenous = names_exogenous,
          exo_lag = exo_lag)
        
        # print("names(var_res)")
        # 
        # print(names(var_res))
        
        var_res[["explored_size"]] <- this_size
        var_res[["used_prechosen"]] <- this_prechosen_variables
        
        var_res_each_prechosen[[ptu]] <- var_res
        
        n_searches_for_this_size <- n_searches_for_this_size + 1
        print("N of searches for this size:")
        print(n_searches_for_this_size)
      }
      
      all_models <- map(var_res_each_prechosen, "accu_rankings_models")
      all_models <- reduce(all_models, rbind)
      
      all_cv_obj <- map(var_res_each_prechosen, "cv_objects")
      all_cv_obj <- reduce(all_cv_obj, rbind)
      
      var_res <- list(accu_rankings_models = all_models,
                      cv_objects = all_cv_obj)
      
    }
    
    if (is.null(set_of_prechosen_to_use)) {
      var_res <- search_var_one_size(
        var_size = this_size,
        vec_lags = this_lags,
        var_data = this_VAR_data,
        rgdp_level_ts = rgdp_level_ts,
        rgdp_yoy_ts = rgdp_yoy_ts,
        target_v = target_variable,
        pre_selected_v = this_prechosen_variables,
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
        add_info_based_lags = add_aic_bic_hq_fpe_lags, 
        names_exogenous = names_exogenous, 
        exo_lag = exo_lag)
      
      n_searches_for_this_size <- n_searches_for_this_size + 1
      print("N of searches for this size:")
      print(n_searches_for_this_size)
      
      var_res[["explored_size"]] <- this_size
      var_res[["used_prechosen"]] <- this_prechosen_variables
    }

    per_size_results[[i]] <- var_res
    
    if (i == 1) {
      current_consolidated_models <- stack_models(
        list(var_res[["accu_rankings_models"]])
      ) 
    } else {
      current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
    }
    
    combn_already_tried <- c(combn_already_tried, 
                             var_res[["combinations_of_variables_considered"]])
    
    file_suffix <- paste0("_size_", this_size,
                          "_t_", this_t_tresh, "mr", max_rank_some_h,
                          "_mrfq", max_rank_some_h_for_freq, ".rds")
    
    filename <- paste0("var_results_", country, file_suffix)
    
    saveRDS(var_res, paste0(output_path, filename))
    
    per_size_results[[i]] <- var_res
    f_vbls_list[[i]] <- f_vbls
    
    prechosen_variables_at_each_step[[i]] <- this_prechosen_variables
    current_consolidated_models_list[[i]] <- current_consolidated_models
    cv_objects_list[[i]] <- var_res[["cv_objects"]]
    
    toc()
  }
  
  toc()
  
  bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
  
  consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
  
  final_time <- Sys.time()
  
  elapsed_time <- final_time - initial_time
  
  if (ret_cv) {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         elapsed_time = elapsed_time, 
                         prechosen = all_prechosen_variables_at_each_step,
                         cv_objects = cv_objects_list,
                         target_variable_transform = rgdp_rec,
                         names_exogenous = names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
    
  } else {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         prechosen = all_prechosen_variables_at_each_step,
                         elapsed_time = elapsed_time,
                         target_variable_transform = rgdp_rec,
                         names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
  }
  
  allsizes <- paste(n_steps, collapse = "")
  allthresh <- "foo"
  # allthresh <- paste(t_tresh, collapse = "")
  allfqlim <- paste(c(9,6,6), collapse = "")
  
  file_suffix_all_sizes <-  paste0("_s", allsizes,
                                   "_t", allthresh, "_mr", max_rank_some_h,
                                   "_mrfq", max_rank_some_h_for_freq,
                                   "_cv",number_of_cv,"_tspan", train_span,
                                   "_h", fc_horizon,".rds")
  
  
  if(is.null(results_file_name)) {
    filename <- paste0("vr_", country, file_suffix_all_sizes)
  } else {
    filename <- results_file_name
  }
  
  print("filename")
  print(filename)
  
  saveRDS(res_and_info, paste0(output_path, filename))
  
  return(res_and_info)
}







# search var one size formerly known as try_sizes_vbls_lags
# then it was modified to work on single size choice
search_var_one_size <- function(var_data,
                                rgdp_yoy_ts,
                                rgdp_level_ts,
                                target_v, 
                                var_size, 
                                vec_lags = c(1,2,3,4),
                                names_exogenous = c(""),
                                exo_lag = NULL,
                                pre_selected_v = "",
                                is_cv = FALSE, 
                                h_max = 5, 
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
                                keep_varest = FALSE,
                                add_info_based_lags = TRUE) {
  
  print("names_exogenous")
  print(names_exogenous)

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
  
  n_pre_selected_v <- length(pre_selected_v[pre_selected_v != ""])
  
  already_chosen <- c(target_v, pre_selected_v)
  already_chosen <- already_chosen[already_chosen != ""]
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- var_size - len_already_chosen
  
  sets_of_other_variables <- get_sets_of_variables(
    df = var_data, this_size = var_size, all_variables = all_names, 
    already_chosen = already_chosen)
  
  len_sets_of_vars <- ncol(sets_of_other_variables)
  
  var_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
  
  combinations_of_variables_considered <- list_along(seq.int(1, len_sets_of_vars))
  
  messa1 <- paste0("This search: VARs with ", target_v, " as target variable, ",
                   n_pre_selected_v, " variables as pre-chosen variables and ",
                   len_other_vbls, " other free variables chosen among the ",
                   ncol(var_data) - len_already_chosen, " available variables.")
  
  
  messa2 <- paste0("That amounts to ", len_sets_of_vars, " different combinations of 
                   variables, each of them paired with ", length(vec_lags), 
                   " choices of max. lag to form " ,
                   len_sets_of_vars*length(vec_lags), " concrete unrestricted VARs.")
  
  messa3 <- paste0("Furthermore each unrestricted VAR will produce ", length(t_tresh), 
                   " more restricted version(s) to be evaluated alonside the unrestricted one.")
  
  print("")
  print(messa1)
  print(messa2)
  print(messa3)
  
  if (n_pre_selected_v > 0) {
    print("Prechosen variables (other than the target variables) for this search:")
    print(pre_selected_v)
    
    print("it should match already_chosen vector:")
    print("already_chosen" )
    print(already_chosen )
    
  }
  
  one_endog_count <- 0
  
  for (j in seq.int(1, len_sets_of_vars)) {
    
    # vec_of_other_vbls <- sets_of_other_variables[[j]]
    
    
    vec_of_other_vbls <- sets_of_other_variables[,j]
    # print("in search var one size, vec_of_other_vbls")
    # print(vec_of_other_vbls)
    # print("in search var one size, already_chosen")
    # print(already_chosen)
    
    vbls_for_var <- c(already_chosen, vec_of_other_vbls)
    combinations_of_variables_considered[[j]] <- vbls_for_var
    
    
    # print("vbls for var")
    # print(vbls_for_var)
    # 
    # print("colnames(var_data)")
    # print(colnames(var_data))
    
    
    sub_data = var_data[, vbls_for_var]
    
    # print("Sub_data for first time")
    # print(sub_data)
    
    sub_data = na.omit(sub_data)
    
    # print("Sub_data after naomit")
    # print(sub_data)
    
    sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
    
    # selectedv <-  c("rgdp", "rpc", "emae_sa", "act_eco_bra")
    # exoall <- c("ip_us", "ip_bra")
    
    endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
    exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
    # print("vbls_for_var")
    # print(vbls_for_var)
    
    if (length(endov) == 1) {
      # print("inside 1 ondo if")
      # print("vbls_for_var")
      # print(vbls_for_var)
      # print("endov")
      # print(endov)
      # print("exov")
      # print(exov)
      # print("Only one endogenous variable. Skip to next selection")
      one_endog_count <- one_endog_count + 1
      next
    }
    
    
    endodata <- sub_data[ , endov]
    exodata <- sub_data[ , exov]
    
    if (is.null(dim(endodata))) {
      names(endodata) <- endov
    } else {
      colnames(endodata) <- endov
    }
    
    if (is.character(vec_lags)) {
      lag_sel_method <- "info"
      info_lag_max <- 8

      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max,
                             exogen = exo_and_lags)
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
      
      if (any(too_high_p)) {
        
        # print(paste0("One or more lags found larger than max_p_for_estimation (",
        #              max_p_for_estimation,"). Replace them with max instead"))
        
        binding_max_p <- binding_max_p + 1
      }
    }
    
    if (is.numeric(vec_lags)) {
      p_for_estimation <- unique(vec_lags)
      
      if (add_info_based_lags) {
        
        info_lag_max <- 8
        
        exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
        
        sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                               exogen = exo_and_lags)
        sel_criteria <- sel$selection
        print(sel_criteria)
        cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                       fpe = "FPE(n)")
        this_cri <- cri_names
        named_lags <- sel_criteria[this_cri]
        info_based_p_for_estimation <- unique(unname(named_lags))
        
        too_high_p <- info_based_p_for_estimation > max_p_for_estimation
        
        info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
        
        p_for_estimation <- unique(c(p_for_estimation, 
                                     info_based_p_for_estimation)
        )
      }
      
    }
    
    
    if (is.numeric(vec_lags)) {
      lag_sel_method <- "manual"
    }
    
    len_lag <- length(p_for_estimation)
    var_fixed_vset_all_lags <- list_along(seq(len_lag))
    fcs_fixed_vset_all_lags <- list_along(seq(len_lag))
    
    for (k in seq.int(1, len_lag)) {
      this_cv <- list()
      this_lag <- p_for_estimation[k]
      
      if (is.null(exo_lag)) {
        exo_lag <- this_lag 
      }
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
      
      full_sample_var <- vars::VAR(y = endodata, type = "const", p = this_lag, 
                                   exogen = exo_and_lags)
      
      # if ("act_eco_bra" %in% vbls_for_var) {
      #   print("exo_and_lags")
      #   print(exo_and_lags)
      #   print("endodata")
      #   print(endodata)
      #   print(full_sample_var)
      # }
      #   
      
      model_number <- model_number + 1
      
      if (restrict_by_signif) {
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
        
        this_root <- try(vars::roots(full_sample_var))
        
        if (class(this_root) == "try-error") {
          print("error computing roots. Possible NAs or Inf in x")
          print(paste0("current variables: "))
          print(colnames(sub_data))
          print(paste0("current max lag: ", this_lag))
          is_stable <- FALSE
        } else {
          is_stable <- all(this_root < 1)
        }
        
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
          # print("paso!")
          # print(models_with_cv_excercises)
          # print("var_restrictions")
          # print(var_restrictions)
          
          if (is.null(exo_lag)) {
            exo_lag <- this_lag 
          }
          
          # print("Sub_data before var_cv")
          # print(sub_data)
          
          this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                            external_idx = sub_data_tk_index, this_p = this_lag,
                            this_type = "const", h_max = h_max,
                            n_cv = n_cv, training_length = training_length, 
                            test_residuals = check_residuals_cv,
                            full_sample_resmat = var_restrictions, 
                            names_exogenous = names_exogenous, exo_lag = exo_lag)
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

      var_fixed_vset_all_lags[[k]] <- this_cv
    }
    
    var_all_vset_all_lags[[j]] <- var_fixed_vset_all_lags
  }
  
  # print("combinations_of_variables_considered")
  # print(combinations_of_variables_considered)
  
  results_all_models <- flatten(var_all_vset_all_lags)

  results_all_models <- discard(results_all_models, 
                                ~ is.null(.x[["cv_test_data"]][[1]]))
  
  if (length(results_all_models) == 0) {
    print("No model passed all tests")
    
    print(paste("Number of models analyzed:", model_number))
    print(paste("Total models dropped after significance restrictions applied:", 
                models_with_eqn_dropping, "out of", model_number))
    print(paste("Total significant models unstable:", 
                models_unstable, "out of", model_number - models_with_eqn_dropping))
    print(paste("Total significant stable models, but with non-white residuals:", 
                models_non_white_fs, "out of", model_number -
                  models_with_eqn_dropping - models_unstable ))
    
    return(list(accu_rankings_models = list(),
                cv_objects = list(),
                combinations_of_variables_considered = combinations_of_variables_considered
    )
    )
  }
  
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
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_current_form,
                                                auxiliary_ts = auxiliary_ts,
                                                n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_current_form,
                                             auxiliary_ts = auxiliary_ts,
                                             n_cv = n_cv) ),
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
          cv_test_data_diff, ~ transform_cv(list_series  = ., 
                                            series_name = "cv_test_data",
                                            current_form = rgdp_current_form,
                                            auxiliary_ts = auxiliary_ts,
                                            n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff,  ~ transform_cv(list_series  = .,
                                         series_name = "cv_fcs",
                                         current_form = rgdp_current_form,
                                         auxiliary_ts = auxiliary_ts,
                                         n_cv = n_cv) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
      
    }
    
  }
  
  if (keep_only_white_noise_fs){
    
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
  print(paste("CV repetitions:", n_cv))
  print(paste("Total estimations (full sample + cv rounds):", 
              n_cv*models_with_cv_excercises + model_number))
  print(paste("Total times p exceeded max_p_for_e:", binding_max_p))
  
  
  if (nrow(results_all_models) > 0) {
    
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
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects,
                combinations_of_variables_considered = combinations_of_variables_considered))
  } else {
    return(list(accu_rankings_models = accu_rankings_models,
                combinations_of_variables_considered = combinations_of_variables_considered))
  }
}



stack_models <- function(models_list) {
  
  all_models <- as_tibble(reduce(models_list, rbind)) %>%
    dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
    dplyr::select(vars_select(names(.), -starts_with("rank"))) 
  
  # print("all_models")
  # print(all_models)
  
  all_models <- all_models %>%
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
                   full_sample_resmat = NULL,
                   names_exogenous = c(""),
                   exo_lag = NULL) {
  
  # print("inside var_cv")
  # print(colnames(var_data))
  # print("firts full_sample_resmat")
  # print(full_sample_resmat)
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                                             type = "tscv", n = n_cv, h_max = h_max, 
                                             training_length = training_length, 
                                             timetk_idx = timetk_idx, 
                                             external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
  }
  

  
  vbls_for_var <- colnames(var_data)
  
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 

  
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 

  
  endodata <- var_data[ , endov]

  exodata <- var_data[ , exov]

  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  if (is.null(exo_lag)) {
    exo_lag <- this_p
  }
  
  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)

  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  # cv_fc_object <- list_along(1:n_cv)
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
    
    # training_y <- window(var_data, 
    #                      start = this_tra_s,
    #                      end = this_tra_e)

    training_y <- window(endodata, 
                         start = this_tra_s,
                         end = this_tra_e)
    
    training_rgdp <- training_y[ , "rgdp"]
    
    # test_y <- window(var_data, 
    #                  start = this_tes_s,
    #                  end = this_tes_e)

    test_y <- window(endodata, 
                     start = this_tes_s,
                     end = this_tes_e)

    if (is.null(exo_and_lags)) {
      training_exo <- NULL
      
      training_exo_and_lags <- NULL
      
      test_exo <- NULL
      
      test_exo_and_lags <- NULL
    } else {
      training_exo <- window(exodata, 
                             start = this_tra_s,
                             end = this_tra_e)
      
      training_exo_and_lags <- window(exo_and_lags, 
                                      start = this_tra_s,
                                      end = this_tra_e)
      
      assign("training_exo_and_lags", training_exo_and_lags, 
             envir = .GlobalEnv)
      
      test_exo <- window(exodata, 
                         start = this_tes_s,
                         end = this_tes_e)
      
      test_exo_and_lags <- window(exo_and_lags, 
                                  start = this_tes_s,
                                  end = this_tes_e)
      
    }

    test_rgdp <- test_y[ , "rgdp"]
    
    # this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
    

    
    
    if (is.null(training_exo_and_lags)) {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
      
    } else {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type, 
                            exogen = training_exo_and_lags)

    }
    

    if (! is.null(full_sample_resmat)) {
      this_var_r <- try(vars::restrict(this_var, method = "manual", 
                                 resmat = full_sample_resmat))
    }
    
    if (class(this_var_r) == "try-error") {
      print("keeping the unrestricted")
      
    } else {
      print("keeping the unrestricted")
      this_var <- this_var_r
    }
    

    

    this_effective_lag <- max_effective_lag(this_var)

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
    
    if (is.null(training_exo_and_lags)) {
      this_fc <- forecast(this_var, h = h_max)
    } else {
      this_fc <- forecast(this_var, h = h_max, dumvar = test_exo_and_lags,
                          exogen = training_exo_and_lags)
    }
    
    this_rgdp_fc_mean <- this_fc[["forecast"]][["rgdp"]][["mean"]]
    
    fc_error <- test_rgdp - this_rgdp_fc_mean
    
    vbl_names <- colnames(training_y)
    
    lag <- this_p
    
    cv_vbl_names[[i]] <- vbl_names
    cv_lag[[i]] <- lag
    cv_errors[[i]] <- fc_error
    cv_test_data[[i]] <- test_rgdp
    cv_fcs[[i]] <- this_rgdp_fc_mean
    # cv_fc_object[[i]] <- this_fc
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




variable_freq_by_n <- function(tbl_of_models, h_max = 8, max_rank = 20, 
                               n_freq = 10, is_wide = FALSE, max_small_rank = 3,
                               max_smallest_rank = 1) {
  
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
  
  
  summary_of_tom <- tbl_of_models %>% 
    group_by(rmse_h) %>% 
    summarize(n_models = n(),
              less_than_max_rank = sum(rank_h < max_rank +1)
    )
  
  vec_of_rmse_h <- sort(unique(tbl_of_models$rmse_h))
  
  list_best <- map(vec_of_rmse_h, 
                   ~ tbl_of_models %>% 
                     filter(rmse_h == .x, rank_h < max_rank + 1) %>% 
                     dplyr::select("variables") %>% 
                     unlist() %>% 
                     table() %>% 
                     as_tibble() %>% 
                     arrange(desc(n)) %>% 
                     rename(., vbl = .)
  ) 
  
  tbl_best <- reduce(list_best, full_join, by = c("vbl"))
  
  names(tbl_best) <- c("vbl", paste("h", seq(h_max), sep = "_"))
  
  
  tbl_best <- tbl_best %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE),
           avg = total_n/length(rmse_names)) %>% 
    arrange(desc(total_n))

  # print("max_small_rank + 1")
  # print(max_small_rank + 1)
  # 
  # print(tbl_of_models)
  # list_best_small <- map(vec_of_rmse_h, 
  #                        ~ tbl_of_models %>% 
  #                          filter(rmse_h == .x, rank_h < max_small_rank + 1) 
  # )
  # 
  # print("list_best_small")
  # print(list_best_small)
  
  list_best_small <- map(vec_of_rmse_h, 
                         ~ tbl_of_models %>% 
                           filter(rmse_h == .x) %>% arrange(rmse)
  )
  # 
  # print("first list_best_small")
  # print(list_best_small)
  
  small_effective_rank <- map_dbl(list_best_small, ~ sort(.x[["rank_h"]])[max_small_rank])
  
  # print("small_effective_rank")
  # print(small_effective_rank)
  
  
  list_best_small <- map2(list_best_small, small_effective_rank,
                          ~ filter(.x, rank_h <= .y) %>% 
                            dplyr::select("variables") %>% 
                            unlist() %>% 
                            table() %>% 
                            as_tibble() %>% 
                            arrange(desc(n)) 
  ) 
  
  # print("second list_best_small ")
  # 
  # print(list_best_small )
  
  for (k in seq_along(list_best_small)) {
    names(list_best_small[[k]]) <- c("vbl", "n")
  }
  
  # print(9)
  # 
  # print(list_best_small )
  
  tbl_best_small <- reduce(list_best_small, full_join, by = c("vbl"))
  
  # print(10)
  
  
  names(tbl_best_small) <- c("vbl", paste("h", seq(h_max), sep = "_"))
  
  # print(11)
  
  tbl_best_small <- tbl_best_small %>% 
    mutate(total_n = rowSums(.[2:(h_max + 1)], na.rm = TRUE),
           avg = total_n / length(rmse_names)) %>% 
    arrange(desc(total_n))
  
  # print("tbl_best_small")
  # print(tbl_best_small)
  
  
  variables_in_top_small <- unique(unlist(tbl_best_small[, "vbl"]))
  
  
  # print(paste0("top_small N = ", length(variables_in_top_small)))
  
  
  # print("tbl_best")
  # print(tbl_best)
  
  # print(paste0("Variables in best-", max_small_rank, " VARs at any h:"))
  # print(variables_in_top_small)
  
  tbl_best_not_in_small <- tbl_best %>% 
    filter(! vbl %in% variables_in_top_small) %>% 
    arrange(desc( total_n ))
  
  variables_not_in_top_small <- unique(unlist(tbl_best_not_in_small[, "vbl"]))
  
  # print("tbl_best_not_in_small")
  # print(tbl_best_not_in_small)
  
  by_total_not_in_top_small <- unique(unlist(tbl_best_not_in_small[, "vbl"]))
  
  # print("variables_not_in_top_small")
  # print(variables_not_in_top_small)
  
  by_total <- tbl_best %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1 <- tbl_best %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_hlast <- tbl_best %>% 
    arrange(desc(h_7)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  vbl_by_h1 <-  by_h1$vbl
  vbl_by_hlast <-  by_hlast$vbl
  vbl_by_total <-  by_total$vbl
  
  vbl_all <- unique(c(vbl_by_h1, vbl_by_total, vbl_by_hlast))
  
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
  
  
  tbl_best_lags <- reduce(list_best_lags, full_join, by = c("max_lag"))
  names(tbl_best_lags) <- c("max_lag", paste("h", seq(h_max), sep = "_"))
  
  tbl_best_lags <- tbl_best_lags %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE),
           avg = total_n/length(rmse_names))
  
  by_total_lags <- tbl_best_lags %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1_lags <- tbl_best_lags %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_hlast_lags <- tbl_best_lags %>% 
    arrange(desc(h_7)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  lags_by_h1 <-  by_h1_lags$max_lag
  lags_by_hlast <-  by_hlast_lags$max_lag
  lags_by_total <-  by_total_lags$max_lag
  
  lags_all <- unique(c(lags_by_h1, lags_by_total, lags_by_hlast))
  
  return(list(vbl_freqs_by_h = tbl_best, vbl_multi = vbl_all, 
              vbl_by_h1 = vbl_by_h1, vbl_by_total = vbl_by_total, 
              vbl_by_hlast = vbl_by_hlast, 
              lags_freqs_by_h = tbl_best_lags,
              lags_multi = lags_all, 
              lags_by_h1 = lags_by_h1, lags_by_total = lags_by_total, 
              lags_by_hlast = lags_by_hlast,
              list_best = list_best, 
              variables_in_top_small = variables_in_top_small,
              by_total_not_in_top_small = by_total_not_in_top_small))
}






# 
# 
# var_search_old <- function(country, 
#                        sizes, 
#                        forecast_exercise_year, 
#                        forecast_exercise_number,
#                        fc_horizon,
#                        target_variable = c("rgdp"),
#                        other_prechosen_variables = list(c(""), c(""), c(""), c("")),
#                        vec_lags = c(1, 2, 3, 4, 5, 6) ,
#                        add_aic_bic_hq_fpe_lags =  FALSE,
#                        vec_freq_limit = list("none", "none", 15, 10),
#                        restrict_by_signif = TRUE,
#                        t_tresh = c(2, 2, 2, 2),
#                        number_of_cv = 8,
#                        train_span = 25,
#                        ret_cv = TRUE,
#                        max_rank_some_h =50,
#                        max_rank_some_h_for_freq = 50,
#                        max_small_rank = 3,
#                        results_file_name = NULL
# ) {
#   
#   initial_time <- Sys.time()
#   tic(msg = "Total time for this country")
#   
#   
#   # file paths
#   excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
#                             "_exercise_", forecast_exercise_number, "/")
#   
#   output_path <- paste0("./analysis/VAR_output/edd_exercises/",
#                         forecast_exercise_year, 
#                         "_exercise_", forecast_exercise_number, "/")
#   
#   country_data_ts <- get_raw_data_ts(country = country, data_path = excel_data_path)
#   external_data_ts <- get_raw_external_data_ts(data_path = excel_data_path)
#   data_ts <- country_data_ts
#   
#   rgdp_level_ts <- data_ts[, "rgdp"]
#   rgdp_level_ts <- na.omit(rgdp_level_ts)
#   rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)
#   
#   print(paste0("This country: ", country))
#   print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
#   print("Names of variables: ")
#   print(colnames(data_ts))
#   
#   tic()
#   print("Finding and applying stationary transformations to all variables")
#   reco_all_variables <- find_statio_diffs(data_ts, country)
#   country_transformed_data <- follow_rec(data_ts, reco_all_variables)
#   print("Done.")
#   toc()
#   
#   rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
#   print(paste0("Stationary transformation for rgdp: ", rgdp_rec))
#   
#   VAR_data_for_estimation  <- country_transformed_data
#   
#   print(paste0("rgdp obs. after transformation: ", 
#                length(na.omit(VAR_data_for_estimation[ , "rgdp"]))
#   )
#   )
#   
#   print(paste0("rgdp obs. before transformation: ", length(rgdp_level_ts)))
#   
#   variable_names <- colnames(VAR_data_for_estimation)
#   ncolumns <- ncol(VAR_data_for_estimation)
#   
#   max_common_train_span_guaranted <- nrow(na.omit(VAR_data_for_estimation)) - fc_horizon - number_of_cv
#   print(paste0("Taking all variables together, maximum common training span is ",
#                max_common_train_span_guaranted))
#   upper_bound_for_train_span <- length(na.omit(VAR_data_for_estimation[ , "rgdp"])) - fc_horizon - number_of_cv
#   print(paste0("For variables encompasing rgdp extent, max training span is ",
#                upper_bound_for_train_span))
#   
#   if (train_span == "common_max") {
#     print(paste0("Using common_max span for training sets: ", max_common_train_span_guaranted))
#     train_span <- max_common_train_span_guaranted
#   }
#   
#   saveRDS(VAR_data_for_estimation, 
#           paste0(output_path, "VAR_data_", country, ".rds"))
#   
#   
#   freq_sel_vbls <- colnames(VAR_data_for_estimation) # same as freq_limit = 'none'
#   
#   
#   # if (train_span + fc_horizon + number_of_cv > nrow(VAR_data_for_estimation)) {
#   #   print("not enough obs")
#   #   stop()
#   # }
#   
#   per_size_results <- list_along(sizes)
#   f_vbls_list <- list_along(sizes)
#   selection_for_next_size_list <- list_along(sizes)
#   current_consolidated_models_list <- list_along(sizes)
#   cv_objects_list <- list_along(sizes)
#   
#   tic(msg = "Finish var search")
#   
#   
#   
#   
#   for (i in seq(length(sizes))) {
#     
#     this_size <- sizes[i]
#     this_t_tresh <- t_tresh[i]
#     this_freq_limit <- vec_freq_limit[[i]]
#     this_prechosen_variables <- other_prechosen_variables[[i]]
#     
#     print(paste0("Starting the estimation of VAR with ", this_size," vbls"))
#     
#     print(paste0("prechosen variables for this size: "))
#     print(this_prechosen_variables)
#     
#     
#     if (i < length(sizes)) {
#       next_freq_limit <- vec_freq_limit[[i + 1]]
#     }
#     
#     if (this_freq_limit == "none") {
#       print("Using all variables")
#       this_VAR_data <- VAR_data_for_estimation
#     }
#     
#     if (this_freq_limit == "cummulative-preselection") {
#       print("Using pre-selected from a previous stage/size")
#       
#       var_res_of_previous_size <- keep(per_size_results[1: (i-1)], 
#                                        ~ .x[["explored_size"]] == (this_size-1))
#       
#       n_previous_size_results <- length(var_res_of_previous_size)
#       
#       print(paste0("There are ", n_previous_size_results, 
#                    " sets of results available from the previous size (i.e ", 
#                    this_size-1, ")."))
#       
#       best_n_VAR_for_preselecting <- 10
#       n_freq_for_preselecting <- 2*this_size
#       
#       nth_after_target <- this_freq_limit$nth
#       
#       preselected_for_this_size <- vector(mode = "integer", length = n_previous_size_results)
#       
#       for (psr in seq(1, n_previous_size_results)) {
#         print(paste0("psr = ", psr))
#         this_results <- var_res_of_previous_size[psr][[1]]
#         
#         f_vbls <- variable_freq_by_n(this_results[["accu_rankings_models"]], 
#                                      h_max = fc_horizon, 
#                                      max_rank = max_rank_some_h_for_freq,
#                                      n_freq = n_freq_for_preselecting , 
#                                      is_wide = TRUE,
#                                      max_small_rank = best_n_VAR_for_preselecting)
# 
#         vbl_table <- f_vbls$vbl_freqs_by_h %>% arrange(desc(total_n))
#         
#         vbl_table_by_total <- vbl_table %>% 
#           arrange(desc(total_n)) %>% 
#           dplyr::select(vbl) %>% 
#           dplyr::filter(row_number() <= n_freq_for_preselecting)
#         
#         vbl_by_total <-  vbl_table_by_total$vbl
#         
#         # print("vbl_table")
#         # print(vbl_table)
#         # print("vbl_by_total")
#         # print(vbl_by_total)
#         new_preselected <- vbl_by_total[1+nth_after_target]
#         # print("new_preselected")
#         # print(new_preselected)
#         
#         preselected_for_this_size[psr] <- new_preselected
#         
#       }
#       
#       print("preselected_for_this_size: ")
#       print(preselected_for_this_size)
#       
#       
#       
#       # this_VAR_data <- VAR_data_for_estimation
#     }
#     
#     
#     
#     if (i > 1 & is.numeric(this_freq_limit)) {
#       print("Using this subset of variables: ")
#       print(new_select_vbls)
#       
#       this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
#     }
#     
#     tic(msg = paste0("Finished VARs with ", this_size, " variables"))
#     
#     var_res <- search_var_one_size(
#       var_size = this_size,
#       vec_lags = vec_lags,
#       var_data = this_VAR_data,
#       rgdp_level_ts = rgdp_level_ts,
#       rgdp_yoy_ts = rgdp_yoy_ts,
#       target_v = target_variable,
#       pre_selected_v = this_prechosen_variables,
#       is_cv = TRUE,
#       training_length = train_span,
#       h_max = fc_horizon,
#       n_cv = number_of_cv,
#       return_cv = ret_cv,
#       rgdp_current_form = rgdp_rec,
#       max_rank = max_rank_some_h,
#       check_residuals_cv = TRUE,
#       check_residuals_full_sample = TRUE,
#       restrict_by_signif = restrict_by_signif,
#       t_tresh = this_t_tresh,
#       max_p_for_estimation = 12,
#       add_info_based_lags = add_aic_bic_hq_fpe_lags)
#     
#     var_res[["explored_size"]] <- this_size
#     
#     per_size_results[[i]] <- var_res
#     
#     if (i == 1) {
#       current_consolidated_models <- stack_models(
#         list(var_res[["accu_rankings_models"]])
#       ) 
#     } else {
#       current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
#     }
#     
#     if (i < length(sizes)) {
#       next_freq_limit <- vec_freq_limit[[i + 1]]
#     }
#     
#     if (i == length(sizes)) {
#       next_freq_limit <- list(type = "none")
#     }
#     
#     
#     if (next_freq_limit$type == "none") {
#       f_vbls <- NULL
#       new_select_vbls <- colnames(VAR_data_for_estimation) 
#       vbls_top_small <- NA
#       by_total_not_in_tsm <- NA
#     }
#     
#     if (next_freq_limit$type == "cummulative-preselection") {
#       print("this is cummulative preselection")
#       
#       # print("previously preselected so far:")
# 
#       vec_of_prechosen <- reduce(other_prechosen_variables, c)
#       vec_of_prechosen <- vec_of_prechosen[vec_of_prechosen != ""]
#       # print(vec_of_prechosen)
#       
#       
#       best_n_VAR_for_preselecting <- 10
#       n_freq_for_preselecting <- 2*this_size
#       
#       nth_after_target <- next_freq_limit$nth
#       
#       # f_vbls <- variable_freq_by_n(current_consolidated_models, 
#       #                              h_max = fc_horizon, 
#       #                              max_rank = max_rank_some_h_for_freq,
#       #                              n_freq = n_freq_for_preselecting , 
#       #                              is_wide = TRUE,
#       #                              max_small_rank = best_n_VAR_for_preselecting)
#       
#       previous_results <- per_size_results[[i-1]]
# 
#       
#       f_vbls <- variable_freq_by_n(previous_results[["accu_rankings_models"]], 
#                                    h_max = fc_horizon, 
#                                    max_rank = max_rank_some_h_for_freq,
#                                    n_freq = n_freq_for_preselecting , 
#                                    is_wide = TRUE,
#                                    max_small_rank = best_n_VAR_for_preselecting)
#       
#       # print("f_vbls")
#       # print(f_vbls)
#       vbl_table <- f_vbls$vbl_freqs_by_h %>% arrange(desc(total_n))
#       
#       vbl_table_by_total <- vbl_table %>% 
#         arrange(desc(total_n)) %>% 
#         dplyr::select(vbl) %>% 
#         dplyr::filter(row_number() <= n_freq_for_preselecting)
#       
#       vbl_by_total <-  vbl_table_by_total$vbl
#       
#       # print("vbl_table")
#       # print(vbl_table)
#       # print("vbl_by_total")
#       # print(vbl_by_total)
#       new_preselected <- vbl_by_total[1+nth_after_target]
#       # print("new_preselected")
#       # print(new_preselected)
#       
#       other_prechosen_variables[[i+1]] <- new_preselected 
#       new_select_vbls <- colnames(VAR_data_for_estimation) 
#     }
#     
#     if (is.numeric(next_freq_limit)) {
#       f_vbls <- variable_freq_by_n(current_consolidated_models, 
#                                    h_max = fc_horizon, max_rank = max_rank_some_h_for_freq,
#                                    n_freq = next_freq_limit, is_wide = TRUE,
#                                    mas_small_rank)
#       freq_sel_vbls_by_multi <- f_vbls$vbl_multi
#       vbls_top_small <- f_vbls$variables_in_top_small
#       
#       if(length(vbls_top_small) > next_freq_limit) {
#         print(paste0("Number of best-n-VAR variables (", length(vbls_top_small), 
#                      "exceeds next_freq_limit (",  next_freq_limit, "). We will preserve 
#         the integrity of best VARs and use those",  length(vbls_top_small), " variables in next size." )  )
#         
#         print(paste0("If you want to decrease the number of variables, reduce the mas_small_rank 
#                      parameter to some value lower than ", max_small_rank))
#         
#         vbls_top_small <- vbls_top_small
#       }
#       
#       
#       by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
#       
#       # print("vector tiene NA:")
#       # print(by_total_not_in_tsm)
#       by_total_na <- is.na(by_total_not_in_tsm)
#       # print("by_total_na")
#       # print(by_total_na)
#       # print(!by_total_na)
#       
#       by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
#       
#       n_gap_vbls <- next_freq_limit - length(vbls_top_small)
#       
#       # print("vbls_top_small")
#       # print(vbls_top_small)
#       # print("by_total_not_in_tsm")
#       # print(by_total_not_in_tsm)
#       # print("n_gap_vbls")
#       # print(n_gap_vbls)
#       
#       if (n_gap_vbls > 0) {
#         extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
#         # print("extra_vbls")
#         # print(extra_vbls)
#       } else {
#         extra_vbls <- c()
#       }
#       
#       new_select_vbls <- c(vbls_top_small, extra_vbls)
#       
#       # print("new_select_vbls")
#       # print(new_select_vbls)
#       
#       
#     }
#     
#     # file_suffix <- paste0("_size_", this_size, "_fqlim_", this_freq_limit,
#     #                       "_t_", this_t_tresh, "mr", max_rank_some_h,
#     #                       "_mrfq", max_rank_some_h_for_freq, ".rds")
#     
#     file_suffix <- paste0("_size_", this_size,
#                           "_t_", this_t_tresh, "mr", max_rank_some_h,
#                           "_mrfq", max_rank_some_h_for_freq, ".rds")
#     
#     filename <- paste0("var_results_", country, file_suffix)
#     
#     print("filename")
#     print(filename)
#     
#     saveRDS(var_res, paste0(output_path, filename))
#     
#     per_size_results[[i]] <- var_res
#     f_vbls_list[[i]] <- f_vbls
#     selection_for_next_size_list[[i]] <- new_select_vbls
#     current_consolidated_models_list[[i]] <- current_consolidated_models
#     cv_objects_list[[i]] <- var_res[["cv_objects"]]
#     
#     toc()
#   }
#   
#   toc()
#   
#   bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
#   
#   consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
#   
#   final_time <- Sys.time()
#   
#   elapsed_time <- final_time - initial_time
#   
#   if (ret_cv) {
#     res_and_info <- list(consolidated_var_res = consolidated_var_res,
#                          f_vbls_all_sizes = f_vbls_list,
#                          selected_for_next_size = selection_for_next_size_list,
#                          var_data = VAR_data_for_estimation,
#                          elapsed_time = elapsed_time,
#                          cv_objects = cv_objects_list)
#     
#   } else {
#     res_and_info <- list(consolidated_var_res = consolidated_var_res,
#                          f_vbls_all_sizes = f_vbls_list,
#                          selected_for_next_size = selection_for_next_size_list,
#                          var_data = VAR_data_for_estimation,
#                          elapsed_time = elapsed_time)
#   }
#   
#   allsizes <- paste(sizes, collapse = "")
#   allthresh <- paste(t_tresh, collapse = "")
#   allfqlim <- paste(vec_freq_limit, collapse = "")
#   
#   # file_suffix_all_sizes <-  paste0("_s", allsizes, "_fq", allfqlim,
#   #                                  "_t", allthresh, "_mr", max_rank_some_h,
#   #                                  "_mrfq", max_rank_some_h_for_freq,
#   #                                  "_cv",number_of_cv,"_tspan", train_span,
#   #                                  "_h", fc_horizon,".rds")
#   
#   file_suffix_all_sizes <-  paste0("_s", allsizes,
#                                    "_t", allthresh, "_mr", max_rank_some_h,
#                                    "_mrfq", max_rank_some_h_for_freq,
#                                    "_cv",number_of_cv,"_tspan", train_span,
#                                    "_h", fc_horizon,".rds")
#   
#   
#   if(is.null(results_file_name)) {
#     filename <- paste0("vr_", country, file_suffix_all_sizes)
#   } else {
#     filename <- results_file_name
#   }
#   
#   print("filename")
#   print(filename)
#   
#   saveRDS(res_and_info, paste0(output_path, filename))
#   
#   return(res_and_info)
# }





