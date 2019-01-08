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


all_mae_from_cv_obj <- function(cv_obj) {
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
  mae <- colMeans(abs(matrix_errors))
  
  return(mae)
}


ave_fc_from_cv <- function(cv_tbl, best_n_to_keep = "all", is_wide = TRUE) {
  
  cv_tbl_na <- filter(cv_tbl, is.na(target_mean_fc_yoy))
  cv_tbl <- filter(cv_tbl, !is.na(target_mean_fc_yoy))
  
  print("cases where the forecast resulted in NA:")
  print(cv_tbl_na[["short_name"]])
  
  if (is_wide) {
    rmse_names <- names(cv_tbl)[str_detect(names(cv_tbl), "rmse")]
    cv_tbl <- cv_tbl %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>%
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
      group_by(rmse_h) %>%
      arrange(rmse_h, rmse) %>%
      mutate(rank_h = rank(rmse)) %>%
      ungroup() %>%
      mutate(lags = unlist(lags),
             model_type = "VAR")
  } else {
    rmse_names <- unique(cv_tbl$rmse_h)
  }
  
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
             weighted_fc_h = map2(target_mean_fc_yoy, model_weight_h, ~ .x * .y)
      ) %>%
      ungroup()
  }
  
  a_fc <- cv_tbl$weighted_fc_h[[1]]
  
  ave_by_h_fc <- vector(mode = "numeric", length = length(rmse_names))
  
  for (r in seq(1, length(rmse_names))) {
    this_rmse <- rmse_names[r]
    
    this_h_fc <- cv_tbl %>% 
      dplyr::filter(rmse_h == this_rmse) %>% 
      dplyr::select(weighted_fc_h) 
    
    this_h_fc <- reduce(this_h_fc[[1]], rbind)
    this_h_fc <- colSums(this_h_fc)
    this_h_fc <- this_h_fc[r]
    
    ave_by_h_fc[r] <- this_h_fc
  }
  
  ave_by_h_fc <- ts(data = ave_by_h_fc, frequency = frequency(a_fc),
                    start = start(a_fc))

  return(list(cv_tbl = cv_tbl, ave_by_h_fc = ave_by_h_fc))
}


check_resid_VAR <- function(fit_VAR, type = "PT.asymptotic", lags.pt = 16,
                            pval_ref = 0.05) {
  
  test_object <- try(serial.test(fit_VAR, type = type, lags.pt = lags.pt),
                     silent = TRUE)
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

cv_var_from_model_tbl <- function(h, n_cv, 
                                  training_length, 
                                  models_tbl, 
                                  var_data, 
                                  new_t_threshold = NULL, 
                                  fit_column = NULL, 
                                  target_transform = "yoy", 
                                  target_level_ts = NULL,
                                  keep_varest_obj = FALSE,
                                  keep_cv_objects = FALSE,
                                  keep_fc_objects = FALSE,
                                  names_exogenous = c(""),
                                  exo_lag = NULL,
                                  future_exo = NULL,
                                  future_exo_cv = NULL,
                                  do_full_sample_fcs = FALSE,
                                  extended_exo_mts = extended_exo_mts
                                  ) { 
  
  # print("in cvvarfrommodel exo vbls")
  # print(names_exogenous)
  
  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = FALSE)
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
    tic()
    models_tbl <- estimate_var_from_model_tbl(
      models_tbl = models_tbl, var_data = var_data, new_t_threshold = new_t_threshold, 
      names_exogenous = names_exogenous)
    toc()
    
    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }
  
  if (do_full_sample_fcs) {
    print("Start forecasts of the estimated models")
    
    tic()
    models_tbl <- forecast_var_from_model_tbl(
      models_tbl = models_tbl, 
      var_data = var_data,
      fc_horizon = fc_horizon, 
      new_t_threshold = c(0, 1.65, 2),
      target_transform = target_transform, 
      target_level_ts = target_level_ts, 
      names_exogenous = names_exogenous, 
      extended_exo_mts = extended_exo_mts, 
      fit_column = "fit", keep_varest_obj = TRUE)
    toc()
  }
  
  # print("as_tibble(models_tbl)")
  # print(as_tibble(models_tbl))
  # 
  # return(as_tibble(models_tbl))
  
  print("Starting cv")
  # print("models_tbl so far")
  # print(models_tbl)
  tic()
  models_tbl <-  models_tbl %>%
    mutate(cv_obj = pmap(list(fit, variables, lags, t_threshold),
                         ~ cv_var_from_one_row(var_data = var_data, fit = ..1,
                                               variables = ..2, lags = ..3,
                                               this_thresh = ..4,
                                               h = h, n_cv = n_cv,
                                               names_exogenous = names_exogenous,
                                               training_length = training_length,
                                               this_type = "const",
                                               future_exo_cv = future_exo_cv)
    )
    )
  toc()
  
  print("transform to yoy")
  
  if (target_transform != "yoy") {
    
    if (target_transform == "diff_yoy") {
      
      print("from diffyoy to yoy")
      
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
  
  print("done transforming")
  
  # print("models_tbl$cv_obj_yoy")
  # print(models_tbl$cv_obj_yoy)
  
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
  } else {
    print("keeping forecast list-columns (they are pretty big ...)")
  }
  
  if (!keep_cv_objects) {
    models_tbl <- models_tbl %>%
      dplyr::select(vars_select(names(.), -starts_with("cv_ob")))
  }
  
  models_tbl <- as_tibble(models_tbl)

  return(models_tbl)
} 


cv_var_from_one_row <- function(var_data, 
                                fit, 
                                variables, 
                                lags, 
                                h, 
                                training_length, 
                                n_cv,
                                names_exogenous = c(""),
                                this_type = "const", 
                                future_exo_cv = NULL,
                                this_thresh = 0) {
  
  # print("in cv var from one row")
  # print(names_exogenous)
  
  # print("inside cvvaronerow")
  # print("this_thresh")
  # print(this_thresh)
  
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
  
  # print(1)
  
  sub_data_tk_index <- tk_index(sub_data, timetk_idx = TRUE, silent = TRUE)

  # print("about to this_cv")
  # print("future_exo_cv")
  # print(future_exo_cv)
  
  
  this_cv <- var_cv(var_data = sub_data,
                    h_max = h,
                    n_cv = n_cv, this_p = lags,  
                    external_idx = sub_data_tk_index,
                    full_sample_resmat = this_restriction_mat,
                    names_exogenous = names_exogenous,
                    training_length = training_length,
                    this_type = this_type,
                    future_exo_cv = future_exo_cv,
                    this_thresh = this_thresh)
  
  # print("just did this_cv")
  
  return(this_cv)
}


estimate_var_from_model_tbl_old <- function(models_tbl, 
                                        var_data, 
                                        new_t_threshold = NULL, 
                                        names_exogenous = c(""),
                                        exo_lag = NULL,
                                        remove_ranks = TRUE) {
  
  # print("in estimate var from model tbl")
  # print(names_exogenous)
  
  starting_names <- names(models_tbl)
  # print("starting_names in estimate var from")
  # print(starting_names)
  
  has_short_name <- "short_name" %in% starting_names
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = FALSE)
  }
  
  
  models_tbl <- models_tbl %>% 
        mutate(model_type = "VAR",
               )

  


  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold),
                               ~ make_model_name(variables = ..1, lags = ..2,
                                                 t_threshold = ..3)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  one_model_per_row <- models_tbl %>% 
     distinct(short_name, .keep_all = TRUE)
  
  
  if (is.null(new_t_threshold)) {
    one_model_per_row <- one_model_per_row %>%
      mutate(fit = pmap(list(variables, lags, t_threshold),
                        ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                       p = ..2, t_thresh = ..3, 
                                       names_exogenous = names_exogenous, 
                                       exo_lag = exo_lag))
      )
  } 
  
  if (!is.null(new_t_threshold)) {
    
    all_one_model_per_row <- list_along( seq(1, length(new_t_threshold))  )
    
    for (i in seq(1, length(new_t_threshold))) {
      
      this_thresh <- new_t_threshold[i]
      
      this_one_model_per_row <- one_model_per_row %>%
        mutate(t_threshold = this_thresh,
               short_name_t = map_chr(short_name, ~ paste0(.x, "_t", this_thresh*100)),
               fit = pmap(list(variables, lags, t_threshold),
                          ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                         p = ..2, t_thresh = ..3, 
                                         names_exogenous = names_exogenous, 
                                         exo_lag = exo_lag))
        )
      
      all_one_model_per_row[[i]] <- this_one_model_per_row
    }
    one_model_per_row <- reduce(all_one_model_per_row, rbind)
  }
  
  return(one_model_per_row)
}


fit_VAR_rest_old <- function(var_data, variables, p,
                         t_thresh = FALSE, type = "const",
                         names_exogenous = c(""),
                         exo_lag = NULL)  {

  
  if (t_thresh == 0) {
    t_thresh <- FALSE
  }
  

  this_var_data <- var_data[, variables]
  # print("this_var_data")
  # print(this_var_data)
  
  this_var_data <- na.omit(this_var_data)
  
  vbls_for_var <- colnames(this_var_data)
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 

  if (length(endov) == 1) {
    this_fit <- NA
    print("only one endogenous variable, not a real VAR, returning NA")
    return(this_fit)
  }
  
  endodata <- this_var_data[ , endov]
  exodata <- this_var_data[ , exov]
  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  if (is.null(exo_lag)) {
    exo_lag <- p
  }
  
  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
  n <- nrow(var_data)
  
  if (is.null(exo_and_lags)) {
    this_fit <- vars::VAR(y = endodata, p = p, type = type) 
    
  } else {
    this_fit <- vars::VAR(y = endodata, p = p, type = type, 
                          exogen = exo_and_lags)
    
  }

  if (is.numeric(t_thresh)) {
    # print(paste0("applying t-tresh = ", t_thresh))
    this_fit <- try(vars::restrict(this_fit, method = "ser", 
                                   thresh = t_thresh), silent = TRUE)
    
    if (class(this_fit) == "try-error") {
      this_fit <- "one_or_more_eqn_drops"
    }
  }
  return(this_fit)
}


fit_VAR_rest <- function(var_data, variables, p,
                         t_thresh = FALSE, type = "const",
                         names_exogenous = c(""),
                         exo_lag = NULL)  {
  
  if(length(t_thresh) == 1) {
    if (t_thresh == 0 | is.null(t_thresh)) {
      t_thresh <- FALSE
    }
  }
  
  
  this_var_data <- var_data[, variables]
  this_var_data <- na.omit(this_var_data)
  
  vbls_for_var <- colnames(this_var_data)
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
  
  if (length(endov) == 1) {
    this_fit <- NA
    print("only one endogenous variable, not a real VAR, returning NA")
    return(this_fit)
  }
  
  endodata <- this_var_data[ , endov]
  exodata <- this_var_data[ , exov]
  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  if (is.null(exo_lag)) {
    exo_lag <- p
  }
  
  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
  n <- nrow(var_data)
  
  if (is.null(exo_and_lags)) {
    unrestricted_fit <- vars::VAR(y = endodata, p = p, type = type) 
    
  } else {
    unrestricted_fit <- vars::VAR(y = endodata, p = p, type = type, 
                          exogen = exo_and_lags)
  }
  
  
  # print(unrestricted_fit)
  
  if (is.numeric(t_thresh)) {
    nrest <- length(t_thresh)
    list_of_varests <- list_along(seq(1, nrest+1)) 
    list_of_varests[[1]] <- unrestricted_fit
    
    # print("t_thresh")
    # print(t_thresh)
    
    for (i in seq(1, nrest)) {
      this_thresh <- t_thresh[i]
      # print(this_thresh)
      
      this_fit <- try(vars::restrict(unrestricted_fit, method = "ser", 
                                     thresh = this_thresh), silent = TRUE)
      
      
      if (class(this_fit) == "try-error") {
        this_fit <- "one_or_more_eqn_drops"
      }
      
      list_of_varests[[i+1]] <- this_fit
    }
    
    thresholds_and_fits <- tibble(t_threshold = c(0, t_thresh),
                                  fit = list_of_varests)
    
    return(thresholds_and_fits)
    
  } else {
    return(unrestricted_fit)
  }
  
}



forecast_VAR_one_row <- function(fit, h, variables, extended_exo_mts, 
                                 names_exogenous = c(""), exo_lag = NULL)  {
  
  are_there_exo <- any(names_exogenous %in% variables)

  if (class(fit) == "varest") {
    
    this_var_data <- fit$y
    endov <- variables[!variables %in% names_exogenous] 
    exov <- variables[variables %in% names_exogenous] 

     
    if (!are_there_exo) {
      # print("terrible de null")
      exo_and_lags <- NULL
      exo_and_lags_extended <- NULL
    } else {
      
      exodata <- extended_exo_mts[, exov]
      
      if (is.null(exo_lag)) {
        exo_lag <- fit$p
      }
      exo_and_lags_extended <- make_exomat(exodata = exodata, 
                                           exov = exov,
                                           exo_lag = exo_lag)

      exo_and_lags <- window(exo_and_lags_extended,
                             end = end(this_var_data))
      exo_and_lags_for_fc <- subset(exo_and_lags_extended, 
                                    start = nrow(exo_and_lags) + 1)

      assign("exo_and_lags", exo_and_lags,
             envir = .GlobalEnv)
      
    }
    
    
    if (is.null(exo_and_lags_extended)) {
      this_fc <- forecast(fit, h = h)
    } else {
      this_fc <- forecast(fit, h = h, dumvar = exo_and_lags_for_fc,
                          exogen = exo_and_lags)
    }
    
  }
  
  if (!class(fit) == "varest") {
    this_fc <- list(forecast = list(rgdp = list(mean = NA)))
    
  }
  return(this_fc)
}

forecast_var_from_model_tbl <- function(models_tbl, 
                                        var_data,
                                        fc_horizon, 
                                        new_t_threshold = NULL, 
                                        fit_column = NULL,
                                        target_transform = "yoy",
                                        target_level_ts = NULL,
                                        keep_fc_obj = FALSE,
                                        keep_varest_obj = FALSE,
                                        names_exogenous = c(""),
                                        extended_exo_mts = NULL
) {
  
  # print("in forecast var from model tbl")
  # print(names_exogenous)
  
  starting_names <- names(models_tbl)
  # print("starting_names in forecasts var from")
  # print(starting_names)
  has_short_name <- "short_name" %in% starting_names
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = FALSE)
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
      models_tbl = models_tbl, var_data = var_data, 
      new_t_threshold = new_t_threshold)
    
    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }
  
  models_tbl <- models_tbl %>% 
    mutate(fc_object_raw = map2(fit, variables,
                                ~ forecast_VAR_one_row(
                                  fit = .x, variables = .y, h = fc_horizon, 
                                  names_exogenous = names_exogenous,
                                  extended_exo_mts = extended_exo_mts)
    )
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

read_compare_var_res <- function(var_res_new, var_res_old, h_max = 8, 
                                 rank_h_max = 30) {
  
  var_res_new <- var_res_new %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "new") %>% 
    dplyr::select(-lag_sel_method) 
  
  var_res_old$t_treshold <- 0 
  var_res_old <- var_res_old %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "old",
           var_size = map_dbl(variables, length)) 
  
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
    unique() %>% sort()
  
  size4_vbls_old <-  var_res_old %>% 
    filter(var_size == 4) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  size5_vbls_new <-  var_res_new %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  size5_vbls_old <-  var_res_old %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  return(list(size4_vbls_new = size4_vbls_new, size4_vbls_old = size4_vbls_old,
              size5_vbls_new = size5_vbls_new, size5_vbls_old = size5_vbls_old,
              var_res_old_and_new = old_and_new, var_res_new = var_res_new,
              var_res_old = var_res_old, 
              plot_best_consolidated  = plot_best_consolidated,
              plot_best_each = plot_best_each))
  
} 


# search var one size formerly known as try_sizes_vbls_lags
# then it was modified to work on single size choice
search_var_one_size_old <- function(var_data,
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

    vec_of_other_vbls <- sets_of_other_variables[,j]

    vbls_for_var <- c(already_chosen, vec_of_other_vbls)
    combinations_of_variables_considered[[j]] <- vbls_for_var

    sub_data = var_data[, vbls_for_var]

    sub_data = na.omit(sub_data)

    sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)

    endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
    exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 

    if (length(endov) == 1) {
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
      cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                     fpe = "FPE(n)")
      this_cri <- cri_names[vec_lags]
      named_lags <- sel_criteria[this_cri]
      p_for_estimation <- unique(unname(named_lags))
      max_found_p <- max(p_for_estimation)
      too_high_p <- p_for_estimation > max_p_for_estimation
      p_for_estimation[too_high_p] <- max_p_for_estimation 

      if (any(too_high_p)) {
        binding_max_p <- binding_max_p + 1
      }
    }
    
    if (is.numeric(vec_lags)) {
      lag_sel_method <- "manual"
      p_for_estimation <- unique(vec_lags)
      
      if (add_info_based_lags) {
        lag_sel_method <- "manual_and_info"
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
  
  accu_rankings_models <- as_tibble(accu_rankings_models)
  accu_rankings_models$model_function <- "VAR"
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects,
                combinations_of_variables_considered = combinations_of_variables_considered))
  } else {
    return(list(accu_rankings_models = accu_rankings_models,
                combinations_of_variables_considered = combinations_of_variables_considered))
  }
}



#' Title provides a set of maximum lag values
#'
#' @param vec_lags 
#' @param max_p_for_estimation 
#' @param add_info_based_lags 
#' @param endodata 
#' @param exodata 
#' @param exov 
#' @param discard_negative 
#'
#' @return
#' @export
#'
#' @examples
lags_for_var <- function(vec_lags,  max_p_for_estimation,
                         add_info_based_lags = FALSE,
                         endodata, exodata = NULL, exov = NULL,
                         discard_negative = FALSE, ret_info_results = FALSE) {
  
  
  info_lag_max <- max_p_for_estimation
  
  if (is.character(vec_lags)) {
    lag_sel_method <- "info"
    info_lag_max <-  max_p_for_estimation
    
    exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
    # print("exo_and_lags")
    # print(exo_and_lags)
    
    sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max,
                           exogen = exo_and_lags)

    sel_criteria <- sel$selection
    
    # print("sel")
    # print(sel)
    
    cleaned_criteria <- t(sel$criteria)
    cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
    
    if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
      print("Caution: NaNs or -Inf values in some of the info criteria")
    }

    info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                 which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
    names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
    
    # print("info_based_p_for_estimation")
    # print(info_based_p_for_estimation)
    
    p_for_estimation <- unique(info_based_p_for_estimation)
    max_found_p <- max(p_for_estimation)
    too_high_p <- p_for_estimation > max_p_for_estimation
    p_for_estimation[too_high_p] <- max_p_for_estimation 
  }
  
  if (is.numeric(vec_lags)) {
    lag_sel_method <- "manual"
    p_for_estimation <- unique(vec_lags)
    
    if (add_info_based_lags) {
      lag_sel_method <- "manual_and_info"
      info_lag_max <- max_p_for_estimation
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                             exogen = exo_and_lags)
      sel_criteria <- sel$selection
      cleaned_criteria <- t(sel$criteria)
      cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
      
      if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
        print("Caution: NaNs or -Inf values in some of the info criteria")
      }
      
      info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                   which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
      names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")

      too_high_p <- info_based_p_for_estimation > max_p_for_estimation
      
      info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
      
      p_for_estimation <- unique(c(p_for_estimation, 
                                   info_based_p_for_estimation)
      )
    }
    
  }
  
  if (ret_info_results) {
    return(list(p_for_estimation = p_for_estimation, 
                info_criteria = info_based_p_for_estimation))
  } else {
    return(p_for_estimation)
  }
  
  
}


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
    
    vec_of_other_vbls <- sets_of_other_variables[,j]
    
    vbls_for_var <- c(already_chosen, vec_of_other_vbls)
    combinations_of_variables_considered[[j]] <- vbls_for_var
    
    sub_data = var_data[, vbls_for_var]
    
    sub_data = na.omit(sub_data)
    
    sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
    
    endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
    exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
    
    if (length(endov) == 1) {
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
      cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                     fpe = "FPE(n)")
      this_cri <- cri_names[vec_lags]
      named_lags <- sel_criteria[this_cri]
      p_for_estimation <- unique(unname(named_lags))
      max_found_p <- max(p_for_estimation)
      too_high_p <- p_for_estimation > max_p_for_estimation
      p_for_estimation[too_high_p] <- max_p_for_estimation 
      
      if (any(too_high_p)) {
        binding_max_p <- binding_max_p + 1
      }
    }
    
    if (is.numeric(vec_lags)) {
      lag_sel_method <- "manual"
      p_for_estimation <- unique(vec_lags)
      
      if (add_info_based_lags) {
        lag_sel_method <- "manual_and_info"
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
  
  accu_rankings_models <- as_tibble(accu_rankings_models)
  accu_rankings_models$model_function <- "VAR"
  
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
  
  # print("inside stack mdoels")
  # print("length(models_list)")
  # print(length(models_list))
  # 
  # print("first loop")
  # 
  # for (m in seq(1, length(models_list))) {
  #   print(paste0("m = ", m))
  #   this_thing <- as_tibble(models_list[[m]])
  #   print(names(this_thing))
  # }


  
  
  
  all_models <- as_tibble(reduce(models_list, rbind)) %>%
    dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
    dplyr::select(vars_select(names(.), -starts_with("rank"))) 
  
  
  all_models <- all_models %>%
    mutate(lags = unlist(lags))
  
  
  all_models <- all_models %>% 
    mutate(short_name = map2(variables, lags,
                             ~ make_model_name(variables = .x, lags = .y)),
           short_name = unlist(short_name),
           m_short_name = paste0(short_name, "_", model_function),
           var_size = map_dbl(variables, length)
    )
  
  all_models <- all_models %>% dplyr::distinct(m_short_name, .keep_all = TRUE)
  all_models_ranked <- add_rmse_rankings(all_models)
  
  all_models_ranked <- all_models_ranked %>% 
    dplyr::select(model_function, everything())
  
  return(all_models_ranked)
}


var_cv <- function(var_data,
                   this_p, 
                   this_type = "const", 
                   n_cv = 8,
                   h_max = 8, 
                   train_test_marks = NULL,
                   training_length = "common_max",
                   timetk_idx = TRUE,
                   external_idx = NULL, 
                   test_residuals = TRUE,
                   full_sample_resmat = NULL,
                   names_exogenous = c(""),
                   exo_lag = NULL,
                   future_exo_cv = NULL,
                   this_thresh = 0) {
  
  # print("in var_cv")
  # print(names_exogenous)
  
  # print("")
  # print("start var cv")
  # print("")
  # 
  # print("full_sample_resmat starting var_cv()")
  # print(full_sample_resmat)
  # 
  # print("this_thresh")
  # print(this_thresh)
  
  vbls_for_var <- colnames(var_data)
  
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
  
  if (length(endov) == 1) {
    this_cv <- NA
    print("only one endogenous variable, not a real VAR, returning NA")
    return(this_cv)
  }
  
  endodata <- var_data[ , endov]
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
  exodata <- var_data[ , exov]

  cv_restriction_status <- NULL
  
  if (is.null(exo_lag)) {
    exo_lag <- this_p
  }
  
  
  if (training_length == "common_max") {
    total_obs <- nrow(var_data)
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                                             type = "tscv",
                                             n = n_cv, 
                                             h_max = h_max, 
                                             training_length = training_length, 
                                             timetk_idx = timetk_idx, 
                                             external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
    
    # print("train_test_dates")
    # print(train_test_dates)
    
  }

  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  # cv_fc_object <- list_along(1:n_cv)
  cv_vbl_names <- list_along(1:n_cv)
  cv_lag <- list_along(1:n_cv)
  cv_is_white_noise <- vector(mode = "logical", length = n_cv)

  total_obs <- nrow(var_data)

  if (is.numeric(training_length)) {
    cv_obs_used <- n_cv + training_length + h_max - 1
    if (total_obs < cv_obs_used) {
      print(paste("Warning: For selected variables, balanced sample has only", 
                  total_obs, "obs. Fixed-length cv needs", cv_obs_used, " obs."))
      
      print(paste0("Forecast length: ", h_max, ". Training length: ", 
                   training_length, ". CV rounds: ", n_cv))
    }
  }
  
  for (i in seq_along(1:n_cv)) {
    
    # print(paste0("i = ", i))
    
    this_tra_s <- train_test_dates[[i]]$tra_s
    this_tra_e <- train_test_dates[[i]]$tra_e
    
    this_tes_s <- train_test_dates[[i]]$tes_s
    this_tes_e <- train_test_dates[[i]]$tes_e
    
    # print(this_tra_s)
    # print(this_tra_e)
    # print(this_tes_s)
    # print(this_tes_e)
    
    training_y <- window(endodata, 
                         start = this_tra_s,
                         end = this_tra_e)
    
    this_training_y <- training_y
    
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

      if (!is.null(future_exo_cv)) {
        this_future_exo_cv <- future_exo_cv[[i]]
        test_exo <- this_future_exo_cv[, exov]

        pretest_exodata <- window(exodata, end = this_tra_e)

        if (is.null(dim(exodata))) {
          # print("one exodata series")
          this_exodata <- ts(c(pretest_exodata, test_exo ), frequency =  frequency(pretest_exodata), start = start(pretest_exodata))
        } else {
          # print("multiple exodata series")
          this_exodata <- ts(rbind(pretest_exodata, test_exo ), frequency =  frequency(pretest_exodata), start = start(pretest_exodata))
        }

        this_exo_and_lags <- make_exomat(exodata = this_exodata, exov = exov, 
                                    exo_lag = exo_lag)
        test_exo_and_lags <- window(this_exo_and_lags, 
                                    start = this_tes_s,
                                    end = this_tes_e)
      } else {
        test_exo <- window(exodata, 
                           start = this_tes_s,
                           end = this_tes_e)
        
        test_exo_and_lags <- window(exo_and_lags, 
                                    start = this_tes_s,
                                    end = this_tes_e)
      }
    }

    if (is.null(dim(test_y))) {
      test_rgdp <- test_y
    } else {
      test_rgdp <- test_y[ , "rgdp"]
    }
    
    if (is.null(training_exo_and_lags)) {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
      
    } else {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type, 
                            exogen = training_exo_and_lags)
    }

    if (!is.null(full_sample_resmat)) {
      # print(paste0("fit restriction inside cv, with t-thresh = "), this_thresh)
      # this_var_r <- try(vars::restrict(this_var, method = "manual", 
      #                            resmat = full_sample_resmat), silent = TRUE)
      this_var_r <- try(vars::restrict(this_var, method = "ser", thresh = this_thresh), silent = TRUE)
      
      # print("this_var_r")
      # print(this_var_r)
      
      if (class(this_var_r) == "try-error") {
        cv_restriction_status <- 0
      } else {
        cv_restriction_status <- 1
        this_var <- this_var_r
      }
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
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag,
              cv_is_white_noise = cv_is_white_noise,
              cv_restriction_status = cv_restriction_status))
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

  data_ts <- ts.union(country_data_ts, external_data_ts)
  colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))

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
  
  exodata_fullsample <- VAR_data_for_estimation[,names_exogenous]
  
  tic()
  print("extending (after stationary transformation) exogenous variables for forecasts")
  extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
                                          endo_end = end_target_in_VAR)
  toc()
  
  
  tic()
  print("extending (after stationary transformation) exogenous variables for cv")
  cv_extension_of_exo  <- extending_exogenous_for_cv(
    exodata = exodata_fullsample, h = 8, endo_end = end_target_in_VAR, 
    n_cv = n_cv, same_model_across_cv = FALSE)
  toc()
  
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




var_search_old <- function(country, 
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
  
  data_ts <- ts.union(country_data_ts, external_data_ts)
  colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))
  
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





transform_all_cv <- function(cv_object, current_form,
                             target_level_ts, n_cv) {
  
  if (all(is.na(cv_object))) {
    return(cv_object)
  }

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











