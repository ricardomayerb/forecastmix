source('./R/VAR_functions.R')

country <- "Peru"
print(paste0("this country: ", country))
# names_exogenous <- c("ip_us", "ip_ue", "ip_asia")
names_exogenous <- c("ip_us", "ip_ue", "ip_asia")
forecast_exercise_year <- 2018
forecast_exercise_number <- 3
fc_horizon <- 8
add_aic_bic_hq_fpe_lags <- FALSE # default value, can be omitted for shorter code
# vec_lags <-  c(5) # default value, can be omitted for shorter code

default_vec_lag <-  c(3,4,5)
default_treshold <- c(1.65)

Search_step_1 <- list(size = 2, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_2 <- list(size = 3, vbl_selection_type = "none", lags = default_vec_lag)
Search_step_3 <- list(size = 4, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 3, lags = default_vec_lag)
# Search_step_3 <- list(size = 4, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 3, lags = default_vec_lag, add_augmented_models = TRUE)
Search_step_4 <- list(size = 5, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag)
# Search_step_4 <- list(size = 5, vbl_selection_type = "incremental_auto_prechosen", n_new_prechosen = 2, lags = default_vec_lag, add_augmented_models = TRUE)
search_plan <- list(Search_step_1, Search_step_2, Search_step_3, Search_step_4)

number_of_cv <- 8 # default value, can be omitted for shorter code
train_span <- "common_max"
max_rank_some_h <- 50 # default value, can be omitted for shorter code
results_file_name <-  paste0(country, "_auto_3s4_2s5.rds")
return_cv <-  TRUE

country <- country
search_plan <- search_plan
forecast_exercise_year <- forecast_exercise_year
forecast_exercise_number <- forecast_exercise_number
fc_horizon <- fc_horizon
add_aic_bic_hq_fpe_lags <- add_aic_bic_hq_fpe_lags
number_of_cv <- number_of_cv
train_span <- train_span
max_rank_some_h <- max_rank_some_h
results_file_name <- results_file_name
ret_cv <- return_cv
max_small_rank <- 3
max_rank_some_h_for_freq <- 30
names_exogenous <- names_exogenous

target_variable = c("rgdp")
default_t_treshold <- default_treshold
restrict_by_signif <- TRUE
max_rank_some_h <- 50
results_file_name <- NULL
exo_lag <-  NULL
combn_already_tried <-  NULL

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

target_used_in_VAR <- VAR_data_for_estimation[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))
exodata_fullsample <- VAR_data_for_estimation[, names_exogenous]

tic()
print("extending (after stationary transformation) exogenous variables for forecasts")
extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
                                        endo_end = end_target_in_VAR)
toc()


tic()
print("extending (after stationary transformation) exogenous variables for cv")
cv_extension_of_exo  <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = 8, endo_end = end_target_in_VAR, 
  n_cv = number_of_cv, same_model_across_cv = FALSE)
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
  
  print("This t tresh = ")
  print(this_t_tresh)
  
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
  
  # print("this_t_tresh")
  # print(this_t_tresh)
  
  this_t_tresh_str <- paste0("_", 100*this_t_tresh)
  this_t_tresh_str <- reduce(this_t_tresh_str, paste0)
  # print("this_t_tresh_str")
  # print(this_t_tresh_str)
  
  
  file_suffix <- paste0("_size_", this_size,
                        "_t", this_t_tresh_str, "_mr", max_rank_some_h,
                        "_mrfq", max_rank_some_h_for_freq, ".rds")
  
  filename <- paste0("var_results_", country, file_suffix)
  print("filename")
  print(filename)
  
  saveRDS(var_res, paste0(output_path, filename))
  
  per_size_results[[i]] <- var_res
  f_vbls_list[[i]] <- f_vbls
  
  prechosen_variables_at_each_step[[i]] <- this_prechosen_variables
  current_consolidated_models_list[[i]] <- current_consolidated_models
  cv_objects_list[[i]] <- var_res[["cv_objects"]]
  
  toc()
}

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

# allsizes <- paste(n_steps, collapse = "")
# allthresh <- "foo"
# # allthresh <- paste(t_tresh, collapse = "")
# allfqlim <- paste(c(9,6,6), collapse = "")
# 
# file_suffix_all_sizes <-  paste0("_s", allsizes,
#                                  "_t", allthresh, "_mr", max_rank_some_h,
#                                  "_mrfq", max_rank_some_h_for_freq,
#                                  "_cv",number_of_cv,"_tspan", train_span,
#                                  "_h", fc_horizon,".rds")


file_suffix_all_sizes <- "_auto_3s4_2s5_norest"

if(is.null(results_file_name)) {
  filename <- paste0("vr_", country, file_suffix_all_sizes)
} else {
  filename <- results_file_name
}

print("filename")
print(filename)

saveRDS(res_and_info, paste0(output_path, filename))
