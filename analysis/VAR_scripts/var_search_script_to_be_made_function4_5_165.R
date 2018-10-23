source('./R/VAR_functions.R')

initial_time <- Sys.time()

tic(msg = "Total time for this country")
##### data selection part -----
# arguments
country_name <- "Argentina"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

# file paths
excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                          "_exercise_", forecast_exercise_number, "/")

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")


##### data pre-processing part -----

country_data_ts <- get_raw_data_ts(country_name, excel_data_path)
external_data_ts <- get_raw_external_data_ts(excel_data_path)

data_ts <- country_data_ts

rgdp_level_ts <- data_ts[, "rgdp"]
rgdp_level_ts <- na.omit(rgdp_level_ts)
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

print(paste0("This country: ", country_name))
print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
print("Names of variables: ")
print(colnames(data_ts))

tic()
print("Finding and applying stationary transformations to all variables")
reco_all_variables <- find_statio_diffs(data_ts, country_name)
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
        paste0(output_path, "VAR_data_",country_name,".rds"))


##### VAR selection part -----
# arguments
## target, pre-chosen and exogenous variables
target_variable <- c("rgdp")
other_prechosen_variables <- c("")

## VAR sizes (how many variables per VAR) to explore:

vec_var_sizes <- c(2, 3, 4, 5)
# vec_var_sizes <- c(2, 3, 4)
vec_freq_limit <- list("none", "none", 20, 15) # number *includes* rgdp
# vec_freq_limit <- list("none", "none", 10)


## VAR lags to explore
vec_lags <- c(1, 2, 3, 4, 5, 6)
add_aic_bic_hq_fpe_lags <-  FALSE

## VAR restrictions
restrict_by_signif <- TRUE
t_tresh <- c(1.65, 1.65, 1.65, 1.65)

## forecast horizon
fc_horizon <- 8

## Cross-validation parameters
number_of_cv <- 8
train_span <- 31
ret_cv = TRUE


## model retention parameters
max_rank_some_h <- 50
max_rank_some_h_for_freq <- 50 


if (train_span + fc_horizon + number_of_cv > nrow(VAR_data_for_estimation)) {
  print("not enough obs")
  stop()
}

per_size_results <- list_along(vec_var_sizes)
f_vbls_list <- list_along(vec_var_sizes)
selection_for_next_size_list <- list_along(vec_var_sizes)
current_consolidated_models_list <- list_along(vec_var_sizes)

tic(msg = "Finish var search")


for (i in seq(length(vec_var_sizes))) {

  this_size <- vec_var_sizes[i]
  print(paste0("Starting the estimation of VAR with ", this_size," variables"))
  
  this_t_tresh <- t_tresh[i]
  this_freq_limit <- vec_freq_limit[[i]]

  if (this_freq_limit == "none") {
    print("Using all variables")
    this_VAR_data <- VAR_data_for_estimation
  }
  

  if (i > 1 & is.numeric(this_freq_limit)) {
    print("Using this subset of variables: ")
    print(new_select_vbls)
    
    this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
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
  
  if (i == 1) {
    current_consolidated_models <- stack_models(
      list(var_res[["accu_rankings_models"]])
      ) 
  } else {
    current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
  }
  
  if (i < length(vec_var_sizes)) {
    next_freq_limit <- vec_freq_limit[[i + 1]]
  }
  
  if (i == length(vec_var_sizes)) {
    next_freq_limit <- "none"
  }
  
  
  if (next_freq_limit == "none") {
    f_vbls <- variable_freq_by_n(current_consolidated_models, 
                                 h_max = fc_horizon, max_rank = max_rank_some_h_for_freq,
                                 n_freq = ncol(data_ts), is_wide = TRUE)
    freq_sel_vbls_by_multi <- colnames(VAR_data_for_estimation) 
    new_select_vbls <- colnames(VAR_data_for_estimation) 
    vbls_top_small <- NA
    by_total_not_in_tsm <- NA
  }
  
  if (is.numeric(next_freq_limit)) {
    f_vbls <- variable_freq_by_n(current_consolidated_models, 
                                 h_max = fc_horizon, max_rank = max_rank_some_h_for_freq,
                                 n_freq = next_freq_limit, is_wide = TRUE)
    freq_sel_vbls_by_multi <- f_vbls$vbl_multi
    vbls_top_small <- f_vbls$variables_in_top_small
    
    if(length(vbls_top_small) > next_freq_limit) {
      "number of best-3 variables exceeds next freq limit. Downsizing."
      vbls_top_small <- vbls_top_small[1:next_freq_limit]
    }
    
    
    by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
    
    print("vector tiene NA:")
    print(by_total_not_in_tsm)
    by_total_na <- is.na(by_total_not_in_tsm)
    print("by_total_na")
    print(by_total_na)
    print(!by_total_na)
    
    by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
    
    n_gap_vbls <- next_freq_limit - length(vbls_top_small)
    
    print("vbls_top_small")
    print(vbls_top_small)
    print("by_total_not_in_tsm")
    print(by_total_not_in_tsm)
    print("n_gap_vbls")
    print(n_gap_vbls)
    
    if (n_gap_vbls > 0) {
      extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
      print("extra_vbls")
      print(extra_vbls)
    } else {
      extra_vbls <- c()
    }
    
    new_select_vbls <- c(vbls_top_small, extra_vbls)
    
    print("new_select_vbls")
    print(new_select_vbls)
    
    
  }
  
  file_suffix <- paste0("_size_", this_size, "_fqlim_", this_freq_limit,
                        "_t_", this_t_tresh, "mr", max_rank_some_h,
                        "_mrfq", max_rank_some_h_for_freq, ".rds")
  filename <- paste0("var_results_", country_name, file_suffix)
  saveRDS(var_res, paste0(output_path, filename))
  
  per_size_results[[i]] <- var_res
  f_vbls_list[[i]] <- f_vbls
  selection_for_next_size_list[[i]] <- new_select_vbls
  current_consolidated_models_list[[i]] <- current_consolidated_models
  
  toc()
}

toc()

bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)

consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))

res_and_info <- list(consolidated_var_res = consolidated_var_res,
                     f_vbls_all_sizes = f_vbls_list,
                     selected_for_next_size = selection_for_next_size_list)


allsizes <- paste(vec_var_sizes, collapse = "")
allthresh <- paste(t_tresh, collapse = "")
allfqlim <- paste(vec_freq_limit, collapse = "")

file_suffix_all_sizes <-  paste0("_s", allsizes, "_fq", allfqlim,
                                 "_t", allthresh, "_mr", max_rank_some_h,
                                 "_mrfq", max_rank_some_h_for_freq,
                                 "_cv",number_of_cv,"_tspan", train_span,
                                 "_h", fc_horizon,".rds")

filename <- paste0("vr_", country_name, file_suffix_all_sizes)
saveRDS(res_and_info, paste0(output_path, filename))

final_time <- Sys.time()

elapsed_time <- final_time - initial_time


