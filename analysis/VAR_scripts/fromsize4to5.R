source('./R/VAR_functions.R')

country_name <- "Peru"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

s2_12345 <- readRDS(file = "analysis/VAR_output/Peru_s2_12345_t2.rds")
s3_ic <- readRDS(file = "analysis/VAR_output/Peru_s3_aic_fpe_hq_sc_t2.rds")
s3_12345 <- readRDS(file = "analysis/VAR_output/Peru_s3_12345_t2.rds")

s4_ic <- readRDS(file = "analysis/VAR_output/Peru_s4_aic_fpe_hq_sc_t2.rds")
s4_12345 <- readRDS(file = "analysis/VAR_output/Peru_s4_12345_t2.rds")
s4_6 <- readRDS(file = "analysis/VAR_output/Peru_s4_6_t2.rds")

all_models <- as_tibble(rbind(s2_12345, s3_12345, s3_ic, s4_12345, s4_6, s4_ic)) %>%
  dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
  mutate(lags = unlist(lags))

# all_models <- as_tibble(rbind(s2_12345, s3_12345, s3_ic)) %>% 
#   dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
#   dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
#   mutate(lags = unlist(lags)) 


all_models <- all_models %>% 
  mutate(short_name = map2(variables, lags,
                           ~ make_model_name(variables = .x, lags = .y)),
         short_name = unlist(short_name),
         var_size = map_dbl(variables, length)
         )

all_models <- all_models %>% dplyr::distinct(short_name, .keep_all = TRUE)

format(object.size(all_models), units = "auto")

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

all_models_ranked <- add_rmse_rankings(all_models)

all_models_ranked  <- all_models_ranked %>% 
  dplyr::select(short_name, var_size, lags, everything()) %>% 
  dplyr::select(everything(), -one_of(c("variables", "full_sample_varest")),
                one_of(c("variables", "full_sample_varest"))) 

format(object.size(all_models_ranked), units = "auto")

all_models_ranked_long  <- all_models_ranked %>% 
  dplyr::select(-full_sample_varest) %>% 
  gather(key = "rmse_h", value = "rmse", 
         c("rmse_1", "rmse_2", "rmse_3", "rmse_4", "rmse_5", "rmse_6", "rmse_7")
  ) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  group_by(rmse_h) %>% 
  arrange(rmse_h, rmse) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup()

rm(all_models)
rm(all_models_ranked)

format(object.size(all_models_ranked_long), units = "auto")


foo <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 20, n_freq = 4)
# foo

foo10 <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 10, n_freq = 4)
# foo10

foo5 <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 5, n_freq = 4)
# foo5

foo$top_h1_total
foo10$top_h1_total
foo5$top_h1_total
