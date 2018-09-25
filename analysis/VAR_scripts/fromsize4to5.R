source('./R/VAR_functions.R')

country_name <- "Peru"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

s2_12345 <- readRDS(file = "analysis/VAR_output/Peru_s2_12345_t2.rds")
s3_ic <- readRDS(file = "analysis/VAR_output/Peru_s3_aic_fpe_hq_sc_t2.rds")
s3_12345 <- readRDS(file = "analysis/VAR_output/Peru_s3_12345_t2.rds")

# s4_ic <- readRDS(file = "analysis/VAR_output/Peru_s4_aic_fpe_hq_sc_t2.rds")
# s4_12345 <- readRDS(file = "analysis/VAR_output/Peru_s4_12345_t2.rds")
# s4_6 <- readRDS(file = "analysis/VAR_output/Peru_s4_6_t2.rds")

# all_models <- as_tibble(rbind(s2_12345, s3_12345, s3_ic, s4_12345, s4_6, s4_ic)) %>% 
#   dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
#   dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
#   mutate(lags = unlist(lags)) 

all_models <- as_tibble(rbind(s2_12345, s3_12345, s3_ic)) %>% 
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

max20_models_ranked <- all_models_ranked %>% 
  filter_at( vars(starts_with("rank")), any_vars(. <= 20)) 

max10_models_ranked <- all_models_ranked %>% 
  filter_at( vars(starts_with("rank")), any_vars(. <= 10)) 


all_models_ranked <- all_models_ranked %>% 
  arrange(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6, rank_7)

lags_all <- unlist(all_models_ranked$lags)
table(lags_all)

max10_models_ranked_long  <- max10_models_ranked %>% 
  gather(key = "rmse_h", value = "rmse", 
         c("rmse_1", "rmse_2", "rmse_3", "rmse_4", "rmse_5", "rmse_6", "rmse_7")
         ) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  group_by(rmse_h) %>% 
  arrange(rmse_h, rmse) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup()
  

variable_n_tbl <- max10_models_ranked %>% 
  dplyr::select(variables, rmse_h, rank_h, short_name) %>% 
  group_by(rmse_h) %>% 
  summarise(unique_variables = list(unique(unlist(variables))),
            non_unique_variables = list(unlist(variables)),
            n = length(unlist(unique_variables)) - 1) %>% 
  select(horizon, n, unique_variables, non_unique_variables)


variable_n_tbl <- this_models_tbl %>% 
  dplyr::select(variables, rmse_h, rank_h, long_name, horizon) %>% 
  group_by(horizon) %>% 
  summarise(unique_variables = list(unique(unlist(variables))),
            non_unique_variables = list(unlist(variables)),
            n = length(unlist(unique_variables)) - 1) %>% 
  select(horizon, n, unique_variables, non_unique_variables)

all_variables <- unlist(this_models_tbl$variables)

all_variables_freq_table <- tibble::as.tibble(table(all_variables)) %>% 
  arrange(desc(n))

all_variables_h_freqs <- this_models_tbl %>% 
  dplyr::select(variables, rmse_h, rank_h, long_name, horizon) %>% 
  group_by(horizon) %>% 
  summarise(freq = list(tibble::as.tibble(table(unlist(variables)))) ) 


tbl_with_freqs_per_h <- reduce(all_variables_h_freqs$freq,
                               full_join, by = "Var1") 

names(tbl_with_freqs_per_h) <- c("variable", paste0("n_", 1:h_max))

tbl_with_freqs_per_h <- tbl_with_freqs_per_h %>% 
  mutate(ave = rowSums(.[2:(h_max+1)], na.rm = TRUE)/h_max) %>% 
  arrange(desc(ave), desc(n_1), desc(n_2), desc(n_3), desc(n_4) ) 



