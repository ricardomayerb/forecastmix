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

# rm(all_models)
# rm(all_models_ranked)

format(object.size(all_models_ranked_long), units = "auto")



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
  
  by_h1 <- tbl_best %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_total <- tbl_best %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both <- unique(c(by_h1_20$vbl, by_total_20$vbl))
  
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
  
  by_h1_lags <- tbl_best_lags %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_total_lags <- tbl_best_lags %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  both_lags <- unique(c(by_h1_lags$max_lag, by_total_lags$max_lag))
  
  return( list(freqs_by_h = tbl_best, top_h1_total = both,
               freqs_by_h_lags = tbl_best_lags, top_h1_total_lags = both_lags))
}



# wfoo <- variable_freq_by_n(all_models_ranked, h_max = 7, max_rank = 20, n_freq = 4, is_wide = TRUE)
# wfoo


foo <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 20, n_freq = 4)
foo


foo10 <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 10, n_freq = 4)
foo10

foo5 <- variable_freq_by_n(all_models_ranked_long, h_max = 7, max_rank = 5, n_freq = 4)
foo5

foo$top_h1_total
foo10$top_h1_total
foo5$top_h1_total
