source('./R/VAR_functions.R')
library(tidyselect)

country_name <- "Peru"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

s2_12345 <- readRDS(file = "analysis/VAR_output/Peru_s2_12345_t2.rds")
s3_ic <- readRDS(file = "analysis/VAR_output/Peru_s3_aic_fpe_hq_sc_t2.rds")
s3_12345 <- readRDS(file = "analysis/VAR_output/Peru_s3_12345_t2.rds")
s4_ic <- readRDS(file = "analysis/VAR_output/Peru_s4_aic_fpe_hq_sc_t2.rds")
s4_12345 <- readRDS(file = "analysis/VAR_output/Peru_s4_12345_t2.rds")
s4_6 <- readRDS(file = "analysis/VAR_output/Peru_s4_6_t2.rds")

lags_12345 <- unlist(s3_12345$lags)
table(lags_12345)

lags_ic <- unlist(s3_ic$lags)
table(lags_ic)

s3_12345_ic <- as_tibble(rbind(s3_12345, s3_ic)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  mutate(lags = unlist(lags)) 

all_models <- as_tibble(rbind(s2_12345, s3_12345, s3_ic, s4_12345, s4_6, s4_ic)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  mutate(lags = unlist(lags)) 

print(all_models %>% arrange(rmse_1), n = 90)

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

}

foo <- add_rmse_rankings(all_models)




