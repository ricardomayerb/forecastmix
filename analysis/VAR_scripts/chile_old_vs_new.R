source('./R/VAR_functions.R')

new_tbl <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/Chile_s1234_t2.rds")
new_tbl <- new_tbl %>% 
  dplyr::select(-c(lag_sel_method, t_treshold, var_size, short_name)) %>% 
  mutate(origin = "new_code") %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank")))
names(new_tbl)

# old_tbl <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Brasil_by_step_12345_to_comp.rds")
old_tbl <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds")

old_tbl <- old_tbl %>% 
  dplyr::select(variables, lags, everything()) %>% 
  dplyr::select(-c(rank_8, rmse_8)) %>% 
  mutate(origin = "old_code") %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank")))

names(old_tbl)

old_and_new <- rbind(new_tbl, old_tbl)
old_and_new_ranked <- add_rmse_rankings(old_and_new) %>% 
  mutate(short_name = map2_chr(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y)),
         lags = unlist(lags),
         size = map_dbl(variables, length)
  ) %>% 
  dplyr::select(short_name, origin, everything()) %>%
  dplyr::select(-variables) %>% 
  arrange(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7)

names(old_and_new_ranked)

print(old_and_new_ranked, n = 30)

country_name <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2

output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")

saveRDS(old_and_new_ranked,
        paste0(output_path, country_name, "_old_and_new.rds"))

