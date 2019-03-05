source('./R/combinations_functions.R')

all_specs_objects_r <- readRDS("./data/examples/end_of_search_models_15_restr.rds")
all_specs_models_r <- all_specs_objects_r[[1]]
all_specs_models_r_not_u <- all_specs_models_r %>% filter(!is_unrestricted)


all_specs_objects_u <- readRDS("./data/examples/end_of_search_models_15.rds")
all_specs_models_u <- all_specs_objects_u

ranking_r <- 20
ranking_u <- 200

best_h1_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_1)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
         )
best_h1_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_1)) %>% 
  filter(ranking <= ranking_u) 
names_best_h1_u <- sort(best_h1_u$short_name)
names_best_h1_r <- sort(best_h1_just_r$name_of_r)
h1_r_derived_from_u <- names_best_h1_r %in% names_best_h1_u
percent_r_in_u_h1 <- 100 * sum(h1_r_derived_from_u)/ranking_r
percent_r_in_u_h1



best_h2_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_2)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h2_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_2)) %>% 
  filter(ranking <= ranking_u) 
names_best_h2_u <- sort(best_h2_u$short_name)
names_best_h2_r <- sort(best_h2_just_r$name_of_r)
h2_r_derived_from_u <- names_best_h2_r %in% names_best_h2_u
percent_r_in_u_h2 <- 100 * sum(h2_r_derived_from_u)/ranking_r
percent_r_in_u_h2



best_h3_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_3)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h3_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_3)) %>% 
  filter(ranking <= ranking_u) 
names_best_h3_u <- sort(best_h3_u$short_name)
names_best_h3_r <- sort(best_h3_just_r$name_of_r)
h3_r_derived_from_u <- names_best_h3_r %in% names_best_h3_u
percent_r_in_u_h3 <- 100 * sum(h3_r_derived_from_u)/ranking_r
percent_r_in_u_h3



best_h4_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_4)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h4_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_4)) %>% 
  filter(ranking <= ranking_u) 
names_best_h4_u <- sort(best_h4_u$short_name)
names_best_h4_r <- sort(best_h4_just_r$name_of_r)
h4_r_derived_from_u <- names_best_h4_r %in% names_best_h4_u
percent_r_in_u_h4 <- 100 * sum(h4_r_derived_from_u)/ranking_r
percent_r_in_u_h4



best_h5_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_5)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h5_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_5)) %>% 
  filter(ranking <= ranking_u) 
names_best_h5_u <- sort(best_h5_u$short_name)
names_best_h5_r <- sort(best_h5_just_r$name_of_r)
h5_r_derived_from_u <- names_best_h5_r %in% names_best_h5_u
percent_r_in_u_h5 <- 100 * sum(h5_r_derived_from_u)/ranking_r
percent_r_in_u_h5



best_h6_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_6)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h6_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_6)) %>% 
  filter(ranking <= ranking_u) 
names_best_h6_u <- sort(best_h6_u$short_name)
names_best_h6_r <- sort(best_h6_just_r$name_of_r)
h6_r_derived_from_u <- names_best_h6_r %in% names_best_h6_u
percent_r_in_u_h6 <- 100 * sum(h6_r_derived_from_u)/ranking_r
percent_r_in_u_h6



best_h7_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_7)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h7_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_7)) %>% 
  filter(ranking <= ranking_u) 
names_best_h7_u <- sort(best_h7_u$short_name)
names_best_h7_r <- sort(best_h7_just_r$name_of_r)
h7_r_derived_from_u <- names_best_h7_r %in% names_best_h7_u
percent_r_in_u_h7 <- 100 * sum(h7_r_derived_from_u)/ranking_r
percent_r_in_u_h7




best_h8_just_r <- all_specs_models_r_not_u %>% 
  mutate(ranking = rank(rmse_8)) %>% 
  filter(ranking <= ranking_r) %>% 
  mutate(name_of_r = map2_chr(variables, lags, ~ make_model_name(.x, .y, 0))
  )
best_h8_u <- all_specs_models_u %>% 
  mutate(ranking = rank(rmse_8)) %>% 
  filter(ranking <= ranking_u) 
names_best_h8_u <- sort(best_h8_u$short_name)
names_best_h8_r <- sort(best_h8_just_r$name_of_r)
h8_r_derived_from_u <- names_best_h8_r %in% names_best_h8_u
percent_r_in_u_h8 <- 100 * sum(h8_r_derived_from_u)/ranking_r
percent_r_in_u_h8


percent_r_in_u_all_h <- c(percent_r_in_u_h1, percent_r_in_u_h2, percent_r_in_u_h3,
                          percent_r_in_u_h4, percent_r_in_u_h5, percent_r_in_u_h6,
                          percent_r_in_u_h7, percent_r_in_u_h8)

print(percent_r_in_u_all_h)

# search_specs_objects <- readRDS("./data/examples/end_of_search_models_15_restr.rds")
# search_specs_objects <- search_specs_orbjects[[1]]
# search_models <- search_specs_orbjects
# 
# all_models_best_h1 <- 
