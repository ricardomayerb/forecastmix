source('./R/combinations_functions.R')

ury <- readRDS("./Uruguay_quasi_ave.rds")
ury_om <- readRDS("./Uruguay_quasi_ave_om.rds")
ury_e <- readRDS("./Uruguay_quasi_ave_e.rds")



ury_rgdp_20 <- ury[[1]]
ury_realized <- window(ury_rgdp_20, end = c(2018, 3))
ury_rgdp_10 <- ts(c(ury_realized, ury[[2]]$ave_by_h_fc), 
                  frequency = frequency(ury_realized), 
                  start = start(ury_realized))
ury_rgdp_30 <- ts(c(ury_realized, ury[[4]]$ave_by_h_fc), 
                  frequency = frequency(ury_realized), 
                  start = start(ury_realized))
ury_2018_10 <- window(ury_rgdp_10, start = c(2018, 1), end = c(2018, 4))
ury_2018_20 <- window(ury_rgdp_20, start = c(2018, 1), end = c(2018, 4))
ury_2018_30 <- window(ury_rgdp_30, start = c(2018, 1), end = c(2018, 4))
ury_2019_10 <- window(ury_rgdp_10, start = c(2019, 1), end = c(2019, 4))
ury_2019_20 <- window(ury_rgdp_20, start = c(2019, 1), end = c(2019, 4))
ury_2019_30 <- window(ury_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(ury_2018_10))
print(mean(ury_2018_20))
print(mean(ury_2018_30))
print(mean(ury_2019_10))
print(mean(ury_2019_20))
print(mean(ury_2019_30))


ury_e_rgdp_20 <- ury_e[[1]]
ury_e_realized <- window(ury_e_rgdp_20, end = c(2018, 3))
ury_e_rgdp_10 <- ts(c(ury_e_realized, ury[[2]]$ave_by_h_fc), 
                  frequency = frequency(ury_e_realized), 
                  start = start(ury_e_realized))
ury_e_rgdp_30 <- ts(c(ury_e_realized, ury[[4]]$ave_by_h_fc), 
                  frequency = frequency(ury_e_realized), 
                  start = start(ury_e_realized))
ury_e_2018_10 <- window(ury_e_rgdp_10, start = c(2018, 1), end = c(2018, 4))
ury_e_2018_20 <- window(ury_e_rgdp_20, start = c(2018, 1), end = c(2018, 4))
ury_e_2018_30 <- window(ury_e_rgdp_30, start = c(2018, 1), end = c(2018, 4))
ury_e_2019_10 <- window(ury_e_rgdp_10, start = c(2019, 1), end = c(2019, 4))
ury_e_2019_20 <- window(ury_e_rgdp_20, start = c(2019, 1), end = c(2019, 4))
ury_e_2019_30 <- window(ury_e_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(ury_e_2018_10))
print(mean(ury_e_2018_20))
print(mean(ury_e_2018_30))
print(mean(ury_e_2019_10))
print(mean(ury_e_2019_20))
print(mean(ury_e_2019_30))




ury_om_rgdp_20 <- ury_om[[1]]
ury_om_realized <- window(ury_om_rgdp_20, end = c(2018, 3))
ury_om_rgdp_10 <- ts(c(ury_om_realized, ury_om[[2]]$ave_by_h_fc), 
                  frequency = frequency(ury_om_realized), 
                  start = start(ury_om_realized))
ury_om_rgdp_30 <- ts(c(ury_om_realized, ury_om[[4]]$ave_by_h_fc), 
                  frequency = frequency(ury_om_realized), 
                  start = start(ury_om_realized))
ury_om_2018_10 <- window(ury_om_rgdp_10, start = c(2018, 1), end = c(2018, 4))
ury_om_2018_20 <- window(ury_om_rgdp_20, start = c(2018, 1), end = c(2018, 4))
ury_om_2018_30 <- window(ury_om_rgdp_30, start = c(2018, 1), end = c(2018, 4))
ury_om_2019_10 <- window(ury_om_rgdp_10, start = c(2019, 1), end = c(2019, 4))
ury_om_2019_20 <- window(ury_om_rgdp_20, start = c(2019, 1), end = c(2019, 4))
ury_om_2019_30 <- window(ury_om_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(ury_om_2018_10))
print(mean(ury_om_2018_20))
print(mean(ury_om_2018_30))
print(mean(ury_om_2019_10))
print(mean(ury_om_2019_20))
print(mean(ury_om_2019_30))


nm_tbl_for_comp <- ury[[4]][["cv_tbl"]] %>% 
  dplyr::select(-c(model_type, inv_mse, model_weight_h, weighted_fc_h, 
                   target_mean_fc_yoy, short_name_t, var_size, t_treshold)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  spread(key = rmse_h, value = rmse) %>% 
  mutate(var_size = length(variables)) %>% 
  mutate(rank_1 = 0, rank_2 = 0, rank_3 = 0, rank_4 = 0, 
         rank_5 = 0, rank_6 = 0, rank_7 = 0, rank_8 = 0) %>% 
  rename(t_treshold = t_threshold)


nm_tbl_for_comp_e <- ury_e[[4]][["cv_tbl"]] %>% 
  dplyr::select(-c(model_type, inv_mse, model_weight_h, weighted_fc_h, 
                   target_mean_fc_yoy, short_name_t, var_size, t_treshold)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  spread(key = rmse_h, value = rmse) %>% 
  mutate(var_size = length(variables)) %>% 
  mutate(rank_1 = 0, rank_2 = 0, rank_3 = 0, rank_4 = 0, 
         rank_5 = 0, rank_6 = 0, rank_7 = 0, rank_8 = 0) %>% 
  rename(t_treshold = t_threshold)

nm_tbl_for_comp_e <- nm_tbl_for_comp_e %>% 
  dplyr::select(-c(model_function, m_short_name))


om_tbl_for_comp <- ury_om[[4]][["cv_tbl"]] %>% 
  dplyr::select(-c(model_type, inv_mse, model_weight_h, weighted_fc_h, 
                   target_mean_fc_yoy, short_name_t)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  spread(key = rmse_h, value = rmse) %>% 
  mutate(var_size = length(variables)) %>% 
  mutate(rank_1 = 0, rank_2 = 0, rank_3 = 0, rank_4 = 0, 
         rank_5 = 0, rank_6 = 0, rank_7 = 0, rank_8 = 0) %>% 
  rename(t_treshold = t_threshold)

names(nm_tbl_for_comp)
names(om_tbl_for_comp)

var_res_new <- nm_tbl_for_comp
var_res_new_e <- nm_tbl_for_comp_e
var_res_old <- om_tbl_for_comp

var_res_new <- var_res_new %>% 
  mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
         model_function = "new") %>% 
  dplyr::select(-lag_sel_method) 


var_res_new_e <- var_res_new_e %>% 
  mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
         model_function = "new") %>% 
  dplyr::select(-lag_sel_method) 


var_res_old$t_treshold <- 0 
var_res_old <- var_res_old %>% 
  mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
         model_function = "old",
         var_size = map_dbl(variables, length)) 



old_and_new <- stack_models(list(var_res_new, var_res_old))  

h_max <- 8
rank_h_max <- 30
rmse_names <- paste("rmse", seq(h_max), sep = "_")
is_wide = TRUE
selected_models_tbl = old_and_new

names(var_res_new_e)
names(var_res_new)
names(var_res_old)

old_and_new_e <- stack_models(list(var_res_new_e, var_res_old))  




# selected_models_tbl = old_and_new
# plot_best_consolidated <- single_plot_rmse_all_h(old_and_new, is_wide = TRUE, 
#                                                  h_max = h_max, rank_h_max = rank_h_max)


nm_vs_om <- read_compare_var_res(var_res_new = nm_tbl_for_comp, var_res_old = om_tbl_for_comp)
print(nm_vs_om$plot_best_consolidated)
print(nm_vs_om$plot_best_each)

nm_vs_om_e <- read_compare_var_res(var_res_new = nm_tbl_for_comp_e, var_res_old = om_tbl_for_comp)
print(nm_vs_om_e$plot_best_consolidated)
print(nm_vs_om_e$plot_best_each)


# print(nm_vs_om$plot_best_each)






