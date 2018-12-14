source('./R/combinations_functions.R')

bra <- readRDS("./Brasil_quasi_ave.rds")
bra_om <- readRDS("./Brasil_quasi_ave_om.rds")


bra_rgdp_20 <- bra[[1]]
bra_realized <- window(bra_rgdp_20, end = c(2018, 3))
bra_rgdp_10 <- ts(c(bra_realized, bra[[2]]$ave_by_h_fc), 
                  frequency = frequency(bra_realized), 
                  start = start(bra_realized))
bra_rgdp_30 <- ts(c(bra_realized, bra[[4]]$ave_by_h_fc), 
                  frequency = frequency(bra_realized), 
                  start = start(bra_realized))
bra_2018_10 <- window(bra_rgdp_10, start = c(2018, 1), end = c(2018, 4))
bra_2018_20 <- window(bra_rgdp_20, start = c(2018, 1), end = c(2018, 4))
bra_2018_30 <- window(bra_rgdp_30, start = c(2018, 1), end = c(2018, 4))
bra_2019_10 <- window(bra_rgdp_10, start = c(2019, 1), end = c(2019, 4))
bra_2019_20 <- window(bra_rgdp_20, start = c(2019, 1), end = c(2019, 4))
bra_2019_30 <- window(bra_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(bra_2018_10))
print(mean(bra_2018_20))
print(mean(bra_2018_30))
print(mean(bra_2019_10))
print(mean(bra_2019_20))
print(mean(bra_2019_30))


bra_om_rgdp_20 <- bra_om[[1]]
bra_om_realized <- window(bra_om_rgdp_20, end = c(2018, 3))
bra_om_rgdp_10 <- ts(c(bra_om_realized, bra_om[[2]]$ave_by_h_fc), 
                  frequency = frequency(bra_om_realized), 
                  start = start(bra_om_realized))
bra_om_rgdp_30 <- ts(c(bra_om_realized, bra_om[[4]]$ave_by_h_fc), 
                  frequency = frequency(bra_om_realized), 
                  start = start(bra_om_realized))
bra_om_2018_10 <- window(bra_om_rgdp_10, start = c(2018, 1), end = c(2018, 4))
bra_om_2018_20 <- window(bra_om_rgdp_20, start = c(2018, 1), end = c(2018, 4))
bra_om_2018_30 <- window(bra_om_rgdp_30, start = c(2018, 1), end = c(2018, 4))
bra_om_2019_10 <- window(bra_om_rgdp_10, start = c(2019, 1), end = c(2019, 4))
bra_om_2019_20 <- window(bra_om_rgdp_20, start = c(2019, 1), end = c(2019, 4))
bra_om_2019_30 <- window(bra_om_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(bra_om_2018_10))
print(mean(bra_om_2018_20))
print(mean(bra_om_2018_30))
print(mean(bra_om_2019_10))
print(mean(bra_om_2019_20))
print(mean(bra_om_2019_30))


nm_tbl_for_comp <- bra[[4]][["cv_tbl"]] %>% 
  dplyr::select(-c(model_type, inv_mse, model_weight_h, weighted_fc_h, 
                   target_mean_fc_yoy, short_name_t, var_size)) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  spread(key = rmse_h, value = rmse) %>% 
  mutate(var_size = length(variables)) %>% 
  mutate(rank_1 = 0, rank_2 = 0, rank_3 = 0, rank_4 = 0, 
         rank_5 = 0, rank_6 = 0, rank_7 = 0, rank_8 = 0) %>% 
  rename(t_treshold = t_threshold)

om_tbl_for_comp <- bra_om[[4]][["cv_tbl"]] %>% 
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

nm_vs_om <- read_compare_var_res(var_res_new = nm_tbl_for_comp, var_res_old = om_tbl_for_comp)

print(nm_vs_om$plot_best_consolidated)
print(nm_vs_om$plot_best_each)






