source('./R/combinations_functions.R')


exodata_fullsample <- var_data[,names_exogenous]
target_used_in_VAR <- var_data[, target_variable]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))
fc_horizon <- 8
n_cv <- 10

tic()
extension_of_exo_list <- extending_exogenous(exodata = exodata_fullsample,
                                        h = fc_horizon,
                                        endo_end = end_target_in_VAR)
toc()

tic()
cv_extension_of_exo_list <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR,
  n_cv = n_cv, same_model_across_cv = FALSE)
toc()

saveRDS(object = extension_of_exo_list, file = "./data/examples/example_extension_of_exo.rds")
saveRDS(object = cv_extension_of_exo_list, file = "./data/examples/cv_example_extension_of_exo.rds")