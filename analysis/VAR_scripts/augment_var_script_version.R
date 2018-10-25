source('./R/VAR_functions.R')

country <- "Chile"
forecast_exercise_year <- 2018
forecast_exercise_number <- 2
output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                      forecast_exercise_year, 
                      "_exercise_", forecast_exercise_number, "/")


chl_filename_old <- "./analysis/VAR_output/edd_exercises/2018_exercise_2/from_older_version_code/Chile_by_step_12345.rds"
chl_old <- readRDS(chl_filename_old)



chl165_partial_filename_new <- "vr_Chile_s2345_fqnonenone2015_t165165165165_mr200_mrfq100_cv8_tspan39_h8.rds"
chl165_filename_new <- paste0(output_path, chl165_partial_filename_new)

chl165_rds <- readRDS(chl165_filename_new)
chl_new <- chl165_rds$consolidated_var_res


chl_s3_rds <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/var_results_Chile_size_3_fqlim_none_t_1.65mr50_mrfq50.rds")
chl_s3_new <- chl_s3_rds$accu_rankings_models

chl_s4_rds <- readRDS("./analysis/VAR_output/edd_exercises/2018_exercise_2/var_results_Chile_size_4_fqlim_20_t_1.65mr50_mrfq50.rds")
chl_s4_new <- chl_s4_rds$accu_rankings_models

names(chl_old)
names(chl_new)
names(chl_s3_new)
names(chl_s4_new)


chile_data <- readRDS(paste0(output_path, "VAR_data_Chile.rds"))

this_max_rank <- 3

str_extract_all(names(chl_s3_new), "rmse")
this_names <- names(chl_s3_new)
rmse_names <- this_names[str_detect(this_names, "rmse")]
rmse_names
ranking_cols <- this_names[str_detect(this_names, "rank")]
ranking_cols


n_group_of_best_models <- 3 

tbl_of_models <- chl_s3_new %>% 
  gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
  dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
  group_by(rmse_h) %>% 
  arrange(rmse_h, rmse) %>% 
  mutate(rank_h = rank(rmse),
         nth_rank_h = nth(unique(rank_h), n_group_of_best_models)
         ) %>% 
  ungroup()



vec_of_rmse_h <- sort(unique(tbl_of_models$rmse_h))

# tbl_of_models[3:4, "rank_h"] <- 3.5

tbl_of_models_best <- tbl_of_models %>% 
  filter(rank_h <= nth_rank_h)

this_variables <- tbl_of_models_best[[1, "variables"]]
print(this_variables)

h_max <-  8
n_cv <- 8
training_length <- 39

augment_one_var <- function(variables, this_lag, t_tresh, var_full_dataset, 
                            previously_estimated = NULL) {
  tic()
  all_variables <- colnames(var_full_dataset)
  # print(all_variables)
  neg_is_in_variables <- ! (all_variables %in% variables)
  rest_of_variables <- all_variables[neg_is_in_variables] 
  # print(rest_of_variables)
  print(length(rest_of_variables))
  j <- 0
  for (i in seq_along(rest_of_variables)) {
    
    additional_variable <- rest_of_variables[i]
    new_variables <- c(variables, additional_variable)
    # print(new_variables)
    new_var_data <- na.omit(var_full_dataset[, new_variables])
    new_var <- vars::VAR(y = new_var_data, p = this_lag, type = "const")
    new_var_restricted <- try(vars::restrict(new_var, method = "ser", 
                                             thresh = t_tresh), silent = TRUE)
    
    if(class(new_var_restricted)[1] == "try-error") {
      model_is_valid <- FALSE
    } 
    
    if(class(new_var_restricted)[1] != "try-error") {
      
      var_restrictions <- new_var_restricted$restrictions
      
      is_stable <- all(roots(new_var_restricted) < 1)
      
      if(is_stable) {
        is_white_noise <- check_resid_VAR(new_var_restricted)
        if(is_white_noise) {
          model_is_valid <- TRUE
        } else {
          model_is_valid <- FALSE
        }
      } else {
        model_is_valid <- FALSE
      }
    }
    
    model_row <- list(variables = new_variables, lags = this_lag, 
                      t_tresh = t_tresh, is_valid = model_is_valid)
    
    if(model_is_valid) {
      j <- j+1
      this_cv <-  var_cv(var_data = new_var_data, this_p = this_lag,
                         this_type = "const", h_max = h_max,
                         n_cv = n_cv, training_length = training_length, 
                         test_residuals = FALSE,
                         full_sample_resmat = var_restrictions)
      
      print(this_cv)
    }
    
    
    
    
  }
  print(paste0("Performed cv on ", j, " models."))
  toc()
}

augment_one_var(variables = this_variables, this_lag = 3, t_tresh = 1.65, var_full_dataset = chile_data)


full_sample_var <- try(vars::restrict(full_sample_var, method = "ser", 
                                      thresh = t_tresh), silent = TRUE)

# idea: at after size 3 look at the second most common (first is rgdp) variable in best 20 VARs and make it pre-chosen and then try all other variables to fill the remaining 2 spots
# after size 4, look at the third most common (rgdp and previously prechose are tied at first) and make it pre-chosen and then try all other variables to fill the remaining 2 spots

foo <- letters
moo <- c("b", "d", "f")
roo <-  c("e", "r", "f")
goo <- c("p", "a", "t")
roo2 <- c("f", "r", "e")

loo <- list(moo, roo, goo)

any(map_lgl(loo, identical, roo))
any(map_lgl(loo, ~ identical(.x, roo)))

any(map_lgl(loo, identical, roo2))
any(map_lgl(loo, ~ identical(.x, roo2)))
any(map_lgl(loo, ~ identical(sort(.x), sort(roo2) )))
