source('./R/VAR_functions.R')

bolivia_var_result <- var_search(country = "Bolivia",
                  sizes = c(2, 3, 4, 5),
                  forecast_exercise_year = 2018, 
                  forecast_exercise_number = 2,
                  fc_horizon = 7) 











