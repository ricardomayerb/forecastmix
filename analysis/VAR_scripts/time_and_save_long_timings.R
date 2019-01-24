source('./R/combinations_functions.R')

data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
print(names(data_object_ury))
country <- data_object_ury$country_name
target_transformation <- data_object_ury$target_transformation
raw_data <- data_object_ury$raw_data
var_data <- data_object_ury$transformed_data
print(target_transformation)

this_target_ts <- na.omit(raw_data[,target_variable])

tic()
this_t_100_u <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts, 
                       reps = 1)
toc()

