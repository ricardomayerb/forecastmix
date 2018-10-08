library(tibbletime)
library(xts)
library(readxl)
library(timetk)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(forecast)
library(tictoc)
library(tidyselect)



comb_ndiffs <- function(this_series, return_4_seas = FALSE, 
                        do_other_seas = FALSE, seas_test = "seas") {
  
  tests_names <- c("kpss", "pp", "adf")
  tests_season_names <- c("seas", "ocsb", "hegy", "ch")
  tests_alpha <- c(0.01, 0.05, 0.1)
  tests_type <- c("level", "trend")
  
  
  tests_of_stationarity <- as_tibble(
    expand.grid(tests_names, tests_type, tests_alpha,
                stringsAsFactors = FALSE)) %>% 
    rename(test = Var1, deter_part = Var2, alpha = Var3) %>% 
    mutate(seas_result = map_dbl(alpha,
                                 ~ nsdiffs(x = this_series, alpha = ., 
                                           test = seas_test)),
           seas_test = seas_test,
           sta_result = pmap_dbl(list(test, alpha, deter_part),
                                 ~ ndiffs(x = this_series, alpha = ..2,
                                          test = ..1, type = ..3)),
           sta_result_after_seas = pmap_dbl(
             list(test, alpha, deter_part, seas_result),
             ~ ndiffs(x = my_diff(this_series, lag = 4, differences = ..4), 
                      alpha = ..2, test = ..1, type = ..3)),
           recommendation = pmap_chr(
             list(seas_result, sta_result, sta_result_after_seas),
             ~ make_recommendation(seas = ..1, sta = ..2, sta_after_seas = ..3)
           )
    ) %>% 
    dplyr::select(test, deter_part, alpha, sta_result, seas_test,
                  seas_result, sta_result_after_seas, recommendation)
  
  if (do_other_seas) {
    tests_of_seasonality <- as_tibble(
      expand.grid(tests_season_names, tests_alpha, stringsAsFactors = FALSE)) %>% 
      rename(test = Var1, alpha = Var2) %>% 
      mutate(seas_result = map2_dbl(test, alpha,
                                    suppressWarnings(
                                      ~ nsdiffs(x = this_series, alpha = .y,
                                                test = .x)))
      )
  }
  
  
  if (return_4_seas) {
    return(list(stationarity = tests_of_stationarity, 
                seas = tests_of_seasonality))
  } else {
    return(tests_of_stationarity)
  }
  
}


drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[,!(names(df) %in% vars_to_drop)]
}

fcs_accu <- function(fc_mat, test_data_mat) {
  
  errors_mat <- test_data_mat - fc_mat
  rmse_vec <- sqrt(colMeans(errors_mat^2))
  mean_rmse <- mean(rmse_vec)
  return(mean_rmse)
}


find_statio_diffs <- function(data_ts, country = "this_country") {
  
  names_of_variables <- colnames(data_ts)
  sta_reco_list <- list_along(names_of_variables)
  stationarity_list <- list_along(names_of_variables)
  
  
  for (j in seq_along(names_of_variables)) {
    this_variable_name <- names_of_variables[j]
    this_variable_ts <- data_ts[ , this_variable_name]
    this_variable_ts <- na.omit(this_variable_ts)
    tests_of_stationarity <- suppressWarnings(comb_ndiffs(this_variable_ts))
    tests_of_stationarity$country <- country_name
    tests_of_stationarity$variable <- this_variable_name
    
    reco <- get_reco_from_sta(tests_of_stationarity, this_variable_name)
    
    stationarity_list[[j]] <- tests_of_stationarity
    sta_reco_list[[j]] <- reco
    
  }
  
  names(stationarity_list) <- names_of_variables
  names(sta_reco_list) <- names_of_variables
  
  reco_all_variables <- reduce(sta_reco_list, rbind)
  
  return(reco_all_variables)
}


follow_rec <- function(data_tbl_ts, table_of_recommendations) {
  
  rec_rows <- nrow(table_of_recommendations)
  
  rec_column <- "kpss_05_level"
  
  new_variables_list <- list_along(1:rec_rows)
  
  for (i in seq_len(rec_rows)) {
    
    this_rec <- table_of_recommendations[[i, rec_column]]
    this_variable <- table_of_recommendations[[i, "variable"]]
    this_variable_ts <- data_tbl_ts[, this_variable] 
    
    
    
    if (this_rec == "level") {
      new_variable_ts <- this_variable_ts
    }
    
    if (this_rec == "yoy") {
      new_variable_ts <- make_yoy_ts(this_variable_ts)
    }
    
    if (this_rec == "diff") {
      new_variable_ts <- base::diff(this_variable_ts)
    }
    
    if (this_rec == "diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts))
    }
    
    if (this_rec == "diff_diff") {
      new_variable_ts <- base::diff(this_variable_ts, differences = 2)
    }
    
    if (this_rec == "diff_diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts),
                                    differences = 2)
    }
    
    new_variables_list[[i]] <- new_variable_ts
    
    
  }
  
  new_data_ts <- reduce(new_variables_list, ts.union)
  colnames(new_data_ts) <- colnames(data_tbl_ts)
  
  return(new_data_ts)
  
}


get_raw_data_ts <- function(country, data_path = "./data/excel/"){
  
  this_file_path <- paste0(data_path, country, ".xlsx")

  general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                      "month", "conf_emp", "conf_ibre", "ip_ine", 
                                      "vta_auto", "exist"))
  # to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
  # "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
  extra_vars_to_drop <- list(Argentina = c("emae", "", "", "", "", "", "", "", "", "", ""), 
                             Bolivia = c("igae", "hidrocarburo", "", "", "", "", "", "", "", "", "", ""), 
                             Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Ecuador = c("confianza_con", "confianza_emp", "m1", "rm", "", "", "", "", "", ""), 
                             Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
                             Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                             Uruguay = c("cred", "imp_nonpetro", "", "", "", "", "", "", "", ""))
  
  variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)
  
  this_variables_to_drop <- variables_to_drop[[country]]
  
  this_q <- read_excel(this_file_path, sheet = "quarterly", na = c("", "NaN"))
  this_q <- as_tbl_time(this_q, index = date)
  this_q <- dplyr::select(this_q, -c(year, hlookup))
  this_q[is.nan(as.matrix(this_q))] <- NA
  
  
  if(country == "Uruguay") {
    this_q[, "rm"] <- - this_q[, "rm"]
  }
  
  this_m <- read_excel(this_file_path, sheet = "monthly", na = c("", "NaN"))
  # this_m <- replace_na(data = this_m, replace = "NaN")
  this_m[is.nan(as.matrix(this_m))] <- NA
  # print(this_m, n = 320)
  this_m <- as_tbl_time(this_m, index = date)
  
  this_m_q <- this_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  # this_m_q[is.nan(as.matrix(as_tibble(this_m_q)))] <- NA
  
  # print(this_m_q, n = 320)
  
  this_q <- drop_this_vars(this_q, this_variables_to_drop)
  this_m_q <- drop_this_vars(this_m_q, this_variables_to_drop)
  
  # print("this_q")
  # print(this_q, n = 120)
  # print("this_m_q")
  # print(this_m_q, n = 120)
 
  m_and_q <- left_join(this_q, this_m_q, by = "date")

  maq_start <- first(tk_index(m_and_q))
  m_and_q_ts <- suppressWarnings(
    tk_ts(m_and_q, frequency = 4, start = c(year(maq_start),
                                            quarter(maq_start)))
    )
  
  m_and_q_ts[is.nan(m_and_q_ts)] <- NA
  
  return(m_and_q_ts)
}


get_raw_external_data_ts <- function(data_path = "./data/excel/"){
  
  external_path <- paste0(data_path,  "external.xlsx")
  
  variables_to_drop <- c("year", "month", "hlookup")
  
  external_m <- read_excel(external_path, sheet = "monthly", na = c("", "NaN"))
  external_m[is.nan(as.matrix(external_m))] <- NA
  
  external_m <- as_tbl_time(external_m, index = date)
  external_m_q <- external_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  external_m_q <- external_m_q %>% dplyr::select(- variables_to_drop)

  external_start <- first(tk_index(external_m_q))
  external_m_q_ts <- suppressWarnings(tk_ts(external_m_q, frequency = 4,
                                            start = c(year(external_start), 
                                                      quarter(external_start))))
  
  external_m_q_ts[is.nan(external_m_q_ts)] <- NA
  
  return(external_m_q_ts)
}



get_reco_from_sta <- function(stdata, variable_name) {
  
  unanim <- stdata %>% 
    mutate(unanimity = min(recommendation) == max(recommendation),
           unanimity = ifelse(unanimity, recommendation, NA)) %>% 
    dplyr::select(country, unanimity) %>% 
    unique()
  
  unanim_deter_level <- stdata %>%
    filter(deter_part == "level" ) %>% 
    mutate(unan_level = min(recommendation) == max(recommendation),
           unan_level = ifelse(unan_level, recommendation, NA)) %>% 
    dplyr::select(country, unan_level) %>% 
    unique()
  
  unanim_05_deter_level <- stdata %>%
    filter(deter_part == "level", alpha == 0.05 ) %>% 
    mutate(unan_05_level = min(recommendation) == max(recommendation),
           unan_05_level = ifelse(unan_05_level, recommendation, NA)) %>% 
    dplyr::select(country, unan_05_level) %>% 
    unique()
  
  unanim_kpss <- stdata %>% 
    filter(test == "kpss") %>% 
    mutate(unan_kpss = min(recommendation) == max(recommendation),
           unan_kpss = ifelse(unan_kpss, recommendation, NA)) %>% 
    dplyr::select(country, unan_kpss) %>% 
    unique()
  
  unanim_kpss_level <- stdata %>% 
    filter(test == "kpss", deter_part == "level") %>% 
    mutate(unan_kpss_lev = min(recommendation) == max(recommendation),
           unan_kpss_lev = ifelse(unan_kpss_lev, recommendation, NA)) %>% 
    dplyr::select(country, unan_kpss_lev) %>% 
    unique()
  
  kpss_reco <- stdata %>% 
    filter(test == "kpss", deter_part == "level", alpha == 0.05) %>%
    dplyr::select(country, recommendation) %>% 
    rename(kpss_05_level = recommendation)
  
  country_recos <- left_join(unanim, unanim_deter_level, by = "country") %>% 
    left_join(unanim_05_deter_level, by = "country") %>% 
    left_join(unanim_kpss, by = "country") %>% 
    left_join(unanim_kpss_level, by = "country") %>% 
    left_join(kpss_reco, by = "country")
  
  country_recos$variable <- variable_name
  
  # yoy_reco <- stdata %>% 
  #   filter(recommendation == "yoy")
  # 
  # diff_yoy_reco <- stdata %>% 
  #   filter(recommendation == "diff_yoy")
  
  return(country_recos)
}



make_model_name <- function(variables, lags, model_function = NULL, 
                            base_variable = "rgdp", remove_base = FALSE) {
  
  variables <- sort(variables)
  
  colap_variables <- paste(variables, collapse = " ")
  # print(colap_variables)
  
  if (remove_base) {
    if (is.null(model_function)) {
      short_name <- paste(colap_variables, lags, sep = "__")
      model_name <- short_name
    } else {
      long_name <- paste(model_function, colap_variables, lags, sep = "__")
      model_name <- long_name
    }
  } else {
    if (is.null(model_function)) {
      short_name <- paste(colap_variables, lags, sep = "__")
      short_name <- str_remove(short_name, "rgdp_")
      model_name <- short_name
    } else {
      long_name <- paste(model_function, colap_variables, lags, sep = "__")
      long_name <- str_remove(long_name, "rgdp_")
      model_name <- long_name
    }
  }
  
  return(model_name)
}


make_recommendation <- function(seas, sta, sta_after_seas) {
  
  if (seas == 1 & sta_after_seas == 0) {
    recommendation <- "yoy"
  } 
  
  if (seas == 0 & sta_after_seas == 0) {
    recommendation <- "level"
  } 
  if (seas == 1 & sta_after_seas == 1) {
    recommendation <- "diff_yoy"
  } 
  if (seas == 0 & sta_after_seas == 1) {
    recommendation <- "diff"
  } 
  if (seas == 0 & sta_after_seas == 2) {
    recommendation <- "diff_diff"
  } 
  if (seas == 1 & sta_after_seas == 2) {
    recommendation <- "diff_diff_yoy"
  } 
  
  return(recommendation)
  
}



make_test_dates_list <- function(ts_data, type = "tscv", n = 8, h_max = 6,
                                 timetk_idx = TRUE, training_length = 20,
                                 external_idx = NULL) {
  
  data_length <- nrow(ts_data)
  
  date_time_index <- as.yearqtr(time(ts_data))
  
  list_of_positions <- list_along(seq(1:n))
  list_of_dates <- list_along(seq(1:n))
  list_of_year_quarter <- list_along(seq(1:n))
  
  if (type == "tscv") {
    
    for (i in seq.int(1:n)) {
      
      from_the_right <-  i - 1
      
      end_test_pos <- data_length - from_the_right 
      start_test_pos <- end_test_pos - h_max + 1
      end_training_pos <- start_test_pos - 1
      start_training_pos <- end_training_pos - training_length + 1
      
      
      end_test_date <- date_time_index[end_test_pos]
      start_test_date <- date_time_index[start_test_pos] 
      end_training_date <- date_time_index[end_training_pos]
      start_training_date <- date_time_index[start_training_pos]
      
      end_test_year <- year(end_test_date)
      start_test_year <- year(start_test_date) 
      end_training_year <- year(end_training_date) 
      start_training_year <- year(start_training_date)
      
      end_test_quarter <- quarter(end_test_date)
      start_test_quarter <- quarter(start_test_date) 
      end_training_quarter <- quarter(end_training_date) 
      start_training_quarter <- quarter(start_training_date)
      
      this_pos <- list(
        tra_s = start_training_pos, 
        tra_e = end_training_pos,
        tes_s = start_test_pos, 
        tes_e = end_test_pos)
      
      this_date <- list(
        tra_s = start_training_date, 
        tra_e = end_training_date,
        tes_s = start_test_date, 
        tes_e = end_test_date)
      
      this_yq <- list(
        tra_s = c(start_training_year, start_training_quarter),
        tra_e = c(end_training_year, end_training_quarter),
        tes_s = c(start_test_year, start_test_quarter),
        tes_e = c(end_test_year, end_test_quarter)
      )
      
      list_of_positions[[i]] <- this_pos
      list_of_dates[[i]] <- this_date
      list_of_year_quarter[[i]] <- this_yq
      
    }
    
    return(list(
      list_of_year_quarter = list_of_year_quarter,
      list_of_dates = list_of_dates,
      list_of_positions = list_of_positions)
    )
    
  }
  
}



make_yoy_ts <- function(df_ts, freq = 4, is_log = FALSE) {
  
  if (is_log) {
    df_ts <- exp(df_ts)
  }
  
  new_ts <- base::diff(df_ts, lag = freq)/stats::lag(df_ts, k = -freq)
  
  return(new_ts)
}


my_diff <- function(series, lag = 1, differences = 1) {
  if (differences == 0) {
    x_diff <- series
  } else {
    x_diff <- diff(series, lag = lag, differences = differences)
  }
  return(x_diff)
}



transform_cv <- function(list_series, series_name, current_form,
                         auxiliary_ts) {
  
  
  current_form <- current_form
  
  series_name <- series_name
  
  if (current_form == "diff_yoy") {
    len_initial_cond <- 1
  }
  
  new_series_list <- list_along(1:number_of_cv)
  
  
  for (td in seq_along(1:number_of_cv)) {
    
    this_test_data <- list_series[[td]]
    test_time <- time(this_test_data)
    start_test <- min(test_time)
    end_initial_cond <- start_test - 0.25
    start_initial_cond <- start_test - 0.25*len_initial_cond
    end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                              quarter(as.yearqtr(end_initial_cond))
    )
    start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                quarter(as.yearqtr(start_initial_cond))
    )
    initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                              end = end_initial_cond_y_q)
    
    if (current_form == "diff_yoy") {
      new_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
    }
    
    
    new_series_list[[td]] <- new_test_data
    
  }
  
  return(new_series_list)
  
}



transform_cv_new <- function(list_series, series_name, current_form,
                             auxiliary_ts) {
  
  # print("in transform_cv_new")
  # current_form <- current_form
  # print("in transform_cv, current form")
  # print(current_form)
  # print("auxiliary_ts")
  # print(auxiliary_ts)
  # print("list_series")
  # print(list_series)
  
  
  series_name <- series_name
  new_series_list <- list_along(1:number_of_cv)
  
  if (current_form == "diff_yoy") {
    len_initial_cond <- 1
    
    for (td in seq_along(1:number_of_cv)) {
      
      this_test_data <- list_series[[td]]
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      
      new_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      
      
      new_series_list[[td]] <- new_test_data
      
    }
    
  }
  
  if (current_form == "diff") {
    len_initial_cond <- 1
    
    for (td in seq_along(1:number_of_cv)) {
      
      this_test_data <- list_series[[td]]
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      level_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      pre_test_level_data <- window(auxiliary_ts, end = end_initial_cond_y_q)
      data_and_test_level <- ts(c(pre_test_level_data, level_test_data),
                                frequency = 4, start = start(auxiliary_ts))
      
      data_and_test_yoy <- make_yoy_ts(data_and_test_level, freq = 4, 
                                       is_log = FALSE)
      
      new_test_data <- window(data_and_test_yoy, start = start(this_test_data),
                              end = end(this_test_data))
      
      new_series_list[[td]] <- new_test_data
    }
    
  }
  
  
  return(new_series_list)
  
}


