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
library(ggplot2)
library(ggthemes)




any_fc_2_fc_yoy <- function(current_fc, rgdp_transformation, rgdp_level_ts) {
  
  if (is.null(current_fc)) {
    yoy_fc <- NULL
    return(yoy_fc)
  }
  
  yq_pre_fc <- as.yearqtr(min(time(current_fc)) - 0.25)
  
  end_adjusted <- c(year(yq_pre_fc), quarter(yq_pre_fc))
  
  rgdp_level_end_adjusted  <- window(rgdp_level_ts, end = end_adjusted )
  
  
  if (rgdp_transformation == "yoy") {
    yoy_fc <- current_fc
  }
  
  
  if (rgdp_transformation == "log") {
    level_fc <- exp(current_fc)
    fc_and_data <- ts(c(rgdp_level_end_adjusted, level_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }
  
  
  if (rgdp_transformation == "none") {
    rgdp_data_transformed <- rgdp_level_end_adjusted 
    fc_and_data <- ts(c(rgdp_level_end_adjusted, current_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }
  
  
  if (rgdp_transformation == "diff_yoy") {
    
    rgdp_yoy_end_adjusted <- make_yoy_ts(rgdp_level_end_adjusted)
    
    last_data_undiff <- window(rgdp_yoy_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    # print("last_data_undiff")
    # print(last_data_undiff)
    # 
    # print("current_fc")
    # print(current_fc)
    
    yoy_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    # print("yoy_fc")
    # print(yoy_fc)
    
  }
  
  
  if (rgdp_transformation == "diff") {
    
    last_data_undiff <- window(rgdp_level_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    level_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    fc_and_data <- ts(c(rgdp_level_end_adjusted, level_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
    
  }
  
  return(yoy_fc)
  
}

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


each_plot_rmse_all_h <- function(selected_one, selected_two, extra_models = NULL,
                                 is_wide = FALSE, h_max = 7, 
                                 rank_h_max = 30) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if (is_wide) {
    selected_one <-  selected_one %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      ungroup()
    
    selected_two <-  selected_two %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      ungroup()
  }
  
  
  
  rmse_table_single_h <- selected_one %>% rbind(selected_two) %>%  
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  rmse_table_single_h <- rmse_table_single_h %>%
    arrange(rmse_h, rmse, model_function) %>% 
    mutate(idx = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  v_ticks <- 1 + n_models_h * (0:(max_horizon - 1))
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.5) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept = v_ticks , alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = v_ticks[1] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 1", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[2] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 2", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[3] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 3", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[4] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 4", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[5] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 5", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[6] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 6", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[7] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 7", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[8] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()) +
    theme(axis.title = element_text(face = "bold"))
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}

facet_rmse_all_h <- function(selected_models_tbl, extra_models = NULL) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon)
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  rmse_table_single_h <- rmse_table_single_h %>% 
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n()) %>% 
    group_by(horizon) %>% 
    mutate(id_in_h = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  
  labels <- c(rmse_1 = "RMSE h = 1", rmse_2 = "RMSE h = 2", 
              rmse_3 = "RMSE h = 3", rmse_4 = "RMSE h = 4",
              rmse_5 = "RMSE h = 5", rmse_6 = "RMSE h = 6",
              rmse_7 = "RMSE h = 7", rmse_8 = "RMSE h = 8")
  
  # labels <- paste0("RMSE h = ", 1:max_horizon)
  # print(labels)
  
  p <- ggplot(rmse_table_single_h, aes(x = id_in_h, y = rmse)) + 
    geom_point(aes(color = model_function), size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    facet_wrap(~ rmse_h, labeller=labeller(rmse_h = labels)) + 
    theme_bw()  + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  
  
  return(p)
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
    tests_of_stationarity$country <- country
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
  
  colap_variables <- paste(variables, collapse = "___")
  # print(colap_variables)
  
  if (!remove_base) {
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




make_models_tbl <- function(arima_res, var_models_and_rmse, VAR_data, h_max, 
                            force.constant, ave_rmse_sel = FALSE, pval_arima = 0.05) {
  

  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy %>% mutate(id = 1:n())
  rmse_level_sarimax <- arima_res$compare_rmse %>% mutate(id = 1:n())
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  
  each_h_just_model_and_ave_rmse_var <- var_models_and_rmse %>% 
    mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
    dplyr::select(- starts_with("rank"))
  
  
  each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
    mutate(model_function = "Arima") %>% 
    dplyr::select(variable, lag, id, starts_with("yoy"), arima_order, arima_seasonal, 
                  model_function) %>% 
    rename(variables = variable, lags = lag) %>% 
    rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))
  
 
  if (ave_rmse_sel) {
    models_rmse_at_each_h_arima  <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
      mutate(ave_rmse = rowMeans(select(., starts_with("rmse")))) %>% 
      group_by(variables) %>%
      mutate(min_ave_per_variable = min(ave_rmse)) %>% 
      filter(ave_rmse == min_ave_per_variable) %>% 
      ungroup() %>% 
      gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
      ungroup() %>% 
      group_by(rmse_h) %>% 
      mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
      filter(rmse <= rgdp_rmse) %>% 
      ungroup() %>% 
      select(-c(ave_rmse, rgdp_rmse, min_ave_per_variable)) %>% 
      arrange(rmse_h, variables)
    
  } else {
    models_rmse_at_each_h_arima <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
      gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
      arrange(variables) %>% 
      group_by(rmse_h, variables) %>% 
      mutate(min_per_variable_and_h = min(rmse)) %>% 
      filter(rmse == min_per_variable_and_h) %>% 
      select(-min_per_variable_and_h ) %>%  
      ungroup() %>% 
      group_by(rmse_h) %>% 
      mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
      filter(rmse <= rgdp_rmse) %>% 
      ungroup() %>% 
      select(-rgdp_rmse) %>% 
      arrange(rmse_h, rmse)
  }
  
  
  models_rmse_at_each_h_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse"))
  
  models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                                 models_rmse_at_each_h_arima) %>% 
    mutate(inv_mse = 1/rmse^2) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    arrange(rmse_h, rank_h)
  
  
  models_rmse_at_each_h <- models_rmse_at_each_h %>%
    mutate(short_name = map2(variables, lags,
                             ~ make_model_name(variables = .x, lags = .y)),
           long_name = pmap(list(variables, lags, model_function),
                            ~ make_model_name(variables = ..1, lags = ..2,
                                              model_function = ..3)),
           short_name = as_factor(unlist(short_name)),
           long_name = as_factor(unlist(long_name))
    ) 
  
  my_stability_fun <- function(model_type, model_object) {
    
    # print(model_type)
    # print(model_object)
    
    if (model_type == "Arima") {
      is.stable <- TRUE
      
    }
    if (model_type == "VAR"){
      is.stable <- all(roots(model_object) < 1)
    }
    
    if(!is.stable) {
      print("Ooops, not stable")
    }
    
    return(is.stable)
  }
  
  tic()
  models_rmse_at_each_h <- models_rmse_at_each_h %>%
    dplyr::distinct(long_name, .keep_all = TRUE) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse),
           fit = pmap(list(model_function, variables, lags, arima_order, 
                           arima_seasonal),
                      ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                      lags = ..3, order = ..4, seasonal = ..5,
                                      extended_x_data_ts = extended_x_data_ts,
                                      arima_rgdp_ts = rgdp_ts_in_arima,
                                      force.constant = force.constant,
                                      var_data = VAR_data)),
           is_stable = map2(model_function, fit, 
                            ~my_stability_fun(model_type = .x, model_object = .y)),
           is_white_noise = map2(model_function, fit, 
                                 ~check_resid_VAR_Arima(model_function = .x, 
                                                        fit = .y,
                                                        pval_arima = pval_arima))
    ) %>% 
    ungroup() %>% filter(is_stable == TRUE, is_white_noise == TRUE) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    select(-fit)
  toc()
  
  return(models_rmse_at_each_h)
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

single_plot_rmse_all_h <- function(selected_models_tbl, extra_models = NULL,
                                   is_wide = FALSE, h_max = 7, 
                                   rank_h_max = 30) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if (is_wide) {
    selected_models_tbl <-  selected_models_tbl %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      ungroup()
  }
  

    
  rmse_table_single_h <- selected_models_tbl %>% 
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  
  # rmse_table_single_h <- rmse_table_single_h %>%
  #   arrange(rmse_h, model_function, rmse) %>% 
  #   mutate(idx = 1:n())
  
  rmse_table_single_h <- rmse_table_single_h %>%
    arrange(rmse_h, rmse, model_function) %>% 
    mutate(idx = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  v_ticks <- 1 + n_models_h * (0:(max_horizon - 1))
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.5) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept = v_ticks , alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = v_ticks[1] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 1", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[2] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 2", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[3] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 3", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[4] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 4", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[5] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 5", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[6] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 6", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[7] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 7", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[8] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()) +
    theme(axis.title = element_text(face = "bold"))
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}





transform_cv <- function(list_series, series_name, current_form,
                             auxiliary_ts, n_cv) {
  
  # print("list_series")
  # print(list_series)
  # print("unlist(list_series)")
  # print(unlist(list_series))
  # print("is.null(unlist(list_series))")
  # print(is.null(unlist(list_series)))
  
  if (is.null(unlist(list_series))) {
    new_series_list <- list_series
    return(new_series_list)
  }
  
  
  series_name <- series_name
  new_series_list <- list_along(1:n_cv)
  
  if (current_form == "diff_yoy") {
    len_initial_cond <- 1
    
    for (td in seq_along(1:n_cv)) {
      
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
    
    for (td in seq_along(1:n_cv)) {
      
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


un_diff_ts <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)
  
  this_year <- as.integer(floor(time(diffed_ts)))
  this_quarter <- as.integer(4 * (time(diffed_ts) - this_year + 0.25))
  undiffed_ts <- ts(undiffed, start = c(first(this_year), first(this_quarter)),
                    end = c(last(this_year), last(this_quarter)), frequency = 4)
  
  return(undiffed_ts)
}
