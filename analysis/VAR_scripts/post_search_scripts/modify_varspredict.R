source('./R/combinations_functions.R')

# data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
data_object_per_new <- readRDS("./data/VAR_data_Ecuador.rds")
print(colnames(data_object_per_new))
target_transformation <- readRDS("./data/target_transformation/target_transformation_Ecuador.rds")
target_transformation <- target_transformation$target_transformation
# country <- data_object_ury$country_name
# target_transformation <- data_object_ury$target_transformation
# raw_data <- data_object_ury$raw_data
# var_data <- data_object_ury$transformed_data
raw_data <- readRDS("./data/raw_VAR_data/raw_VAR_data_Ecuador.rds")
var_data <- data_object_per_new
old_data <- var_data

# exclude "exp_tradicional" and keep "exp" and "exp_notradicional" 
var_data <- var_data[, ! colnames(var_data) == "exp_tradicional"]

target_variable <- "rgdp"
print(target_transformation)
n_cv <- 8
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])
threshold <- 1.65

# Extend exogenous variables
exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")
exogenous_variables_with_rgc <- c("ip_us", "ip_asia", "ip_ue", "rgc")

names_exogenous <- exogenous_variables 
names_exogenous_with_rgc <- exogenous_variables_with_rgc 

# Forecast the exogenous variables with Arima models. These are used later on in the VAR forecasts and cv with exo variables
exodata_fullsample <- var_data[,exogenous_variables] # note that exogenous_variables is specified at the start of the scirpt and contains all exogenous variables to Uruguay's economic activity.
target_used_in_VAR <- var_data[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))


# tic()
# extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8,
#                                         endo_end = end_target_in_VAR)
# toc()
# tic()
# cv_extension_of_exo <- extending_exogenous_for_cv(
#   exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR,
#   n_cv = n_cv, same_model_across_cv = FALSE)
# toc()
# saveRDS(extension_of_exo, file = "./data/extension_of_exo_us_ue_asia.rds")
# saveRDS(cv_extension_of_exo, file = "./data/cv_extension_of_exo_us_ue_asia.rds")

extension_of_exo <- readRDS(file = "./data/extension_of_exo_us_ue_asia.rds")
cv_extension_of_exo <- readRDS(file = "./data/cv_extension_of_exo_us_ue_asia.rds")

names_all <- colnames(var_data)
names_all

models_from_search <- readRDS("./data/forecast_models/all_ecuador_models_new_data_all_variables_restricted_combos_t165_lag_4.rds")

must_rgc <- models_from_search %>% 
  mutate(has_rgc = map_lgl(variables, ~ "rgc" %in% .x)) %>% 
  filter(has_rgc)

must_rgc_best_10 <- discard_by_rank(must_rgc, 10) %>% arrange(rmse_1)
must_rgc_best_5 <- discard_by_rank(must_rgc, 5) %>% arrange(rmse_1)


rgc_data <- na.omit(var_data[, "rgc"])
rgc_data
rgc_raw <- na.omit(raw_data[, "rgc"])
rgc_raw

future_rgc_raw_no_growth <- rep(last(rgc_raw), fc_horizon)
rgc_raw_extended <- ts(data=c(rgc_raw, future_rgc_raw_no_growth ),
                       start = start(rgc_raw), frequency = frequency(rgc_raw))

rgc_data_extended <- diff(rgc_raw_extended, lag = 1, differences = 2)



length(var_data[, "rgc"] )
length(rgc_data_extended)
early_start <- start(var_data[, "rgc"])
early_nas <- rep(NA, length(var_data[, "rgc"] ) - length(rgc_data_extended) )
replacement_rgc_data <- ts(c(early_nas, rgc_data_extended ),
                           start = early_start, frequency = frequency(rgc_data_extended ))
var_data[, "rgc"] <- replacement_rgc_data

model_one <- must_rgc_best_5[1,]

fit_1 <- fit_VAR_rest(var_data = var_data, variables = model_one$variables[[1]], 
             p = model_one$lags, t_thresh = model_one$t_threshold, 
             names_exogenous = names_exogenous)


new_extension_of_exo <- ts.union(extension_of_exo$extended_exo, replacement_rgc_data)
colnames(new_extension_of_exo) <- names_exogenous_with_rgc

old_fc <- forecast_VAR_one_row(fit = fit_1, variables = model_one$variables[[1]], 
                     names_exogenous = names_exogenous,
                     extended_exo_mts = extension_of_exo$extended_exo, h = fc_horizon)

exo_rgc_fc <- forecast_VAR_one_row(fit = fit_1, variables = model_one$variables[[1]], 
                                   names_exogenous = names_exogenous_with_rgc,
                                   extended_exo_mts = new_extension_of_exo, h = fc_horizon)


predict_conditional <-
  function(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL){
    K <- object$K
    p <- object$p
    obs <- object$obs
    type <- object$type
    data.all <- object$datamat
    ynames <- colnames(object$y)
    n.ahead <- as.integer(n.ahead)
    Z <- object$datamat[, -c(1 : K)]
    B <- Bcoef(object)
    ##
    ## Deterministic and lagged y's
    ## Retrieval of A in matrix (whole)
    ## Deterministic variables in Zdet
    ##
    if(type == "const"){
      Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "const"
    }else if(type == "trend"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "trend"
    }else if(type == "both"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)), nrow = n.ahead, ncol = 2)
      colnames(Zdet) <- c("const", "trend")
    }else if(type == "none"){
      Zdet <- NULL
    }
    ## Include seasonal if applicable
    if(!is.null(eval(object$call$season))){
      season <- eval(object$call$season)
      seas.names <- paste("sd", 1:(season-1), sep = "")
      cycle <- tail(data.all[, seas.names], season)
      seasonal <- as.matrix(cycle, nrow = season, ncol = season - 1)
      if(nrow(seasonal) >= n.ahead){
        seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead, ncol = season -1 )
      } else {
        while(nrow(seasonal) < n.ahead){
          seasonal <- rbind(seasonal, cycle)
        }
        seasonal <- seasonal[1:n.ahead, ]
      }
      rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
      if(!is.null(Zdet)){
        Zdet <- as.matrix(cbind(Zdet, seasonal))
      } else {
        Zdet <- as.matrix(seasonal)
      }
    }
    ## Include exogenous variables if applicable
    if(!is.null(eval(object$call$exogen))){
      if(is.null(dumvar)){
        stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
      }
      if(!all(colnames(dumvar) %in% colnames(data.all))){
        stop("\nColumn names of dumvar do not coincide with exogen.\n")
      }
      if(!identical(nrow(dumvar), n.ahead)){
        stop("\nRow number of dumvar is unequal to n.ahead.\n")
      }
      if(!is.null(Zdet)){
        Zdet <- as.matrix(cbind(Zdet, dumvar))
      } else {
        Zdet <- as.matrix(dumvar)
      }
    }
    ## Retrieving predetermined y variables
    Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))])
    yse <- matrix(NA, nrow = n.ahead, ncol = K)
    sig.y <- .fecov(x = object, n.ahead = n.ahead)
    for(i in 1 : n.ahead){
      yse[i, ] <- sqrt(diag(sig.y[, , i]))
    }
    yse <- -1 * qnorm((1 - ci) / 2) * yse
    colnames(yse) <- paste(ci, "of", ynames)
    ## forecast recursion
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    lasty <- c(Zy[nrow(Zy), ])
    for(i in 1 : n.ahead){
      lasty <- lasty[1 : (K * p)]
      Z <- c(lasty, Zdet[i, ])
      forecast[i, ] <- B %*% Z
      temp <- forecast[i, ]
      lasty <- c(temp, lasty)
    }
    colnames(forecast) <- paste(ynames, ".fcst", sep="")
    lower <- forecast - yse
    colnames(lower) <- paste(ynames, ".lower", sep="")
    upper <- forecast + yse
    colnames(upper) <- paste(ynames, ".upper", sep="")
    forecasts <- list()
    for(i in 1 : K){
      forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[, i], yse[, i])
      colnames(forecasts[[i]]) <- c("fcst", "lower", "upper", "CI")
    }
    names(forecasts) <- ynames
    result <- list(fcst = forecasts, endog = object$y, model = object, exo.fcst = dumvar)
    class(result) <- "varprd"
    return(result)
  }
