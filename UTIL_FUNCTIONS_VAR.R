#**************************************************************************************
#*
#* VAR FUNCTIONS 
#*
#***************************************************************************************

GET_TS_DATA <- function(data_mpox) {
  
  'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
  var_data <- data_mpox %>%
    dplyr::select(Jurisdiction, date_week_start, Week_Number, Cases) %>%
    pivot_wider(id_cols = c(Week_Number, date_week_start), names_from = Jurisdiction, values_from = Cases) #id_cols = c(date_week_start, Week_Number)
  #%>% dplyr::select(-Week_Number)
  
  print(paste0('nrow data_ts: ', nrow(var_data)))
  print(paste0('ncol data_ts: ', ncol(var_data)))
  
  return(var_data)
}

GET_TS_DATA_DIFF <- function(data_mpox) {
  
  'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
  data_diff_long <- data_mpox %>%
    group_by(Jurisdiction) %>%
    mutate(Cases_Diff = c(NA, diff(Cases))) %>%
    ungroup() %>%
    filter(!is.na(Cases_Diff)) # Remove the first row of each jurisdiction
  
  data_diff <- data_diff_long %>%
    pivot_wider(id_cols = c(Week_Number), names_from = Jurisdiction, values_from = Cases_Diff) %>%
    dplyr::select(-Week_Number, - date_week_start)
  
  #data_diff = data_diff[, correct_col_order]
  
  print(paste0('nrow data_ts: ', nrow(data_diff)))
  print(paste0('ncol data_ts: ', ncol(data_diff)))
  
  return(data_diff)
}


#1. check_stationarity
check_stationarity <- function(time_series){
  
  string_explanation = 'Null hypothesis: Time series is Non-stationary. 
  If the p value < 0.05, we reject the null hypothesis that the time-series is non-stationary,
  In favour of the alternate hypothesis; that the time series is stationary'
  print(string_explanation)
  
  adf_test <- adf.test(time_series)
  print(adf_test)
}

#2. Check_stationarity: AT Jurisdiction level
check_stationarity_state <- function(df_time_series, DIFF = FALSE){
  
  string_explanation = 'Null hypothesis: Time series is Non-stationary. 
  If the p value < 0.05, we reject the null hypothesis that the time-series is non-stationary,
  In favour of the alternate hypothesis; that the time series is stationary'
  print(string_explanation)
  unique_states = unique(df_time_series$Jurisdiction)
  
  for (state_i in unique_states){
    
    print(paste0(state_i))
    df_state = df_time_series %>% filter(Jurisdiction == state_i)
    
    if(DIFF){
      print('DIFFERENCED')
      diff_df_state <- diff(df_state$Cases)
      adf_test <- adf.test(diff_df_state)
      
    } else {
      print('ORIGINAL - NOT DIFFERENCED TIME-SERIES')
      adf_test <- adf.test(df_state$Cases)
     
    }
    
    print(adf_test)
    print('************')
  }
}



#Optimal Lag 
get_optimal_lag <- function(data, max_lag = 10,
                            list_cols = c("data_diff_sd", "data_diff_la", "data_diff_nyc")){
  
  lag_selection <- VARselect(data[, list_cols], 
                             lag.max = max_lag, 
                             type = "const")
  print(lag_selection)
}


get_var_forecast <- function(data, list_cols = c("data_diff_sd", "data_diff_la", "data_diff_nyc"),
                             num_days_forecast = 14, num_lags = 10){
  
  #Data
  print(paste0('num days forecast:', num_days_forecast))
  train_data <- data[1:(nrow(data) - num_days_forecast),
                     list_cols]
  test_data <- data[(nrow(data) - (num_days_forecast - 1)):nrow(data),
                    list_cols]
  
  #Lag selection
  lag_selection <- VARselect(data[, list_cols], 
                             lag.max = num_lags, 
                             type = "const")
  print(lag_selection)
  var_model <- VAR(train_data, p = num_lags, type = "const")
  summary(var_model)
  
  forecast <- predict(var_model, n.ahead = num_days_forecast)
  
  list_return = list(train_data = train_data, test_data = test_data, forecast = forecast, var_model = var_model)
  
  return(list_return)
  
}

#OTHER
# GET_TS_DATA <- function(data_mpox) {
#   
#   'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
#   var_data <- data_mpox %>%
#     dplyr::select(Jurisdiction, Week_Number, Cases) %>%
#     pivot_wider(id_cols = c(date_week_start, Week_Number), names_from = Jurisdiction, values_from = Cases) %>%
#     dplyr::select(-Week_Number)
#   
#   print(paste0('nrow data_ts: ', nrow(var_data)))
#   print(paste0('ncol data_ts: ', ncol(var_data)))
#   
#   return(var_data)
# }
# 
# GET_TS_DATA_DIFFX <- function(data_mpox, correct_col_order) {
#   
#   'GET DATA IN TIME-SERIES FORMAT; TIME X JURISDICTION'
#   data_diff_long <- data_mpox %>%
#     group_by(Jurisdiction) %>%
#     mutate(Cases_Diff = c(NA, diff(Cases))) %>%
#     ungroup() %>%
#     filter(!is.na(Cases_Diff)) # Remove the first row of each jurisdiction
#   
#   data_diff <- data_diff_long %>%
#     pivot_wider(id_cols = c(date_week_start, Week_Number), names_from = Jurisdiction, values_from = Cases_Diff) %>%
#     dplyr::select(-Week_Number)
#   
#   data_diff = data_diff[, correct_col_order]
#   
#   print(paste0('nrow train_data_ts: ', nrow(data_diff)))
#   print(paste0('ncol train_data_ts: ', ncol(data_diff)))
#   
#   return(data_diff)
# }
