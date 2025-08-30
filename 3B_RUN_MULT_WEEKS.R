#*******************************************
#*
#* RUN VAR MODEL
#* 
#*******************************************

#SETUP
#data_start_date = as.Date("2024-05-25") #1 case in San diego
n_lags = 25
#n_step_ahead = 2


#1. TRAIN WEEK 104
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
#2 step
list_results_2_104 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                            ROLL_WINDOW)

#3 step
list_results_3_104 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                      ROLL_WINDOW)


#4 step
list_results_4_week_104 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                  ROLL_WINDOW)

list_results_4_104 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                         ROLL_WINDOW)

#RMSE
list_results_2_104$rmse
list_results_3_104$rmse
list_results_4_104$rmse

#MAE
list_results_2_104$mae
list_results_3_104$mae
list_results_4_104$mae

#*********
#MODEL COEFFICIENTS
df_coeffs_2 = list_results_2_104$df_coeffs
df_coeffs_2 <- df_coeffs_2 %>%
  arrange(desc(Coefficient))
df_coeffs_fin = list_results_2_104$coeffs_final

df_coeffs_2_sd = df_coeffs_2 %>% filter(Response == 'SanDiego')

#*************
#1. TRAIN WEEK 105

#2 step
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
list_results_2_105 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                             n_step_ahead, n_lags, ROLL_WINDOW)

#3 step
list_results_3_105 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                         n_step_ahead, n_lags, ROLL_WINDOW)

#4 step
list_results_4_105 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                         ROLL_WINDOW)

#************************
#1. TRAIN WEEK 106
#2 step
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
list_results_2_106 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                 n_step_ahead, n_lags, ROLL_WINDOW)

#3 step
list_results_3_106 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                   n_step_ahead, n_lags, ROLL_WINDOW)

#4 step
list_results_4_106 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                  ROLL_WINDOW)

#MAE
list_results_2_106$mae
list_results_3_106$mae
list_results_4_106$mae


#MODEL COEFFICIENTS 


#UN-SMOOTHED
#1. TRAIN WEEK 
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
#2 step
list_results_2_104_us = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                 ROLL_WINDOW)

#3 step
list_results_3_104 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                   ROLL_WINDOW)


#4 step
list_results_4_104_us = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                  ROLL_WINDOW)



#DIFFERENT LAGS!


list_results_2_104_lag_10 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                 ROLL_WINDOW, n_step_ahead = 2, n_lags = 10)
















