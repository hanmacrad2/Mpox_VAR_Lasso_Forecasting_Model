#*******************************************
#*
#* RUN VAR MODEL
#* 
#*******************************************

#SETUP
#data_start_date = as.Date("2024-05-25") #1 case in San diego
n_lags = 25
n_step_ahead = 2

#*******************************
# TOP 5 JURISDICTIONS

forecasts_5_2w = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                           n_step_ahead, n_lags, ROLL_WINDOW)

#*******************************
# TOP 10 JURISDICTIONS

#RESULTS: PAPER: ROLLING AVERAGE WINDOW OF 4. 2, 3, 4 WEEKS AHEAD 

forecasts_roll4_3ahead = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                     n_step_ahead, n_lags, ROLL_WINDOW)

forecasts_roll4_4ahead = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                     n_step_ahead, n_lags, ROLL_WINDOW)


#OTHER WINDOWS
list_results = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                           n_step_ahead, n_lags, ROLL_WINDOW)


list_results_2 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                           n_step_ahead, n_lags, ROLL_WINDOW)

list_results_3 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                             n_step_ahead, n_lags, ROLL_WINDOW)

list_results_4 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                             n_step_ahead, n_lags, ROLL_WINDOW)

list_results_5 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                           n_step_ahead, n_lags, ROLL_WINDOW)

#NO SMOOTHING
ROLL_WINDOW = 0 
list_results_0 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                             n_step_ahead, n_lags, ROLL_WINDOW)

#*******************************
#3 WEEK AHEAD
list_results_3_week_roll4 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                            ROLL_WINDOW)

list_results_3_week_roll5 = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                   ROLL_WINDOW)

#4 WEEK AHEAD
list_results_4_week_roll4 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                          ROLL_WINDOW)

list_results_4_week_roll5 = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                          ROLL_WINDOW)


















