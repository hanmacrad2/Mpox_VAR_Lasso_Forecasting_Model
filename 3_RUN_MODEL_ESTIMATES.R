#******************************
#* RUN MODEL ESTIMATES
#* ****************************

#********************************
#1.BEST FORECASTS: 2 WEEK AHEAD 

n_lags = 25

#RUN 1: 
print(paste0('TRAIN FINAL WEEK: ', TRAIN_WEEK))
print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))

#1. VAR LASSO SMOOTH
forecasts_var_2w = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                             ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_2w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                          future_data_ts, n_step_ahead = 2)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_2w =  VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_2w <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                            future_data_ts, n_step_ahead = 2)



#********************************************************
#1. FORECASTS: 3 WEEK AHEAD 
n_step_ahead = 3

#1. VAR LASSO SMOOTH
forecasts_var_3w = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                ROLL_WINDOW)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_3w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                         future_data_ts, n_step_ahead = n_step_ahead)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_3w =  VAR_LASSO_FORECAST_THREE_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                    ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_3w <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                           future_data_ts, n_step_ahead = n_step_ahead)


#********************************************************
#1. FORECASTS: 4 WEEK AHEAD 
n_step_ahead = 4

#1. VAR LASSO SMOOTH
forecasts_var_4w = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                           ROLL_WINDOW)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_4w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                      future_data_ts, n_step_ahead = n_step_ahead)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_4w =  VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_4w <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                        future_data_ts, n_step_ahead = n_step_ahead)


#**********************
#1.FORECASTS: 1 WEEK AHEAD 

#1. VAR LASSO SMOOTH
forecasts_var_1w = VAR_LASSO_FORECAST(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                      ROLL_WINDOW)


#2. NAIVE ESTIMATE SMOOTH
naive_smooth_1w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                      future_data_ts, n_step_ahead = 1)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_1w =  VAR_LASSO_FORECAST(train_data_ts, future_data_ts, future_data_ts,
                                          ROLL_WINDOW)


#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_1w <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                        future_data_ts, n_step_ahead = 1)


#*******************************************************************************
#* TOP 9

#1. VAR LASSO SMOOTH
forecasts_var_9_2w = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                            ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_9_2w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                      future_data_ts, n_step_ahead = 2)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_9_2w =  VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_9_2w  <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                        future_data_ts, n_step_ahead = 2)

#******************
#3 WEEK AHEAD


#1. VAR LASSO SMOOTH
forecasts_var_9_3w = VAR_LASSO_FORECAST_THREE_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                 ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_9_3w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                           future_data_ts, n_step_ahead = 3)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_9_3w =  VAR_LASSO_FORECAST_THREE_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                     ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_9_3w  <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                              future_data_ts, n_step_ahead = 3)

#******************
#4 WEEK AHEAD
n_step_ahead = 4

#1. VAR LASSO SMOOTH
forecasts_var_9_4w = VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                   ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_9_4w <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                           future_data_ts, n_step_ahead = n_step_ahead)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_9_4w =  VAR_LASSO_FORECAST_FOUR_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                       ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_9_4w <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                              future_data_ts, n_step_ahead = n_step_ahead)


#*****************
#ROLL WINDOW; 3
#********************************************************
#1. FORECASTS: 3 WEEK AHEAD 
n_step_ahead = 3

#1. VAR LASSO SMOOTH
forecasts_var_2w_3 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                               ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_2w_3 <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                         future_data_ts, n_step_ahead = n_step_ahead)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_2w_3 =  VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                   ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_2w_3 <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                           future_data_ts, n_step_ahead = n_step_ahead)

print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))


#N LAGS
n_lags = 10

#**********************
#1.FORECASTS: 2 WEEK AHEAD 

#RUN 1: 
print(paste0('TRAIN FINAL WEEK: ', TRAIN_WEEK))
print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))

#1. VAR LASSO SMOOTH
forecasts_var_2w_10 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                  ROLL_WINDOW, n_lags = n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_2w_10 <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                            future_data_ts, n_step_ahead = 2)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_2w_10 =  VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                      ROLL_WINDOW, n_lags = n_lags)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_2w_10 <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                              future_data_ts, n_step_ahead = 2)

print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))

#CHECK RESIDUALS
var_res = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                      ROLL_WINDOW, n_lags = n_lags)


#********************************
#1.START OF 2024

n_lags = 25

#RUN 1: 
print(paste0('TRAIN FINAL WEEK: ', TRAIN_WEEK))
print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))

#1. VAR LASSO SMOOTH
forecasts_var_2w_85 = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                               ROLL_WINDOW, n_lags)

#2. NAIVE ESTIMATE SMOOTH
naive_smooth_2w_85 <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                         future_data_ts, n_step_ahead = 2)

#3. VAR LASSO UNSMOOTH
forecasts_var_us_2w_85 =  VAR_LASSO_FORECAST_TWO_STEP(train_data_ts, future_data_ts, future_data_ts,
                                                   ROLL_WINDOW)

#4. NAIVE ESTIMATE UNSMOOTH
naive_unsmooth_2w_85 <- SIMPLE_NAIVE_FORECAST(train_data_ts, future_data_ts,
                                           future_data_ts, n_step_ahead = 2)

print(paste0('ROLL_WINDOW: ', ROLL_WINDOW))
