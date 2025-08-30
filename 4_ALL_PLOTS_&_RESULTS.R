#*******************************************
#*PLOT FORECASTS + NAIVE ESTIMATES 
#* 
#*******************************************

#*************************************
#BEST MODEL: 2 WEEK AHEAD, TOP 9

df_preds1 = forecasts_var_2w$df_pred_results
df_preds2 = naive_smooth_2w$df_pred_results
df_preds3 = forecasts_var_us_2w$df_pred_results 
df_preds4 = naive_unsmooth_2w$df_pred_results

df_results_best = PERFORMANCE_RESULTS_COMBINED(df_preds1, df_preds2, df_preds3, df_preds4, list_jur)
df_metrics = METRICS_COMBINED(df_preds1, df_preds2, df_preds3, df_preds4, list_jur)


#PLOT
title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Forecasts start from 01/21/24. Top 9 jurisdictions in 2023-2024 (exc California)' 

#New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)


#TWO WEEKS
list_rmse = GET_LIST_RMSE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X2(jur_identifier, df_preds1, df_preds2,
                               data_24_ts_forecast_start, title, list_rmse, list_mae)


#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1, 
                               df_preds2, df_preds3,
                               df_preds4,
                               data_ts_24, title, list_rmse, list_mae)


jur_label = 'Texas'
jur_identifier = 'Texas'
title = paste0(jur_label, title_main)

jur_label = 'LA County'
jur_identifier = 'LA'
title = paste0(jur_label, title_main)

jur_label = 'Florida'
jur_identifier = 'Florida'
title = paste0(jur_label, title_main)

jur_label = 'Illinois'
jur_identifier = 'Illinois'
title = paste0(jur_label, title_main)

jur_label = 'Georgia'
jur_identifier = 'Georgia'
title = paste0(jur_label, title_main)

#San Diego
jur_label = 'San Diego County'
jur_identifier = 'SanDiego'
title = paste0(jur_label, title_main)

jur_label = 'Washington'
jur_identifier = 'Washington'
title = paste0(jur_label, title_main)

jur_label = 'Colorado'
jur_identifier = 'Colorado'
title = paste0(jur_label, title_main)

#NYC
df1_nyc = df_preds1 %>% filter(Jurisdiction == jur_identifier)
df_preds1_nyc = df1_nyc$Predicted
df_true1_nyc = df1_nyc$Actual
cor(diff(df_true1_nyc), diff(df_preds1_nyc)) #0.47
#mean(ccf(df_true1_nyc, df_preds1_nyc))


direction_accuracy <- mean(sign(diff(df_true1_nyc)) == sign(diff(df_preds1_nyc))) #0.58


df2_nyc = df_preds2 %>% filter(Jurisdiction == jur_identifier)
df_preds2_nyc = df2_nyc$Predicted
df_true2_nyc = df2_nyc$Actual
cor(diff(df_true2_nyc), diff(df_preds2_nyc)) #0

#direction_accuracy
mean(sign(diff(df_true2_nyc)) == sign(diff(df_preds2_nyc))) #0.46

#*****************************
#San Diego
df1_sd = df_preds1 %>% filter(Jurisdiction == jur_identifier)

GET_W_RMSE_MAE_DF(df1_sd)

bias1 <- mean(df1_sd$Predicted - df1_sd$Actual, na.rm = TRUE) # 0.07
bias1

df_preds1_sd = df1_sd$Predicted
df_true1_sd = df1_sd$Actual
cor(diff(df_true1_sd), diff(df_preds1_sd)) #

GET_CORR_POS_SLOPE(df1_sd$Actual, df1_sd$Predicted) #0.625

#WEEK 105
df1_sd_sub = df1_sd %>% filter(Week_Number > 105)
df1_sd_sub = df1_sd_sub %>% filter(Week_Number <= 116)
GET_RMSE_MAE_DF(df1_sd_sub)

#Bias
mean(df1_sd_sub$Predicted - df1_sd_sub$Actual, na.rm = TRUE) #-0.0008


cor(diff(df1_sd$Actual), diff(df1_sd$Predicted)) #0.1
#GET_CORR_POS_SLOPE(df1_sd$Actual, df1_sd$Predicted) #0.36


direction_accuracy <- mean(sign(diff(df_true1_sd)) == sign(diff(df_preds1_sd)))

#PREDS 2
df2_sd = df_preds2 %>% filter(Jurisdiction == jur_identifier)
GET_W_RMSE_MAE_DF(df2_sd)

bias2 <- mean(df2_sd$Predicted - df2_sd$Actual, na.rm = TRUE) # --0.005952381
bias2

df_preds2_sd = df2_sd$Predicted
df_true2_sd = df2_sd$Actual
cor(diff(df_true2_sd), diff(df_preds2_sd)) #

#WEEK 105
df2_sd_sub = df2_sd %>% filter(Week_Number > 105)
df2_sd_sub = df2_sd_sub %>% filter(Week_Number <= 116)
GET_RMSE_MAE_DF(df2_sd_sub)

bias22 <- mean(df2_sd_sub$Predicted - df2_sd_sub$Actual, na.rm = TRUE) # -0.3863636
bias22


cor(diff(df2_sd$Actual), diff(df2_sd$Predicted)) #0.1
GET_CORR_POS_SLOPE(df2_sd$Actual, df2_sd$Predicted) #0.36

#direction_accuracy
mean(sign(diff(df_true2_sd)) == sign(diff(df_preds2_sd)))

#DATAFRAME SAN DIEGO
df1_sd_gt1 = df1_sd %>% filter(Actual > 2)
GET_RMSE(df1_sd_gt1$Actual, df1_sd_gt1$Predicted) #1.844174
GET_MAE(df1_sd_gt1$Actual, df1_sd_gt1$Predicted) # 1.606987
cor(diff(df1_sd_gt1$Actual), diff(df1_sd_gt1$Predicted)) 

GET_CORR_POS_SLOPE(df1_sd_gt1$Actual, df1_sd_gt1$Predicted) #0.52

mean(sign(diff(df1_sd_gt1$Actual)) == sign(diff(df1_sd_gt1$Predicted))) #0.5

df2_sd_gt1 = df2_sd %>% filter(Actual > 2)
GET_RMSE(df2_sd_gt1$Actual, df2_sd_gt1$Predicted) #
GET_MAE(df2_sd_gt1$Actual, df2_sd_gt1$Predicted) # 
cor(diff(df2_sd_gt1$Actual), diff(df2_sd_gt1$Predicted)) 
mean(sign(diff(df2_sd_gt1$Actual)) == sign(diff(df2_sd_gt1$Predicted))) #0.6

#********************
#* OTHER JURISDICTIONS

jur_label = 'Texas'
jur_identifier = 'Texas'
title = paste0(jur_label, title_main)

jur_label = 'LA County'
jur_identifier = 'LA'
title = paste0(jur_label, title_main)

jur_label = 'Florida'
jur_identifier = 'Florida'
title = paste0(jur_label, title_main)

jur_label = 'Illinois'
jur_identifier = 'Illinois'
title = paste0(jur_label, title_main)

jur_label = 'Georgia'
jur_identifier = 'Georgia'
title = paste0(jur_label, title_main)

#San Diego
jur_label = 'San Diego County'
jur_identifier = 'SanDiego'
title = paste0(jur_label, title_main)

jur_label = 'Washington'
jur_identifier = 'Washington'
title = paste0(jur_label, title_main)

jur_label = 'Colorado'
jur_identifier = 'Colorado'
title = paste0(jur_label, title_main)

jur_label = 'California'
jur_identifier = 'California'
title = paste0(jur_label, title_main)



#*************************************************************************
#* FORECASTS START OF 2024

df_preds1_85 = forecasts_var_2w_85$df_pred_results
df_preds2_85 = naive_smooth_2w_85$df_pred_results
df_preds3_85 = forecasts_var_us_2w_85$df_pred_results 
df_preds4_85 = naive_unsmooth_2w_85$df_pred_results

df_results_85 = PERFORMANCE_RESULTS_COMBINED(df_preds1_85, df_preds2_85, df_preds3_85, df_preds4_85, list_jur)

#PLOT
title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Forecasts for all 2024.'  

#New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)


#TWO WEEKS
list_rmse = GET_LIST_RMSE_JUR(df_preds1_85, df_preds2_85, df_preds3_85, df_preds4_85, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1_85, df_preds2_85, df_preds3_85, df_preds4_85, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1_85, 
                               df_preds2_85, df_preds3_85,
                               df_preds4_85,
                               data_24_ts_forecast_start, title, list_rmse, list_mae)


#******************
#TOP 9
#PREDICTIONS
df_preds1_9_2w = forecasts_var_9_2w$df_pred_results
df_preds2_9_2w = naive_smooth_9_2w$df_pred_results
df_preds3_9_2w = forecasts_var_us_9_2w$df_pred_results 
df_preds4_9_2w = naive_unsmooth_9_2w$df_pred_results

df_preds1_9_3w = forecasts_var_9_3w$df_pred_results
df_preds2_9_3w = naive_smooth_9_3w$df_pred_results
df_preds3_9_3w = forecasts_var_us_9_3w$df_pred_results 
df_preds4_9_3w = naive_unsmooth_9_3w$df_pred_results

df_preds1_9_4w = forecasts_var_9_4w$df_pred_results
df_preds2_9_4w = naive_smooth_9_4w$df_pred_results
df_preds3_9_4w = forecasts_var_us_9_4w$df_pred_results 
df_preds4_9_4w = naive_unsmooth_9_4w$df_pred_results

#RESULTS
df_results_9_2w = PERFORMANCE_RESULTS_COMBINED(df_preds1_9_2w, df_preds2_9_2w, df_preds3_9_2w, df_preds4_9_2w, list_jur)
df_results_9_3w = PERFORMANCE_RESULTS_COMBINED(df_preds1_9_3w, df_preds2_9_3w, df_preds3_9_3w, df_preds4_9_3w, list_jur)
df_results_9_4w = PERFORMANCE_RESULTS_COMBINED(df_preds1_9_4w, df_preds2_9_4w, df_preds3_9_4w, df_preds4_9_4w, list_jur)

#ROLLING WINDOW 5
df_preds1_2w_5 = forecasts_var_2w_5$df_pred_results
df_preds2_2w_5 = naive_smooth_2w_5$df_pred_results
df_preds3_2w_5 = forecasts_var_us_2w_5$df_pred_results 
df_preds4_2w_5 = naive_unsmooth_2w_5$df_pred_results

df_results_2w_5 = PERFORMANCE_RESULTS_COMBINED(df_preds1_2w_5, df_preds2_2w_5, df_preds3_2w_5, df_preds4_2w_5, list_jur)


#TRAIN WEEK 104 
df_preds1_2w_104 = forecasts_var_2w$df_pred_results
df_preds2_2w_104 = naive_smooth_2w$df_pred_results
df_preds3_2w_104 = forecasts_var_us_2w$df_pred_results 
df_preds4_2w_104 = naive_unsmooth_2w$df_pred_results

df_results_2w_104 = PERFORMANCE_RESULTS_COMBINED(df_preds1_2w_104, df_preds2_2w_104, df_preds3_2w_104, df_preds4_2w_104, list_jur)



#TRAIN WEEK 106, 
df_preds1_2w_106 = forecasts_var_2w$df_pred_results
df_preds2_2w_106 = naive_smooth_2w$df_pred_results
df_preds3_2w_106 = forecasts_var_us_2w$df_pred_results 
df_preds4_2w_106 = naive_unsmooth_2w$df_pred_results

df_results_2w_106 = PERFORMANCE_RESULTS_COMBINED(df_preds1_2w_106, df_preds2_2w_106, df_preds3_2w_106, df_preds4_2w_106, list_jur)


#********************
#ROLL WINDOW 3

df_preds1_2w_3 = forecasts_var_2w_3$df_pred_results
df_preds2_2w_3 = naive_smooth_2w_3$df_pred_results
df_preds3_2w_3 = forecasts_var_us_2w_3$df_pred_results 
df_preds4_2w_3 = naive_unsmooth_2w_3$df_pred_results

df_results_2w_3 = PERFORMANCE_RESULTS_COMBINED(df_preds1_2w_3, df_preds2_2w_3, df_preds3_2w_3, df_preds4_2w_3, list_jur)



#*************************
#* PLOTS

title_main = ' 2 week ahead Mpox VAR Forecasts vs Naive Estimates. Fit to 2023 & 2024 data from the Top 9 jurisdictions in 2023-2024 (exc California)' 

title_main = ' 2 week ahead Mpox VAR Forecasts vs Naive Estimates. Forecasts start from 06/16/24. Fit to 2023 & 2024 data from the Top 9 jurisdictions in 2023-2024 (exc California)' 


#New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)

jur_label = 'Texas'
jur_identifier = 'Texas'
title = paste0(jur_label, title_main)

jur_label = 'LA County'
jur_identifier = 'LA'
title = paste0(jur_label, title_main)

jur_label = 'Florida'
jur_identifier = 'Florida'
title = paste0(jur_label, title_main)

jur_label = 'Illinois'
jur_identifier = 'Illinois'
title = paste0(jur_label, title_main)

jur_label = 'Georgia'
jur_identifier = 'Georgia'
title = paste0(jur_label, title_main)

#San Diego
jur_label = 'San Diego County'
jur_identifier = 'SanDiego'
title = paste0(jur_label, title_main)

jur_label = 'Washington'
jur_identifier = 'Washington'
title = paste0(jur_label, title_main)

jur_label = 'Colorado'
jur_identifier = 'Colorado'
title = paste0(jur_label, title_main)

jur_label = 'California'
jur_identifier = 'California'
title = paste0(jur_label, title_main)



#****************************
#RUN METRICS + PLOTS

#TWO WEEKS
list_rmse = GET_LIST_RMSE_JUR(df_preds1_9_2w, df_preds2_9_2w, df_preds3_9_2w, df_preds4_9_2w, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1_9_2w, df_preds2_9_2w, df_preds3_9_2w, df_preds4_9_2w, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1_9_2w, 
                               df_preds2_9_2w, df_preds3_9_2w,
                               df_preds4_9_2w,
                               data_ts_24, title, list_rmse, list_mae)

#***************************
#FOUR WEEKS
list_rmse = GET_LIST_RMSE_JUR(df_preds1_9_4w, df_preds2_9_4w, df_preds3_9_4w, df_preds4_9_4w, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1_9_4w, df_preds2_9_4w, df_preds3_9_4w, df_preds4_9_4w, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1_9_4w, 
                               df_preds2_9_4w, df_preds3_9_4w,
                               df_preds4_9_4w,
                               data_ts_24, title, list_rmse, list_mae)


#TWO WEEKS: TRAIN WEEK 106
list_rmse = GET_LIST_RMSE_JUR(df_preds1_2w_106, df_preds2_2w_106, df_preds3_2w_106, df_preds4_2w_106, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1_2w_106, df_preds2_2w_106, df_preds3_2w_106, df_preds4_2w_106, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1_2w_106, 
                               df_preds2_2w_106, df_preds3_2w_106,
                               df_preds4_2w_106,
                               data_ts_24, title, list_rmse, list_mae)





#**************************************************************************************************
#TOP 10
#PREDICTIONS
df_preds1_10_2w = forecasts_var$df_pred_results
df_preds2_10_2w = naive_smooth$df_pred_results
df_preds3_10_2w = forecasts_var_us$df_pred_results 
df_preds4_10_2w = naive_unsmooth$df_pred_results

#RESULTS
print(list_jur)
df_results_10_2w = PERFORMANCE_RESULTS_COMBINED(df_preds1_10_2w, df_preds2_10_2w, df_preds3_10_2w, df_preds4_10_2w, list_jur)

#****************
#TOP 10 (ORIG)
#PREDICTIONS
df_preds1 = forecasts_var$df_pred_results
df_preds2 = naive_smooth$df_pred_results
df_preds3 = forecasts_var_us$df_pred_results 
#df_preds3 = forecasts_var_cv$df_pred_results 
df_preds4 = naive_unsmooth$df_pred_results


#RESULTS
df_results_2w = PERFORMANCE_RESULTS_COMBINED(df_preds1, df_preds2, df_preds3, df_preds4, list_jur)


#3 WEEK PREDICTIONS
df_preds1_3w = forecasts_var_3w$df_pred_results
df_preds2_3w = naive_smooth_3w$df_pred_results
df_preds3_3w = forecasts_var_us_3w$df_pred_results 
df_preds4_3w = naive_unsmooth_3w$df_pred_results

#RESULTS
df_results_3w = PERFORMANCE_RESULTS_COMBINED(df_preds1_3w, df_preds2_3w, df_preds3_3w, df_preds3_4w, list_jur)

#4 WEEK PREDICTIONS
df_preds1_4w = forecasts_var_4w$df_pred_results
df_preds2_4w = naive_smooth_4w$df_pred_results
df_preds3_4w = forecasts_var_us_4w$df_pred_results 
df_preds4_4w = naive_unsmooth_4w$df_pred_results


#RESULTS
df_results_4w = PERFORMANCE_RESULTS_COMBINED(df_preds1_4w, df_preds2_4w, df_preds3_4w, df_preds4_4w, list_jur)




#*************************
#* PLOTS

title_main = ' 2 week ahead Mpox VAR Forecasts vs Naive Estimates. Fit to 2023 & 2024 data from the Top 10 jurisdictions in 2023-2024' 


#San Diego
jur_label = 'San Diego'
jur_identifier = 'SanDiego'
title = paste0(jur_label, title_main)

jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)

jur_label = 'California'
jur_identifier = 'California'
title = paste0(jur_label, title_main)

jur_label = 'LA'
jur_identifier = 'LA'
title = paste0(jur_label, title_main)

jur_label = 'Texas'
jur_identifier = 'Texas'
title = paste0(jur_label, title_main)

jur_label = 'Florida'
jur_identifier = 'Florida'
title = paste0(jur_label, title_main)


#****************************
#RUN METRICS + PLOTS
print(paste0('Jur: ', jur_identifier))
rmse_1 = GET_RMSE_JUR(forecasts_var$df_pred_results, jur_identifier)
rmse_2 = GET_RMSE_JUR(naive_smooth$df_pred_results, jur_identifier)
rmse_3 = GET_RMSE_JUR(forecasts_var_us$df_pred_results, jur_identifier)
rmse_4 =  GET_RMSE_JUR(naive_unsmooth$df_pred_results, jur_identifier)
list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4); print(list_rmse)

mae_1 = GET_MAE_JUR(forecasts_var$df_pred_results, jur_identifier)
mae_2 = GET_MAE_JUR(naive_smooth$df_pred_results, jur_identifier)
mae_3 = GET_MAE_JUR(forecasts_var_us$df_pred_results, jur_identifier)
mae_4 = GET_MAE_JUR(naive_unsmooth$df_pred_results, jur_identifier)
list_mae = c(mae_1, mae_2, mae_3, mae_4); print(list_mae)

#****************
#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, forecasts_var$df_pred_results, 
                               naive_smooth$df_pred_results, forecasts_var_us$df_pred_results,
                               naive_unsmooth$df_pred_results,
                               data_ts_24, title, list_rmse, list_mae)


#********************
#* THREE WEEK AHEAD
#****************************

title_main = ' 3 week ahead Mpox VAR Forecasts vs Naive Estimates. Fit to 2023 & 2024 data from the Top 10 jurisdictions in 2023-2024' 
#Forecast & Estimates start from 05/19/24 (model fit to smoothed data using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#RUN METRICS + PLOTS
print(paste0('Jur: ', jur_identifier))
rmse_1 = GET_RMSE_JUR(forecasts_var_3w$df_pred_results, jur_identifier)
rmse_2 = GET_RMSE_JUR(naive_smooth_3w$df_pred_results, jur_identifier)
rmse_3 = GET_RMSE_JUR(forecasts_var_3w$df_pred_results, jur_identifier)
rmse_4 =  GET_RMSE_JUR(forecasts_var_3w$df_pred_results, jur_identifier)
list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4); print(list_rmse)

mae_1 = GET_MAE_JUR(forecasts_var_3w$df_pred_results, jur_identifier)
mae_2 = GET_MAE_JUR(naive_smooth_3w$df_pred_results, jur_identifier)
mae_3 = GET_MAE_JUR(forecasts_var_us_3w$df_pred_results, jur_identifier)
mae_4 = GET_MAE_JUR(naive_unsmooth_3w$df_pred_results, jur_identifier)
list_mae = c(mae_1, mae_2, mae_3, mae_4); print(list_mae)

#****************
#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, forecasts_var_3w$df_pred_results, 
                               naive_smooth_3w$df_pred_results, forecasts_var_us_3w$df_pred_results,
                               naive_unsmooth_3w$df_pred_results,
                               data_ts_24, title, list_rmse, list_mae)


#********************
#* FOUR WEEK AHEAD
#****************************
#RUN METRICS + PLOTS
print(paste0('Jur: ', jur_identifier))
rmse_1 = GET_RMSE_JUR(forecasts_var_4w$df_pred_results, jur_identifier)
rmse_2 = GET_RMSE_JUR(naive_smooth_4w$df_pred_results, jur_identifier)
rmse_3 = GET_RMSE_JUR(forecasts_var_4w$df_pred_results, jur_identifier)
rmse_4 =  GET_RMSE_JUR(forecasts_var_4w$df_pred_results, jur_identifier)
list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4); print(list_rmse)

mae_1 = GET_MAE_JUR(forecasts_var_4w$df_pred_results, jur_identifier)
mae_2 = GET_MAE_JUR(naive_smooth_4w$df_pred_results, jur_identifier)
mae_3 = GET_MAE_JUR(forecasts_var_us_4w$df_pred_results, jur_identifier)
mae_4 = GET_MAE_JUR(naive_unsmooth_4w$df_pred_results, jur_identifier)
list_mae = c(mae_1, mae_2, mae_3, mae_4); print(list_mae)

#****************
#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, forecasts_var_4w$df_pred_results, 
                               naive_smooth_4w$df_pred_results, forecasts_var_us_4w$df_pred_results,
                               naive_unsmooth_4w$df_pred_results,
                               data_ts_24, title, list_rmse, list_mae)




#********************
#* ONE WEEK AHEAD
#****************************
#RUN METRICS + PLOTS
print(paste0('Jur: ', jur_identifier))
rmse_1 = GET_RMSE_JUR(forecasts_var_1w$df_pred_results, jur_identifier)
rmse_2 = GET_RMSE_JUR(naive_smooth_1w$df_pred_results, jur_identifier)
rmse_3 = GET_RMSE_JUR(forecasts_var_us_1w$df_pred_results, jur_identifier)
rmse_4 =  GET_RMSE_JUR(naive_unsmooth_1w$df_pred_results, jur_identifier)
list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4); print(list_rmse)

mae_1 = GET_MAE_JUR(forecasts_var_1w$df_pred_results, jur_identifier)
mae_2 = GET_MAE_JUR(naive_smooth_1w$df_pred_results, jur_identifier)
mae_3 = GET_MAE_JUR(forecasts_var_us_1w$df_pred_results, jur_identifier)
mae_4 = GET_MAE_JUR(naive_unsmooth_1w$df_pred_results, jur_identifier)
list_mae = c(mae_1, mae_2, mae_3, mae_4); print(list_mae)

#****************
#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, forecasts_var_1w$df_pred_results, 
                               naive_smooth_1w$df_pred_results, forecasts_var_us_1w$df_pred_results,
                               naive_unsmooth_1w$df_pred_results,
                               data_ts_24, title, list_rmse, list_mae)


#*******************
#* 10 LAGS
#* *******************

#******************
#LAG 10 TOP 9
#PREDICTIONS
df_preds1_2w_10 = forecasts_var_2w_10$df_pred_results
df_preds2_2w_10 = naive_smooth_2w_10$df_pred_results
df_preds3_2w_10 = forecasts_var_us_2w_10$df_pred_results 
df_preds4_2w_10 = naive_unsmooth_2w_10$df_pred_results

df_results_2w_10_lag = PERFORMANCE_RESULTS_COMBINED(df_preds1_2w_10, df_preds2_2w_10, df_preds3_2w_10, df_preds4_2w_10, list_jur)

#PLOT
title_main = ' VAR with 10 lags, 2 week ahead Mpox VAR Forecasts vs Naive Estimates. Forecasts start from 06/16/24. Top 9 jurisdictions in 2023-2024 (exc California)' 

#New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)


#TWO WEEKS
list_rmse = GET_LIST_RMSE_JUR(df_preds1_2w_10, df_preds2_2w_10, df_preds3_2w_10, df_preds4_2w_10, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1_2w_10, df_preds2_2w_10, df_preds3_2w_10, df_preds4_2w_10, jur_identifier) 

#PLOT
PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1_2w_10, 
                               df_preds2_2w_10, df_preds3_2w_10,
                               df_preds4_2w_10,
                               data_ts_24, title, list_rmse, list_mae)

