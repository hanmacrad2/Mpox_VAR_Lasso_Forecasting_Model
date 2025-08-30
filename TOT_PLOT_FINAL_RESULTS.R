#*******************************************
#*PLOT FORECASTS + NAIVE ESTIMATES 
#* 
#*******************************************

#*************************************
#BEST MODEL: 2 WEEK AHEAD, TOP 8

df_preds1 = forecasts_var_2w$df_pred_results
df_preds2 = naive_smooth_2w$df_pred_results
df_preds3 = forecasts_var_us_2w$df_pred_results 
df_preds4 = naive_unsmooth_2w$df_pred_results

#****************************
#* SLOPE-WEIGHTED METRICS

df_slope = GET_DF_WEIGHTED_SLOPE(df_mpox_smooth, list_jur)
list_metrics1 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
list_metrics2 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)

#I. PLOT ALL FORECASTS
data_start_date = as.Date("2024-05-25")
df_preds1 = forecasts_var_2w$df_pred_results
list_states = list_jur
title = 'VAR two week-ahead Forecasts - Top 8 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start , df_preds1, list_jur, title, n_col_plot = 3)

#*************************************
#METRICS ORIG

df_metrics = METRICS_COMBINED(df_preds1, df_preds2, list_jur)
df_metrics2 = METRICS_COMBINED(df_preds1, df_preds2, list_jur)

df_metrics_diff = METRICS_DIFF_COMBINED(df_preds1, df_preds2, list_jur)

#GT 2
df_preds1_gt2 = df_preds1 %>% filter(Actual > 2)
df_preds2_gt2 = df_preds2  %>% filter(Actual > 2)

df_metrics_gt2 = METRICS_COMBINED(df_preds1_gt2, df_preds2_gt2, list_jur)

#ORIGINAL
df_results_best = PERFORMANCE_RESULTS_COMBINED(df_preds1, df_preds2, df_preds3, df_preds4, list_jur)
df_metrics = METRICS_COMBINED(df_preds1, df_preds2, df_preds3, df_preds4, list_jur)
df_metrics_diff = METRICS_DIFF_COMBINED(df_preds1, df_preds2, list_jur)
GLOBAL_DIFF_METRICS(df_preds1, df_preds2)


#PLOT
title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Forecasts start from 01/21/24. Top 8 jurisdictions in 2023-2024 (exc California)' 
#title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Top 9 jurisdictions in 2023-2024 (exc California). Weighted Metrics' #. Weighted Metrics' 
#title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Top 9 jurisdictions in 2023-2024 (inc California)' #. Weighted Metrics' 

#JURISDICTIONS

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

#SLOPE WEIGHTED METRICS
list_sw_rmse = GET_LIST_SLOPE_WEIGHTED_RMSE_JUR(df_slope, df_preds1, df_preds2, jur_identifier) 
list_sw_mae = GET_LIST_SLOPE_WEIGHTED_MAE_JUR(df_slope, df_preds1, df_preds2, jur_identifier) 
PLOT_COMPARE_FORECAST_NAIVE_X2(jur_identifier, df_preds1, df_preds2,
                               data_24_ts_forecast_start, title, list_sw_rmse, list_sw_mae)

#LABEL TOP LEFT
PLOT_COMPARE_FORECAST_NAIVE_X2(jur_identifier, df_preds1, df_preds2,
                               data_24_ts_forecast_start, title, list_sw_rmse, list_sw_mae, label_pos_right = FALSE)

















#***********************
#NORMAL FORECASTS + PLOT
list_rmse = GET_LIST_RMSE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 
list_mae = GET_LIST_MAE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 
PLOT_COMPARE_FORECAST_NAIVE_X2(jur_identifier, df_preds1, df_preds2,
                               data_24_ts_forecast_start, title, list_rmse, list_mae)

#WEIGHTED PREDS
list_w_rmse = GET_LIST_WEIGHTED_RMSE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 
list_w_mae = GET_LIST_WEIGHTED_MAE_JUR(df_preds1, df_preds2, df_preds3, df_preds4, jur_identifier) 
PLOT_COMPARE_FORECAST_NAIVE_X2(jur_identifier, df_preds1, df_preds2,
                               data_24_ts_forecast_start, title, list_w_rmse, list_w_mae)




#****************************
#* SLOPE-WEIGHTED METRICS

df_slope = GET_DF_WEIGHTED_SLOPE(df_mpox_smooth, list_jur)

#ALL METRICS
list_metrics1 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
list_metrics2 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)

#PERCENTAGE IMPROVEMENT 

#JUR LEVEL RESULTS
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds1, df_preds2, df_slope, list_jur)

#GLOBAL RESULTS
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds1, df_preds2)

# #PLOT
# PLOT_COMPARE_FORECAST_NAIVE_X4(jur_identifier, df_preds1, 
#                                df_preds2, df_preds3,
#                                df_preds4,
#                                data_ts_24, title, list_rmse, list_mae)

