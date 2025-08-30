#*******************************************
#*PLOT FORECASTS + NAIVE ESTIMATES 
#*******************************************

#BEST MODEL: 2 WEEK AHEAD, TOP 8
df_preds1 = forecasts_var_2w$df_pred_results
df_preds2 = naive_smooth_2w$df_pred_results
df_preds3 = forecasts_var_us_2w$df_pred_results 
df_preds4 = naive_unsmooth_2w$df_pred_results

#METRIC: SLOPE-WEIGHTED 
df_slope = GET_DF_WEIGHTED_SLOPE(df_mpox_smooth, list_jur)

list_metrics1 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
list_metrics2 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)
list_metrics3 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds3)
list_metrics4 = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds4)

#METRICS: JUR LEVEL
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds1, df_preds2, df_slope, list_jur)
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds1, df_preds2)

#SLOPE_JUR
df_slope_jur = df_slope %>% filter(Jurisdiction == 'S')

#PLOT: ALL FORECASTS
data_start_date = as.Date("2024-05-25")
df_preds1 = forecasts_var_2w$df_pred_results
list_states = list_jur
title = 'VAR two week-ahead Forecasts - Top 8 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start , df_preds1, list_jur, title, n_col_plot = 3)

#********************************************
#JURISDICTIONS

#PLOT
#title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Forecasts start from 01/21/24. Top 8 jurisdictions in 2023-2024 (exc California)' 
#title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Top 9 jurisdictions in 2023-2024 (exc California). Weighted Metrics' #. Weighted Metrics' 
title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Top 8 jurisdictions in 2023-2024 (inc California)' #. Weighted Metrics' 


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

#***************************************************************
#* PLOT + SLOPE-WEIGHTED EMPHASIS
list_sw_rmse = GET_LIST_SLOPE_WEIGHTED_RMSE_JUR(df_slope, df_preds1, df_preds2, jur_identifier) 
list_sw_mae = GET_LIST_SLOPE_WEIGHTED_MAE_JUR(df_slope, df_preds1, df_preds2, jur_identifier) 
PLOT_COMPARE_FORECAST_SLOPE(jur_identifier, df_preds1, df_preds2,
                            data_24_ts_forecast_start, df_slope, title, list_sw_rmse, list_sw_mae)

#*************
#PLOT SINGLES
# For VAR forecast only
PLOT_COMPARE_FORECAST("SanDiego", df_preds1, df_preds2, data_24_ts_forecast_start, NULL, list_sw_rmse, list_sw_mae, forecast_type = "VAR")

# For Naive forecast only
PLOT_COMPARE_FORECAST("SanDiego", df_preds1, df_preds2, data_24_ts_forecast_start, NULL, list_sw_rmse, list_sw_mae, forecast_type = "Naive")


#*****************
#PLOT ONE FIGURE 
#1.REPORTED
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start, plot_level = "Reported", title_blank = TRUE)
#2. REPORTED + Naive
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start, plot_level = "True+Naive", title_blank = TRUE)
#3. REPORTED + Naive + VAR
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                      plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

#REPEAT
par(mfrow = c(2, 2))

#*****************
#PLOT ERRORS
PLOT_COMPARE_FORECAST_ERRORS(jur_identifier, df_preds1, df_preds2,
                             data_24_ts_forecast_start, title, list_sw_rmse, list_sw_mae, title_blank = FALSE)

#***************
#* PLOT MULTIPLE

#1. New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title = paste0(jur_label, title_main)

p1 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

jur_label = 'Texas'
jur_identifier = 'Texas'
title = paste0(jur_label, title_main)

p2 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

jur_label = 'LA County'
jur_identifier = 'LA'
title = paste0(jur_label, title_main)

p3 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

jur_label = 'Florida'
jur_identifier = 'Florida'
title = paste0(jur_label, title_main)

p4 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

#PLOT 4
(p1 | p2) / (p3 | p4)

#PLOTS 2
jur_label = 'Illinois'
jur_identifier = 'Illinois'
title = paste0(jur_label, title_main)

p5 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

jur_label = 'Georgia'
jur_identifier = 'Georgia'
title = paste0(jur_label, title_main)

p6 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

#San Diego
jur_label = 'San Diego County'
jur_identifier = 'SanDiego'
title = paste0(jur_label, title_main)

p7 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

jur_label = 'Washington'
jur_identifier = 'Washington'
title = paste0(jur_label, title_main)

p8 = PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start,
                           plot_level = "True+Naive+VAR", LEGEND_TRUE = FALSE, title_blank = FALSE)

(p5 | p6) / (p7 | p8)
