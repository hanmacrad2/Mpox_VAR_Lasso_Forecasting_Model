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
title_main = ' 2 week ahead Mpox VAR Forecasts (25 lags) vs Naive Estimates. Top 9 jurisdictions in 2023-2024 (inc California)' #. Weighted Metrics' 


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

#PLOT
# For VAR forecast only
PLOT_COMPARE_FORECAST("SanDiego", df_preds1, df_preds2, data_24_ts_forecast_start, NULL, list_sw_rmse, list_sw_mae, forecast_type = "VAR")

# For Naive forecast only
PLOT_COMPARE_FORECAST("SanDiego", df_preds1, df_preds2, data_24_ts_forecast_start, NULL, list_sw_rmse, list_sw_mae, forecast_type = "Naive")


#APPLY
# Only true cases
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start, plot_level = "Reported")
# True + Naive
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start, plot_level = "True+Naive")
# True + Naive + VAR
PLOT_COMPARE_FORECAST(jur_identifier, jur_label, df_preds1, df_preds2, data_24_ts_forecast_start, plot_level = "True+Naive+VAR")


#*********************************************
#* PLOT VAR vs NAIVE WINDOWS

var_rmse  <- c(4.5, 3.80, 3.0, 1.84, 2.0)
naive_rmse <- c(3.29, 3.10, 2.90, 2.08, 2.34)

#MAE
var_rmse  <- c(2.9, 2.82, 2.35, 1.33, 1.52)
naive_rmse <- c(2.5, 2.4, 2.23, 1.39, 1.87)

#MAKE FUNCTION
col_naive = "red"
col_naive = "black"

# numeric x for plotting
x_numeric <- 1:5
x_labels  <- c("No smoothing", "2", "3", "4", "5")

plot(x_numeric, var_rmse, col = "blue", type = "l", lwd = 2,
     xaxt = "n",  # suppress default axis
     xlab = "VAR-Lasso  Naive  Moving average window in weeks",
     ylab = "RMSE",
     cex.axis = 1.3,   # increase axis tick labels
     cex.lab  = 1.3,
     ylim = c(1.0, 5.0)) 

# custom x-axis labels
axis(1, at = x_numeric, labels = x_labels)

# add points
points(x_numeric, var_rmse, col = "blue", pch = 16, cex = 1.5)

# add naive line and points
lines(x_numeric, naive_rmse, col = col_naive, lwd = 2)
points(x_numeric, naive_rmse, col = col_naive, pch = 16, cex = 1.5)


legend("topleft", legend = c("VAR-Lasso", "Reported Cases"),
       col = c("blue", col_naive),
       lty = 1, pch = c(16, 16),
       cex = 1.5,
       bty = "n")
       # ,       # remove surrounding box
       # xjust = 1, yjust = 1,  # tighten placement
       # inset = 0.02)    # nudge inward if needed





