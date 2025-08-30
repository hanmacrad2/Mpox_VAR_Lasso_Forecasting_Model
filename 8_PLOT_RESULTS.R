#*******************************************
#*
#* FORECASTS + PLOT
#* 
#*******************************************

#I. PLOT ALL FORECASTS
data_start_date = as.Date("2024-05-25")
df_preds = list_results$df_pred_results
list_states = list_jur
title = 'Forecasts - Top 10 Cases in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds, list_states, title, n_col_plot = 3)

#II. PLOT SAN DIEGO
title_sd = paste0('San Diego County ', title)
df_preds_sd = df_preds %>% filter(Jurisdiction == 'SanDiego')
PLOT_DATES_TRUE_FORECAST_SD(df_preds_sd, title_sd)


#************************
#ROLLING WINDOW = 4
df_preds_4 = list_results_4$df_pred_results
df_preds_4_sd = df_preds_4 %>% filter(Jurisdiction == 'SanDiego')

title = '2 week ahead Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks'
title_sd = paste0('San Diego County ', title)
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds_4, list_states, title, n_col_plot = 3)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_4_sd, title_sd)



#************************
#ROLLING WINDOW = 5
df_preds_5 = list_results_5$df_pred_results
df_preds_5_sd = df_preds_5 %>% filter(Jurisdiction == 'SanDiego')

title = 'Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 5 weeks'
title_sd = paste0('San Diego County ', title)
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds_5, list_states, title, n_col_plot = 3)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_5_sd, title_sd)

#PLOT TWO FIGURES
title = 'Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks'
title_sd = paste0('San Diego County ', title)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_4_sd, title_sd) / PLOT_DATES_TRUE_FORECAST_SD(df_preds_5_sd, 'San Diego County Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 5 weeks') 




#******************************
#* 3 week ahead

df_preds_3_week_roll4 = list_results_3_week_roll4$df_pred_results
df_preds_3_week_roll4_sd = df_preds_3_week_roll4 %>% filter(Jurisdiction == 'SanDiego')

#PLOTS
title = 'Forecasts - Top 10 Cases in 2023 & 2024. 3 week ahead forecast. Rolling Average Window of 4 weeks'
title_sd = paste0('San Diego County ', title)
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds_3_week_roll4, list_states, title, n_col_plot = 3)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_3_week_sd, title_sd)


#PLOT TWO FIGURES: ROLLING AVERAGE OF 4
title = 'Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks'
title_sd = paste0('2 week ahead San Diego County ', title)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_4_sd, title_sd) / PLOT_DATES_TRUE_FORECAST_SD(df_preds_3_week_roll4_sd, '3 week ahead San Diego County Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks') 

#PLOT TWO FIGURES: ROLLING AVERAGE OF 5
df_preds_3_week_roll5 = list_results_3_week_roll5$df_pred_results
df_preds_3_week_roll5_sd = df_preds_3_week_roll5 %>% filter(Jurisdiction == 'SanDiego')

title = 'Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 5 weeks'
title_sd = paste0('2 week ahead San Diego County ', title)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_5_sd, title_sd) / PLOT_DATES_TRUE_FORECAST_SD(df_preds_3_week_roll5_sd, '3 week ahead San Diego County Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 5 weeks') 


#**************************
# 4 week ahead
title = '4 week ahead Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks'
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, list_results_4_week_roll4$df_pred_results, list_states, title, n_col_plot = 3)

#PLOT TWO FIGURES: ROLLING AVERAGE OF 4
df_preds_4_week_roll4_sd = list_results_4_week_roll4$df_pred_results %>% filter(Jurisdiction == 'SanDiego')


title = 'Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks'
title_sd = paste0('3 week ahead San Diego County ', title)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_3_week_roll4_sd, title_sd) / PLOT_DATES_TRUE_FORECAST_SD(df_preds_4_week_roll4_sd, '4 week ahead San Diego County Forecasts - Top 10 Cases in 2023 & 2024. Rolling Average Window of 4 weeks') 



#PLOT DATA
title_data = 'Mpox Case Data: 2023-2024'
PLOT_DATA(data_ts_23_24, list_results_4_week_roll4$df_pred_results, list_states, title_data, n_col_plot = 3)


#PLOT DATA SMOOTHED
title_data = 'Mpox Case Data: 2023-2024'
PLOT_DATA(data_ts_23_24, list_results_4_week_roll4$df_pred_results, list_states, title_data, n_col_plot = 3)

title_data = 'Smoothed Mpox Case Data (Rolling Average Window of 4 weeks): 2023-2024'
PLOT_DATA(data_ts_23_24_smooth, list_results_4_week_roll4$df_pred_results, list_states, title_data, n_col_plot = 3)




#************************************
#OTHER SMOOTHING WINDOWS
df_preds_other = list_results_3$df_pred_results
df_preds_other_sd = df_preds_other %>% filter(Jurisdiction == 'SanDiego')

PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds_other, list_states, title, n_col_plot = 3)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_other_sd, title_sd)

#ROLLING WINDOW = 0; No smoothing
df_preds_0 = list_results_0$df_pred_results
df_preds_0_sd = df_preds_0 %>% filter(Jurisdiction == 'SanDiego')

title = 'Forecasts - Top 10 Cases in 2023 & 2024. No Smoothing.'
title_sd = paste0('San Diego County ', title)
PLOT_DATES_TRUE_FORECAST(data_ts_24_all, df_preds_0, list_states, title, n_col_plot = 3)
PLOT_DATES_TRUE_FORECAST_SD(df_preds_0_sd, title_sd)
