#PLOT DATA
title_data = 'Mpox Weekly Reported Case Data: 2023-2024'
PLOT_DATA(data_tot_ts, df_preds1, list_jur, title_data, n_col_plot = 3)

#PLOT DATA SMOOTHED
title_data = 'Mpox Case Data: 2023-2024'
PLOT_DATA(data_tot_ts_smooth, list_results_4_week_roll4$df_pred_results, list_states, title_data, n_col_plot = 3)

title_data = 'Smoothed Mpox Case Data (Rolling Average Window of 4 weeks): 2023-2024'
PLOT_DATA(data_tot_ts_smooth, df_preds1, list_jur, title_data, n_col_plot = 3)


#2023
data_tot_ts_23 = data_tot_ts %>% filter(Week_Number > 32)
data_tot_ts_23 = data_tot_ts_23 %>% filter(Week_Number < 85)
#list_plot_jur = c('SanDiego', "California", 'LA', 'Illinois', 'NewYorkCity')
list_plot_jur = c('SanDiego', 'LA', 'Illinois', 'NewYorkCity', 'Washington', 'Florida')
title_data = '2023 Weekly Reported Mpox Cases - San Diego County, LA, Illinois, NYC, Washington, Florida'
PLOT_MULT_DATA(data_tot_ts_23, list_plot_jur, title_data)

#PLOT SUBPLOTS
PLOT_DATA(data_tot_ts_23, df_preds1, list_plot_jur, title_data, n_col_plot = 1)

#2024
data_tot_ts_24 = data_tot_ts %>% filter(Week_Number > 84)
list_plot_jur = c('SanDiego', "California", 'LA', 'Illinois')
list_plot_jur = c('SanDiego', "California", 'LA', 'Illinois', 'NewYorkCity')
title_data = '2024 Weekly Reported Mpox Cases - San Diego County, California, LA, Illinois, NYC'

title_data = '2024 Weekly Reported Mpox Cases - San Diego County, LA, Illinois, NYC, Washington, Florida'
PLOT_MULT_DATA(data_tot_ts_24, list_plot_jur, title_data)

#PLOT SUBPLOTS
PLOT_DATA(data_tot_ts_24, df_preds1, list_plot_jur, title_data, n_col_plot = 1)

#San Diego County (Forecast start week)
data_tot_ts_24 = data_tot_ts %>% filter(Week_Number > 86)
list_sd = c('SanDiego')
title_data = 'San Diego County - Weekly Reported Mpox Cases in 2024'
PLOT_DATA(data_tot_ts_24, df_preds1, list_sd, title_data, n_col_plot = 1)

