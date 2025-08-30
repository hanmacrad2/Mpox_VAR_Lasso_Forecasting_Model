#*******************************************
#* FORECASTS + PLOT


#PLOT TITLE
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts from June 2024, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'

#***********************
# TOP 5 JURISDICTIONS

#SAN DIEGO
jur_identifier = 'SanDiego'
jur_label = 'San Diego'
title = paste0(jur_label, title_main)
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, forecasts_5_2w$df_pred_results, forecasts_5_2w$df_pred_results,
                           forecasts_5_2w $df_pred_results, 
                           data_ts_24, title)


#***********************
# TOP 10 JURISDICTIONS: PREDICTIONS, 4 WEEEK ROLLING AVERAGE, 2,3,4 WEEK AHEAD FORECASTS
df_preds_2_weeks = list_results_4$df_pred_results #4 WEEEK ROLLING AVERAGE, 2 week ahead
df_preds_3_weeks = list_results_3_week_roll4$df_pred_results
df_preds_3_weeks = forecasts_roll4_3ahead$df_pred_results
df_preds_4_weeks = list_results_4_week_roll4$df_pred_results


#**************************
#SAN DIEGO
jur_identifier = 'SanDiego'
jur_label = 'San Diego'
title = paste0(jur_label, title_main)
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#*************************
#NEW YORK CITY
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'California'
jur_identifier = 'California'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'LA'
jur_identifier = 'LA'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'Florida'
jur_identifier = 'Florida'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'Texas'
jur_identifier = 'Texas'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'Georgia'
jur_identifier = 'Georgia'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)

#PLOT TITLE
jur_label = 'Washington'
jur_identifier = 'Washington'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)


#PLOT TITLE
jur_label = 'Colorado'
jur_identifier = 'Colorado'
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/19/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

#WEEK 104
PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_104$df_pred_results, list_results_3_104$df_pred_results,
                           list_results_4_104$df_pred_results, 
                           data_ts_24_all, title)








#WEEK 105
title_main = ' Mpox Forecasts: 2, 3, and 4 Weeks Ahead. 
Forecasts start from 05/26/24, fit to 2023 & 2024 Mpox case data (smoothed using a 4-week rolling average).'
title = paste0(jur_label, title_main)

PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_105$df_pred_results, list_results_3_105$df_pred_results,
                           list_results_4_105$df_pred_results, 
                           data_ts_24_all, title)

PLOT_JUR_HORIZON_FORECASTS(jur_identifier, list_results_2_106$df_pred_results, list_results_3_106$df_pred_results,
                           list_results_4_106$df_pred_results, 
                           data_ts_24_all, title)
