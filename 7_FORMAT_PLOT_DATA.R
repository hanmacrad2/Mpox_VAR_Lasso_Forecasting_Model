
#****************************
#* PLOT DATA
#**************************

data_ts = GET_TS_DATA(data_mpox_model)
data_model_24 = data_mpox_model %>% filter(Year == '2024')
data_ts_24 = GET_TS_DATA(data_model_24)

#WEEK NUMBER
WEEK_FORECAST = TRAIN_WEEK + 2
data_24_forecast_start = data_mpox_model %>% filter(Week_Number >= WEEK_FORECAST)
data_24_ts_forecast_start = GET_TS_DATA(data_24_forecast_start)




#SAN DIEGO
data_ts_24_sd = data_ts_24 %>% dplyr::select('date_week_start', 'Week_Number', 'SanDiego')

#*************************
#* OTHER NOT USED

# #YEAR 2024, TRAIN WEEK 107-128
# FORECAST_START = 107; FORECAST_END = 128 
# data_ts_24_june = data_ts_24  %>% filter(Week_Number >= FORECAST_START)
# data_ts_24_june = data_ts_24_june  %>% filter(Week_Number <= FORECAST_END)
# 
# #***********************************
# #PLOT2024
# data_start_date = as.Date("2024-01-01") 
# 
# FORECAST_START = 85 #WEEK 1, 2023
# data_ts_24_all = data_ts  %>% filter(Week_Number >= FORECAST_START)
# FORECAST_END = 128 #127
# data_ts_24_all = data_ts_24_all  %>% filter(Week_Number <= FORECAST_END)
# 
# #***********************************
# #2023 + 2024
# FORECAST_START = 33 #WEEK 1, 2023
# data_start_date = as.Date("2023-01-01") 
# data_ts_23_24 = data_ts  %>% filter(Week_Number >= FORECAST_START)
# FORECAST_END = 128 #127
# data_ts_23_24 = data_ts_23_24  %>% filter(Week_Number <= FORECAST_END)
# 
# 
# #********************
# #* SMOOTHED
# data_ts_23_24_smooth = data_tot_ts_smooth  %>% filter(Week_Number >= FORECAST_START)
# data_ts_23_24_smooth = data_ts_23_24_smooth  %>% filter(Week_Number <= FORECAST_END)
# 
# #2024
# WEEK_1_24 = 85
# data_ts_24_smooth = data_tot_ts_smooth  %>% filter(Week_Number >= WEEK_1_24)
# 
# 
