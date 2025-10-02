#**************************************
#* 4. TRAIN TEST SPLIT
#***************************************

#1. DATA 
TRAIN_WEEK = 85 #104, 105
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))

TRAIN_WEEK = 135 #end of 2024
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
list_data = get_train_test_data(data_mpox_model, TRAIN_WEEK)
train_data = list_data$train_data
future_data = list_data$future_data

#**************************************
#* 5. TIME SERIES FORMATTED
train_data_ts = GET_TS_DATA(train_data)
future_data_ts = GET_TS_DATA(future_data)
data_tot_ts = GET_TS_DATA(data_mpox_model)

#**************************************
#2. SMOOTHED DATA
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))
list_data_smooth = get_train_test_data(df_mpox_smooth, TRAIN_WEEK)
train_data_smooth = list_data_smooth$train_data
future_data_smooth = list_data_smooth$future_data

#**************************************
#* 5. TIME SERIES FORMATTED
train_data_ts_smooth = GET_TS_DATA(train_data_smooth)
future_data_ts_smooth = GET_TS_DATA(future_data_smooth)
data_tot_ts_smooth = GET_TS_DATA(df_mpox_smooth)

head(train_data_ts_smooth)
tail(train_data_ts_smooth)
head(future_data_ts_smooth)
tail(future_data_ts_smooth)
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))


#****************************
#* 6. PLOT DATA; RE-DO

data_ts = GET_TS_DATA(data_mpox_model)
data_model_24 = data_mpox_model %>% filter(Year == '2024')
data_ts_24 = GET_TS_DATA(data_model_24)

#WEEK NUMBER
WEEK_FORECAST = TRAIN_WEEK + 2
#WEEK_END = 171 #128
WEEK_END = 169
data_24_forecast_start = data_mpox_model %>% filter(Week_Number >= WEEK_FORECAST)
data_24_forecast_start = data_24_forecast_start %>% filter(Week_Number <= WEEK_END)
data_24_ts_forecast_start = GET_TS_DATA(data_24_forecast_start)

#JOIN TO GET date_week_start 
data_mpox_nyc = data_mpox_tot %>% filter(Jurisdiction == 'NewYorkCity')

#JOIN
data_24_ts_forecast_start <- data_24_ts_forecast_start %>%
  left_join(
    data_mpox_nyc %>% dplyr::select(Week_Number, date_week_start),
    by = "Week_Number"
  )
