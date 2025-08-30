#********************************************
#*
#* UTIL PERFORMANCE FUNCTIONS 
#*
#*******************************************

GET_MAE <- function(actual_col, pred_col){
  
  mae = mean(abs(actual_col - pred_col))
  
  return(mae)
  
}

GET_RMSE <- function(actual_col, pred_col){
  
  rmse =  sqrt(mean((actual_col - pred_col)^2))
  
  return(rmse)
  
}

GET_MAE_DF <- function(df_preds){
  
  mae = mean(abs(df_preds$Actual - df_preds$Predicted))
  
  return(mae)
  
}

GET_RMSE_DF <- function(df_preds){
  
  rmse =  sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
  
  return(rmse)
  
}

GET_RMSE_MAE_DF <- function(df_preds){
  
  rmse =  sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
  print(paste0('RMSE: ', round(rmse,2)))
  mae = mean(abs(df_preds$Actual - df_preds$Predicted))
  print(paste0('MAE: ', round(mae,2)))
  
}


PERFORMANCE_RESULTS_JURISDICTION <- function(df_pred_results, list_order) {
  
  df_pred_summary <- df_pred_results %>%
    group_by(Jurisdiction) %>%
    summarise(
      mae = GET_MAE(Actual, Predicted),
      rmse = GET_RMSE(Actual, Predicted),
      .groups = "drop"
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  # Create average row
  avg_row <- tibble(
    Jurisdiction = "Average",
    mae = mean(df_pred_summary$mae, na.rm = TRUE),
    rmse = mean(df_pred_summary$rmse, na.rm = TRUE)
  )
  
  # Bind average row to the bottom
  df_pred_summary_add <- bind_rows(df_pred_summary, avg_row)
  
  return(df_pred_summary_add)
}

GET_RMSE_JUR <- function(df_pred_results, jur_name){
  
  df_jur = df_pred_results %>% filter(Jurisdiction == jur_name)
  rmse_jur = GET_RMSE(df_jur$Actual, df_jur$Predicted)
  
  return(rmse_jur)
  
}

GET_W_MAE_JUR <- function(df_pred_results, jur_name){
  
  df_jur = df_pred_results %>% filter(Jurisdiction == jur_name)
  w_mae_jur = GET_WEIGHTED_MAE(df_jur$Actual, df_jur$Predicted)
  
  return(w_mae_jur)
  
}

GET_W_RMSE_JUR <- function(df_pred_results, jur_name){
  
  df_jur = df_pred_results %>% filter(Jurisdiction == jur_name)
  w_rmse_jur = GET_WEIGHTED_RMSE(df_jur$Actual, df_jur$Predicted)
  
  return(w_rmse_jur)
  
}

GET_MAE_JUR <- function(df_pred_results, jur_name){
  
  df_jur = df_pred_results %>% filter(Jurisdiction == jur_name)
  rmse_jur = GET_MAE(df_jur$Actual, df_jur$Predicted)
  
  return(rmse_jur)
  
}


#****************************
#RUN METRICS + PLOTS

GET_LIST_RMSE_JUR <- function(df1, df2, df3, df4, jur_identifier) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  
  print(paste0('Jur: ', jur_identifier))
  rmse_1 = GET_RMSE_JUR(df1, jur_identifier)
  rmse_2 = GET_RMSE_JUR(df2, jur_identifier)
  rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4)
  print(list_rmse)
  
  return(list_rmse)
}

GET_LIST_WEIGHTED_RMSE_JUR <- function(df1, df2, df3, df4, jur_identifier) {
  
  'GET_LIST_WRMSE_JUR FOR PLOT'
  
  print(paste0('Jur: ', jur_identifier))
  rmse_1 = GET_W_RMSE_JUR(df1, jur_identifier)
  rmse_2 = GET_W_RMSE_JUR(df2, jur_identifier)
  rmse_3 = GET_W_RMSE_JUR(df3, jur_identifier)
  rmse_4 =  GET_W_RMSE_JUR(df4, jur_identifier)
  
  list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4)
  print(list_rmse)
  
  return(list_rmse)
}

GET_LIST_MAE_JUR <- function(df1, df2, df3, df4, jur_identifier) {
  
  'GET_LIST_MAE_JUR FOR PLOT'
  
  print(paste0('Jur: ', jur_identifier))
  mae_1 = GET_MAE_JUR(df1, jur_identifier)
  mae_2 = GET_MAE_JUR(df2, jur_identifier)
  mae_3 = GET_MAE_JUR(df3, jur_identifier)
  mae_4 =  GET_MAE_JUR(df4, jur_identifier)
  
  list_mae = c(mae_1, mae_2, mae_3, mae_4)
  print(list_mae)
  
  return(list_mae)
}

GET_LIST_WEIGHTED_MAE_JUR <- function(df1, df2, df3, df4, jur_identifier) {
  
  'GET_LIST_MAE_JUR FOR PLOT'
  
  print(paste0('Jur: ', jur_identifier))
  mae_1 = GET_W_MAE_JUR(df1, jur_identifier)
  mae_2 = GET_W_MAE_JUR(df2, jur_identifier)
  mae_3 = GET_W_MAE_JUR(df3, jur_identifier)
  mae_4 =  GET_W_MAE_JUR(df4, jur_identifier)
  
  list_mae = c(mae_1, mae_2, mae_3, mae_4)
  print(list_mae)
  
  return(list_mae)
}


#**************************
#WINDOW
GET_LIST_RMSE_JUR_WINDOW <- function(df1, df2, df3, df4, df_window_detect, jur_identifier) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  
  df1 <- df1 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df2 <- df2 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df3 <- df3 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df4 <- df4 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  #FILTER
  df1 = df1 %>% filter(increasing_plot == 1)
  df2 = df2 %>% filter(increasing_plot == 1)
  df3 = df3 %>% filter(increasing_plot == 1)
  df4 = df4 %>% filter(increasing_plot == 1)
  
  print(paste0('Jur: ', jur_identifier))
  rmse_1 = GET_RMSE_JUR(df1, jur_identifier)
  rmse_2 = GET_RMSE_JUR(df2, jur_identifier)
  rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  list_rmse = c(rmse_1, rmse_2, rmse_3, rmse_4)
  print(list_rmse)
  
  return(list_rmse)
}


GET_LIST_MAE_JUR_WINDOW <- function(df1, df2, df3, df4, df_window_detect, jur_identifier) {
  
  'GET_LIST_MAE_JUR FOR PLOT'
  
  df1 <- df1 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df2 <- df2 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df3 <- df3 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df4 <- df4 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  #FILTER
  df1 = df1 %>% filter(increasing_plot == 1)
  df2 = df2 %>% filter(increasing_plot == 1)
  df3 = df3 %>% filter(increasing_plot == 1)
  df4 = df4 %>% filter(increasing_plot == 1)
  
  print(paste0('Jur: ', jur_identifier))
  mae_1 = GET_MAE_JUR(df1, jur_identifier)
  mae_2 = GET_MAE_JUR(df2, jur_identifier)
  mae_3 = GET_MAE_JUR(df3, jur_identifier)
  mae_4 =  GET_MAE_JUR(df4, jur_identifier)
  
  list_mae = c(mae_1, mae_2, mae_3, mae_4)
  print(list_mae)
  
  return(list_mae)
}


GET_LIST_RMSE_FAR_ALARMS <- function(df1, df2, farr_df_jur, jur_identifier) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  df1 = df1 %>% filter(Jurisdiction == jur_identifier)
  df2 = df2 %>% filter(Jurisdiction == jur_identifier)
  
  #ALARMS: LEFT JOIN FOR ALARMS
  df1 = df1 %>% left_join(farr_df_jur, by = 'date_week_start')
  df2 = df2 %>% left_join(farr_df_jur, by = 'date_week_start')

  
  #FILTER: ALARMS == TRUE
  df1 = df1 %>% filter(alarms == TRUE)
  df2 = df2 %>% filter(alarms == TRUE)
  
  print(paste0('Jur: ', jur_identifier))
  rmse_1 = GET_RMSE_JUR(df1, jur_identifier)
  rmse_2 = GET_RMSE_JUR(df2, jur_identifier)
  #rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  #rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  list_rmse = c(rmse_1, rmse_2) #, rmse_3, rmse_4)
  print(list_rmse)
  
  return(list_rmse)
}

GET_LIST_MAE_FAR_ALARMS <- function(df1, df2, farr_df_jur, jur_identifier) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  df1 = df1 %>% filter(Jurisdiction == jur_identifier)
  df2 = df2 %>% filter(Jurisdiction == jur_identifier)
  
  #ALARMS: LEFT JOIN FOR ALARMS
  df1 = df1 %>% left_join(farr_df_jur, by = 'date_week_start')
  df2 = df2 %>% left_join(farr_df_jur, by = 'date_week_start')
  
  
  #FILTER: ALARMS == TRUE
  df1 = df1 %>% filter(alarms == TRUE)
  df2 = df2 %>% filter(alarms == TRUE)
  
  print(paste0('Jur: ', jur_identifier))
  mae_1 = GET_MAE_JUR(df1, jur_identifier)
  mae_2 = GET_MAE_JUR(df2, jur_identifier)
  #rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  #rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  list_mae = c(mae_1, mae_2) #, rmse_3, rmse_4)
  print(list_mae)
  
  return(list_mae)
}

#************************
#GLOBAL

GET_ALL_MAE_FAR_ALARMS <- function(df_preds_1, df_preds_2, list_farr_jur, list_jur) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  df_tot_1 = data.frame()
  df_tot_2 = data.frame()
  
  for (i in seq_along(list_jur)){
    
    jur = list_jur[i]
    print(paste0('jur: ', jur))
    
    df_alarm_jur = list_farr_jur[[i]]  
    df_alarm_jur$Jurisdiction = rep(jur, length(df_alarm_jur))
    
    #PREDS i
    df_preds_1_jur = df_preds_1 %>% filter(Jurisdiction == jur)
    
    df_alarm_preds1 = df_alarm_jur %>% left_join(df_preds_1_jur, by = 'date_week_start')
    df_alarm_preds1 = df_alarm_preds1 %>% filter(alarms == TRUE)
    df_tot_1 = rbind(df_tot_1, df_alarm_preds1)
    
    #PREDS i
    df_preds_2_jur = df_preds_2 %>% filter(Jurisdiction == jur)
    
    df_alarm_preds2 = df_alarm_jur %>% left_join(df_preds_2_jur, by = 'date_week_start')
    df_alarm_preds2 = df_alarm_preds2 %>% filter(alarms == TRUE)
    df_tot_2 = rbind(df_tot_2, df_alarm_preds2)
    
    print(paste0('nrow true alarm: ', nrow(df_alarm_preds1)))
    print(paste0('nrow true alarm: ', nrow(df_alarm_preds2)))

  }

  
  #FILTER: ALARMS == TRUE
  df_tot_1 = df_tot_1 %>% filter(alarms == TRUE)
  df_tot_2 = df_tot_2 %>% filter(alarms == TRUE)
  
  mae_1 = GET_MAE_DF(df_tot_1)
  mae_2 = GET_MAE_DF(df_tot_2)
  #rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  #rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  list_mae = c(mae_1, mae_2) #, rmse_3, rmse_4)
  print(list_mae)
  
  return(list_mae)
}

GET_ALL_RMSE_MAE_FAR_ALARMS <- function(df_preds_1, df_preds_2, list_farr_jur, list_jur) {
  
  'GET_LIST_RMSE_JUR FOR PLOT'
  df_tot_1 = data.frame()
  df_tot_2 = data.frame()
  
  for (i in seq_along(list_jur)){
    
    jur = list_jur[i]
    print(paste0('jur: ', jur))
    
    df_alarm_jur = list_farr_jur[[i]]  
    df_alarm_jur$Jurisdiction = rep(jur, length(df_alarm_jur))
    
    #PREDS i
    df_preds_1_jur = df_preds_1 %>% filter(Jurisdiction == jur)
    
    df_alarm_preds1 = df_alarm_jur %>% left_join(df_preds_1_jur, by = 'date_week_start')
    df_alarm_preds1 = df_alarm_preds1 %>% filter(alarms == TRUE)
    df_tot_1 = rbind(df_tot_1, df_alarm_preds1)
    
    #PREDS i
    df_preds_2_jur = df_preds_2 %>% filter(Jurisdiction == jur)
    
    df_alarm_preds2 = df_alarm_jur %>% left_join(df_preds_2_jur, by = 'date_week_start')
    df_alarm_preds2 = df_alarm_preds2 %>% filter(alarms == TRUE)
    df_tot_2 = rbind(df_tot_2, df_alarm_preds2)
    
    print(paste0('nrow true alarm: ', nrow(df_alarm_preds1)))
    print(paste0('nrow true alarm: ', nrow(df_alarm_preds2)))
    
  }
  
  
  #FILTER: ALARMS == TRUE
  df_tot_1 = df_tot_1 %>% filter(alarms == TRUE)
  df_tot_2 = df_tot_2 %>% filter(alarms == TRUE)
  
  rmse_1 = GET_RMSE_DF(df_tot_1)
  rmse_2 = GET_RMSE_DF(df_tot_2)
  
  mae_1 = GET_MAE_DF(df_tot_1)
  mae_2 = GET_MAE_DF(df_tot_2)
  #rmse_3 = GET_RMSE_JUR(df3, jur_identifier)
  #rmse_4 =  GET_RMSE_JUR(df4, jur_identifier)
  
  print('************')
  print('var rmse, naive rmse, var mae, naive mae')
  list_rmse_mae = c(rmse_1, rmse_2, mae_1, mae_2) #, rmse_3, rmse_4)
  print(list_rmse_mae)
  
  return(list_rmse_mae)
}

GLOBAL_RMSE_MAE <- function(df1, df2, df3, df4){
  
  print(paste0('VAR Smooth Forecast'))
  GET_RMSE_MAE_DF(df1)
  print('*************')
  print(paste0('Naive Smooth Estimate'))
  GET_RMSE_MAE_DF(df2)
  print('*************')
  print(paste0('VAR Raw data Forecast'))
  GET_RMSE_MAE_DF(df3)
  print('*************')
  print(paste0('Naive Raw data Estimate'))
  GET_RMSE_MAE_DF(df4)
  print('*************')
  
}


PERFORMANCE_RESULTS_COMBINED <- function(df1, df2, df3, df4, list_order) {
  
  #1. GLOBAL RMSE & MAE PRINT
  GLOBAL_RMSE_MAE(df1, df2, df3, df4)
  # Get summaries for each model using your existing function
  res1 <- PERFORMANCE_RESULTS_JURISDICTION(df1, list_order)
  res2 <- PERFORMANCE_RESULTS_JURISDICTION(df2, list_order)
  res3 <- PERFORMANCE_RESULTS_JURISDICTION(df3, list_order)
  res4 <- PERFORMANCE_RESULTS_JURISDICTION(df4, list_order)
  
  # Merge them by Jurisdiction
  df_combined <- res1 %>%
    dplyr::select(Jurisdiction, rmse, mae) %>%
    rename(var_smooth_rmse = rmse, var_smooth_mae = mae) %>%
    left_join(res2 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(naive_smooth_rmse = rmse, naive_smooth_mae = mae),
              by = "Jurisdiction") %>%
    left_join(res3 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(var_raw_rmse = rmse, var_raw_mae = mae),
              by = "Jurisdiction") %>%
    left_join(res4 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(naive_raw_rmse = rmse, naive_raw_mae = mae),
              by = "Jurisdiction")
  
  df_combined = df_combined %>% dplyr::select(Jurisdiction, var_smooth_rmse, naive_smooth_rmse, var_smooth_mae, naive_smooth_mae, var_raw_rmse, naive_raw_rmse, var_raw_mae, naive_raw_mae)
  
  return(df_combined)
}

#PERFORMANCE RESULTS WINDOW

GLOBAL_RMSE_MAE_WINDOW <- function(df1, df2, df3, df4){

  print(paste0('Results in periods of Increased Cases'))
  print(paste0('VAR Smooth Forecast'))
  GET_RMSE_MAE_DF(df1)
  print('*************')
  print(paste0('Naive Smooth Estimate'))
  GET_RMSE_MAE_DF(df2)
  print('*************')
  print(paste0('VAR Raw data Forecast'))
  GET_RMSE_MAE_DF(df3)
  print('*************')
  print(paste0('Naive Raw data Estimate'))
  GET_RMSE_MAE_DF(df4)
  print('*************')

}

PERFORMANCE_RESULTS_WINDOW <- function(df1, df2, df3, df4, df_window_detect, list_order) {
  
  df1 <- df1 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df2 <- df2 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df3 <- df3 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  df4 <- df4 %>%
    left_join(
      df_window_detect %>% dplyr::select(Jurisdiction, Week_Number, increasing_plot),
      by = c("Jurisdiction", "Week_Number")
    )
  
  #FILTER
  df1 = df1 %>% filter(increasing_plot == 1)
  df2 = df2 %>% filter(increasing_plot == 1)
  df3 = df3 %>% filter(increasing_plot == 1)
  df4 = df4 %>% filter(increasing_plot == 1)
  
  #1. GLOBAL RMSE & MAE PRINT
  GLOBAL_RMSE_MAE(df1, df2, df3, df4)
  # Get summaries for each model using your existing function
  res1 <- PERFORMANCE_RESULTS_JURISDICTION(df1, list_order)
  res2 <- PERFORMANCE_RESULTS_JURISDICTION(df2, list_order)
  res3 <- PERFORMANCE_RESULTS_JURISDICTION(df3, list_order)
  res4 <- PERFORMANCE_RESULTS_JURISDICTION(df4, list_order)
  
  # Merge them by Jurisdiction
  df_combined <- res1 %>%
    dplyr::select(Jurisdiction, rmse, mae) %>%
    rename(var_smooth_rmse = rmse, var_smooth_mae = mae) %>%
    left_join(res2 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(naive_smooth_rmse = rmse, naive_smooth_mae = mae),
              by = "Jurisdiction") %>%
    left_join(res3 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(var_raw_rmse = rmse, var_raw_mae = mae),
              by = "Jurisdiction") %>%
    left_join(res4 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(naive_raw_rmse = rmse, naive_raw_mae = mae),
              by = "Jurisdiction")
  
  df_combined = df_combined %>% dplyr::select(Jurisdiction, var_smooth_rmse, naive_smooth_rmse, var_smooth_mae, naive_smooth_mae, var_raw_rmse, naive_raw_rmse, var_raw_mae, naive_raw_mae)
  
  return(df_combined)
}
