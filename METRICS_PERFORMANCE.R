#METRICS ADDITIONAL

GLOBAL_METRICS <- function(df1, df2){
  
  print(paste0('VAR Smooth Forecast'))
  #GET_RMSE_MAE_DF(df1)
  GET_W_RMSE_MAE_DF(df1)
  print(paste0('bias: ', GET_BIAS(df1$Actual, df1$Predicted)))
  
  print('*************')
  
  print(paste0('Naive Smooth Estimate'))
  #GET_RMSE_MAE_DF(df2)
  GET_W_RMSE_MAE_DF(df2)
  print(paste0('bias: ', GET_BIAS(df2$Actual, df2$Predicted)))
  
  print('*************')
  
}

GLOBAL_DIFF_METRICS <- function(df1, df2){
  
  print(paste0('VAR Smooth Forecast:'))
  print('Positive Slope')
  GET_CORR_POS_SLOPE(df1$Actual, df1$Predicted)
  print('Positive Difference')
  GET_POS_DIFF(df1$Actual, df1$Predicted)
  print('*************')
  
  print(paste0('Naive Smooth Estimate'))
  print('Positive Slope')
  GET_CORR_POS_SLOPE(df2$Actual, df2$Predicted)
  print('Positive Difference')
  GET_POS_DIFF(df2$Actual, df2$Predicted)
  print('*************')

}


GET_DF_WEIGHTED_SLOPE <- function(df_mpox_smooth, list_jur){
  
  df_slope = data.frame()
  
  for (jur in list_jur){
    
    df_smooth_jur = df_mpox_smooth %>% filter(Jurisdiction == jur) %>%
      arrange(date_week_start)
    
    #Slope
    delta_raw <- diff(df_smooth_jur$Cases)
    delta_yt = pmax(delta_raw, 0) #NB: pmax
    
    #Dated dataframe
    date_week_start <- df_smooth_jur$date_week_start[-1]
    
    df_slope_jur <- data.frame(
      Jurisdiction = jur,
      date_week_start = date_week_start,
      delta_yt = delta_yt
    )
    
    #Combine
    df_slope = rbind(df_slope, df_slope_jur)
  }
  
  
  return(df_slope)
}


GET_SLOPE_WEIGHTED_RMSE <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt
  
  slope_weighted_rmse <- sqrt(sum(weights * (df_preds_slope$Actual - df_preds_slope$Predicted)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  
  slope_weighted_rmse = round(slope_weighted_rmse, 3)
  print(paste0('Slope weighted rmse:', slope_weighted_rmse))
  
  return(slope_weighted_rmse)
}

GET_SLOPE_WEIGHTED_MAE <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt
  
  slope_weighted_mae =  sum(weights * abs(df_preds_slope$Actual - df_preds_slope$Predicted)) / sum(weights)
  
  slope_weighted_mae = round(slope_weighted_mae, 3)
  print(paste0('Slope weighted mae:', slope_weighted_mae))
  
  return(slope_weighted_mae)
}


GET_LIST_SLOPE_WEIGHTED_RMSE_JUR <- function(df_slope, df1, df2, jur_identifier) {
  
  'GET_LIST_SLOPE_WEIGHTED RMSE_JUR FOR PLOT'
  
  #FILTER JURISDICTION
  print(paste0('RMSE, Jur: ', jur_identifier))
  df1 = df1 %>% filter(Jurisdiction == jur_identifier)
  df2 = df2 %>% filter(Jurisdiction == jur_identifier)

  rmse_1 = GET_SLOPE_WEIGHTED_RMSE(df_slope, df1)
  rmse_2 = GET_SLOPE_WEIGHTED_RMSE(df_slope, df2)

  list_rmse = c(rmse_1, rmse_2)
  print(list_rmse)
  
  return(list_rmse)
}

GET_LIST_SLOPE_WEIGHTED_MAE_JUR <- function(df_slope, df1, df2, jur_identifier) {
  
  'GET_LIST_WRMSE_JUR FOR PLOT'
  
  #FILTER JURISDICTION
  print(paste0('MAE, Jur: ', jur_identifier))
  df1 = df1 %>% filter(Jurisdiction == jur_identifier)
  df2 = df2 %>% filter(Jurisdiction == jur_identifier)
  
  mae1 = GET_SLOPE_WEIGHTED_MAE(df_slope, df1)
  mae2 = GET_SLOPE_WEIGHTED_MAE(df_slope, df2)
  
  list_mae = c(mae1, mae2)
  print(list_mae)
  
  return(list_mae)
}

GET_SLOPE_WEIGHTED_METRICS <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Jurisdiction, date_week_start, Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt

  #RMSE
  slope_weighted_rmse <- sqrt(sum(weights * (df_preds_slope$Actual - df_preds_slope$Predicted)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  slope_weighted_rmse = round(slope_weighted_rmse, 10) #3
  print(paste0('Slope weighted RMSE:', slope_weighted_rmse))
  
  #MAE
  slope_weighted_mae =  sum(weights * abs(df_preds_slope$Actual - df_preds_slope$Predicted), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_mae = round(slope_weighted_mae, 10) #3
  print(paste0('Slope weighted MAE:', slope_weighted_mae))
  
  #BIAS
  slope_weighted_bias = sum(weights * (df_preds_slope$Predicted - df_preds_slope$Actual), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_bias = round(slope_weighted_bias, 10) #3
  print(paste0('Slope weighted BIAS:', slope_weighted_bias))
  
  return(list(slope_weighted_rmse = slope_weighted_rmse, slope_weighted_mae = slope_weighted_mae, slope_weighted_bias = slope_weighted_bias))
}



GET_SLOPE_METRICS_JURISDICTION <- function(df_preds, df_slope, list_order) {
  
  df_metrics <- data.frame()
  
  for (jur in list_order) {
    
    # Filter predictions and slope for the jurisdiction
    df_preds_jur <- df_preds %>% filter(Jurisdiction == jur)
    df_slope_jur <- df_slope %>% filter(Jurisdiction == jur)
    
    # Get slope-weighted metrics as a named list
    slope_metrics <- GET_SLOPE_WEIGHTED_METRICS(df_slope_jur, df_preds_jur)
    
    # Add row to summary dataframe
    df_metrics <- bind_rows(df_metrics, tibble(
      Jurisdiction = jur,
      slope_weighted_rmse = slope_metrics$slope_weighted_rmse,
      slope_weighted_mae  = slope_metrics$slope_weighted_mae,
      slope_weighted_bias = slope_metrics$slope_weighted_bias
    ))
  }

  
  return(df_metrics)
}


GET_JUR_METRICS_MODELS_COMPARED1 <- function(df1, df2, df_slope, list_order) {
  
  res1 <- GET_SLOPE_METRICS_JURISDICTION(df1, df_slope, list_order)
  res2 <- GET_SLOPE_METRICS_JURISDICTION(df2, df_slope, list_order)
  
  df_combined <- res1 %>%
    rename(
      var_w_slope_rmse = slope_weighted_rmse,
      var_w_slope_mae  = slope_weighted_mae,
      var_w_slope_bias = slope_weighted_bias
    ) %>%
    left_join(
      res2 %>% rename(
        naive_w_slope_rmse = slope_weighted_rmse,
        naive_w_slope_mae  = slope_weighted_mae,
        naive_w_slope_bias = slope_weighted_bias
      ),
      by = "Jurisdiction"
    ) %>%
    mutate(
      var_rmse_pcent_improve = round(100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse, 2),
      var_mae_pcent_improve  = round(100 * (naive_w_slope_mae  - var_w_slope_mae)  / naive_w_slope_mae, 2),
      var_bias_pcent_improve = round(100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias, 2)
    ) %>%
    dplyr::select(
      Jurisdiction,
      var_w_slope_rmse, naive_w_slope_rmse, var_rmse_pcent_improve,
      var_w_slope_mae,  naive_w_slope_mae,  var_mae_pcent_improve,
      var_w_slope_bias, naive_w_slope_bias, var_bias_pcent_improve
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  df_combined
  
  return(df_combined)
}

GET_JUR_METRICS_MODELS_COMPARED <- function(df1, df2, df_slope, list_order) {
  
  res1 <- GET_SLOPE_METRICS_JURISDICTION(df1, df_slope, list_order)
  res2 <- GET_SLOPE_METRICS_JURISDICTION(df2, df_slope, list_order)
  
  df_combined <- res1 %>%
    rename(
      var_w_slope_rmse = slope_weighted_rmse,
      var_w_slope_mae  = slope_weighted_mae,
      var_w_slope_bias = slope_weighted_bias
    ) %>%
    left_join(
      res2 %>% rename(
        naive_w_slope_rmse = slope_weighted_rmse,
        naive_w_slope_mae  = slope_weighted_mae,
        naive_w_slope_bias = slope_weighted_bias
      ),
      by = "Jurisdiction"
    ) %>%
    mutate(
      var_rmse_pcent_improve = round(100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse, 10),
      var_mae_pcent_improve  = round(100 * (naive_w_slope_mae  - var_w_slope_mae)  / naive_w_slope_mae, 10),
      var_bias_pcent_improve = round(100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias, 10)
    ) %>%
    dplyr::select(
      Jurisdiction,
      var_w_slope_rmse, naive_w_slope_rmse, var_rmse_pcent_improve,
      var_w_slope_mae,  naive_w_slope_mae,  var_mae_pcent_improve,
      var_w_slope_bias, naive_w_slope_bias, var_bias_pcent_improve
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  # Create the average row
  avg_row <- df_combined %>%
    dplyr::select(-Jurisdiction) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(Jurisdiction = "Average") %>%
    dplyr::select(Jurisdiction, everything())
  
  # Bind to bottom of full table
  df_final <- bind_rows(df_combined, avg_row)
  
  return(df_final)
}


GET_SLOPE_WEIGHTED_IMPROVEMENT <- function(df_slope, df_preds1, df_preds2) {
  
  # Get slope-weighted metrics for each prediction set
  list_metrics1 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
  list_metrics2 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)
  
  # Extract values
  var_w_slope_rmse <- list_metrics1$slope_weighted_rmse
  naive_w_slope_rmse <- list_metrics2$slope_weighted_rmse
  var_w_slope_mae <- list_metrics1$slope_weighted_mae
  naive_w_slope_mae <- list_metrics2$slope_weighted_mae
  var_w_slope_bias <- list_metrics1$slope_weighted_bias
  naive_w_slope_bias <- list_metrics2$slope_weighted_bias
  
  # Calculate percentage improvements
  var_rmse_pcent_improve <- 100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse
  var_mae_pcent_improve <- 100 * (naive_w_slope_mae - var_w_slope_mae) / naive_w_slope_mae
  var_bias_pcent_improve <- 100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias
  
  # Return as one-row dataframe
  var_w_slope_rmse = round(var_w_slope_rmse, 3)
  naive_w_slope_rmse = round(naive_w_slope_rmse, 3)
  var_rmse_pcent_improve = round(var_rmse_pcent_improve, 1)
  var_w_slope_mae = round(var_w_slope_mae, 3)
  naive_w_slope_mae = round(naive_w_slope_mae, 3)
  var_mae_pcent_improve = round(var_mae_pcent_improve, 1)
  var_w_slope_bias = round(var_w_slope_bias, 3)
  naive_w_slope_bias = round(naive_w_slope_bias, 3)
  var_bias_pcent_improve = round(var_bias_pcent_improve, 1)
  
  df_out <- data.frame(
    Evaluation_Metric = c('Slope weighted RMSE', 'Slope weighted MAE', 'Slope weighted Bias'),
    VAR_result = c(var_w_slope_rmse, var_w_slope_mae, var_w_slope_bias),
    Naive_result = c(naive_w_slope_rmse, naive_w_slope_mae, naive_w_slope_bias),
    Percent_improve_var = c(var_rmse_pcent_improve, var_mae_pcent_improve, var_bias_pcent_improve)
  )
  
  return(df_out)
}



#*********************
#*
GET_WEIGHTED_MAE <- function(actual_col, pred_col){
  
  weights = actual_col
  w_mae = sum(weights * abs(actual_col - pred_col)) / sum(weights)
  
  return(w_mae)
  
}


GET_RMSE_MAE_DF <- function(df_preds){
  
  rmse =  sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
  print(paste0('RMSE: ', round(rmse,3)))
  mae = mean(abs(df_preds$Actual - df_preds$Predicted))
  print(paste0('MAE: ', round(mae,3)))
  
}

GET_W_RMSE_MAE_DF <- function(df_preds){
  
  #Bias
  bias = GET_BIAS(df_preds$Actual, df_preds$Predicted)
  print(paste0('bias: ', round(bias,3)))
  
  w_bias = GET_WEIGHTED_BIAS(df_preds$Actual, df_preds$Predicted)
  print(paste0('Weighted bias: ', round(w_bias,3)))
  
  #browser()
  mae = mean(abs(df_preds$Actual - df_preds$Predicted))
  print(paste0('MAE: ', round(mae,3)))
  rmse =  sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
  print(paste0('RMSE: ', round(rmse,3)))
  #WEIGHTED
  weights = df_preds$Actual
  w_rmse = sqrt(sum(weights*(df_preds$Actual - df_preds$Predicted)^2) / sum(weights))
  print(paste0('Weighted RMSE: ', round(w_rmse,3)))
  
  w_mae =  sum(weights * abs(df_preds$Actual - df_preds$Predicted)) / sum(weights)
  print(paste0('Weighted MAE: ', round(w_mae,3)))

  
}

GET_MAE <- function(actual_col, pred_col){
  
  #pos_indices <- which(actual_col > 0)
  #mae = mean(abs(actual_col[pos_indices] - pred_col[pos_indices]))
  mae = mean(abs(actual_col - pred_col))
  
  return(mae)
  
}

GET_RMSE <- function(actual_col, pred_col){
  
  rmse =  sqrt(mean((actual_col - pred_col)^2))
  return(rmse)
  
}


GET_WEIGHTED_RMSE <- function(actual_col, pred_col){
  
  weights = actual_col
  w_rmse = sqrt(sum(weights*(actual_col - pred_col)^2) / sum(weights))

  return(w_rmse)
  
}

GET_WEIGHTED_MAE <- function(actual_col, pred_col){
  
  weights = actual_col
  w_mae = sum(weights * abs(actual_col - pred_col)) / sum(weights)
  
  return(w_mae)
  
}


GET_BIAS <- function(actual_col, pred_col){
  
  bias = mean(pred_col - actual_col, na.rm = TRUE)
  bias = round(bias, 3)

  return(bias)
  
}

GET_WEIGHTED_BIAS <- function(actual_col, pred_col) {
  
  weights = actual_col
  # Weighted mean of signed errors
  bias <- sum(weights * (pred_col - actual_col), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  return(round(bias, 3))
}



GET_CORR_POS_SLOPE <- function(actual_col, pred_col){
  
  corr_slope = cor(diff(actual_col), diff(pred_col))
  
  #Positive only
  dy_true <- diff(actual_col)
  dy_pred <- diff(pred_col) 
  
  # Identify indices where true values increased
  increase_indices <- which(dy_true > 0)
  # print(paste0('true data increase_indices: '))
  # print(dy_true[increase_indices])
  # print(dy_pred[increase_indices])
  # 
  corr_pos_slope = cor(dy_true[increase_indices], dy_pred[increase_indices]) #0.25
  #corr_pos_slope = cor(diff(actual_col[increase_indices]), diff(pred_col[increase_indices]))
  
  # print(paste0('Corr positive slope: ', round(corr_pos_slope, 3)))
  # print(paste0('Corr slope: ', round(corr_slope, 3)))
  
  return(corr_pos_slope)
  
}

GET_CORR_SLOPE <- function(actual_col, pred_col){
  
  corr_slope = cor(diff(actual_col), diff(pred_col))
  
  return(corr_slope)
  
}

GET_POS_DIFF <- function(actual_col, pred_col){
  
  # First differences
  dy_true <- diff(actual_col)
  dy_pred <- diff(pred_col)
  
  # Identify indices where true values increased
  increase_indices <- which(dy_true > 0)
  direction_agreement <- sign(dy_true[increase_indices]) == sign(dy_pred[increase_indices])
  accuracy_on_increases <- mean(direction_agreement)
  
  #print(paste0('Positive diff: ', round(accuracy_on_increases, 3)))
  
  return(accuracy_on_increases)
  
}


METRICS_JURISDICTION <- function(df_pred_results, list_order) {
  
  df_pred_summary <- df_pred_results %>%
    group_by(Jurisdiction) %>%
    summarise(
      mae = GET_MAE(Actual, Predicted),
      rmse = GET_RMSE(Actual, Predicted),
      bias = GET_BIAS(Actual, Predicted),
      pos_diff = GET_POS_DIFF(Actual, Predicted),
      weighted_rmse = GET_WEIGHTED_RMSE(Actual, Predicted),
      weighted_mae = GET_WEIGHTED_MAE(Actual, Predicted),
      .groups = "drop"
    ) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_order)) %>%
    arrange(Jurisdiction)
  
  # Create average row
  avg_row <- tibble(
    Jurisdiction = "Average",
    mae = mean(df_pred_summary$mae, na.rm = TRUE),
    rmse = mean(df_pred_summary$rmse, na.rm = TRUE),
    bias = mean(df_pred_summary$bias, na.rm = TRUE),
    pos_diff = mean(df_pred_summary$pos_diff, na.rm = TRUE),
    weighted_rmse = mean(df_pred_summary$weighted_rmse, na.rm = TRUE),
    weighted_mae = mean(df_pred_summary$weighted_mae, na.rm = TRUE),
  )
  
  # Bind average row to the bottom
  df_pred_summary_add <- bind_rows(df_pred_summary, avg_row)
  
  return(df_pred_summary_add)
}

METRICS_COMBINED <- function(df1, df2, df3, df4, list_order) {
  
  #1. GLOBAL RMSE & MAE PRINT
  GLOBAL_RMSE_MAE(df1, df2, df3, df4)
  # Get summaries for each model using your existing function
  res1 <- METRICS_JURISDICTION(df1, list_order)
  res2 <- METRICS_JURISDICTION(df2, list_order)
  res3 <- METRICS_JURISDICTION(df3, list_order)
  res4 <- METRICS_JURISDICTION(df4, list_order)
  
  # Merge them by Jurisdiction
  df_combined <- res1 %>%
    dplyr::select(Jurisdiction, rmse, mae, pos_diff, corr_slope) %>%
    rename(var_smooth_rmse = rmse, var_smooth_mae = mae, var_smooth_poss_diff = pos_diff, var_smooth_corr_slope = corr_slope) %>%
    left_join(res2 %>%  dplyr::select(Jurisdiction, rmse, mae,  pos_diff, corr_slope) %>%
                rename(naive_smooth_rmse = rmse, naive_smooth_mae = mae, naive_smooth_poss_diff = pos_diff, naive_smooth_corr_slope = corr_slope),
              by = "Jurisdiction") %>%
    left_join(res3 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(var_raw_rmse = rmse, var_raw_mae = mae),
              by = "Jurisdiction") %>%
    left_join(res4 %>%  dplyr::select(Jurisdiction, rmse, mae) %>%
                rename(naive_raw_rmse = rmse, naive_raw_mae = mae),
              by = "Jurisdiction")
  
  df_combined = df_combined %>% dplyr::select(Jurisdiction, var_smooth_rmse, naive_smooth_rmse, var_smooth_poss_diff, naive_smooth_poss_diff,
                                              var_smooth_corr_slope, naive_smooth_corr_slope,
                                              var_smooth_mae, naive_smooth_mae, var_raw_rmse, naive_raw_rmse, var_raw_mae, naive_raw_mae)
  
  return(df_combined)
}

METRICS_COMBINED <- function(df1, df2, list_order) {
  
  #1. GLOBAL RMSE & MAE PRINT
  GLOBAL_METRICS(df1, df2)
  # Get summaries for each model using your existing function
  res1 <- METRICS_JURISDICTION(df1, list_order)
  res2 <- METRICS_JURISDICTION(df2, list_order)
  
  # Merge them by Jurisdiction
  df_combined <- res1 %>%
    dplyr::select(Jurisdiction, bias, rmse, mae, weighted_rmse, weighted_mae) %>% #, pos_diff, corr_slope
    rename(var_bias = bias, var_smooth_rmse = rmse, var_smooth_mae = mae, var_weighted_rmse = weighted_rmse, var_weighted_mae = weighted_mae) %>% #var_smooth_poss_diff = pos_diff, var_corr_slope = corr_slope
    left_join(res2 %>%  dplyr::select(Jurisdiction, bias, rmse, mae, weighted_rmse, weighted_mae) %>% #pos_diff, corr_slope
                rename(naive_bias = bias, naive_smooth_rmse = rmse, naive_smooth_mae = mae, naive_weighted_rmse = weighted_rmse, naive_weighted_mae = weighted_mae), #naive_smooth_poss_diff = pos_diff, naive_corr_slope = corr_slope
              by = "Jurisdiction")
  
  df_combined = df_combined %>% dplyr::select(Jurisdiction, var_bias, naive_bias, var_smooth_rmse, naive_smooth_rmse, 
                                              var_weighted_rmse, naive_weighted_rmse, 
                                              var_smooth_mae, naive_smooth_mae, var_weighted_mae, naive_weighted_mae) #, var_corr_slope, naive_corr_slope)
  
  return(df_combined)
}

METRICS_DIFF_COMBINED <- function(df1, df2, list_order) {
  
  #1. GLOBAL RMSE & MAE PRINT
  #GLOBAL_RMSE_MAE(df1, df2, df3, df4)
  # Get summaries for each model using your existing function
  res1 <- METRICS_JURISDICTION(df1, list_order)
  res2 <- METRICS_JURISDICTION(df2, list_order)

  # Merge them by Jurisdiction
  df_combined <- res1 %>%
    dplyr::select(Jurisdiction, rmse, mae, pos_diff, corr_poss_slope, corr_slope) %>%
    rename(var_smooth_poss_diff = pos_diff, var_corr_pos_slope = corr_poss_slope, var_corr_slope = corr_slope) %>%
    left_join(res2 %>%  dplyr::select(Jurisdiction, rmse, mae, pos_diff, corr_poss_slope, corr_slope) %>%
                rename(naive_smooth_poss_diff = pos_diff, naive_corr_pos_slope = corr_poss_slope, naive_corr_slope = corr_slope),
              by = "Jurisdiction") 
  
  df_combined = df_combined %>% dplyr::select(Jurisdiction, var_corr_pos_slope, naive_corr_pos_slope,
                                              var_corr_slope, naive_corr_slope,
                                              var_smooth_poss_diff, naive_smooth_poss_diff)
  
  return(df_combined)
}