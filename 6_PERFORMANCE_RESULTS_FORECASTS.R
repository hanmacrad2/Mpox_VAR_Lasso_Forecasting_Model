#************************************************
#* RESULTS OF FORECAST
#***********************************************

FOLDER_RESULTS = "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/CODE_FINAL/RESULTS/"

#LIST RESULTS
df_preds = list_results$df_pred_results
#SAVE
saveRDS(list_results, paste0(FOLDER_RESULTS, 'list_res_IV_top_10_23_24'))

#PREDICTIONS
rmse = sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
print(rmse)

df_forecast_summary = PERFORMANCE_RESULTS_JURISDICTION(df_preds, list_jur)

#Other Smoothing windows
df_forecast_summary_2 = PERFORMANCE_RESULTS_JURISDICTION(df_preds1, list_jur)

df_forecast_summary_2 = PERFORMANCE_RESULTS_JURISDICTION(list_results_2$df_pred_results, list_jur)
df_forecast_summary_3 = PERFORMANCE_RESULTS_JURISDICTION(list_results_3$df_pred_results, list_jur)
df_forecast_summary_4 = PERFORMANCE_RESULTS_JURISDICTION(list_results_4$df_pred_results, list_jur)

#Other Steps Ahead
df_summary_3_weeks = PERFORMANCE_RESULTS_JURISDICTION(list_results_3_week_roll4$df_pred_results, list_jur)
df_summary_4_weeks = PERFORMANCE_RESULTS_JURISDICTION(list_results_4_week_roll4$df_pred_results, list_jur)


#OTHER Jurisdictions
df_forecast_summary_11 = PERFORMANCE_RESULTS_JURISDICTION(df_preds, list_top_11)
df_forecast_summary_14 = PERFORMANCE_RESULTS_JURISDICTION(df_preds, list_top_14)






