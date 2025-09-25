#FORECAST RESULTS CHECK

#COMPARE
global_rmse <- sqrt(mean((df_preds$Actual - df_preds$Predicted)^2))
global_rmse <- sqrt(MSE(df_preds$Actual, df_preds$Predicted))

# Average of jurisdiction-level RMSEs
per_jur_rmse <- df_preds %>%
  group_by(Jurisdiction) %>%
  summarise(rmse = sqrt(mean((Actual - Predicted)^2))) %>%
  summarise(avg_rmse = mean(rmse)) %>%
  pull(avg_rmse)

print(global_rmse)
print(global_rmse)
print(per_jur_rmse)

#PLOT
titleX = paste0(n_step_ahead, " step ahead forecast using VAR (LASSO) applied to smoothed data, n lags = ", n_lags, ". Rolling weekly forecasts from June 2024. Fit to all prior 2023-2024 data")
