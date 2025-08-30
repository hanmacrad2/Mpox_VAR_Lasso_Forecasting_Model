#****************************
#* COEFFICIENTS EXTRACT

#SAVE
FOLDER_COEFF_RESULTS = 'C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/CODE_FINAL/RESULTS/COEFFICIENTS/'

get_df_coeff_jur <- function(forecasts, jur_name){
  
  #Coefficients
  df_coeffs = forecasts$df_coeffs 
  
  #Jurisdiction
  df_coeffs_jur = df_coeffs %>% filter(Response == jur_name)
  df_coeffs_jur <- df_coeffs_jur %>%
    arrange(desc(Coefficient))
  df_coeffs_jur$Coefficient = round(df_coeffs_jur$Coefficient, 2)
  
  return(df_coeffs_jur)
  
}

PLOT_COEFFS <- function(df_coeffs_jur, jur_label){
  
  df_summary <- df_coeffs_jur %>%
    group_by(Predictor) %>%
    summarize(avg_coefficient = mean(Coefficient, na.rm = TRUE)) %>%
    ungroup() %>%
    slice_max(order_by = avg_coefficient, n = 10)  # top 10
  
  # Reorder Predictor factor for plotting
  df_summary <- df_summary %>%
    mutate(Predictor = factor(Predictor, levels = Predictor[order(-avg_coefficient)]))
  
  # Plot
  ggplot(df_summary, aes(x = Predictor, y = avg_coefficient, fill = avg_coefficient)) +
    geom_col() +
    scale_fill_gradientn(
      colors = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
    ) +
    labs(
      title = paste0("Top 10 Predictors (Jurisdictional Lags) for ", jur_label, " (January - November 2024)"),
      x = "Predictor (Jurisdictional Lag)",
      y = "Average Coefficient",
      fill = "Avg Coefficient"
    ) +
    theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  
  
}

#APPLY
forecasts = forecasts_var_2w

#San Diego
jur_label = 'San Diego County'
jur_identifier = 'SanDiego'

#New York City
jur_label = 'New York City'
jur_identifier = 'NewYorkCity'

#Texas
jur_label = 'Texas'
jur_identifier = 'Texas'

#LA County
jur_label = 'LA County'
jur_identifier = 'LA'

jur_label = 'Florida'
jur_identifier = 'Florida'

jur_label = 'Illinois'
jur_identifier = 'Illinois'

jur_label = 'Georgia'
jur_identifier = 'Georgia'

jur_label = 'Washington'
jur_identifier = 'Washington'


#PLOT
df_coeff_jur = get_df_coeff_jur(forecasts, jur_identifier)
PLOT_COEFFS(df_coeff_jur, jur_label)

#Week level


#EXTRACT WEEKS FOR SAN DIEGO
df_jur = df_preds1 %>% filter(Jurisdiction == jur_identifier)
#SAN DIEGO
df_jur = df_jur %>% filter(Week_Number >= 105)
df_jur = df_jur %>% filter(Week_Number <= 120)

#JOIN
df_jur_coeff = df_jur %>%
  left_join(df_coeff_jur, by = "Week_Number")

#PLOT
PLOT_COEFFS(df_jur_coeff, jur_label)


#COEFFICIENTS
df_summary <- df_jur_coeff %>%
  filter(Coefficient > 0) %>% 
  group_by(Week_Number, date_week_start, Jurisdiction, Predicted, Actual, bic_value, error, Response) %>%
  summarise(
    Coefficients = paste0("(", Predictor, ", ", round(Coefficient, 2), ")", collapse = "; "),
    .groups = "drop"
  )

#Add column
df_summary$dates_model_fit = c('01_08_23 - 05_12_24', '01_08_23 - 05_19_24', '01_08_23 - 05_26_24', '01_08_23 - 06_02_24',
                               '01_08_23 - 06_09_24', '01_08_23 - 06_16_24', '01_08_23 - 06_23_24', '01_08_23 - 06_30_24',
                               '01_08_23 - 07_07_24', '01_08_23 - 07_14_24', '01_08_23 - 07_21_24', '01_08_23 - 07_28_24',
                               '01_08_23 - 08_04_24', '01_08_23 - 08_11_24', '01_08_23 - 08_18_24', '01_08_23 - 08_25_24')

#Rename
df_summary<- df_summary %>%
  rename(date_2_week_ahead_forecast = date_week_start)

#Date
df_summary <- df_summary %>%
  mutate(date_2_week_ahead_forecast = format(as.Date(date_2_week_ahead_forecast), "%m/%d/%Y"))

#SELECT
df_summary = df_summary %>% dplyr::select(dates_model_fit, date_2_week_ahead_forecast, Jurisdiction, Predicted, Actual, Coefficients)



#SAVE CSV
write.csv(df_summary, paste0(FOLDER_COEFF_RESULTS, 'table_coeffs_sandiego_2024.csv'))



