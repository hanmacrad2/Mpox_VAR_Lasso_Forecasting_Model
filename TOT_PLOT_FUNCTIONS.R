#****************
#* PLOT DATA

PLOT_DATA <- function(data_tot_ts, df_pred_true_long, list_ordered_jur,
                      titleX, n_col_plot = 2) {
  
  #FORMAT
  data_tot_ts <- data_tot_ts %>%
    mutate(date_week_start = as.Date(date_week_start))
  df_pred_true_long <- df_pred_true_long %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  # Step 1: Format true cases from full dataset
  df_true_long <- data_tot_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # Step 2: Prepare predictions long format (already has date_week_start)
  df_pred_long <- df_pred_true_long %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Step 3: Combine and filter
  df_plot <- bind_rows(df_true_long, df_pred_long) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur)
    )
  
  # Step 4: Use actual dates and select every 4th week for axis ticks
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  # Every 4th date to reduce label clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Make sure last date is included (e.g. November)
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Step 5: Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(data = filter(df_plot, Source == "True"), size = 0.9) +
    geom_point(data = filter(df_plot, Source == "True"), size = 1.75) +
    #geom_line(data = filter(df_plot, Source == "Predicted"), size = 0.6) +
    #geom_point(data = filter(df_plot, Source == "Predicted"), size = 1.75) +
    scale_color_manual(values = c("True" = "black")) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y"
    ) +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
    labs(x = "Date (Date of week start of reported cases)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}



#PRINT OUT PREDICTIONS
print_jur_predictions <- function(jur_name, df2, df3, df4, data_tot_ts, 
                                  TRAIN_WEEK = 105, min_n_ahead = 2) {
  
  print(paste0('2 week ahead:'))
  df2_jur = df2 %>% filter(Jurisdiction == jur_name)
  print(df2_jur$date_week_start)
  print(df2_jur$Week_Number)
  print(df2_jur$Predicted)
  
  print(paste0('3 week ahead:'))
  df3_jur = df3 %>% filter(Jurisdiction == jur_name)
  print(df3_jur$date_week_start)
  print(df3_jur$Week_Number)
  print(df3_jur$Predicted)
  
  print(paste0('4 week ahead:'))
  df4_jur = df4 %>% filter(Jurisdiction == jur_name)
  print(df4_jur$date_week_start)
  print(df4_jur$Week_Number)
  print(df4_jur$Predicted)
  
  data_tot_ts = data_tot_ts %>% filter(Week_Number >= (TRAIN_WEEK + min_n_ahead))
  print(paste0('True Data'))
  print(as.Date(as.vector(as.Date(data_tot_ts$date_week_start))))
  print(as.vector(data_tot_ts$Week_Number)) 
  print(as.vector(data_tot_ts[jur_name]))
}


#PLOT 2, 3, 4 STEP AHEAD FORECASTS
PLOT_JUR_HORIZON_FORECASTS <- function(jur_name, df2, df3, df4, data_tot_ts, titleX = NULL) {
  
  # Ensure all date columns are Date class
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  df3 <- df3 %>% mutate(date_week_start = as.Date(date_week_start))
  df4 <- df4 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  #PRINT OUT PREDICTIONS
  print_jur_predictions(jur_name, df2, df3, df4, data_tot_ts)
  
  # TRUE DATA (repeated across all horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data across all horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "2_week_ahead"),
    df_true %>% mutate(horizon = "3_week_ahead"),
    df_true %>% mutate(horizon = "4_week_ahead")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine and label forecasts
  df_pred_all <- bind_rows(
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "2_week_ahead"),
    df3 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "3_week_ahead"),
    df4 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "4_week_ahead")
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Combine true + predicted data
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("2_week_ahead", "3_week_ahead", "4_week_ahead")),
        "2 week ahead forecast" = "2_week_ahead",
        "3 week ahead forecast" = "3_week_ahead",
        "4 week ahead forecast" = "4_week_ahead"
      )
    )
  
  # x-axis tick control
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


#PLOT
PLOT_COMPARE_FORECAST_NAIVE <- function(jur_name, df1, df2, df3, data_tot_ts, titleX,
                                        list_rmse, list_mae) {
  
  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  df3 <- df3 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # PRINT OUT PREDICTIONS
  print_jur_predictions(jur_name, df1, df2, df3, data_tot_ts)
  
  # TRUE DATA (repeated across all horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data across all horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth"),
    df_true %>% mutate(horizon = "naive_estimate_unsmooth")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine and label forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth"),
    df3 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_unsmooth")
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Combine true + predicted data
  # df_plot <- bind_rows(df_true_long, df_pred_all) %>%
  #   mutate(
  #     Source = factor(Source, levels = c("True", "Predicted")),
  #     horizon = fct_recode(
  #       factor(horizon, levels = c("var_forecast", "naive_estimate_smooth", "naive_estimate_unsmooth")),
  #       "VAR forecast" = "var_forecast",
  #       "Naive estimate smooth data" = "naive_estimate_smooth",
  #       "Naive estimate un-smooth data" = "naive_estimate_unsmooth"
  #       
  #     ))
  # 
  # 
  # df_plot <- bind_rows(df_true_long, df_pred_all) %>%
  #   mutate(
  #     Source = factor(Source, levels = c("True", "Predicted")),
  #     horizon = factor(horizon, levels = c("var_forecast", "naive_estimate_smooth", "naive_estimate_unsmooth"))
  #   )
  # 
  # # Define annotation text (RMSE/MAE) per horizon
  # annotation_df <- tibble(
  #   horizon = factor(c("var_forecast", "naive_estimate_smooth", "naive_estimate_unsmooth"),
  #                    levels = c("var_forecast", "naive_estimate_smooth", "naive_estimate_unsmooth")),
  #   label = c(
  #     paste0("RMSE: ", round(list_rmse[1], 2), "\nMAE: ", round(list_mae[1], 2)),
  #     paste0("RMSE: ", round(list_rmse[2], 2), "\nMAE: ", round(list_mae[2], 2)),
  #     paste0("RMSE: ", round(list_rmse[3], 2), "\nMAE: ", round(list_mae[3], 2))
  #   )
  # )

  # Step 1: recode horizon factor with pretty labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth", "naive_estimate_unsmooth")),
        "VAR forecast" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth",
        "Naive estimate un-smooth data" = "naive_estimate_unsmooth"
      )
    )
  
  # Step 2: define annotation_df using the same recoded labels exactly
  annotation_df <- tibble(
    horizon = factor(
      c("VAR forecast", "Naive estimate smooth data", "Naive estimate un-smooth data"),
      levels = levels(df_plot$horizon)  # keep factor levels consistent
    ),
    label = c(
      paste0("RMSE: ", round(list_rmse[1], 2), "\nMAE: ", round(list_mae[1], 2)),
      paste0("RMSE: ", round(list_rmse[2], 2), "\nMAE: ", round(list_mae[2], 2)),
      paste0("RMSE: ", round(list_rmse[3], 2), "\nMAE: ", round(list_mae[3], 2))
    )
  )
  
  
  # Calculate y-position and x-position for top-right corner annotation
  max_x = max(max(df1$date_week_start, df2$date_week_start)) #, df3$date_week_start))
              
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(
      x = max_x, #max(date_week_start, na.rm = TRUE),
      y = max(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(annotation_df, by = "horizon")
  
  # x-axis tick control
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}

PLOT_COMPARE_FORECAST_NAIVE_X4 <- function(jur_name, df1, df2, df3, df4, data_tot_ts, titleX,
                                        list_rmse, list_mae) {
  
  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  df3 <- df3 %>% mutate(date_week_start = as.Date(date_week_start))
  df4 <- df4 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # PRINT OUT PREDICTIONS
  #print_jur_predictions(jur_name, df1, df2, df3, df4, data_tot_ts)
  
  # TRUE DATA (repeated across all horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data across all horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth"),
    df_true %>% mutate(horizon = "var_forecast_unsmooth"),
    df_true %>% mutate(horizon = "naive_estimate_unsmooth")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine and label forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth"),
    df3 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast_unsmooth"),
    df4 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_unsmooth")
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Step 1: recode horizon factor with pretty labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth", "var_forecast_unsmooth", "naive_estimate_unsmooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth",
        "VAR forecast on un-smooth data" = "var_forecast_unsmooth",
        "Naive estimate un-smooth data" = "naive_estimate_unsmooth"
      )
    )
  
  # Step 2: define annotation_df using the same recoded labels exactly
  annotation_df <- tibble(
    horizon = factor(
      c("VAR forecast on smooth data", "Naive estimate smooth data", "VAR forecast on un-smooth data", "Naive estimate un-smooth data"),
      levels = levels(df_plot$horizon)  # keep factor levels consistent
    ),
    label = c(
      paste0("RMSE: ", round(list_rmse[1], 2), "\nMAE: ", round(list_mae[1], 2)),
      paste0("RMSE: ", round(list_rmse[2], 2), "\nMAE: ", round(list_mae[2], 2)),
      paste0("RMSE: ", round(list_rmse[3], 2), "\nMAE: ", round(list_mae[3], 2)),
      paste0("RMSE: ", round(list_rmse[4], 2), "\nMAE: ", round(list_mae[4], 2))
    )
  )
  
  
  # Calculate y-position and x-position for top-right corner annotation
  max_x = max(max(df1$date_week_start, df2$date_week_start)) #, df3$date_week_start))
  
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(
      x = max_x, #max(date_week_start, na.rm = TRUE),
      y = max(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(annotation_df, by = "horizon")
  
  # x-axis tick control
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  # 
  # x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  # if (tail(date_seq, 1) != tail(x_breaks, 1)) {
  #   x_breaks <- c(x_breaks, tail(date_seq, 1))
  # }
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Only proceed if both tails are non-NA
  if (length(date_seq) > 0 && length(x_breaks) > 0 && !is.na(tail(date_seq, 1)) && !is.na(tail(x_breaks, 1))) {
    if (tail(date_seq, 1) != tail(x_breaks, 1)) {
      x_breaks <- c(x_breaks, tail(date_seq, 1))
    }
  }
  
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}

PLOT_COMPARE_FORECAST_NAIVE_X2 <- function(jur_name, df1, df2, data_tot_ts, titleX,
                                           list_rmse, list_mae, slope_weighted = TRUE, label_pos_right = TRUE) {
  
  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  #LABEL METRICS
  if(slope_weighted) {
    label_metrics = c(paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2), "\nSlope-weighted MAE: ", round(list_mae[1], 2)),
                            paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2), "\nSlope-weighted MAE: ", round(list_mae[2], 2)))
  } else {
    label_metrics = c(paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3)),
      paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3)))
  }
  
  # TRUE DATA (repeated across all horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data across all horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine and label forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth"),
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Step 1: recode horizon factor with pretty labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  # Step 2: define annotation_df using the same recoded labels exactly
  annotation_df <- tibble(
    horizon = factor(
      c("VAR forecast on smooth data", "Naive estimate smooth data"),
      levels = levels(df_plot$horizon)  # keep factor levels consistent
    ),
    label = label_metrics)
  
  #LABEL POSITION
  if (label_pos_right) {
    print(paste0('label_pos_right: ', label_pos_right))
    label_x <- max(df_plot$date_week_start, na.rm = TRUE)
    hjust_val <- 1
  } else {
    label_x <- min(df_plot$date_week_start, na.rm = TRUE) + 35
    hjust_val <- 0
  } 
  
  # Calculate y-position and x-position for top-right corner annotation
  max_x = max(max(df1$date_week_start, df2$date_week_start)) #, df3$date_week_start))
  
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(
      x = label_x, #max(date_week_start, na.rm = TRUE),
      y = max(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(annotation_df, by = "horizon")
  
  # x-axis tick control
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)

  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Only proceed if both tails are non-NA
  if (length(date_seq) > 0 && length(x_breaks) > 0 && !is.na(tail(date_seq, 1)) && !is.na(tail(x_breaks, 1))) {
    if (tail(date_seq, 1) != tail(x_breaks, 1)) {
      x_breaks <- c(x_breaks, tail(date_seq, 1))
    }
  }
  
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


#PLOT
PLOT_TRUE_WITH_INCREASE <- function(data, titleX, col_plot = 'cyan', n_col_plot = 3) {
  
  data <- data %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  # Identify consecutive increasing_plot blocks for proper line grouping
  data <- data %>%
    group_by(Jurisdiction) %>%
    mutate(block_id = cumsum((increasing_plot == 1 & lag(increasing_plot, default = 0) == 0))) %>%
    ungroup()
  
  # Red subset for points and lines
  red_data <- data %>% filter(increasing_plot == 1)
  
  # X-axis breaks (every 4 weeks)
  date_seq <- data %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(data, aes(x = date_week_start, y = Cases)) +
    geom_line(size = 0.9, color = "black") +
    geom_point(size = 1.8, color = "black") +
    geom_point(data = red_data, aes(x = date_week_start, y = Cases), 
               color = col_plot, size = 2.8, shape = 16) +
    geom_line(data = red_data, 
              aes(group = interaction(Jurisdiction, block_id)), 
              color = col_plot, size = 1) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~Jurisdiction, scales = "free_y", ncol = n_col_plot) +
    labs(x = "Date (week start)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


#PLOT
PLOT_TRUE_WITH_INCREASE <- function(data, titleX, list_jur, col_plot = 'cyan', n_col_plot = 3) {
  
  data <- data %>%
    mutate(date_week_start = as.Date(date_week_start),
           Jurisdiction = factor(Jurisdiction, levels = list_jur))  # <-- custom order
  
  # Identify consecutive increasing_plot blocks for proper line grouping
  data <- data %>%
    group_by(Jurisdiction) %>%
    mutate(block_id = cumsum((increasing_plot == 1 & lag(increasing_plot, default = 0) == 0))) %>%
    ungroup()
  
  # Red subset for points and lines
  red_data <- data %>% filter(increasing_plot == 1)
  
  # X-axis breaks (every 4 weeks)
  date_seq <- data %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(data, aes(x = date_week_start, y = Cases)) +
    geom_line(size = 0.9, color = "black") +
    geom_point(size = 1.8, color = "black") +
    geom_point(data = red_data, aes(x = date_week_start, y = Cases), 
               color = col_plot, size = 2.8, shape = 16) +
    geom_line(data = red_data, 
              aes(group = interaction(Jurisdiction, block_id)), 
              color = col_plot, size = 1) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~Jurisdiction, scales = "free_y", ncol = n_col_plot) +
    labs(x = "Date (week start)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}



PLOT_FORECAST_PERIODS_INCREASE_X2 <- function(jur_name, df1, df2, data_tot_ts, df_detect_smooth, 
                                              titleX, list_rmse, list_mae, 
                                              col_raw = "black", col_pred = "red", 
                                              col_smooth = "skyblue", col_detect = "blue") {
  
  # Ensure Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_detect_smooth <- df_detect_smooth %>% mutate(date_week_start = as.Date(date_week_start))
  
  # TRUE DATA (raw)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Raw_Cases = all_of(jur_name))
  
  # Smoothed data for the same jurisdiction
  df_smooth <- df_detect_smooth %>%
    filter(Jurisdiction == jur_name) %>%
    dplyr::select(date_week_start, Smooth_Cases = Cases, increasing_plot) %>%
    mutate(block_id = cumsum((increasing_plot == 1 & lag(increasing_plot, default = 0) == 0)))
  
  # Merge raw + smoothed
  df_merged <- full_join(df_true, df_smooth, by = "date_week_start")
  
  # Forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    dplyr::select(date_week_start, Predicted = Predicted, horizon)
  
  # Duplicate merged raw/smoothed for both horizons
  df_long <- df_merged %>%
    crossing(horizon = c("var_forecast", "naive_estimate_smooth")) %>%
    left_join(df_pred_all, by = c("date_week_start", "horizon"))
  
  # ✅ Apply recoding & order fix HERE
  df_long <- df_long %>%
    mutate(
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  # Prepare annotation
  annotation_df <- tibble(
    horizon = factor(c("VAR forecast on smooth data", "Naive estimate smooth data"),
                     levels = levels(df_long$horizon)),
    label = c(
      paste0("RMSE: ", round(list_rmse[1], 2), "\nMAE: ", round(list_mae[1], 2)),
      paste0("RMSE: ", round(list_rmse[2], 2), "\nMAE: ", round(list_mae[2], 2))
    )
  )
  
  max_x <- max(df_long$date_week_start, na.rm = TRUE)
  annotation_coords <- df_long %>%
    group_by(horizon) %>%
    summarise(x = max_x,
              y = max(c(Raw_Cases, Smooth_Cases, Predicted), na.rm = TRUE), .groups = "drop") %>%
    left_join(annotation_df, by = "horizon")
  
  # X breaks
  date_seq <- df_long %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # Plot
  ggplot(df_long, aes(x = date_week_start)) +
    geom_line(aes(y = Raw_Cases, color = "Raw"), size = 1) +
    geom_point(aes(y = Raw_Cases, color = "Raw"), size = 1.8) +
    geom_line(aes(y = Smooth_Cases, color = "Smoothed"), size = 1, alpha = 0.8) +
    geom_point(aes(y = Smooth_Cases, color = "Smoothed"), size = 2.2) +
    geom_line(aes(y = Predicted, color = "Forecast"), size = 1.2) +
    geom_point(aes(y = Predicted, color = "Forecast"), size = 1.8) +
    geom_line(data = df_long %>% filter(increasing_plot == 1),
              aes(x = date_week_start, y = Smooth_Cases, group = interaction(block_id, horizon), color = "Period of Increase"),
              inherit.aes = FALSE, size = 2.0) +
    geom_point(data = df_long %>% filter(increasing_plot == 1),
               aes(x = date_week_start, y = Smooth_Cases, color = "Period of Increase"),
               inherit.aes = FALSE, size = 3) +
    scale_color_manual(
      name = "Data Type",
      values = c("Raw" = col_raw, "Smoothed" = col_smooth, "Forecast" = col_pred, "Period of Increase" = col_detect)
    ) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(data = annotation_coords,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE, hjust = 1, vjust = 1, size = 4.5) +
    labs(x = "Date (week start)", y = "Cases",
         title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


#*****************************
#*PLOT_ALARMS_FORECASTS
#TAKE 5
PLOT_ALARMS_FORECASTS <- function(jur_name, df1, df2, data_tot_ts, farr_alarms,
                                  titleX = NULL, list_rmse, list_mae, date_start = "2024-02-15",
                                  col_raw = "black", col_pred = "red",
                                  col_alarm = 'blue',
                                  highlight_alarm_bg = FALSE) {
  
  # Ensure Date columns are Date type
  df1$date_week_start <- as.Date(df1$date_week_start)
  df2$date_week_start <- as.Date(df2$date_week_start)
  data_tot_ts$date_week_start <- as.Date(data_tot_ts$date_week_start)
  farr_alarms$date_week_start <- as.Date(farr_alarms$date_week_start)
  
  #FILTER
  df1 = df1 %>% filter(date_week_start >=  date_start)
  df2 = df2 %>% filter(date_week_start >=  date_start)
  
  # Extract observed counts for this jurisdiction
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Raw_Cases = all_of(jur_name))
  
  # Align dates for matching alarms
  farr_alarms$date_week_start <- lubridate::floor_date(farr_alarms$date_week_start, unit = "week")
  
  # Join alarms with observed data (keep all rows)
  df_with_alarms <- df_true %>%
    left_join(farr_alarms, by = "date_week_start")
  
  # Points where alarms == TRUE
  alarm_points <- df_with_alarms %>% filter(!is.na(alarms) & alarms == TRUE)
  
  # Combine forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    dplyr::select(date_week_start, Predicted, horizon)
  
  # Combine observed data and forecasts for plotting
  df_long <- df_true %>%
    crossing(horizon = c("var_forecast", "naive_estimate_smooth")) %>%
    left_join(df_pred_all, by = c("date_week_start", "horizon"))
  
  # Recode horizon to descriptive labels and create explicit ordering
  df_long <- df_long %>%
    mutate(
      horizon_label = fct_recode(horizon,
                                 "VAR forecast on smooth data" = "var_forecast",
                                 "Naive estimate smooth data" = "naive_estimate_smooth"),
      horizon_order = case_when(
        horizon_label == "VAR forecast on smooth data" ~ 1,
        horizon_label == "Naive estimate smooth data" ~ 2,
        TRUE ~ 99
      )
    )
  
  # Set horizon_label factor levels for plotting
  df_long$horizon_label <- factor(df_long$horizon_label,
                                  levels = c("VAR forecast on smooth data",
                                             "Naive estimate smooth data"))
  
  # Prepare annotation dataframe with horizon_order to match facets
  annotation_df <- tibble(
    horizon_label = levels(df_long$horizon_label),
    horizon_order = c(1, 2),
    label = c(
      paste0("RMSE: ", round(list_rmse[1], 2), "\nMAE: ", round(list_mae[1], 2)),
      paste0("RMSE: ", round(list_rmse[2], 2), "\nMAE: ", round(list_mae[2], 2))
    )
  )
  
  max_x <- max(df_long$date_week_start, na.rm = TRUE)
  
  # Calculate annotation coordinates using horizon_order (to match facet var)
  annotation_coords <- df_long %>%
    group_by(horizon_order) %>%
    summarise(
      x = max_x,
      y = max(coalesce(Raw_Cases, 0), coalesce(Predicted, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(annotation_df, by = "horizon_order") %>%
    distinct(horizon_order, .keep_all = TRUE)
  
  # X-axis breaks every 4 weeks
  date_seq <- sort(unique(df_long$date_week_start))
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  p <- ggplot(df_long, aes(x = date_week_start)) +
    geom_line(aes(y = Raw_Cases, color = "Raw"), size = 1) +
    geom_point(aes(y = Raw_Cases, color = "Raw"), size = 1.8) +
    geom_line(aes(y = Predicted, color = "Forecast"), size = 1.2) +
    geom_point(aes(y = Predicted, color = "Forecast"), size = 1.8) +
    
    # Alarm points
    
    geom_point(data = alarm_points,
               aes(x = date_week_start, y = Raw_Cases, color = "Alarm"),
               size = 4, shape = 19) +
  
    # geom_point(data = alarm_points, aes(x = date_week_start, y = Raw_Cases),
    #            color = col_alarm, size = 4, shape = 19) +
    
    # Optional alarm week shaded background
    { if (highlight_alarm_bg) geom_rect(
      data = farr_alarms %>% filter(alarms == TRUE),
      aes(xmin = date_week_start, xmax = date_week_start + 7,
          ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "pink", alpha = 0.2
    ) } +
    
    scale_color_manual(name = "Data Type",
                       values = c("Raw" = col_raw,
                                  "Forecast" = col_pred,
                                  "Alarm" = col_alarm)) +
    # scale_color_manual(name = "Data Type",
    #                    values = c("Raw" = col_raw, "Forecast" = col_pred)) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    
    # Facet by horizon_order (numeric) but label with descriptive names
    facet_wrap(~horizon_order, ncol = 1, scales = "free_y",
               labeller = as_labeller(c(`1` = "VAR forecast on smooth data",
                                        `2` = "Naive estimate smooth data"))) +
    
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = 1, vjust = 1, size = 4.5) +
    
    labs(x = "Date (week start)", y = "Cases",
         title = ifelse(is.null(titleX),
                        paste0("Forecast horizons for ", jur_name),
                        titleX)) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
  
  return(p)
}


#PLOT + SLOPE
PLOT_COMPARE_FORECAST_SLOPE1 <- function(jur_name, df1, df2, data_tot_ts, df_slope, titleX,
                                        list_rmse, list_mae, slope_weighted = TRUE, label_pos_right = TRUE) {

  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # LABEL METRICS
  if (slope_weighted) {
    label_metrics = c(
      paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2), "\nSlope-weighted MAE: ", round(list_mae[1], 2)),
      paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2), "\nSlope-weighted MAE: ", round(list_mae[2], 2))
    )
  } else {
    label_metrics = c(
      paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3)),
      paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3))
    )
  }
  
  # TRUE DATA
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data for both horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine predictions
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth"),
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Factor labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  annotation_df <- tibble(
    horizon = factor(
      c("VAR forecast on smooth data", "Naive estimate smooth data"),
      levels = levels(df_plot$horizon)
    ),
    label = label_metrics
  )
  
  # Label positioning
  if (label_pos_right) {
    label_x <- max(df_plot$date_week_start, na.rm = TRUE)
    hjust_val <- 1
  } else {
    label_x <- min(df_plot$date_week_start, na.rm = TRUE) + 35
    hjust_val <- 0
  }
  
  # Highlight positive slope periods
  df_highlight <- df_plot %>%
    filter(Source == "True") %>%
    left_join(df_slope, by = c("Jurisdiction", "date_week_start")) %>%
    filter(delta_yt > 0) %>%
    mutate(alpha_level = scales::rescale(delta_yt, to = c(0.15, 0.5)))
  
  df_highlight_box <- df_highlight %>%
  group_by(horizon) %>%
  mutate(
    ymin = min(df_plot$Cases[df_plot$horizon == horizon], na.rm = TRUE),
    ymax = max(df_plot$Cases[df_plot$horizon == horizon], na.rm = TRUE)
  )
  
  # VAR-better flag
  df_errors <- df_plot %>%
    filter(Source == "Predicted") %>%
    left_join(df_true, by = c("date_week_start", "Week_Number"), suffix = c("_pred", "_true")) %>%
    rename(Pred = Cases_pred, TrueVal = Cases_true) %>%
    left_join(df_slope, by = c("Jurisdiction", "date_week_start")) %>%
    filter(delta_yt > 0) %>%
    group_by(date_week_start) %>%
    summarise(
      var_err = abs(Pred[horizon == "VAR forecast on smooth data"] - TrueVal[horizon == "VAR forecast on smooth data"]),
      naive_err = abs(Pred[horizon == "Naive estimate smooth data"] - TrueVal[horizon == "Naive estimate smooth data"]),
      horizon = "VAR forecast on smooth data",
      Jurisdiction = jur_name,
      .groups = "drop"
    ) %>%
    filter(var_err < naive_err)
  
  # Annotation coordinates
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(
      x = label_x,
      y = max(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(annotation_df, by = "horizon")
  
  # X-axis breaks
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_rect(
      data = df_highlight_box,
      aes(xmin = date_week_start - 3, xmax = date_week_start + 3,
          ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = NA,
      color = "orange",
      linewidth = 1) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    geom_text(
      data = df_errors,
      aes(x = date_week_start, y = max(df_plot$Cases, na.rm = TRUE) * 0.95,
          label = "VAR better"),
      color = "darkgreen", fontface = "bold", size = 3, inherit.aes = FALSE
    ) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_alpha_identity() +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = hjust_val, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


#PLOT
PLOT_COMPARE_FORECAST_SLOPE <- function(jur_name, df1, df2, data_tot_ts, df_slope, titleX,
                                        list_rmse, list_mae, slope_weighted = TRUE, label_pos_right = TRUE) {
  
  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_slope <- df_slope %>% mutate(date_week_start = as.Date(date_week_start))
  df_slope = df_slope %>% filter(Jurisdiction == jur_name)
  
  # Metric labels
  if(slope_weighted) {
    label_metrics <- c(
      paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2), "\nSlope-weighted MAE: ", round(list_mae[1], 2)),
      paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2), "\nSlope-weighted MAE: ", round(list_mae[2], 2))
    )
  } else {
    label_metrics <- c(
      paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3)),
      paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3))
    )
  }
  
  # TRUE DATA duplicated across horizons
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth")
  ) %>% mutate(Jurisdiction = jur_name)
  
  # Combine forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Combine TRUE + PRED, factor levels, nice labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  # Keep VAR on top
  df_plot$horizon <- factor(df_plot$horizon, levels = c("VAR forecast on smooth data", "Naive estimate smooth data"))
  
  # Annotation dataframe
  annotation_df <- tibble(
    horizon = factor(c("VAR forecast on smooth data", "Naive estimate smooth data"), levels = levels(df_plot$horizon)),
    label = label_metrics
  )
  
  # Label position
  label_x <- if(label_pos_right) max(df_plot$date_week_start, na.rm = TRUE) else min(df_plot$date_week_start, na.rm = TRUE) + 35
  hjust_val <- if(label_pos_right) 1 else 0
  
  # Highlight positive slope periods
  df_highlight <- df_plot %>%
    filter(Source == "True") %>%
    left_join(df_slope, by = c("Jurisdiction", "date_week_start")) %>%
    filter(delta_yt > 0) %>%
    mutate(alpha_level = scales::rescale(delta_yt, to = c(0.15, 0.5)))
  
  # Boxes for highlights (smaller height)
  df_highlight_box <- df_highlight %>%
    group_by(horizon) %>%
    mutate(
      ymin = min(Cases, na.rm = TRUE) + 0.05 * diff(range(Cases, na.rm = TRUE)),
      ymax = max(Cases, na.rm = TRUE) - 0.05 * diff(range(Cases, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Error bars for positive slopes
  df_errorbars <- df_pred_all %>%
    left_join(df_true, by = c("date_week_start", "Week_Number")) %>%
    left_join(df_slope, by = c("Jurisdiction", "date_week_start")) %>%
    filter(delta_yt > 0) %>%
    rename(Predicted = Cases.x, True = Cases.y)
  # horizon column is already in df_pred_all, so the error bars will facet correctly
  
  df_errorbars <- df_errorbars %>%
    mutate(
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  df_errorbars <- df_errorbars %>%
    mutate(
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  df_errorbars <- df_pred_all %>%
    left_join(df_true, by = c("date_week_start", "Week_Number")) %>%
    left_join(df_slope, by = c("Jurisdiction", "date_week_start")) %>%
    filter(delta_yt > 0) %>%
    rename(Predicted = Cases.x, True = Cases.y) %>%

    filter(!is.na(horizon)) %>%
    mutate(
      horizon = factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
      horizon = fct_recode(
        horizon,
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  
  # Coordinates for metrics
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(x = label_x, y = max(Cases, na.rm = TRUE), .groups = "drop") %>%
    left_join(annotation_df, by = "horizon")
  
  # X-axis breaks
  date_seq <- sort(unique(df_plot$date_week_start))
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if(tail(date_seq,1) != tail(x_breaks,1)) x_breaks <- c(x_breaks, tail(date_seq,1))
  
  # Final plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_rect(
      data = df_highlight_box,
      aes(xmin = date_week_start - 3, xmax = date_week_start + 3, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = NA, color = "orange", linewidth = 1
    ) +
    geom_text(
      data = df_highlight_box,
      aes(x = date_week_start, y = ymax + 0.05 * (ymax - ymin), label = round(delta_yt, 1)),
      inherit.aes = FALSE, color = "orange", size = 3
    ) +
    geom_errorbar(
      data = df_errorbars,
      aes(x = date_week_start, ymin = True, ymax = Predicted),
      inherit.aes = FALSE, color = "blue", width = 0.6, size = 1.2
    ) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = hjust_val, vjust = 1, size = 4.5
    ) +
    labs(x = "Date (week start)", y = "Cases",
         title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12))
  
}

  #PLOT ERRORS ONLY
  # plot_orange = TRUE
  # if(!plot_orange){
  #   
  #   ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
  #     # geom_rect(
  #     #   data = df_highlight_box,
  #     #   aes(xmin = date_week_start - 3, xmax = date_week_start + 3, ymin = ymin, ymax = ymax),
  #     #   inherit.aes = FALSE, fill = NA, color = "orange", linewidth = 1
  #     # ) +
  #     # geom_text(
  #     #   data = df_highlight_box,
  #     #   aes(x = date_week_start, y = ymax + 0.05 * (ymax - ymin), label = round(delta_yt, 1)),
  #     #   inherit.aes = FALSE, color = "orange", size = 3
  #     # ) +
  #     geom_errorbar(
  #       data = df_errorbars,
  #       aes(x = date_week_start, ymin = True, ymax = Predicted),
  #       inherit.aes = FALSE, color = "blue", width = 0.6, size = 1.2
  #     ) +
  #     geom_line(size = 0.9, aes(group = Source)) +
  #     geom_point(size = 1.75) +
  #     scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
  #     scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
  #     facet_wrap(~horizon, ncol = 1, scales = "free_y") +
  #     geom_text(
  #       data = annotation_coords,
  #       aes(x = x, y = y, label = label),
  #       inherit.aes = FALSE, hjust = hjust_val, vjust = 1, size = 4.5
  #     ) +
  #     labs(x = "Date (week start)", y = "Cases",
  #          title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)) +
  #     theme_minimal(base_size = 13) +
  #     theme(legend.position = "bottom",
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           strip.text = element_text(size = 12))
  #   
  # }

  
  


#PLOT FORECAST
PLOT_COMPARE_FORECAST <- function(jur_name, df1, df2, data_tot_ts, titleX,
                                           list_rmse, list_mae, slope_weighted = TRUE, label_pos_right = TRUE) {
  
  # Ensure all date columns are Date class
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  #LABEL METRICS
  if(slope_weighted) {
    label_metrics = c(paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2), "\nSlope-weighted MAE: ", round(list_mae[1], 2)),
                      paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2), "\nSlope-weighted MAE: ", round(list_mae[2], 2)))
  } else {
    label_metrics = c(paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3)),
                      paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3)))
  }
  
  # TRUE DATA (repeated across all horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  # Duplicate true data across all horizons
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    mutate(Jurisdiction = jur_name)
  
  # Combine and label forecasts
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth"),
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Step 1: recode horizon factor with pretty labels
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  # Step 2: define annotation_df using the same recoded labels exactly
  annotation_df <- tibble(
    horizon = factor(
      c("VAR forecast on smooth data", "Naive estimate smooth data"),
      levels = levels(df_plot$horizon)  # keep factor levels consistent
    ),
    label = label_metrics)
  
  #LABEL POSITION
  if (label_pos_right) {
    print(paste0('label_pos_right: ', label_pos_right))
    label_x <- max(df_plot$date_week_start, na.rm = TRUE)
    hjust_val <- 1
  } else {
    label_x <- min(df_plot$date_week_start, na.rm = TRUE) + 35
    hjust_val <- 0
  } 
  
  # Calculate y-position and x-position for top-right corner annotation
  max_x = max(max(df1$date_week_start, df2$date_week_start)) #, df3$date_week_start))
  
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(
      x = label_x, #max(date_week_start, na.rm = TRUE),
      y = max(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(annotation_df, by = "horizon")
  
  # x-axis tick control
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Only proceed if both tails are non-NA
  if (length(date_seq) > 0 && length(x_breaks) > 0 && !is.na(tail(date_seq, 1)) && !is.na(tail(x_breaks, 1))) {
    if (tail(date_seq, 1) != tail(x_breaks, 1)) {
      x_breaks <- c(x_breaks, tail(date_seq, 1))
    }
  }
  
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast horizons for ", jur_name), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}

#PLOT
PLOT_COMPARE_FORECAST <- function(jur_name, 
                                  df_var, 
                                  df_naive, 
                                  data_tot_ts, 
                                  titleX,
                                  list_rmse, 
                                  list_mae, 
                                  forecast_type = c("VAR", "Naive"), 
                                  slope_weighted = TRUE, 
                                  label_pos_right = TRUE) {
  
  forecast_type <- match.arg(forecast_type)
  
  # Ensure date columns are Date
  df_var   <- df_var   %>% mutate(date_week_start = as.Date(date_week_start))
  df_naive <- df_naive %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # Choose the forecast & metrics
  if (forecast_type == "VAR") {
    df_forecast <- df_var %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast")
    metric_label <- if (slope_weighted) {
      paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2), "\nSlope-weighted MAE: ", round(list_mae[1], 2))
    } else {
      paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3))
    }
    horizon_label <- "VAR forecast on smooth data"
  } else {
    df_forecast <- df_naive %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth")
    metric_label <- if (slope_weighted) {
      paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2), "\nSlope-weighted MAE: ", round(list_mae[2], 2))
    } else {
      paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3))
    }
    horizon_label <- "Naive estimate smooth data"
  }
  
  # True data
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True", horizon = unique(df_forecast$horizon), Jurisdiction = jur_name)
  
  # Predictions
  df_pred <- df_forecast %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Combine
  df_plot <- bind_rows(df_true, df_pred) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = factor(horizon_label, levels = horizon_label)
    )
  
  # Annotation
  if (label_pos_right) {
    label_x <- max(df_plot$date_week_start, na.rm = TRUE)
    hjust_val <- 1
  } else {
    label_x <- min(df_plot$date_week_start, na.rm = TRUE) + 35
    hjust_val <- 0
  }
  
  annotation_coords <- tibble(
    horizon = factor(horizon_label, levels = horizon_label),
    x = label_x,
    y = max(df_plot$Cases, na.rm = TRUE),
    label = metric_label
  )
  
  # X-axis breaks
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 0.9, aes(group = Source)) +
    geom_point(size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = hjust_val, vjust = 1, size = 4.5
    ) +
    labs(
      x = "Date (week start)",
      y = "Cases",
      title = ifelse(is.null(titleX), paste0("Forecast for ", jur_name, " (", forecast_type, ")"), titleX)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}



