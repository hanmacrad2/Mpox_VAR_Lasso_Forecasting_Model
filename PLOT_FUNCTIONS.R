#****************************
#* PLOT
#*

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

PLOT_MULT_DATA <- function(data_tot_ts, list_ordered_jur, titleX,
                           col_plot = c("black", "blue", "darkgreen",'orange', "red", 'purple')) {
  
  # Format dates
  data_tot_ts <- data_tot_ts %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  # Reshape into long format (true cases only)
  df_true_long <- data_tot_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur))
  
  # Create x-axis breaks
  date_seq <- df_true_long %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_true_long, aes(x = date_week_start, y = Cases, color = Jurisdiction)) +
    geom_line(size = 1.0, alpha = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = setNames(col_plot, list_ordered_jur)  # map colors to jurisdictions
    ) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    labs(
      x = "Date (Date of week start of reported cases)", 
      y = "Cases", 
      title = titleX, 
      color = "Jurisdiction"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 16)
    )
}

#*******************************************
#* ERROR BARS, TWO PLOTS: NAIVE, FORECAST

PLOT_COMPARE_FORECAST_ERRORS <- function(jur_name, df1, df2, data_tot_ts, titleX,
                                         list_rmse, list_mae, 
                                         slope_weighted = TRUE, label_pos_right = TRUE, title_blank = TRUE) {
  
  df1 <- df1 %>% mutate(date_week_start = as.Date(date_week_start))
  df2 <- df2 %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # Metric labels
  if (slope_weighted) {
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
  
  if(title_blank){
    label_metrics = c("", "")
  }
  
  #Title
  if(title_blank){
    titleX = ""
  } 
  
  # True data (duplicated to both horizons)
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True")
  
  df_true_long <- bind_rows(
    df_true %>% mutate(horizon = "var_forecast"),
    df_true %>% mutate(horizon = "naive_estimate_smooth")
  ) %>% mutate(Jurisdiction = jur_name)
  
  # Forecast data
  df_pred_all <- bind_rows(
    df1 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "var_forecast"),
    df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "naive_estimate_smooth")
  ) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Plot frame
  df_plot <- bind_rows(df_true_long, df_pred_all) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      )
    )
  
  # Error bars for ALL points
  df_errorbars <- df_pred_all %>%
    left_join(df_true, by = c("date_week_start", "Week_Number")) %>%
    rename(Predicted = Cases.x, True = Cases.y) %>%
    mutate(
      horizon = fct_recode(
        factor(horizon, levels = c("var_forecast", "naive_estimate_smooth")),
        "VAR forecast on smooth data" = "var_forecast",
        "Naive estimate smooth data" = "naive_estimate_smooth"
      ),
      error_color = ifelse(horizon == "VAR forecast on smooth data", "blue", "red"),
      Error = Predicted - True,
      AbsError = abs(Predicted - True)
    )
  
  # Metric annotations
  annotation_df <- tibble(
    horizon = factor(c("VAR forecast on smooth data", "Naive estimate smooth data"),
                     levels = levels(df_plot$horizon)),
    label = label_metrics
  )
  label_x <- if (label_pos_right) max(df_plot$date_week_start, na.rm = TRUE) else min(df_plot$date_week_start, na.rm = TRUE) + 35
  hjust_val <- if (label_pos_right) 1 else 0
  annotation_coords <- df_plot %>%
    group_by(horizon) %>%
    summarise(x = label_x, y = max(Cases, na.rm = TRUE), .groups = "drop") %>%
    left_join(annotation_df, by = "horizon")
  
  # X breaks
  date_seq <- sort(unique(df_plot$date_week_start))
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (length(date_seq) > 0 && length(x_breaks) > 0 && tail(date_seq,1) != tail(x_breaks,1)) {
    x_breaks <- c(x_breaks, tail(date_seq,1))
  }
  
  ggplot(df_plot, aes(x = date_week_start, y = Cases)) + 
    geom_errorbar(
      data = df_errorbars,
      aes(x = date_week_start, ymin = True, ymax = Predicted, color = error_color), #linetype = error_color
      linetype = "dashed", inherit.aes = FALSE, width = 0.6, size = 1.0, alpha = 0.65
    )  + #+ scale_linetype_identity()
    geom_line(
      aes(color = ifelse(horizon == "VAR forecast on smooth data" & Source == "Predicted", "blue",
                         ifelse(horizon == "Naive estimate smooth data" & Source == "Predicted", "red", "black")),
          group = Source),
      size = 0.9
    ) +
    geom_point(
      aes(color = ifelse(horizon == "VAR forecast on smooth data" & Source == "Predicted", "blue",
                         ifelse(horizon == "Naive estimate smooth data" & Source == "Predicted", "red", "black"))),
      size = 1.75
    ) +
    scale_color_identity() +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~horizon, ncol = 1, scales = "free_y") +
    geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = hjust_val, vjust = 1, size = 4.5
    ) +
    labs(x = "Date (Week start of reported cases)", y = "Cases",
         title = titleX) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12))
}


#***************************
#* ALL ONE PLOT PLOT TRUE, NAIVE, FORECAST
PLOT_COMPARE_FORECAST <- function(jur_name, jur_label, df_var, df_naive, data_tot_ts, titleX,
                                  list_rmse = NULL, list_mae = NULL, 
                                  plot_level = c("Reported", "True+Naive", "True+Naive+VAR"), 
                                  slope_weighted = TRUE, label_pos_right = TRUE, LEGEND_TRUE = TRUE, title_blank = FALSE) {
  
  plot_level <- match.arg(plot_level)
  
  # Ensure date columns are Date
  df_var <- df_var %>% mutate(date_week_start = as.Date(date_week_start))
  df_naive <- df_naive %>% mutate(date_week_start = as.Date(date_week_start))
  data_tot_ts <- data_tot_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # True data
  df_true <- data_tot_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "Reported", Jurisdiction = jur_name)
  
  df_plot <- df_true
  
  # Add Naive if requested
  if (plot_level %in% c("True+Naive", "True+Naive+VAR")) {
    df_naive_jur <- df_naive %>%
      filter(Jurisdiction == jur_name) %>%
      dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted) %>%
      rename(Cases = Predicted) %>%
      mutate(Source = "Naive")
    df_plot <- bind_rows(df_plot, df_naive_jur)
  }
  
  # Add VAR if requested
  if (plot_level == "True+Naive+VAR") {
    df_var_jur <- df_var %>%
      filter(Jurisdiction == jur_name) %>%
      dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted) %>%
      rename(Cases = Predicted) %>%
      mutate(Source = "Model")
    df_plot <- bind_rows(df_plot, df_var_jur)
    titleX = paste0('Reported Mpox Cases, Naive Estimate and VAR prediction for ', jur_label, ', 2024')
    titleX = paste0('                                             ', jur_label)
  }
  
  #Title 
  if (plot_level == "Reported") {
    titleX = paste0('Reported Mpox Cases for ', jur_label, ', 2024')
  } else if (plot_level == "True+Naive"){
    titleX = paste0('Reported Mpox Cases and Naive Estimate for ', jur_label, ', 2024')
  } 
  
  #NO TITLE
  if (title_blank) {
    titleX = ''
  }
  
  # Annotation for metrics if provided
  annotation_coords <- NULL
  if (!is.null(list_rmse) & !is.null(list_mae)) {
    metric_label <- ""
    if (plot_level == "True+Naive") {
      metric_label <- if (slope_weighted) {
        paste0("Slope-weighted RMSE: ", round(list_rmse[2], 2),
               "\nSlope-weighted MAE: ", round(list_mae[2], 2))
      } else {
        paste0("RMSE: ", round(list_rmse[2], 3), "\nMAE: ", round(list_mae[2], 3))
      }
    } else if (plot_level == "True+Naive+VAR") {
      metric_label <- if (slope_weighted) {
        paste0("Slope-weighted RMSE: ", round(list_rmse[1], 2),
               "\nSlope-weighted MAE: ", round(list_mae[1], 2))
      } else {
        paste0("RMSE: ", round(list_rmse[1], 3), "\nMAE: ", round(list_mae[1], 3))
      }
    }
    if (metric_label != "") {
      label_x <- if (label_pos_right) max(df_plot$date_week_start) else min(df_plot$date_week_start) + 35
      hjust_val <- if (label_pos_right) 1 else 0
      annotation_coords <- tibble(
        x = label_x,
        y = max(df_plot$Cases, na.rm = TRUE),
        label = metric_label
      )
    }
  }
  
  # X-axis breaks
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # Colors
  color_vals <- c("Reported" = "black", "Naive" = "red", "Model" = "blue")
  color_vals <- color_vals[names(color_vals) %in% unique(df_plot$Source)]
  
  # Plot
  p <- ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(size = 1.1, aes(group = Source)) +
    geom_point(size = 2.4) +
    scale_color_manual(values = color_vals) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y",
      expand = expansion(mult = c(0.01, 0.06)),
      guide = guide_axis(check.overlap = TRUE)
    ) + labs(
      x = "Date (Week start of reported cases)",
      y = "Weekly Cases",
      title = titleX
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 20)
    )
  
  # Suppress legend if needed
  if (!LEGEND_TRUE) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)
    )
  }
  
  # Add annotations if present
  if (!is.null(annotation_coords)) {
    p <- p + geom_text(
      data = annotation_coords,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = hjust_val, vjust = 1, size = 4.5
    )
  }
  # p = p + theme(
  #   plot.margin = margin(5, 30, 5, 5)  # top, right, bottom, left (in pts)
  # )
  
  return(p)
  
}
  
