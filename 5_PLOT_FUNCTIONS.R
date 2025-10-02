#*******************************
#* PLOT FORECASTS
#******************************
library(patchwork)

PLOT_DATES_TRUE_FORECAST <- function(data_tot_ts, df_pred_true_long, list_ordered_jur,
                                     titleX, n_col_plot = 2, predicted_col = "red") {
  
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
    geom_line(data = filter(df_plot, Source == "Predicted"), size = 0.6) +
    geom_point(data = filter(df_plot, Source == "Predicted"), size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = predicted_col)) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y"
    ) +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
    labs(x = "Date (week start date of reported cases)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}

PLOT_DATES_TRUE_FORECAST_SD <- function(data_sd, titleX) {
  
  # Ensure Date column exists as date_week_start or week_start_date (rename if needed)
  # Assume data_sd has a column named 'date_week_start' for actual dates

  data_sd <- data_sd %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  data_sd <- data_sd %>%
    rename(Date = date_week_start) %>%
    dplyr::select(Week_Number, Date, Predicted, Actual) %>%
    pivot_longer(cols = c("Predicted", "Actual"), 
                 names_to = "Source", 
                 values_to = "Cases")
  
  # Get all unique dates in ascending order
  date_seq <- data_sd %>%
    distinct(Date) %>%
    arrange(Date) %>%
    pull(Date)
  
  # Pick every 4th date to reduce clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Ensure last date included
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(data_sd, aes(x = Date, y = Cases, color = Source)) +
    geom_line(size = 0.7) +
    geom_point(size = 3.0) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    scale_y_continuous(
      breaks = 0:10,
      limits = c(0, 10),
      expand = expansion(mult = c(0.02, 0))
    ) + 
    labs(x = "Date (week start date of reported cases)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_line(),
      axis.ticks.length = unit(5, "pt")
    )
}

#*************************************************
#* PLOT JURISDICTION LEVEL 
#*************************************************

PLOT_JUR_HORIZON_FORECASTS_V1 <- function(jur_name, df2, df3, df4, data_ts) {
  # Add forecast horizon labels
  df_preds2 <- df2 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "2_week_ahead")
  df_preds3 <- df3 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "3_week_ahead")
  df_preds4 <- df4 %>% filter(Jurisdiction == jur_name) %>% mutate(horizon = "4_week_ahead")
  
  # Combine predictions
  df_preds_all <- bind_rows(df_preds2, df_preds3, df_preds4) %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  # Format actual data
  df_true <- data_ts %>%
    dplyr::select(Week_Number, date_week_start, all_of(jur_name)) %>%
    rename(Cases = all_of(jur_name)) %>%
    mutate(Source = "True",
           Jurisdiction = jur_name, 
           date_week_start = as.Date(date_week_start))  # Add Jurisdiction for compatibility
  
  # Format predictions
  df_preds_long <- df_preds_all %>%
    dplyr::select(Week_Number, date_week_start, Predicted, horizon) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted",
           Jurisdiction = jur_name)  # Add Jurisdiction for compatibility
  
  # Combine true and predicted
  df_plot <- bind_rows(df_true, df_preds_long) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      horizon = factor(horizon, levels = c("2_week_ahead", "3_week_ahead", "4_week_ahead"))
    )
  
  # Setup x-axis breaks (every 4th week)
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
    geom_line(data = filter(df_plot, Source == "True"), size = 0.9) +
    geom_point(data = filter(df_plot, Source == "True"), size = 1.75) +
    geom_line(data = filter(df_plot, Source == "Predicted"), size = 0.6) +
    geom_point(data = filter(df_plot, Source == "Predicted"), size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap(~ horizon, ncol = 3, scales = "free_y") +
    labs(x = "Date (week start)", y = "Cases", title = paste0("Forecast Horizons for ", jur_name)) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}




#*************************************************
#* PLOT DATA
#* ************************************************

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
    labs(x = "Date (week start date of reported cases)", y = "Cases", title = titleX) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}


# #PLOT INCREASE DETECTED
# 
# PLOT_TRUE_WITH_INCREASE <- function(data, titleX, n_col_plot = 3) {
#   
#   # Ensure dates are in Date format
#   data <- data %>%
#     mutate(date_week_start = as.Date(date_week_start))
#   
#   # Get x-axis breaks (every 4th week)
#   date_seq <- data %>%
#     distinct(date_week_start) %>%
#     arrange(date_week_start) %>%
#     pull(date_week_start)
#   
#   x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
#   if (tail(date_seq, 1) != tail(x_breaks, 1)) {
#     x_breaks <- c(x_breaks, tail(date_seq, 1))
#   }
#   
#   # Plot
#   ggplot(data, aes(x = date_week_start, y = Cases)) +
#     geom_line(size = 0.9, color = "black") +
#     geom_point(size = 1.0, color = "black") +
#     geom_point(data = filter(data, increasing_plot == 1), 
#                aes(x = date_week_start, y = Cases), 
#                color = "red", size = 2.8, shape = 16) +
#     scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
#     facet_wrap(~Jurisdiction, scales = "free_y", ncol = n_col_plot) +
#     labs(x = "Date (week start)", y = "Cases", title = titleX) +
#     theme_minimal(base_size = 13) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       strip.text = element_text(size = 12)
#     )
# }

