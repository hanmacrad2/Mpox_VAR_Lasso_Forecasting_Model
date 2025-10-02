#UTIL FUNCTIONS

#********************************
#* 3. TRAIN VS TEST SPLIT

get_train_test_data <- function (data_mpox_model, TRAIN_WEEK){
  
  #TRAIN VS TEST SPLIT
  train_data <- data_mpox_model %>% filter(Week_Number <= TRAIN_WEEK)
  print(paste0('n weeks train_data: ', nrow(train_data)/(length(unique(data_mpox_model$Jurisdiction)))))
  future_data  <- data_mpox_model %>% filter(Week_Number > TRAIN_WEEK)
  print(paste0('nrow weeks future_data: ', nrow(future_data)/(length(unique(data_mpox_model$Jurisdiction)))))
  
  print(paste0('Number unique jurisdictions: ', length(unique(train_data$Jurisdiction))))
  
  list_data = list(train_data = train_data, future_data = future_data)
  
  return(list_data)
}

#FILES
GET_FILE_NAME_CURRENT_TIME <- function(TRAIN_YEAR, TRAIN_WEEK, file_name = 'data_mpox_forecasts_date_') {
  
  #SAVE
  current_datetime_string <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S_")
  data_file_name = paste0(file_name, current_datetime_string, 'trained_',
                          TRAIN_YEAR, '_week_', TRAIN_WEEK, '.rds')
  
  return(data_file_name)
  
}

#GET DATA OF TOP JURISDICTIONS
df_top_jurisdiction <- function(data_mpox, top_num = 10){
  
  #1. ORDER
  jurisdiction_order <- data_mpox %>%
    group_by(Jurisdiction) %>%
    summarise(Total_Cases = sum(Cases, na.rm = TRUE)) %>%
    arrange(desc(Total_Cases)) %>%
    pull(Jurisdiction) %>%
    as.character()
  
  list_top_jurs <- jurisdiction_order[1:top_num]
  
  df_mpox_top <- data_mpox %>% 
    filter(Jurisdiction %in% list_top_jurs)
  
  return(df_mpox_top)
  
}

get_ordered_list_jurisdictions <- function(data_mpox){
  
  #1. ORDER
  top_num = length(unique(data_mpox$Jurisdiction))
  
  jurisdiction_order <- data_mpox %>%
    group_by(Jurisdiction) %>%
    summarise(Total_Cases = sum(Cases, na.rm = TRUE)) %>%
    arrange(desc(Total_Cases)) %>%
    pull(Jurisdiction) %>%
    as.character()
  
  list_top_jurs <- jurisdiction_order[1:top_num]
  
  #df_mpox_top <- data_mpox %>% 
  #filter(Jurisdiction %in% list_top_jurs)
  
  return(list_top_jurs)
  
}

get_list_top_jurisdiction <- function(data_mpox, top_num = 10){
  
  jurisdiction_order <- data_mpox %>%
    group_by(Jurisdiction) %>%
    summarise(Total_Cases = sum(Cases, na.rm = TRUE)) %>%
    arrange(desc(Total_Cases)) %>%
    pull(Jurisdiction) %>%
    as.character()
  
  list_top_jurs <- jurisdiction_order[1:top_num]
  
  #df_mpox_top <- data_mpox %>% 
    #filter(Jurisdiction %in% list_top_jurs)
  
  return(list_top_jurs)
  
}

MSE <- function(actual, predicted) {
  
  mse_result = mean((actual - predicted)^2)
  print(paste0('mse_result: ', mse_result))
  
  return(mse_result)
}


get_state_abbreviations <- function() {
  
  state_abbreviations <- data.frame(
    Jurisdiction = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                     "Connecticut", "Delaware", "DistrictofColumbia", "Florida", "Georgia",
                     "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                     "LA", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "NewHampshire",
                     "NewJersey", "NewMexico", "NewYork", "NewYorkCity", "NorthCarolina",
                     "NorthDakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Philadelphia",
                     "PuertoRico", "RhodeIsland", "SanDiego", "SouthCarolina", "SouthDakota", "Tennessee",
                     "Texas", "Utah", "Vermont", "Virginia", "Washington", "WestVirginia", "Wisconsin",
                     "Wyoming"),
    Abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO",
                     "CT", "DE", "DC", "FL", "GA",
                     "HI", "ID", "IL", "IN", "IA", "KS", "KY",
                     "LA", "LA", "ME", "MD", "MA", "MI", "MN",
                     "MS", "MO", "MT", "NE", "NV", "NH",
                     "NJ", "NM", "NY", "NYC", "NC",
                     "ND", "OH", "OK", "OR", "PA", "Phil",
                     "PR", "RI", "SDC", "SC", "SD", "TN",
                     "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                     "WY")
  )
  
  return(state_abbreviations)
  
}

#CORRELATIONS
get_san_diego_correlations <- function(data) {
  # Extract San Diego column
  san_diego_col <- data$SanDiego
  
  # Compute correlations with all other columns
  corrs <- sapply(data[, colnames(data) != "SanDiego"], function(col) cor(san_diego_col, col, use = "complete.obs"))
  
  corr_sd = sort(corr_sd, decreasing = TRUE)
  print('Correlation with San Diego: ')
  print(corr_sd)
  
  return(corrs)
}

#DATA SUMMARIES
count_weeks_year <- function(df_mpox){
  
  df_mpox %>%
    group_by(Year) %>%
    summarise(n_rows = n()) %>%
    arrange(Year)
  
}
