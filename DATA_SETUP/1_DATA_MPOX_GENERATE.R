#**********************************************
#*
#* DATA MPOX GENERATE UPDATED FILE 
#* 
#*********************************************
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)


#DATA FORMATTING SUMMARY
#US DATA: MAKE WEEK_YEAR 53_2023 -> 1_2024 to allign with SD & LA
#SAN DIEGO DATA: ADD date_week_start column 

#DATA MPOX - FINAL FORMATTED
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_UPDATE_25_JUNE_25/WEEK_46_2024/"
data_mpox = readRDS(paste0(DATA_FOLDER, 'data_mpox.rds')) 

#DATA + DATES 
data_sd = readRDS(paste0(DATA_FOLDER, 'data_sd_complete.rds'))
data_la = readRDS(paste0(DATA_FOLDER, 'data_la_complete.rds'))


#********************************* 
#DATA SOURCES

#1.US DATA
file_name = 'data_mpox_orig.xlsx'
data_mpox_orig = read_xlsx(paste0(DATA_FOLDER, file_name))

#SD
file_name = 'data_sd.xlsx'
data_sd = read_xlsx(paste0(DATA_FOLDER, file_name))

#LA
file_name = 'data_la.xlsx'
data_la = read_xlsx(paste0(DATA_FOLDER, file_name))
head(data_la)

#***********************************
#*
#DATA FORMATTING
#
#***********************************

#******************************
#* I. US MPOX DATA

#US State names (remove gaps)
data_mpox_orig$Jurisdiction <- gsub(" ", "", data_mpox_orig$Jurisdiction)
unique(data_mpox_orig$Jurisdiction); length(unique(data_mpox_orig$Jurisdiction))

#I. US DATA: REMOVE FIRST 2 WEEKS OF US DATA TO MATCH SAN DIEGO
data_mpox_orig <- data_mpox_orig %>%
  filter(!(Week_Year %in% c("20_2022", "21_2022")))

#MAKE 53RD WEEK IN 2023 the first week in 2024
data_mpox_orig <- data_mpox_orig %>%
  mutate(
    Week_Year = if_else(Week_Year == "53_2023", "1_2024", Week_Year),
    Week = if_else(Week_Year == "1_2024", 1, Week),
    Year = if_else(Week_Year == "1_2024", 2024, Year)
  )

#CALI
df_cali = data_mpox_orig %>% filter(Jurisdiction == 'California')

#**********************************
#*
#* II. SAN DIEGO
#* 
#***************************************

#SAN DIEGO: ADD DATA_WEEK_START 
data_sd <- data_sd %>%
  mutate(
    date_week_start = date_week_end - days(7),
    #Month = month(date_week_start, label = TRUE, abbr = FALSE),
    #Month_Year = paste0(Month, "_", year(date_week_start)),
    Year = year(date_week_start)
  )


#*************************************
#* 3. ADD WEEK_YEAR TO SD + LA
#*************************************
add_week_year_all_years <- function(df_mpox, list_years, start_weeks = NULL) {
  df_out <- df_mpox
  
  for (i in seq_along(list_years)) {
    year_interest <- list_years[i]
    
    # Optional start week (e.g., 22 for 2022); default to 1 if not provided
    start_week <- if (!is.null(start_weeks)) start_weeks[i] else 1
    
    idx_year <- which(df_out$Year == year_interest)
    sorted_idx <- idx_year[order(df_out$date_week_start[idx_year])]
    
    week_seq <- character(nrow(df_out))
    week_seq[sorted_idx] <- paste0(start_week + seq_along(sorted_idx) - 1, "_", year_interest)
    
    df_out$Week_Year[sorted_idx] <- week_seq[sorted_idx]
  }
  
  return(df_out)
}

list_years <- c(2022, 2023, 2024) 
start_weeks <- c(22, 1, 1) 

data_sd <- add_week_year_all_years(data_sd, list_years, start_weeks)
data_la <- add_week_year_all_years(data_la, list_years, start_weeks)


#*********************************
#5. COUNT WEEKS 
#*********************************
count_weeks_month_year <- function(df_mpox){
  
  agg_month_year = df_mpox %>%
    group_by(Month_Year) %>%
    summarise(n_rows = n()) %>%
    arrange(Month_Year)
  
  print(agg_month_year, n = Inf)
}

count_weeks_year <- function(df_mpox){
  
  df_mpox %>%
    group_by(year) %>%
    summarise(n_rows = n()) %>%
    arrange(year)
  
}

count_weeks_year(data_sd)
count_weeks_year(data_la)
count_weeks_year(df_cali)

#*******************************************
# 5. ADD JURISDICTION TO SAN DIEGO & LA
#*******************************************
data_sd$Jurisdiction = rep('SanDiego', nrow(data_sd))
data_la$Jurisdiction = rep('LA', nrow(data_la))

#***********************************************
#* 4. ADD WEEK_NUMBER
#**********************************************
add_week_number <- function(df_subset){
  
  df_subset <- df_subset %>%
    arrange(date_week_start) %>%
    mutate(Week_Number = seq(1, n())) 
  
  #CHECK
  df_check = df_subset %>%
    dplyr::select(date_week_start, Week_Number) %>%
    arrange(Week_Number) %>%
    head(10)
  print(df_check)
  
  df_check = df_subset %>%
    dplyr::select(date_week_start, Week_Number) %>%
    arrange(Week_Number) %>%
    tail(10)
  print(df_check)
  
  return(df_subset)
  
}

add_week_number_all_jurisdictions <- function(df_total) {
  
  df_total = df_total %>%
    group_by(Jurisdiction) %>%
    arrange(Year, Week, .by_group = TRUE) %>%
    mutate(Week_Number = row_number()) %>%
    ungroup()
  
  #CHECK
  df_check = df_total %>%
    #dplyr::select(date_week_start, Week_Number) %>%
    arrange(Week_Number) %>%
    head(10)
  print(df_check)
  
  df_check = df_total %>%
    #dplyr::select(date_week_start, Week_Number) %>%
    arrange(Week_Number) %>%
    tail(10)
  print(df_check)
  
  df_check = df_total %>% slice(50:60)
  print(df_check)
  
  return(df_total)
}


#ADD WEEK_NUMBER
data_sd = add_week_number(data_sd)
data_la = add_week_number(data_la)
data_mpox_orig = add_week_number_all_jurisdictions(data_mpox_orig)

#SAVE SD + LA
saveRDS(data_sd, paste0(DATA_FOLDER, 'data_sd_complete.rds'))
saveRDS(data_la, paste0(DATA_FOLDER, 'data_la_complete.rds'))

writexl::write_xlsx(data_sd, path = paste0(DATA_FOLDER, 'data_sd_complete.xlsx'))
writexl::write_xlsx(data_la, path = paste0(DATA_FOLDER, 'data_la_complete.xlsx'))


#*********************************************************
# RE-LOAD
data_la = readRDS(paste0(DATA_FOLDER, 'data_la_complete.rds'))
data_la <- data_la %>%
  left_join(data_sd %>% 
              dplyr::select(Week_Year, date_week_start), by = "Week_Year")

#*********************************************************
# 6 SELECT RELEVANT COLUMNS
#*********************************************************
colnames(data_sd)

data_sd = data_sd %>% dplyr::select(Jurisdiction, Week_Number, Week_Year, Year, Cases)
data_la = data_la %>% dplyr::select(Jurisdiction, Week_Number, Week_Year, Year, Cases)
data_mpox_orig = data_mpox_orig %>% dplyr::select(Jurisdiction, Week_Number, Week_Year, Year, Cases)

#*********************************
#* DATA MPOX 
#*********************************

data_mpox = rbind(data_mpox_orig, data_la, data_sd)


#***********************************************
#2. CALIFORNIA Cases: REMOVE LA + SAN DIEGO
#***********************************************

# Step 1: Calculate adjusted Cases per week
adjusted_cases <- data_mpox %>%
  filter(Jurisdiction %in% c("California", "LA", "SanDiego")) %>%
  dplyr::select(Week_Number, Jurisdiction, Cases) %>%
  pivot_wider(names_from = Jurisdiction, values_from = Cases, values_fill = 0) %>%
  mutate(Adjusted_Cases = pmax(California - LA - SanDiego, 0)) %>%
  dplyr::select(Week_Number, Adjusted_Cases)

# Step 2: Get all original California rows (all columns)
california_rows <- data_mpox %>%
  filter(Jurisdiction == "California")

# Step 3: Join adjusted cases to California rows and replace Cases with Adjusted_Cases
adjusted_ca <- california_rows %>%
  left_join(adjusted_cases, by = "Week_Number") %>%
  mutate(Cases = Adjusted_Cases) %>%
  dplyr::select(-Adjusted_Cases)   # remove the helper column

# Step 4: Replace old California rows with adjusted ones in original data
data_mpox_final <- data_mpox %>%
  filter(Jurisdiction != "California") %>%
  bind_rows(adjusted_ca) %>%
  arrange(Week_Number, Jurisdiction)

data_mpox = data_mpox_final


#***********************************
#* ADD START DATE
#**********************************

# Step 1: Select only the matching key and desired column from data_la
date_la <- data_la %>%
  dplyr::select(Week_Year, date_week_start) %>%
  distinct()

# Step 2: Join to the full dataset (data_mpox) using Week_Year as the key
data_mpox_with_dates <- data_mpox %>%
  left_join(date_la, by = "Week_Year")

list_cols = c("Jurisdiction", "date_week_start", "Week_Number", "Week_Year", "Year", "Cases")
data_mpox = data_mpox_with_dates %>% dplyr::select(all_of(list_cols))

#*********************************
#* FINAL DATA MPOX 
#*********************************

#SAVE
saveRDS(data_mpox, paste0(DATA_FOLDER, 'data_mpox.rds'))
writexl::write_xlsx(data_mpox, path = paste0(DATA_FOLDER, 'data_mpox.xlsx'))

#QED
#YEARS
#1  2022     32
#2  2023     52
#3  2024     46


#FINAL DATE FIX (FIXED 29TH SEPTEMBER 2025)

