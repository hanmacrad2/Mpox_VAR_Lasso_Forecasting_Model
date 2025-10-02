#FORMAT LA DATA 

#FOLDER
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_2025/"

#DATA
data_la_22_25_orig = read.csv(file = paste0(DATA_FOLDER, 'data_la_2022_2025.csv'))

#**********************
#* FORMATTING
#**********************

#NEW DATA
data_la_add = data_la_22_25 %>% filter(Week_Number > 130)

#DATA (WEEK_NUMBER & CASES ARE CORRENT; DATE IS WRONG)
data_la_add = data_la_add %>% dplyr::select(-date_week_start, -Week_Year)

data_la_add <- data_la_add %>%
  mutate(
    Year = if_else(Week_Number >= 136, 2025, Year)  # adjust cutoff as needed
  )


data_la_add <- data_la_add %>%
  left_join(
    df_nyc_24_25 %>% dplyr::select(Year, Week_Number, date_week_start, Week_Year),
    by = c("Year", "Week_Number")
  )


#**************************** QED

#SHIFT
data_la_shifted <- data_la_22_25_orig %>%
  mutate(
    Week_Year_old = Week_Year,
    Week_Year = Week_Year
  )

for(i in 1:nrow(data_la_shifted)){
  # Extract original week and year
  wk <- as.integer(str_extract(data_la_shifted$Week_Year[i], "^\\d+"))
  yr <- as.integer(str_extract(data_la_shifted$Week_Year[i], "\\d{4}$"))
  
  # Shift week by 1
  wk_new <- wk + 1
  yr_new <- yr
  if(wk_new > 52){
    wk_new <- 1
    yr_new <- yr + 1
  }
  
  # Assign new Week_Year
  data_la_shifted$Week_Year[i] <- paste0(wk_new, "_", yr_new)
}

# Check the first few rows
head(data_la_shifted, 10)

#DATA LA
data_la_22_25 = data_la_shifted %>% dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year, Year, Cases)

#SAVE FORMATTED + FINAL
file_name = 'data_la_2022_2025_complete.rds'
saveRDS(data_la_22_25, paste0(DATA_FOLDER, file_name))


#*************** QED

#GET WEEKS > 130 (after CDC DATA)
data_la_25 = data_la_22_25 %>% filter(Week_Number > 130) 

#********

#LA
file_name = 'data_la_orig_25.csv'
data_la = read_csv(paste0(DATA_FOLDER, file_name))

#ADD WEEK NUMBER
data_la$Week_Number = seq(1,length(data_la$date_week_start))

#ADD JURISDICTION
data_la$Jurisdiction = rep('LA',length(data_la$date_week_start))

#GET LA + DATES
data_la <- data_la %>%
  dplyr::select(Jurisdiction, Cases, Week_Number) %>%   # keep only LA columns
  left_join(
    data_sd_22_25 %>% 
      dplyr::select(Week_Number, date_week_start, Year, Week_Year),
    by = "Week_Number"
  )

#REMOVE NAs
data_la <- data_la %>%
  mutate(Cases = coalesce(Cases, 0))

#SELECT MATCHING COLUMNS
data_la_22_25 = data_la %>% dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year, Year, Cases)

#SAVE AND MANUALLY ADD 
write.csv(data_la_22_25, file = paste0(DATA_FOLDER, 'data_la_2022_2025.csv'))
