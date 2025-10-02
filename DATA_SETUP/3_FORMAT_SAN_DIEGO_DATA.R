#**********************************************
#*
#* DATA - FORMAT SAN DIEGO DATA
#* 
#*********************************************
library(readxl)
library(writexl)
library(dplyr)
library(stringr)

#STEPS
#USE ORIGINAL SD DATA FOR 2022/2023
#USE DATES FROM ONLINE DATA FOR 2024/2025

#SAVE FORMATTED + FINAL
file_name = 'data_sd_2022_2025_complete.rds'
saveRDS(data_sd_22_25, paste0(DATA_FOLDER, file_name))

file_name = 'data_sd_2022_2025_complete.csv'
write.csv(data_sd_22_25, paste0(DATA_FOLDER, file_name))

#NEW DATA
data_sd_add = data_sd_22_25 %>% filter(Week_Number > 130)

#DATA (WEEK_NUMBER & CASES ARE CORRENT; DATE IS WRONG)
data_sd_add = data_sd_add %>% dplyr::select(-date_week_start, -date_week_end, -Week_Year)

#JOIN ON CORRECT DATES

data_sd_add <- data_sd_add %>%
  left_join(
    df_nyc_24_25 %>% dplyr::select(Year, Week_Number, date_week_start, Week_Year),
    by = c("Year", "Week_Number")
  )

data_sd_add <- data_sd_add %>%
  mutate(
    Year = if_else(Week_Number >= 136, 2025, Year)  # adjust cutoff as needed
  )

#DATA < LA 171
data_sd_add = data_sd_add %>% filter(Week_Number <= 171)
  
#******************************* QED 

#CORRECT
#SD
file_name = 'data_sd_complete.xlsx'
data_sd = read_xlsx(paste0(DATA_FOLDER, file_name))


#READ IN FINAL
file_name = 'data_sd_22_25.csv'
data_sd_22_25 = read_csv(paste0(DATA_FOLDER, file_name))

#SHIFT
data_sd_shifted <- data_sd_22_25 %>%
  mutate(
    Week_Year_old = Week_Year,
    Week_Year = Week_Year
  )

for(i in 1:nrow(data_sd_shifted)){
  # Extract original week and year
  wk <- as.integer(str_extract(data_sd_shifted$Week_Year[i], "^\\d+"))
  yr <- as.integer(str_extract(data_sd_shifted$Week_Year[i], "\\d{4}$"))
  
  # Shift week by 1
  wk_new <- wk + 1
  yr_new <- yr
  if(wk_new > 52){
    wk_new <- 1
    yr_new <- yr + 1
  }
  
  # Assign new Week_Year
  data_sd_shifted$Week_Year[i] <- paste0(wk_new, "_", yr_new)
}

# Check the first few rows
head(data_sd_shifted, 10)

#RENAME 
#data_sd_shifted = data_sd_shifted %>% rename(Week_Year = Week_Year_new)

#DATA SD
data_sd = data_sd_shifted %>% dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year, Year, Cases)

#*************** QED

#GET WEEKS > 130 (after CDC DATA)
data_sd_25 = data_sd %>% filter(Week_Number > 130) 

#MATCH LA DATA 
data_sd_25 = data_sd_25 %>% filter(Week_Number <= 171) 

#GET 

#**************************
#* orig formatting

#Weeks
data_sd_22_25 = data_sd_22_25 %>% filter(Week_Number < 172)

#DATA
data_sd_22_25 = read.csv(file = paste0(DATA_FOLDER, 'data_sd_2022_2025.csv'))

#FORMAT DATA (SHIFT WEEK_NUMBER BACK BY 1)
data_sd <- data_sd %>%
  # split Week_Year into Week and Year
  mutate(Week = as.integer(str_extract(Week_Year, "^[0-9]+")),
         Yr   = as.integer(str_extract(Week_Year, "[0-9]+$"))) %>%
  # shift weeks back by 1
  mutate(Week = Week - 1,
         Yr   = ifelse(Week == 0, Yr - 1, Yr),   # if week drops to 0, go back a year
         Week = ifelse(Week == 0, 52, Week)) %>% # set week 0 → 52
  # rebuild Week_Year
  mutate(Week_Year = paste0(Week, "_", Yr)) %>%
  dplyr::select(-Week, -Yr)   # optional cleanup


data_sd <- data_sd %>%
  arrange(date_week_start) %>%
  mutate(
    # sequential index starting at 0
    idx = row_number() - 1,
    
    # absolute week number starting at 21 for 2022
    week_abs = 21 + idx,
    
    # year shift based on completed sets of 52 weeks
    Year_new = 2022 + (week_abs %/% 52),
    
    # week number: 1–52 only
    Week_new = (week_abs %% 52),
    Week_new = ifelse(Week_new == 0, 52, Week_new),
    
    # if we hit week 0, adjust year back
    Year_new = ifelse(week_abs %% 52 == 0, Year_new - 1, Year_new),
    
    Week_Year = paste0(Week_new, "_", Year_new)
  ) %>%
  dplyr::select(-idx, -week_abs, -Year_new, -Week_new)


#SPLIT YEARS
df_22_23 = data_sd %>% filter(Year < 2024)
df_24 = data_sd %>% filter(Year > 2023)


#************************************
#* DATES FOR ONLINE DATA

#GET 2024-2025 DATES
mpox_data_online_24_25 = mpox_data_online %>% filter(Year > 2023)

df_nyc_24_25 = mpox_data_online_24_25  %>% filter(Jurisdiction == 'NewYorkCity')

#2024
dates_24_25 = df_nyc_24_25 %>% dplyr::select(date_week_start, Week_Number, Year)
#dates_24_25$Cases = rep(0, length(dates_24_25$Year))

#DATE WEEK END
dates_24_25 <- dates_24_25 %>%
  mutate(
    date_week_start = ymd_hms(date_week_start),        # make sure it's datetime
    #date_week_end   = date_week_start + days(7)        # add 7 days
  )

dates_24_25 <- dates_24_25 %>%
  mutate(Week_Year = paste0(Week_Number, "_", Year))

#DATES NEEDED > WEEK 46_2024
dates_24_25 = dates_24_25 %>% filter

#San Diego
df_sd_24_25 = dates_24_25
df_sd_24_25$Jurisdiction = rep('SanDiego', length(dates_24_25$Year))

S
#*****************************************
#* JOIN

#USE CASES FROM DF_24
df_sd_24_25 <- df_sd_24_25 %>%
  left_join(
    df_24 %>%
      dplyr::select(Week_Year, Jurisdiction, Cases),
    by = c("Week_Year", "Jurisdiction")
  ) %>%
  # keep the Cases from df_24 if available, otherwise 0
  mutate(Cases = coalesce(Cases.y, Cases.x)) %>%
  dplyr::select(-Cases.x, -Cases.y)

#Update Week Number
df_sd_24_25$Week_Number = seq(from=85, by = 1, length.out = length(df_sd_24_25$Week_Year))


#JOIN
df_sd_tot = rbind(df_22_23, df_sd_24_25)
writexl::write_xlsx(df_sd_tot, path = paste0(DATA_FOLDER, 'data_sd_temp_24_25.xlsx'))


#READ IN FINAL
file_name = 'data_sd_22_25.csv'
data_sd_22_25 = read_csv(paste0(DATA_FOLDER, file_name))


#San Diego
data_sd_22_25 <- data_sd_22_25 %>%
  dplyr::select(Jurisdiction, date_week_start, Week_Number, Year, Week_Year, Cases)


#FORMAT
data_sd_22_25 = data_sd_22_25 %>% filter(Week_Number < 172)

#SELECT MATCHING COLUMNS
data_sd_22_25 = data_sd_22_25 %>% dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year, Year, Cases)

#SAVE AND MANUALLY ADD 
write.csv(data_sd_22_25, file = paste0(DATA_FOLDER, 'data_sd_2022_2025.csv'))

data_sd_22_25 = data_sd_25

#READ
data_sd_22_25 = read.csv(file = paste0(DATA_FOLDER, 'data_sd_2022_2025.csv'))
