#DATA MPOX REMFORMAT

library(dplyr)
library(stringr)

#DATA
DATA_FOLDER_orig <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_UPDATE_25_JUNE_25/WEEK_46_2024/"
data_mpox_orig = readRDS(paste0(DATA_FOLDER_orig, 'data_mpox.rds')) 


#FOLDER
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_2025/"

#REMOVE SD + LA
# data_mpox_orig = data_mpox_orig %>% 
#   filter(!Jurisdiction %in% c('SanDiego', 'LA'))

#SHIFT
library(dplyr)
library(stringr)

data_mpox_shifted <- data_mpox_orig %>%
  group_by(Jurisdiction) %>%
  arrange(date_week_start) %>%
  mutate(
    # Extract numeric week and year
    week_num_orig = Week_Number,
    year_orig = as.integer(str_extract(Week_Year, "\\d{4}$")),
    Week_Year_new = Week_Year
  ) %>%
  ungroup()

# Function to shift Week_Year for one jurisdiction
shift_week_year <- function(df_jur){
  # Find 53_YYYY row (if exists)
  shift_idx <- which(str_detect(df_jur$Week_Year_new, "^53_2022$"))
  if(length(shift_idx) == 0) return(df_jur)  # nothing to shift
  
  # Replace 53_2022 → 1_2023
  df_jur$Week_Year_new[shift_idx] <- "1_2023"
  
  # Shift all subsequent weeks by 1
  for(i in (shift_idx + 1):nrow(df_jur)){
    prev <- df_jur$Week_Year_new[i-1]
    wk <- as.integer(str_extract(prev, "^\\d+")) + 1
    yr <- as.integer(str_extract(prev, "\\d{4}$"))
    if(wk > 52){
      wk <- 1
      yr <- yr + 1
    }
    df_jur$Week_Year_new[i] <- paste0(wk, "_", yr)
  }
  return(df_jur)
}

# Apply to each jurisdiction
data_mpox_shifted <- data_mpox_shifted %>%
  group_by(Jurisdiction) %>%
  group_modify(~shift_week_year(.x)) %>%
  ungroup() %>%
  dplyr::select(-week_num_orig, -year_orig)

# Check Alabama
data_mpox_shifted %>%
  filter(Jurisdiction == "Alabama") %>%
  dplyr::select(date_week_start, Week_Number, Week_Year, Week_Year_new, Cases) %>%
  head(20)

#CHECK
data_mpox_shifted %>%
  mutate(
    week_num = as.integer(str_extract(Week_Year_new, "^\\d+")),
    year_num = as.integer(str_extract(Week_Year_new, "\\d{4}$"))
  ) %>%
  group_by(Jurisdiction, year_num) %>%
  summarise(max_week = max(week_num), .groups = "drop") %>%
  arrange(Jurisdiction, year_num)

#SELECT + RENAME
colnames(data_mpox_shifted)
data_mpox <- data_mpox_shifted %>%
  dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year_new, Year, Cases)

#RENAME
data_mpox = data_mpox %>% rename(Week_Year = Week_Year_new)

#SAVE
saveRDS(data_mpox, paste0(DATA_FOLDER, 'data_mpox_2022_2024.rds')) 


#******************
# MERGE

#GET JURISIDCTIONS FROM data_mpox
list_jur_cdc = unique(data_mpox$Jurisdiction)
mpox_data_part_II = mpox_data_part_II %>%filter(Jurisdiction %in% list_jur_cdc)

data_mpox_total = rbind(data_mpox, mpox_data_part_II, data_sd_add, data_la_add)

#SAVE RDS
saveRDS(data_mpox_total, file = paste0(DATA_FOLDER, 'data_mpox_final_22_2025.rds'))
write.csv(data_mpox_total, file = paste0(DATA_FOLDER, 'data_mpox_final_22_2025.csv'))



















#***************************************
#* 2. ONLINE DATA

#GET JURISIDCTIONS FROM data_mpox
list_jur_cdc = unique(data_mpox$Jurisdiction)
mpox_data_part_II = mpox_data_part_II %>%filter(Jurisdiction %in% list_jur_cdc)

#NEED TO ADD SD + LA TO ONLINE DATA AFTER 46_2024
list_jur_cdc


#COMBINE
data_mpox_total = rbind(data_mpox_orig, mpox_data_part_II)

#FILTER WEEKS
data_mpox_total = data_mpox_total %>% filter(Week_Number < 172)

#ADD SD + LA
data_mpox_tot = rbind(data_mpox_total, data_sd_22_25, data_la_22_25)

#SAVE AND MANUALLY ADD 
write.csv(data_mpox_tot, file = paste0(DATA_FOLDER, 'data_mpox_2022_2025.csv'))


#************************
#* 3. MERGE 
