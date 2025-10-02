#2. DATA MPOX GENERATE 2025

#DATA -> 2024
DATA_FOLDER_orig <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_UPDATE_25_JUNE_25/WEEK_46_2024/"
data_mpox_orig = readRDS(paste0(DATA_FOLDER_orig, 'data_mpox.rds')) 

#REMOVE SD + LA
data_mpox_orig = data_mpox_orig %>% 
  filter(!Jurisdiction %in% c('SanDiego', 'LA'))

#DATA FOLDER
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_2025/"

#1. LOAD ONLINE DATA
url <- "https://oss.resilientservice.mooo.com/resilentpublic/pathogens/cdc/nndss/output/mpox_weekly.csv"
mpox_data_online <- read.csv(url)
head(mpox_data_online)

#SELECT
mpox_data_online = mpox_data_online %>% dplyr::select(year, current_week, week, location1, previous_YTD__cummulative, date)

#RENAME COLUMNS
mpox_data_online = mpox_data_online %>% rename(date_week_start = date, Week_Number = week, Year = year,  
                                 Jurisdiction = location1,
                                 Cases_orig = current_week, previous_YTD_cummulative = previous_YTD__cummulative)

colnames(mpox_data_online)

#RENAME STATES
mpox_data_online$Jurisdiction <- str_to_title(mpox_data_online$Jurisdiction)
unique(mpox_data_online$Jurisdiction); 
length(unique(mpox_data_online$Jurisdiction))
mpox_data_online$Jurisdiction <- gsub(" ", "", mpox_data_online$Jurisdiction)


#ADD DATA COLUMN
mpox_data_online <- mpox_data_online %>%
  group_by(Jurisdiction) %>%
  arrange(date_week_start) %>%
  mutate(Cases = previous_YTD_cummulative - lag(previous_YTD_cummulative, default = first(previous_YTD_cummulative))) %>%
  ungroup()

#ADD DATA COLUMN
mpox_data_online <- mpox_data_online %>%
  group_by(Jurisdiction) %>%
  arrange(date_week_start) %>%
  mutate(Cases = if_else(Week_Number == 1, previous_YTD_cummulative, Cases)) %>%
  ungroup()

mpox_data_online$Cases = pmax(mpox_data_online$Cases, 0)

#USE 'GOLDEN DATA' DATA_GOLDEN UP UNTIL 17/11/2024. NEW DATA AFTER THAT
mpox_data_part_II = mpox_data_online %>%filter(date_week_start > as.Date("2024-11-18"))
#WEEK_NUMBER > 47_2024


#PREP DATA TO MATCH GODEN CDC DATA
mpox_data_part_II <- mpox_data_part_II %>%
  # 1. create Week_Year
  mutate(Week_Year = paste0(Week_Number, "_", Year)) %>%
  # 2. re-number Week_Number per Jurisdiction starting at 131
  group_by(Jurisdiction) %>%
  mutate(Week_Number = row_number() + 130) %>%
  ungroup() %>%
  # 3. keep only columns in data_mpox
  dplyr::select(Jurisdiction, date_week_start, Week_Number, Week_Year, Year, Cases)

#LESS THAN WEEK 173
mpox_data_part_II = mpox_data_part_II %>% filter(Week_Number <= 171)


#**************************************************************************** QED


#*******
#* OLD

#COMBINE
data_mpox_total = rbind(data_mpox_orig, mpox_data_part_II)

#FILTER WEEKS
data_mpox_total = data_mpox_total %>% filter(Week_Number < 172)
  
#ADD SD + LA
data_mpox_tot = rbind(data_mpox_total, data_sd_22_25, data_la_22_25)

#SAVE AND MANUALLY ADD 
write.csv(data_mpox_tot, file = paste0(DATA_FOLDER, 'data_mpox_2022_2025.csv'))

data_mpox = data_mpox_tot



#***************************** QED

#CHECKS
#Duplicated Jurisdiction + Week_Number pairs
dupes <- data_mpox_tot[duplicated(data_mpox_tot[c("Jurisdiction", "Week_Number")]) |
                      duplicated(data_mpox_tot[c("Jurisdiction", "Week_Number")], fromLast = TRUE), ]

# See which weeks are repeated per jurisdiction
dupe_summary <- dupes %>%
  group_by(Jurisdiction, Week_Number) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Jurisdiction, Week_Number)

print(dupe_summary)

#Weeks
weeks_per_jurisdiction <- data_mpox_tot %>%
  group_by(Jurisdiction) %>%
  summarise(n_weeks = n_distinct(Week_Number))
print(weeks_per_jurisdiction)


train_data <- data_mpox_model %>% filter(Week_Number <= TRAIN_WEEK)




#************************************
#GET 2024-2025 DATES

df_nyc_24_25 = mpox_data_part_II  %>% filter(Jurisdiction == 'NewYorkCity') #PICK ONE JUR
dates_24_25 = df_nyc_24_25 %>% dplyr::select(date_week_start, Week_Number, Year)



#***********************
#OLD
mpox_data_online_24_25 = mpox_data_online %>% filter(Year > 2023)

mpox_data_part_II_24_25 = mpox_data_part_II
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

#San Diego
df_sd_24_25 = dates_24_25
df_sd_24_25$Jurisdiction = rep('SanDiego', length(dates_24_25$Year))


#*********************************
#PLOT CASES
data_mpox_sub_24_25 = data_mpox_sub %>% filter(Year > 2023)
data_mpox_sub_24 = data_mpox_sub %>% filter(Year == 2024)
data_mpox_sub_25 = data_mpox_sub %>% filter(Year == 2025)
  
ggplot(data_mpox_sub_25, aes(x = date_week_start, y = Cases)) +
  geom_line(color = "steelblue") +       # line plot for cases over time
  geom_point(color = "steelblue", size = 1) +
  facet_wrap(~ Jurisdiction, scales = "free_y") +  # one panel per jurisdiction
  labs(
    title = "Weekly mpox Cases by Jurisdiction",
    x = "Week Start Date",
    y = "Cases"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),  # facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#PLOT CASES VS CASES2
# 1. Reshape the data from wide to long format
plot_data <- mpox_data_online %>%
  pivot_longer(
    cols = c(Cases_orig, Cases),  # Columns to be reshaped
    names_to = "CaseType",     # New column for the variable names
    values_to = "WeeklyCases"  # New column for the values
  )

# 2. Create the facet wrap plot
ggplot(plot_data, aes(x = Week_Number, y = WeeklyCases, color = CaseType)) +
  geom_line(size = 1) +  # Use geom_line for time-series data
  facet_wrap(~ Jurisdiction, scales = "free_y") +  # Create a subplot for each jurisdiction
  labs(
    title = "Weekly Cases by Jurisdiction",
    x = "Week Number",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Cases" = "blue", "Cases2" = "red")) # Set the colors


#************************************
#GET 2024-2025 DATES
df_nyc_24_25 = mpox_data_sub_24  %>% filter(Jurisdiction == 'NewYorkCity')

#2024
dates_24_25 = df_nyc_24_25 %>% dplyr::select(date_week_start, Week_Number, Year)
#dates_24_25$Cases = rep(0, length(dates_24_25$Year))

#DATE WEEK END
dates_24_25 <- dates_24_25 %>%
  mutate(
    date_week_start = ymd_hms(date_week_start),        # make sure it's datetime
    date_week_end   = date_week_start + days(7)        # add 7 days
  )

dates_24_25 <- dates_24_25 %>%
  mutate(Week_Year = paste0(Week_Number, "_", Year))

#San Diego
df_sd_24_25 = dates_24_25
df_sd_24_25$Jurisdiction = rep('SanDiego', length(dates_24_25$Year))

#SAVE AND MANUALLY ADD 
writexl::write_xlsx(dates_24_25, path = paste0(DATA_FOLDER, 'data_sd_24_25_temp.xlsx'))

#2025
df_nyc_25 = mpox_data_sub2 %>% filter(Jurisdiction == 'NewYorkCity')
dates_25 = df_nyc_25 %>% dplyr::select(date_week_start, Week_Number, Year)
dates_25$Cases = rep(0, length(dates_25$Year))


dates_25$date_week_end = rep(0, length(dates_25$Year))

dates_25 <- dates_25 %>%
  mutate(Week_Year = paste0(Week_Number, "_", Year))

#San Diego
df_sd_25 = dates_25
df_sd_25$Jurisdiction = rep('SanDiego', length(df_sd_25$Year))


#CHECK DATA
data_orig_234 = data_mpox_orig %>% filter(Year < 2025) #data_sd_22_25

#
data_sd_234 = data_sd_22_25 %>% filter(Year < 2025)
