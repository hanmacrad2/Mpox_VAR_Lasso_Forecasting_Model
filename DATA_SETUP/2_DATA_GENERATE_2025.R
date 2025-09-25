#2. DATA MPOX GENERATE 2025

#1. LOAD ONLINE DATA
url <- "https://oss.resilientservice.mooo.com/resilentpublic/pathogens/cdc/nndss/output/mpox_weekly.csv"
mpox_data <- read.csv(url)
head(mpox_data)

#DATA FOLDER
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_2025/"


#SELECT
mpox_data = mpox_data %>% dplyr::select(year, current_week, week, location1, previous_YTD__cummulative, date)

#RENAME
mpox_data = mpox_data %>% rename(date_week_start = date, Week_Number = week, Year = year,  
                                 Jurisdiction = location1,
                                 Cases = current_week, previous_YTD_cummulative = previous_YTD__cummulative)

colnames(mpox_data)

#RENAME STATES
mpox_data$Jurisdiction <- str_to_title(mpox_data$Jurisdiction)
unique(mpox_data$Jurisdiction); 
length(unique(mpox_data$Jurisdiction))
mpox_data$Jurisdiction <- gsub(" ", "", mpox_data$Jurisdiction)

#SUBSET
list_jur = c("NewYorkCity", "Texas", "Florida", "Illinois", "Georgia",
             "Washington") # "LA", "SanDiego"

mpox_data_sub = mpox_data %>% filter(Jurisdiction %in% list_jur)

#ADD DATA COLUMN
mpox_data_sub <- mpox_data_sub %>%
  group_by(Jurisdiction) %>%
  arrange(date_week_start) %>%
  mutate(Cases2 = previous_YTD_cummulative - lag(previous_YTD_cummulative, default = first(previous_YTD_cummulative))) %>%
  ungroup()

#ADD DATA COLUMN
mpox_data_sub <- mpox_data_sub %>%
  group_by(Jurisdiction) %>%
  arrange(date_week_start) %>%
  mutate(Cases2 = if_else(Week_Number == 1, previous_YTD_cummulative, Cases2)) %>%
  ungroup()

mpox_data_sub$Cases2 = pmax(mpox_data_sub$Cases2, 0)

#DATA_GOLDEN UP UNTIL 17/11/2024. NEW DATA AFTER THAT
mpox_data_sub2 = mpox_data_sub %>%filter(date_week_start > as.Date("2024-11-17"))

#PLOT CASES VS CASES2
# 1. Reshape the data from wide to long format
plot_data <- mpox_data_sub2 %>%
  pivot_longer(
    cols = c(Cases, Cases2),  # Columns to be reshaped
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

#GET 2024-2025 DATES
df_nyc_25 = mpox_data_sub2 %>% filter(Jurisdiction == 'NewYorkCity')
dates_25 = df_nyc_25 %>% dplyr::select(date_week_start, Week_Number, Year)
dates_25$Cases = rep(0, length(dates_25$Year))
dates_25$date_week_end = rep(0, length(dates_25$Year))

dates_25 <- dates_25 %>%
  mutate(Week_Year = paste0(Week_Number, "_", Year))

#San Diego
df_sd_25 = dates_25
df_sd_25$Jurisdiction = rep('SanDiego', length(df_sd_25$Year))

#SAVE AND MANUALLY ADD 
writexl::write_xlsx(df_sd_25, path = paste0(DATA_FOLDER, 'data_sd_25_temp.xlsx'))

#JOIN TO ORIGINAL DATA SAN DIEGO
data_25_total = rbind(data_sd, df_sd_25)

#2. LA DATA

#3. SAN FRAN DATA