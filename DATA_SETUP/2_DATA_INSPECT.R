#******************************
#* INSPECT DATA
#******************************
library(lubridate)
library(dplyr)
#library(readxl)

#DATA MPOX - FINAL FORMATTED
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_UPDATE_25_JUNE_25/WEEK_46_2024/"
data_mpox = readRDS(paste0(DATA_FOLDER, 'data_mpox.rds')) 

#YEARS
df_22 = data_mpox %>% filter(Year == 2022)

df_22_summary = df_22 %>%
  group_by(Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Cases)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = Jurisdiction))

#TOP 10, 2022
list_top_11 = c(head(df_22_summary, 10))$Jurisdiction

list_top_11 <- c("NewYorkCity", "California", "Texas", "Florida", "LA", 
                 "Georgia", "Illinois", "NewJersey", "Maryland", "NorthCarolina",  "SanDiego")

#TOP 25, 2022
list_top_25 = c(head(df_22_summary, 25))$Jurisdiction
list_top_25 = c("California", "NewYorkCity", "Texas", "Florida", "LA", "Georgia", "Illinois",
                "NewJersey", "Maryland", "NorthCarolina", "Washington", "Arizona", "Virginia",
                "Philadelphia", "DistrictofColumbia", "SanDiego", "Massachusetts", "Colorado", 
                "Michigan", "Tennessee", "Ohio", "NewYork", "Nevada", "Pennsylvania", "Louisiana")


#2023
df_23 = data_mpox %>% filter(Year == 2023)

df_23_summary = df_23 %>%
  group_by(Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Cases)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = Jurisdiction))

#2024
df_24 = data_mpox %>% filter(Year == 2024)

df_24_summary = df_24 %>%
  group_by(Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Cases)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = Jurisdiction))

#2023 & 2024
df_23_24 = data_mpox %>% filter(Year != 2022)
df_23_24_top8 = df_23_24 %>% filter (Jurisdiction %in% list_jur)

df_23_24_summary = df_23_24 %>%
  group_by(Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Cases)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = Jurisdiction))


list_top_10_23_24 = c(head(df_23_24_summary, 10))$Jurisdiction
list_top_10_23_24 = c("NewYorkCity", "California", "Texas", "LA", "Florida", "Illinois", "Georgia",
                      "SanDiego", "Washington", "Colorado")

#Sums
sum(df_23_24$Cases) #4267
sum(df_23_24_top8$Cases) #2315

  
#***************************************
#1. DATA - TOP 15 IN 2023/2024 (Contains 3 states close to San Diego County)

list_top_15 = c(head(df_23_24_summary, 15))$Jurisdiction

list_top_15 = c("NewYorkCity", "California", "Texas", "LA", "Florida", 
                "Illinois", "Georgia", "SanDiego", "Washington", "Colorado", 
                "NewJersey", "NewYork", "NorthCarolina", "Arizona", "Ohio")


#***************************************
#1. DATA - TOP 10 BY CASE COUNT
list_jur_11 <- c("NewYorkCity", "California", "Texas", "Florida", "LA", 
                 "Georgia", "Illinois", "NewJersey", "Maryland", "NorthCarolina",  "SanDiego")

df_11 = data_mpox %>% filter(Jurisdiction %in% list_jur_11)

#SUMMARISE DATA
df_11_summary <- df_11 %>%
  group_by(Year, Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = list_jur_11)) %>%
  arrange(Jurisdiction, Year)


#TOP 10 MOST VISITED TO SAN DIEGO
list_visited <- c("Arizona", "Texas", "Nevada", "Washington", "Florida",
                 "NewYork", "Oregon", "Illinois", "Colorado", "Utah")

df_10 = data_mpox %>% filter(Jurisdiction %in% list_visited)


#SUMMARISE DATA
df_year_summary <- df_10 %>%
  group_by(Year, Jurisdiction) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = list_visited)) %>%
  arrange(Jurisdiction, Year)

#*****************************************************

#SAN FRANCISCO DATA
data_sf = read.csv(paste0(DATA_FOLDER, 'data_sf.csv'))

#PLOT
data_sf$episode_date <- as.Date(data_sf$episode_date, format = "%d/%m/%Y")
plot(data_sf$episode_date, data_sf$new_cases)
plot(data_sf$episode_date, data_sf$new_cases, type = "l", xlab = "Date", ylab = "New Cases")
points(data_sf$episode_date, data_sf$new_cases, pch = 16) #, xlab = "Date", ylab = "New Cases")

#ADD YEAR
data_sf$Year = year(data_sf$episode_date)

#FILTER
sum((data_sf %>% filter(Year == 2022))$new_cases) #840
sum((data_sf %>% filter(Year == 2023))$new_cases) #93
sum((data_sf %>% filter(Year == 2024))$new_cases) #33

#PLOT
df_sf_23 = data_sf %>% filter(Year == 202)
plot.ts(df_sf_23$new_cases)

#***************************
#* CALIFORNIA
df_cali = data_mpox %>% filter(Jurisdiction == 'California')
sum(df_cali$Cases)


#***************************
#* PLOTS

#* PHILADELPHIA
df_philly_23_24 = df_23_24 %>% filter(Jurisdiction == 'Philadelphia')
plot.ts(df_philly_23_24$Cases)

#NEW JERSERY
df_nj_23_24 = df_23_24 %>% filter(Jurisdiction == 'NewJersey')
plot.ts(df_nj_23_24$Cases)
points(df_nj_23_24$Cases, pch = 16)

#GEORGIA
df_ge_23_24 = df_23_24 %>% filter(Jurisdiction == 'Georgia')
plot.ts(df_ge_23_24$Cases)
points(df_ge_23_24$Cases, pch = 16)


#* MARY LAND 
df_myland_23_24 = df_23_24 %>% filter(Jurisdiction == 'Maryland')
plot.ts(df_myland_23_24$Cases)
sum(df_myland_23_24$Cases)


#*****************************************************
#* STATES CLOSE TO SAN DIEGO
#*****************************************************

list_top_11 <- c("NewYorkCity", "California", "Texas", "Florida", "LA", 
                 "Georgia", "Illinois", "NewJersey", "Maryland", "NorthCarolina",  "SanDiego")

#CALIFORNIA
df_ca = df_23 %>% filter(Jurisdiction == 'California')
plot.ts(df_arizona_23_24$Cases, main = ('California Reported Cases, 2023'))

#ARIZONA (#INCLUDE)
df_arizona_23_24 = df_23_24 %>% filter(Jurisdiction == 'Arizona')
plot.ts(df_arizona_23_24$Cases)

df_az_23 = df_23  %>% filter(Jurisdiction == 'Arizona')
plot.ts(df_az_23$Cases)
sum(df_az_23$Cases) 

#COLORADO (#INCLUDE)
df_co_23_24 = df_23_24 %>% filter(Jurisdiction == 'Colorado')
plot.ts(df_co_23_24$Cases)

df_co_23 = df_23  %>% filter(Jurisdiction == 'Colorado')
plot.ts(df_co_23$Cases)
sum(df_co_23$Cases) #15

#WASHINGTON (#INCLUDE)
df_wa_23_24 = df_23_24 %>% filter(Jurisdiction == 'Washington')
plot.ts(df_wa_23_24$Cases)

#OREGON (NO)
df_oregon_23_24 = df_23_24 %>% filter(Jurisdiction == 'Oregon')
plot.ts(df_oregon_23_24$Cases)

#NEVADA (NO)
df_nv_23_24 = df_23_24 %>% filter(Jurisdiction == 'Nevada')
plot.ts(df_nv_23_24$Cases)

#OTHER
df_go_23 = df_23  %>% filter(Jurisdiction == 'Georgia')
plot.ts(df_go_23$Cases)
sum(df_go_23$Cases) #15

#*****************************************************
#* TOP 15 IN 2023 & 2024
#*****************************************************

#OHIO
df_oh_23_24 = df_23_24 %>% filter(Jurisdiction == 'Ohio')
plot.ts(df_oh_23_24$Cases)

#**********
#* 2025 data
df_25 = data_mpox_model %>% filter(Year == 2025)

#WASHINGTON (#INCLUDE)
df_wa_25 = df_25 %>% filter(Jurisdiction == 'Washington')
plot.ts(df_wa_25$Cases)
plot.ts(data_24_ts_forecast_start['Washington'])

df_nyc_25 = df_25 %>% filter(Jurisdiction == 'NewYorkCity')
plot.ts(df_nyc_25$Cases)
plot.ts(data_24_ts_forecast_start['NewYorkCity'])

df_sd_25 = df_25 %>% filter(Jurisdiction == 'SanDiego')
plot.ts(df_sd_25$Cases)
plot.ts(data_24_ts_forecast_start['SanDiego'])

df_la_25 = df_25 %>% filter(Jurisdiction == 'LA')
plot.ts(df_la_25$Cases)
plot.ts(data_24_ts_forecast_start['LA'])

df_tex_25 = df_25 %>% filter(Jurisdiction == 'Texas')
plot.ts(df_tex_25$Cases)
plot.ts(data_24_ts_forecast_start['Texas'])


df_ill_25 = df_25 %>% filter(Jurisdiction == 'Illinois')
plot.ts(df_ill_25$Cases)
plot.ts(data_24_ts_forecast_start['Illinois'])


#*****************
#* PLOT TOTAL WEEKLY

library(dplyr)
library(ggplot2)

# Summarize total weekly cases across all jurisdictions
weekly_totals <- data_mpox %>%
  group_by(date_week_start) %>%
  summarise(total_cases = sum(Cases, na.rm = TRUE))

# Plot
ggplot(weekly_totals, aes(x = date_week_start, y = total_cases)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Weekly Total Mpox Cases Across All Jurisdictions",
    x = "Week Start Date",
    y = "Total Cases"
  ) +
  theme_minimal()



# Make sure date_week_start is a Date
data_mpox$date_week_start <- as.Date(data_mpox$date_week_start)

# Summarize weekly cases per jurisdiction
weekly_jurisdiction <- data_mpox %>%
  group_by(Jurisdiction, date_week_start) %>%
  summarise(total_cases = sum(Cases, na.rm = TRUE), .groups = "drop")

# Custom colors for specific jurisdictions
custom_colors <- c(
  "California" = "palegreen4",      # Pine green
  "NYC" = "red",
  "Texas" = "purple",
  "Florida" = "yellow",
  "LA" = "blue",
  "Georgia" = "magenta",            # Fuchsia pink
  "Illinois" = "cyan",
  "New Jersey" = "orange",          # light orange
  "North Carolina" = "darkorange",
  "Maryland" = "greenyellow",       # highlighter green
  "San Diego" = "black"
)

# Identify jurisdictions not in custom_colors
other_jurisdictions <- setdiff(unique(weekly_jurisdiction$Jurisdiction), names(custom_colors))

# Assign random colors to the other jurisdictions
if(length(other_jurisdictions) > 0){
  custom_colors <- c(custom_colors, setNames(rainbow(length(other_jurisdictions)), other_jurisdictions))
}

# Plot all time series together
ggplot(weekly_jurisdiction, aes(x = date_week_start, y = total_cases, color = Jurisdiction)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Weekly Mpox Cases Across All Jurisdictions",
    x = "Week Start Date",
    y = "Weekly Cases"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


#**************************
#* PLOT

# make sure time variable exists
df_mpox <- df_mpox %>%
  group_by(Jurisdiction) %>%
  mutate(timepoint = row_number())

# define your color palette
list_colors <- c(
  "California" = "palegreen4",     # pine green
  "NYC" = "red",
  "Texas" = "purple",
  "Florida" = "yellow",
  "LA" = "blue",
  "Georgia" = "magenta",
  "Illinois" = "cyan",
  "New Jersey" = "orange",
  "North Carolina" = "darkorange",
  "Maryland" = "greenyellow",
  "San Diego" = "black"
)

# add random rainbow colors for any extra jurisdictions
other_jurisdictions <- setdiff(unique(df_mpox$Jurisdiction), names(list_colors))
if (length(other_jurisdictions) > 0) {
  list_colors <- c(list_colors, setNames(rainbow(length(other_jurisdictions)), other_jurisdictions))
}

# plot: thin lines + larger filled dots
ggplot(df_mpox, aes(x = timepoint, y = Cases, color = Jurisdiction, fill = Jurisdiction)) +
  geom_line(linewidth = 0.7) +                     # thinner lines
  geom_point(size = 3.2, shape = 21) +             # filled circles
  scale_color_manual(values = list_colors) +
  scale_fill_manual(values = list_colors) +        # ensures fill matches line
  labs(
    title = "Mpox Reported Cases across 56 Unique States & Cities",
    x = "Time",
    y = "Case Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5)
  )


#******
#* PLOT


# Make sure date_week_start is a Date
data_mpox$date_week_start <- as.Date(data_mpox$date_week_start)

# Summarize weekly cases per jurisdiction
weekly_jurisdiction <- data_mpox %>%
  group_by(Jurisdiction, date_week_start) %>%
  summarise(total_cases = sum(Cases, na.rm = TRUE), .groups = "drop")

# Custom colors
custom_colors <- c(
  "California" = "palegreen4",      # Pine green
  "NYC" = "#E41A1C",                # vivid red
  "Texas" = "purple",
  "Florida" = "yellow",
  "LA" = "blue",
  "Georgia" = "magenta",            # Fuchsia pink
  "Illinois" = "cyan",
  "New Jersey" = "orange",          # light orange
  "North Carolina" = "darkorange",
  "Maryland" = "greenyellow",       # highlighter green
  "San Diego" = "black"
)

# Add colors for any others
other_jurisdictions <- setdiff(unique(weekly_jurisdiction$Jurisdiction), names(custom_colors))
if (length(other_jurisdictions) > 0) {
  custom_colors <- c(custom_colors, setNames(rainbow(length(other_jurisdictions)), other_jurisdictions))
}

# Plot: thinner lines, filled points, larger axis labels
ggplot(weekly_jurisdiction, aes(x = date_week_start, y = total_cases, color = Jurisdiction, fill = Jurisdiction)) +
  geom_line(linewidth = 0.7) +                 # slim lines
  geom_point(size = 3, shape = 21) +           # filled circles
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Weekly Mpox Cases Across All Jurisdictions",
    x = "Year",
    y = "Weekly Case count per U.S. jurisdiction"
  ) +
  theme_minimal(base_size = 16) +              # larger base font
  theme(
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 15), 
    axis.text.Y = element_text(size = 15), 
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.position = "right"
  )

