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
