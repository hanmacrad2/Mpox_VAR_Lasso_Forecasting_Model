
#DATA CHECK

#OUR DATA
data_mpox_24 = data_mpox %>% filter(Year == 2024)

url <- "https://oss.resilientservice.mooo.com/resilentpublic/pathogens/cdc/nndss/output/mpox_weekly.csv"
mpox_data <- read.csv(url)
head(mpox_data)

#DATA BY YEAR 
#2025 Data
df_25 = mpox_data %>% filter(year == 2025)
#2024 DATA
df_24 = mpox_data %>% filter(year == 2024)

#MATCH
df_24_v2 = df_24 %>% filter(week < 47)

#Locations
unique(df_24_v2$location1)

#***********
#NYC DATA
df_nyc_25 = df_25 %>% filter(location1 == 'New York City')
df_nyc_24 = df_24_v2 %>% filter(location1 == 'NEW YORK CITY')

#PLOT
plot.ts(df_nyc_25$current_week, main = 'NYC, 2025 Weekly Data')

#BAR PLOT
barplot(df_nyc_25$current_week,
        names.arg = df_nyc_25$week,
        main = "Online Weekly Data - NYC 2025",
        xlab = "Current Week",
        ylab = "Cases",
        col = "black")
        #las = 2)



#***********
#Illinois DATA
df_ill_25 = df_25 %>% filter(location1 == 'Illinois')


df_ill_25 <- df_ill_25 %>%
  arrange(week) %>%  # ensure correct order
  mutate(weekly_data = previous_YTD__cummulative - lag(previous_YTD__cummulative, default = 0))

#BAR PLOT
barplot(df_ill_25$weekly_data,
        names.arg = df_nyc_25$week,
        main = "Online Weekly Data - Illinois 2025",
        xlab = "Week",
        ylab = "Cases (using previous_ytd_cumul)",
        col = "black")
#las = 2)




#OUR DATA
data_mpox_24_nyc = data_mpox %>% filter(Year == 2024)
data_mpox_24_nyc = data_mpox_24_nyc %>% filter(Jurisdiction == 'NewYorkCity')
sum(data_mpox_24_nyc$Cases)

#PloT
plot.ts(data_mpox_24_nyc$Cases, ylim = c(0,20), ylab = 'Cases',
        main = 'New York City weekly data comparison; our data (black); using current_week for online data (red)')

lines(df_nyc_24$current_week, col = 'red')

#*****
#Illinois DATA
df_ill_24 = df_24_v2 %>% filter(location1 == 'ILLINOIS')


#out data Illinois
data_mpox_24_ill = data_mpox_24 %>% filter(Jurisdiction == 'Illinois')
sum(data_mpox_24_ill$Cases)

#Plot
plot.ts(data_mpox_24_ill$Cases, ylim = c(0,20), ylab = 'Cases',
        main = 'Illinois weekly data comparison; our data (black); using current_week for online data (red)')

lines(df_ill_24$current_week, col = 'red')

#****************************

#TEXAS

#Illinois DATA
df_tex_24 = df_24_v2 %>% filter(location1 == 'TEXAS')


#out data Illinois
data_mpox_24_tex = data_mpox_24 %>% filter(Jurisdiction == 'Texas')
sum(data_mpox_24_tex$Cases)



#TEXAS
plot.ts(data_mpox_24_tex$Cases, ylim = c(0,20), ylab = 'Cases',
        main = 'Texas weekly data comparison; our data (black); using current_week for online data (red)')

lines(df_tex_24$current_week, col = 'red')


