#****************************************************
#1. GET DATA
#****************************************************

#SOURCE CODE
FOLDER_CODE = 'C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/CODE_FINAL/'
source(paste0(FOLDER_CODE, 'SETUP.R'))
source(paste0(FOLDER_CODE, '4_VAR_LASSO_MODEL.R')) 

#DATA
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_UPDATE_25_JUNE_25/WEEK_46_2024/"
data_mpox = readRDS(paste0(DATA_FOLDER, 'data_mpox.rds')) 

data_mpox <- data_mpox %>%
  mutate(date_week_start = as.Date(date_week_start))

#**************************************
#* 3. SMOOTH THE DATA
#************************************
ROLL_WINDOW = 4

df_smooth <- data_mpox %>%
  group_by(Jurisdiction) %>%
  mutate(
    Cases = rollmean(Cases, k = ROLL_WINDOW, fill = NA, align = "center")
  ) %>%
  ungroup() %>%
  filter(!is.na(Cases))

plot.ts(df_smooth$Cases[df_smooth$Jurisdiction == 'SanDiego'])


#****************************************************
# TOP 10 (IN 2023 & 2024)

list_jur = c("NewYorkCity", "Texas", "LA", "Florida", "Illinois", "Georgia",
            "SanDiego", "Washington")
# 
# list_jur = c("NewYorkCity", "California", "Texas", "LA", "Florida", "Illinois", "Georgia",
#              "SanDiego", "Washington")

#list_jur = c("NewYorkCity", "Texas", "Florida", "Illinois", "Georgia")
#list_jur = c("Texas", "Florida", "Georgia")
# list_jur = c("NewYorkCity", "California", "Texas", "Florida", "LA", "Georgia", "Illinois", "NewJersey", "Maryland", "NorthCarolina", "SanDiego")

#SMOOTH DATA
df_mpox_smooth = df_smooth %>% filter(Jurisdiction %in% list_jur)
length(unique(df_mpox_smooth$Jurisdiction))
unique(df_mpox_smooth$Jurisdiction)
df_mpox_smooth = df_mpox_smooth %>% filter(Year >= 2023)

#ORIGINAL DATA 
data_mpox_model = data_mpox %>% filter(Jurisdiction %in% list_jur)
length(unique(data_mpox_model$Jurisdiction))
data_mpox_model = data_mpox_model %>% filter(Year >= 2023)






#CHECKS
df_mpox_smooth_x = df_mpox_smooth %>% filter(Jurisdiction == 'NewYorkCity')
plot.ts(df_mpox_smooth_x$Cases)
plot.ts(df_mpox_smooth$Cases[df_mpox_smooth$Jurisdiction == 'SanDiego'])
points(df_mpox_smooth$Cases[df_mpox_smooth$Jurisdiction == 'SanDiego'])



