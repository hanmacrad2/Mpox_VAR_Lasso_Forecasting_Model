#****************************************************
# SETUP
#****************************************************

#LIBRARIRES
library(dplyr); library(tidyr); library(stringr); library(ggplot2)
library(caret) 
library(vars); library(tseries); library(forecast); library(BigVAR)
library(tidyverse); library(ggh4x); library(lubridate)
library(zoo); library(forcats); library(scales)
library(viridis)
library(surveillance)
library(stringr)

#SOURCE CODE
FOLDER_FINAL = "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/CODE_FINAL/"
FOLDER_UTILS = 'C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/PROJECT_CODE/'

#FILES
file1 = 'UTIL_FUNCTIONS.R'; 
source(paste0(FOLDER_UTILS, file1))

file2 = 'UTIL_FUNCTIONS_VAR.R'
source(paste0(FOLDER_FINAL, file2))

file3 = 'UTIL_FUNCTIONS_PERFORMANCE.R'
source(paste0(FOLDER_FINAL, file3))

file7 = '4B_NAIVE_ESTIMATE.R'
source(paste0(FOLDER_FINAL, file7))

file8 = 'METRICS_PERFORMANCE.R'
source(paste0(FOLDER_FINAL, file8))

#FINAL CODE 

file4 = '5_PLOT_FUNCTIONS.R'
source(paste0(FOLDER_FINAL, file4))

file5 = 'PLOT_FUNCTIONS.R'
source(paste0(FOLDER_FINAL, file5))

