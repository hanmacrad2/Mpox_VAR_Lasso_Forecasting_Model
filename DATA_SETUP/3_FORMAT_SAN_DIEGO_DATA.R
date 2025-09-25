#**********************************************
#*
#* DATA - FORMAT SAN DIEGO DATA
#* 
#*********************************************
library(readxl)
library(writexl)

#SD
file_name = 'data_sd.xlsx'
data_sd = read_xlsx(paste0(DATA_FOLDER, file_name))