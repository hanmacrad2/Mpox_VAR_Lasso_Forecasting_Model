#5 DATA COMBINE + SUBSET

#SUBSET
list_jur = c("NewYorkCity", "Texas", "Florida", "Illinois", "Georgia",
             "Washington", "LA", "SanDiego")


data_mpox_sub = data_mpox_tot %>% filter(Jurisdiction %in% list_jur)
