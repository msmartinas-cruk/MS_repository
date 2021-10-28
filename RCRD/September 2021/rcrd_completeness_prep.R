#------------ 1. LOAD LIBRARIES --------------

library(tidyverse)
library(zoo)
library(stats)
library(openxlsx)

#----------- 2. LOAD DATA ------------------

#The datasets should be changed to the latest two, where 'new' is the latest dataset available
raw_old <- read_csv("Raw/Incidence_Treatment_statistics_England_Aug21.csv") %>% 
  mutate(data_type = "old") %>% 
  filter(Metric == "New cancer diagnoses (observed)") %>% 
  rename(old = Statistic) %>% 
  select(-data_type,-"Date", - "Data completeness")

raw_new <- read_csv("Raw/Incidence_Treatment_statistics_England_Sep21.csv") %>% 
  mutate(data_type = "new") %>% 
  filter(Metric == "New cancer diagnoses (observed)") %>%
  rename(new = Statistic) %>%
  select(-data_type, -"Date", - "Data completeness")


# ------------- 3. CREATE A DATA FRAME FOR AN EXCEL FLAT FILE --------------
data <- left_join(raw_new, raw_old)

rm(raw_new,raw_old)

data$Date <- as.yearmon(paste(data$Month, data$Year, sep = " "))

data <- data %>% 
  select(Date, `Cancer group`:old) %>%
  filter(Date >= "Jan 2019")

rcrd_demo_data <- data %>% 
  mutate(
    ndiff_newOld = new - old,
    propchange_newOld = round(ndiff_newOld/old,2))

# ------------- 4. SAVE AS EXCEL FILE - TO BE WRITTEN OVER RAW DATA IN EXCEL SPREADSHEET -----------
write.csv(rcrd_demo_data, "rcrd_comp_raw_Sept2021.csv")
