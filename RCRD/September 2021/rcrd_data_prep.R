#------------ 1. LOAD LIBRARIES --------------

library(tidyverse)
library(zoo)
library(stats)
library(openxlsx)
library(readxl)

#----------- 2. LOAD DATA ------------------

## Data downloaded from here: https://www.cancerdata.nhs.uk/covid-19/rcrd
## Demographic factors > Downloads > second link ("Download time trend data for all cancer groups and demographic factors")


#The datasets should be changed to the latest two, where 'new' is the latest dataset available
rcrd_data <- read_csv("C:\\Users\\slapko01\\OneDrive - Cancer Research UK\\Downloads\\Incidence_Treatment_statistics_England.csv") 

wd <- read_excel("C:\\RProjects\\myrepo\\WorkingDays.xlsx")
  
#----------- 2. CLEAN DATA ------------------

rcrd_data$Date <- as.yearmon(paste(rcrd_data$Month, rcrd_data$Year, sep = " "))
wd$Date <- as.yearmon(paste(wd$Month, wd$Year, sep = " "))

rcrd_data <- rcrd_data %>% 
  filter(Metric == "New cancer diagnoses (observed)", 
         Breakdown %in% c("Route to Diagnosis", "Stage at diagnosis"), 
         Year != "2018") %>% 
  select(- "Data completeness", -"Metric")


stage <- rcrd_data %>% 
  filter(Breakdown == "Stage at diagnosis", 
         `Cancer group` != "Other malignant neoplasms") %>%
  select(-"Breakdown") 

stage2019 <- stage %>%
  filter(Year == "2019") %>%
  select(-"Year", - "Date") %>%
  rename(Stat2019 = "Statistic")

stage <- left_join(stage, stage2019, by = c("Month", "Cancer group", "Demographic"), keep = FALSE)

stageCalcs <- left_join(stage, wd, by = "Date", keep = FALSE) %>%
  mutate(Statistic2019WdAdj = Stat2019*wd2019AdjRatio, 
         nDiff = Statistic - Statistic2019WdAdj, 
         propChange = nDiff / Statistic2019WdAdj) %>%
  select(Date:Statistic, Statistic2019WdAdj, nDiff, propChange)

stageCalcs$Breakdown <- "Stage at diagnosis"

route <- rcrd_data %>% 
  filter(Breakdown == "Route to Diagnosis", 
         `Cancer group` != "Other malignant neoplasms") %>%
  select(-"Breakdown") 

route2019 <- route %>%
  filter(Year == "2019") %>%
  select(-"Year", - "Date") %>%
  rename(Stat2019 = "Statistic")

route <- left_join(route, route2019, by = c("Month", "Cancer group", "Demographic"), keep = FALSE)

routeCalcs <- left_join(route, wd, by = "Date", keep = FALSE) %>%
  mutate(Statistic2019WdAdj = Stat2019*wd2019AdjRatio, 
         nDiff = Statistic - Statistic2019WdAdj, 
         propChange = nDiff / Statistic2019WdAdj) %>%
  select(Date:Statistic, Statistic2019WdAdj, nDiff, propChange)

routeCalcs$Breakdown <- "Route to Diagnosis"

rcrd_clean <- rbind(routeCalcs, stageCalcs)

# ------------- 4. SAVE AS EXCEL FILE - TO BE WRITTEN OVER RAW DATA IN EXCEL SPREADSHEET -----------
write.csv(rcrd_clean, "rcrd_comp_raw_Sept2021.csv")
