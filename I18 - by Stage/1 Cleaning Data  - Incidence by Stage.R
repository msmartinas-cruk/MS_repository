#
# ======================================= #
# INCIDENCE BY STAGE - DATA PREPARATION
# ======================================= #
# Created by: Martina Slapkova (September 2021)
#
# Data requirements:
# - ideally by year or 2-3-yearly combined incidence if data too granular
# - National incidence: by year, stage, sex, 5-year age bands, cancer site (as granular as possible but depends on data availability)
# - Cancer Alliance incidence: by stage, sex, 5-year age bands, cancer site

# "NCRAS Cancer Registration Data Dictionary v4.4" was used for interpretation of variables and values (STAGE_BEST, SEX)


#-------------- 1. Data and libraries import  ---------------



rm(list=ls())

# load packages
library(tidyverse)
library(readxl)
library(readr)
library(naniar)

# Load data (Cancer Alliance data is quite large so will take some time to load)

national_data <- 
  read_excel("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Indicators\\Incidence\\GN0001-England\\SN0102-2018\\4_Working\\SN0102_ODR 1819_060A2_Incidence14to18_byStage.xlsx")

ca_data <- 
  read_excel("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Indicators\\Incidence\\GN0001-England\\SN0102-2018\\4_Working\\SN0102_ODR 1819_060A2_Incidence14to18_byStage_byCanAlliance.xlsx")

######################## NATIONAL DATA #################################

# Will need to remove Stage 0 data and possibly A/B/C - can this be categorised as something else?

#-------------- 2. Clean up of STAGE_BEST variable ---------------

# Staging information as per NCRAS  ODR data dictionary:
#stage_0: "0","0A","0B","0C","0IS")
#stage_1: "1","1A","1A1", "1A2", "1A3", "1AE", "1AES", "1AEX", "1AEXS", "1AS", "1AX", "1B", "1B1", "1B2", "1BE", "1BES", "1BEX", "1BS", "1BX", "1C", "1C1", "1C2", "IC3", "1E", "1ES", "1EX", "1S", "1X", "1XS")
#stage_2: "2","2A","2A1", "2A2", "2AE", "2AES", "2AEX", "2AS", "2AX", "2B", "2BE", "2BEX", "2BS", "2BX", "2C", "2E", "2ES", "2EX", "2S", "2X")
#stage_3: "3","3A","3A1", "3A1i", "3A1ii", "3A2", "3AE", "3AES", "3AEX", "3AS", "3AX", "3B", "3BE", "3BES", "3BEX", "3BEXS", "3BS", "3BX", "3BXS", "3C", "3C1", "3C2", "3D",  "3E", "3ES", "3EX", "3S", "3X", "3XS")
#stage_4: "4","4A","4AE", "4AES", "4AEX", "4AEXS", "4AS", "4AX", "4AXS", "4B", "4BE", "4BES", "4BEX", "4BEXS", "4BS", "4BX", "4BXS", "4BS", "4BX", "4BXS", "4C", "4E", "4ES", "4EX", "4EXS", "4S", "4X")
# Outdated or invalid code: 5
# Not stageable: 6
# Insufficient information: ? 
# Unstageable: U  
# Not staged: X
# RaiBinet stage: A, B, C = RaiBinet stage

# For the purposes of the analysis, codes 5,6,?, U, X, NA(missing) will be marked as 'Unknown' stage

# Adding a column with higher-level staging and removing all but first character
clean_national <- national_data %>% 
  mutate(stage = substr(STAGE_BEST,1,1))

# Creating a group of unknown values 
clean_national$stage <- recode(clean_national$stage, 
                         "?" = "Unknown", 
                         "6" = "Unknown", 
                         "I" = "Unknown", 
                         "5" = "Unknown", 
                         "U" = "Unknown", 
                         "X" = "Unknown", 
                         "NA" = "Unknown")

# Replacing NAs with Unknown
clean_national$stage <- clean_national$stage %>% replace_na("Unknown") 

clean_national %>% group_by(stage) %>% summarise(count = n())


# QUESTIONS TO ANSWER FOR STAGING DATA PREP
# - what is IC2? Categorized under Unknown
# - Do we want to keep ABC?
# - Do we want to keep in situ?
# - How granular the stage data?


#-------------- 3. Clean up of SEX variable ---------------

# Sex of the patient when the tumour was diagnosed.
# 0=Not known, 1=Male, 2=Female, 9=Not specified.

clean_national$sex <- recode(clean_national$SEX, 
                               "0" = "Unknown", 
                               "1" = "Male", 
                               "2" = "Female", 
                               "9" = "Not specified")

clean_national %>% group_by(sex) %>% summarise(count = n())


#-------------- 3. Clean up of SITE_ICD10_O2GROUP variable ---------------

# Data includes Level 5 sites (e.g. Bowel cancer C18-C20 split into bowel, rectosigmoid junction and rectum)
# Picked CancerSiteKey from the ICD dimensions dataset with the most detailed code and Level 3 description, then matched the appropriate L3 code 

clean_national %>% group_by(SITE_ICD10_O2GROUP) %>% summarise(count = n())

icd_10_raw <- read_excel("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Dimensions\\Dim_CancerSite.xlsx") 

icd_10_l5 <- icd_10_raw %>%
  select(CancerSiteKey, CancerSiteLevel3Desc, CancerSiteLevel3Code) 

icd_10_l5 <- icd_10_l5 %>% distinct() %>% rename("SITE_ICD10_O2GROUP" = "CancerSiteKey")

clean_national <- left_join(clean_national, icd_10_l5, by = "SITE_ICD10_O2GROUP")

table(clean_national$CancerSiteLevel3Desc)


#-------------- 4A. Creating final dataset & checking for granularity ---------------

IncidenceByStage_national <- clean_national %>% 
  select(DIAGNOSISYEAR, CancerSiteLevel3Code, CancerSiteLevel3Desc, FIVEYEARAGEBAND, stage, sex, INCIDENCE) %>%
  rename("STAGE" = "stage", 
         "SEX" = "sex")

#Check there are no missing values
miss_var_summary(IncidenceByStage_national)

# Grouping Level 3 sites 
IncidenceByStage_national <- IncidenceByStage_national %>% 
  group_by(DIAGNOSISYEAR, CancerSiteLevel3Code, CancerSiteLevel3Desc, FIVEYEARAGEBAND, STAGE, SEX) %>% 
  filter(STAGE != "0") %>%
  summarise(Incidence_sum = sum(INCIDENCE))


write.csv(IncidenceByStage_national, "C:\\Users\\slapko01\\WORKING FILES\\Projects\\I18 - stage\\Incidence_by_stage_national.csv")

######################## CANCER ALLIANCE DATA #################################

# Will need to remove Stage 0 data and possibly A/B/C - can this be categorised as something else?

#-------------- 2. Clean up of STAGE_BEST variable ---------------

# Staging information as per NCRAS  ODR data dictionary:
#stage_0: "0","0A","0B","0C","0IS")
#stage_1: "1","1A","1A1", "1A2", "1A3", "1AE", "1AES", "1AEX", "1AEXS", "1AS", "1AX", "1B", "1B1", "1B2", "1BE", "1BES", "1BEX", "1BS", "1BX", "1C", "1C1", "1C2", "IC3", "1E", "1ES", "1EX", "1S", "1X", "1XS")
#stage_2: "2","2A","2A1", "2A2", "2AE", "2AES", "2AEX", "2AS", "2AX", "2B", "2BE", "2BEX", "2BS", "2BX", "2C", "2E", "2ES", "2EX", "2S", "2X")
#stage_3: "3","3A","3A1", "3A1i", "3A1ii", "3A2", "3AE", "3AES", "3AEX", "3AS", "3AX", "3B", "3BE", "3BES", "3BEX", "3BEXS", "3BS", "3BX", "3BXS", "3C", "3C1", "3C2", "3D",  "3E", "3ES", "3EX", "3S", "3X", "3XS")
#stage_4: "4","4A","4AE", "4AES", "4AEX", "4AEXS", "4AS", "4AX", "4AXS", "4B", "4BE", "4BES", "4BEX", "4BEXS", "4BS", "4BX", "4BXS", "4BS", "4BX", "4BXS", "4C", "4E", "4ES", "4EX", "4EXS", "4S", "4X")
# Outdated or invalid code: 5
# Not stageable: 6
# Insufficient information: ? 
# Unstageable: U  
# Not staged: X
# RaiBinet stage: A, B, C = RaiBinet stage

# For the purposes of the analysis, codes 5,6,?, U, X, NA(missing) will be marked as 'Unknown' stage

# Adding a column with higher-level staging and removing all but first character
clean_ca <- ca_data %>% 
  mutate(stage = substr(STAGE_BEST,1,1))

# Creating a group of unknown values 
clean_ca$stage <- recode(clean_ca$stage, 
                               "?" = "Unknown", 
                               "6" = "Unknown", 
                               "I" = "Unknown", 
                               "5" = "Unknown", 
                               "U" = "Unknown", 
                               "X" = "Unknown", 
                               "NA" = "Unknown")

# Replacing NAs with Unknown
clean_ca$stage <- clean_ca$stage %>% replace_na("Unknown") 

clean_ca %>% group_by(stage) %>% summarise(count = n())


# QUESTIONS TO ANSWER FOR STAGING DATA PREP
# - what is IC2? Categorized under Unknown
# - Do we want to keep ABC?
# - Do we want to keep in situ?
# - How granular the stage data?


#-------------- 3. Clean up of SEX variable ---------------

# Sex of the patient when the tumour was diagnosed.
# 0=Not known, 1=Male, 2=Female, 9=Not specified.

clean_ca$sex <- recode(clean_ca$SEX, 
                             "0" = "Unknown", 
                             "1" = "Male", 
                             "2" = "Female", 
                             "9" = "Not specified")

clean_ca %>% group_by(sex) %>% summarise(count = n())


#-------------- 3. Clean up of SITE_ICD10_O2GROUP variable ---------------

# Data includes Level 5 sites (e.g. Bowel cancer C18-C20 split into bowel, rectosigmoid junction and rectum)
# Picked CancerSiteKey from the ICD dimensions dataset with the most detailed code and Level 3 description, then matched the appropriate L3 code 

clean_ca %>% group_by(SITE_ICD10_O2GROUP) %>% summarise(count = n())

icd_10_raw <- read_excel("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Dimensions\\Dim_CancerSite.xlsx") 

icd_10_l5 <- icd_10_raw %>%
  select(CancerSiteKey, CancerSiteLevel3Desc, CancerSiteLevel3Code) 

icd_10_l5 <- icd_10_l5 %>% distinct() %>% rename("SITE_ICD10_O2GROUP" = "CancerSiteKey")

clean_ca <- left_join(clean_ca, icd_10_l5, by = "SITE_ICD10_O2GROUP")

table(clean_ca$CancerSiteLevel3Desc)


#-------------- 4A. Creating final dataset & checking for granularity ---------------

IncidenceByStage_ca <- clean_ca %>% 
  select(DIAGNOSISYEAR, CANALLIANCE_2019_NAME, CancerSiteLevel3Code, CancerSiteLevel3Desc, FIVEYEARAGEBAND, stage, sex, INCIDENCE) %>%
  rename("STAGE" = "stage", 
         "SEX" = "sex")

#Check there are no missing values
miss_var_summary(IncidenceByStage_ca)

# Grouping Level 3 sites and removing Stage 0
IncidenceByStage_ca <- IncidenceByStage_ca %>% 
  group_by(DIAGNOSISYEAR, CANALLIANCE_2019_NAME, CancerSiteLevel3Code, CancerSiteLevel3Desc, FIVEYEARAGEBAND, STAGE, SEX) %>% 
  filter(STAGE != "0") %>%
  summarise(Incidence_sum = sum(INCIDENCE))


write.csv(IncidenceByStage_ca, "C:\\Users\\slapko01\\WORKING FILES\\Projects\\I18 - stage\\Incidence_by_stage_ca.csv")



