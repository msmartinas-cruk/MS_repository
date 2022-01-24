#==================================================#
# CAN06-07
# Cancer Waiting times - USCR
# Created by Martina Slapkova - 21 January 2022
#==================================================#

# Packages ----------------------------------------------------------------

library(tidyverse)
library(statswalesr)
library(janitor)
library(ggplot2)

setwd("~/MS_repository")

# Pull data for 62 and 31 days from StatsWales - commented to avoid re-downloading. Accessed 21 January 2022.
data62 <- statswales_get_dataset("hlth0050") 
data31 <- statswales_get_dataset("hlth0055")

# Clean 62-day data
select_data <- data62 %>% 
  filter(LHBProvider_ItemName_ENG == "Wales") %>%
  select (Data, Measure_ItemName_ENG, Date_Code, Date_ItemName_ENG, Site_ItemName_ENG) %>%
  rename(value = Data, 
         measure = Measure_ItemName_ENG, 
         date = Date_ItemName_ENG, 
         site = Site_ItemName_ENG)

select_data$month <- substr(select_data$Date_Code, start = 6, stop = 7)
select_data$year <- substr(select_data$Date_Code, start = 1, stop = 4)
select_data$Date <- as.Date(paste("01", select_data$month, select_data$year, sep = "-"),"%d-%m-%Y")


### Colorectal cancer

## 1. Preparing CRC data

# 1a. USCR & 62 days - Creating subset for CRC 
crc_data <- select_data %>% 
  filter(site == "Lower Gastrointestinal", 
         Date >= "2019-01-01") %>%
  select (date, Date, measure, value) %>%
  group_by(measure, Date, date) %>%
  summarise(count = sum(value)) %>%
  spread(measure, count)


crc_data$year <- substr(crc_data$Date, 1,4)
crc_data$month <- substr(crc_data$Date, 6,7)

crc_data <- crc_data %>% 
  rename(total_uscr_received = `Total number of urgent suspected cancer referrals received and confirmed urgent by the specialist`,
         total_starting_treatment = `Total starting treatment`, 
         started_treatment_within_62 = `Total starting treatment within 62 days`) %>%
  select(Date, year, month,total_uscr_received, total_starting_treatment, started_treatment_within_62) %>%
  mutate(prop_started_within_62 =  (started_treatment_within_62 / total_starting_treatment)*100)


# Create standard deviation - SDs by year
d2019 <- crc_data %>% 
  filter(year == 2019) 

d2019$sd_total_uscr <- sd(d2019$total_uscr_received)
d2019$sd_prop_within_62 <- sd(d2019$prop_started_within_62)

d2020 <- crc_data %>% 
  filter(year == 2020) 

d2020$sd_total_uscr <- sd(d2020$total_uscr_received)
d2020$sd_prop_within_62 <- sd(d2020$prop_started_within_62)

crc_uscr <- rbind(d2019, d2020)


## 2. Plotting data

# 1A. Plotting number of total UCS referrals with separate SDs for 2019 and 2020
ggplot(crc_uscr, aes(month, total_uscr_received, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=total_uscr_received-sd_total_uscr, ymax=total_uscr_received+sd_total_uscr), width=.2) + 
  labs(title = "Monthly number of UCSR referrals", x = "Month", y = "Number of referrals") + 
  scale_y_continuous(limit = c(0, 4000)) 

# 1B. Plotting number of total UCS referrals with 2019 SDs
crc_uscr_2019SD <- crc_data
crc_uscr_2019SD$sd_total_uscr <- 247.4561 #taken from d2019 data frame
crc_uscr_2019SD$sd_prop_within_62 <- 4.479825 #taken from d2019 data frame

ggplot(crc_uscr_2019SD, aes(month, total_uscr_received, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=total_uscr_received-sd_total_uscr, ymax=total_uscr_received+sd_total_uscr), width=.2) + 
  labs(title = "Monthly number of UCSR referrals", x = "Month", y = "Number of referrals") + 
  scale_y_continuous(limit = c(0, 4000)) 


# 2A. Plotting proportion of USCR referrals within 62 days with separate SDs for 2019 and 2020
ggplot(crc_uscr, aes(month, prop_started_within_62, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=prop_started_within_62-sd_prop_within_62, ymax=prop_started_within_62+sd_prop_within_62), width=.2) +
  labs(title = "Proportion of UCSR referrals starting treatment within 62 days", x = "Month", y = "Number of referrals") + 
  scale_y_continuous(limit = c(0, 100)) 


# 2B. Plotting proportion of USCR referrals within 62 days with 2019 SDs

ggplot(crc_uscr_2019SD, aes(month, prop_started_within_62, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=prop_started_within_62-sd_prop_within_62, ymax=prop_started_within_62+sd_prop_within_62), width=.2) + 
  labs(title = "Proportion of UCSR referrals starting treatment within 62 days", x = "Month", y = "Number of referrals") + 
  scale_y_continuous(limit = c(0, 100)) 

## 3. Data tables

#TO DO - need to add 2020 monthly mean - just for April - November?

uscr_2019_comparison <- d2020 %>%
  select(Date:prop_started_within_62)

uscr_2019_comparison$uscr_2019 <- mean(d2019$total_uscr_received) # calculating monthly mean for 2019 
uscr_2019_comparison$uscr_sd_2019 <- sd(d2019$total_uscr_received) # calculating monthly SD for 2019 
uscr_2019_comparison$uscr_prop_reduction <- 
  ((uscr_2019_comparison$uscr_2019-uscr_2019_comparison$total_uscr_received)/uscr_2019_comparison$uscr_2019)*100 # calculating percent change 2019 vs 2020


uscr_2019_comparison$within62_2019 <- mean(d2019$started_treatment_within_62) # calculating monthly mean for 2019 
uscr_2019_comparison$within62_sd_2019 <- sd(d2019$started_treatment_within_62) # calculating monthly SD for 2019 
uscr_2019_comparison$within62_prop_reduction <- 
  ((uscr_2019_comparison$within62_2019-uscr_2019_comparison$started_treatment_within_62)/uscr_2019_comparison$within62_2019)*100 # calculating percent change 2019 vs 2020


uscr_2019_comparison$total_starting_treatment_2019 <- mean(d2019$total_starting_treatment) # calculating monthly mean for 2019 
uscr_2019_comparison$total_starting_treatment_sd_2019 <- sd(d2019$total_starting_treatment) # calculating monthly SD for 2019 
uscr_2019_comparison$total_starting_treatment_prop_reduction <- 
  ((uscr_2019_comparison$total_starting_treatment_2019-uscr_2019_comparison$total_starting_treatment)/uscr_2019_comparison$total_starting_treatment_2019)*100 # calculating percent change 2019 vs 2020



###### LUNG
lung_data
