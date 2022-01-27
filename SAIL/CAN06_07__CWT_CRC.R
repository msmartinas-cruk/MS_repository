#==================================================#
# CAN06-07 CWT - Colorectal cancer
# Cancer Waiting times - USCR
# Created by Martina Slapkova - 21 January 2022
#==================================================#

# Packages ----------------------------------------------------------------

library(tidyverse)
library(statswalesr)
library(janitor)
library(ggplot2)
library(formattable)
library(zoo)

setwd("~/MS_repository")


###### CLEANING OLD DATA - USCR & 62 + 31 day wait #####

# Pull data for 62 and 31 days from StatsWales - commented to avoid re-downloading. Accessed 21 January 2022.
# old_data62 <- statswales_get_dataset("hlth0050") 
#old_data31 <- statswales_get_dataset("hlth0049")
# new_data <- statswales_get_dataset("hlth0055")


# Load datasets
old_data62 <- readRDS("data62.rds")
old_data31 <- readRDS("old_data31.rds")
new_data <- readRDS("new_data.rds")


# * caveat re.the data: fhanges in coding of data from December 2020 onwards, 
#   the number of USCR referrals is no longer recorded.


# Clean 62-day data
select_data62 <- old_data62 %>% 
  filter(LHBProvider_ItemName_ENG == "Wales", 
         Site_ItemName_ENG == "Lower Gastrointestinal") %>%
  select (Data, Measure_ItemName_ENG, Date_Code, Date_ItemName_ENG) %>%
  rename(value = Data, 
         measure = Measure_ItemName_ENG, 
         date = Date_ItemName_ENG) %>%
  spread(measure, value)

# Clean 31-day data
select_data31 <- old_data31 %>% 
  filter(LHBProvider_ItemName_ENG == "Wales", 
         Site_ItemName_ENG == "Lower Gastrointestinal") %>%
  select (Data, Measure_ItemName_ENG, Date_Code, Date_ItemName_ENG) %>%
  rename(value = Data, 
         measure = Measure_ItemName_ENG, 
         date = Date_ItemName_ENG) %>%
  spread(measure, value)

# join the two datasets
old_data <- left_join(select_data62, select_data31, by = "date") # joining 31 and 62 data frames

# converting the date column into a date variable
old_data$Date <- as.yearmon(old_data$date) 

#cleaning the final data frame
old_data <- old_data %>%
  select(Date, `Total number of urgent suspected cancer referrals received and confirmed urgent by the specialist`,
         `Total starting treatment.x`, `Total starting treatment within 62 days`, `Total starting treatment.y`, 
         `Total starting treatment within 31 days`) %>%
  rename("n_uscr_referrals" = `Total number of urgent suspected cancer referrals received and confirmed urgent by the specialist`,
         "total_starting_treatment_62" = `Total starting treatment.x`, 
         "within62" = `Total starting treatment within 62 days`, 
         "total_starting_treatment_31" = `Total starting treatment.y`, 
         "within31" = `Total starting treatment within 31 days`)

# adding variables (prop)
old_data <- old_data %>%
  mutate(prop_within_62 = (within62 / total_starting_treatment_62)*100, 
         prop_within_31 = (within31 / total_starting_treatment_31)*100, 
         year = substr(Date, 5,9), 
         month = substr(Date, 1,3), 
         total_starting_treatment = total_starting_treatment_62 + total_starting_treatment_31, 
         total_within_target = within62 + within31)

# Create standard deviation for 2019 year 
d2019 <- old_data %>% 
  filter(year == 2019) 

old_data$sd_n_uscr_referrals <- sd(d2019$n_uscr_referrals)
old_data$sd_within62 <- sd(d2019$within62)
old_data$sd_within31 <- sd(d2019$within31)
old_data$sd_total_starting_treatment_62 <- sd(d2019$total_starting_treatment_62)
old_data$sd_total_starting_treatment_31 <- sd(d2019$total_starting_treatment_31)
old_data$sd_total_starting_treatment <- sd(d2019$total_starting_treatment)
old_data$sd_prop_within_62 <- sd(d2019$prop_within_62)
old_data$sd_prop_within_31 <- sd(d2019$prop_within_31)

data <- old_data %>% 
  filter(Date >= "Jan 2019")


###### CLEANING NEW DATA - SINGLE CANCER PATHWAY #####

# Clean new data
select_newdata <- new_data %>% 
  filter(Localhealthboard_ItemName_ENG == "Wales", 
         Tumoursite_ItemName_ENG == "Lower Gastrointestinal", 
         AgeGroup_ItemName_ENG == "Total", 
         Sex_ItemName_ENG == "Total") %>%
  select (Month_ItemName_ENG, Month_Code, Measure_ItemName_ENG, Data) %>%
  rename(value = Data, 
         measure = Measure_ItemName_ENG, 
         date = Month_ItemName_ENG) %>%
  spread(measure, value)


# converting the date column into a date variable
select_newdata$Date <- as.yearmon(select_newdata$date) 

#cleaning the final data frame
select_newdata <- select_newdata %>%
  select(Date, `The number of patients starting their first definitive treatment in the month`,
         `The number of patients starting their first definitive treatment in the month within 62 days of first being suspected of cancer (no suspensions)`) %>%
  rename("total_starting_treatment" = `The number of patients starting their first definitive treatment in the month`, 
         "total_within_target" = `The number of patients starting their first definitive treatment in the month within 62 days of first being suspected of cancer (no suspensions)`)

# adding variables (prop)
select_newdata <- select_newdata %>%
  mutate(year = substr(Date, 5,9), 
         month = substr(Date, 1,3)) %>%
  arrange(Date)


# match new data frame with 'old' variables
sd_n_uscr_referrals <- sd(d2019$n_uscr_referrals)
sd_within62 <- sd(d2019$within62)
sd_within31 <- sd(d2019$within31)
sd_total_starting_treatment_62 <- sd(d2019$total_starting_treatment_62)
sd_total_starting_treatment_31 <- sd(d2019$total_starting_treatment_31)
sd_prop_within_62 <- sd(d2019$prop_within_62)
sd_prop_within_31 <- sd(d2019$prop_within_31)
sd_total_starting_treatment <- sd(d2019$total_starting_treatment)


select_newdata <- select_newdata %>%
  mutate("n_uscr_referrals" = NA, 
         "prop_within_31" = NA,  
         "prop_within_62" = NA,  
         "sd_n_uscr_referrals" = sd_n_uscr_referrals, 
         "sd_prop_within_31" = sd_prop_within_31,  
         "sd_prop_within_62" = sd_prop_within_62, 
         "sd_total_starting_treatment_31" = sd_total_starting_treatment_31, 
         "sd_total_starting_treatment_62" = sd_total_starting_treatment_62, 
         "sd_within31" = sd_within31, 
         "sd_within62" = sd_within62,  
         "total_starting_treatment_31" = NA, 
         "total_starting_treatment_62" = NA, 
         "within62"  = NA, 
         "within31" = NA, 
         "sd_total_starting_treatment" = sd_total_starting_treatment, 
  ) %>%
  select("Date", "month", "n_uscr_referrals", "prop_within_31", "prop_within_62", "sd_n_uscr_referrals","sd_prop_within_31", 
         "sd_prop_within_62","sd_total_starting_treatment_31", "sd_total_starting_treatment_62", "sd_total_starting_treatment", "sd_within31",
         "sd_within62", "total_starting_treatment", "total_starting_treatment_31", "total_starting_treatment_62", 
         "total_within_target", "within31", "within62", "year") 



# Join with the old dataset
all_data <- rbind(data, select_newdata, by = NULL)

all_data <- all_data %>%
  mutate(prop_within_target = (total_within_target / total_starting_treatment)*100)

d2019_all <- all_data %>% 
  filter(year == 2019) 

all_data$sd_prop_within_target <- sd(d2019_all$prop_within_target)


##### Plotting data ####

# 1. Plotting number of UCSR referrals

all_data$month = factor(all_data$month, levels = month.abb)

all_data %>% 
  filter(year <= 2020) %>%
  ggplot(aes(month, n_uscr_referrals, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=n_uscr_referrals-sd_n_uscr_referrals, ymax=n_uscr_referrals+sd_n_uscr_referrals), width=.2) + 
  labs(title = "Monthly number of bowel UCSR referrals", x = "Month", y = "Number of referrals") + 
  scale_y_continuous(limit = c(0, 2000)) +
  scale_color_manual(values=c( "dodgerblue3", "firebrick2"))


# 2. Plotting proportion of referrals who started treatment within target

ggplot(all_data, aes(month, prop_within_target, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=prop_within_target-sd_prop_within_target, ymax=prop_within_target+sd_prop_within_target), width=.2) + 
  labs(title = "Proportion of bowel cancer referrals starting treatment within target", x = "Month", y = "Proportion starting treatment within target") + 
  scale_y_continuous(limit = c(0, 100))  +
  scale_color_manual(values=c( "dodgerblue3", "firebrick2", "limegreen"))

# 3. Plotting number of people starting treatment

ggplot(all_data, aes(month, total_starting_treatment, colour = year, group = year)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=total_starting_treatment-sd_total_starting_treatment, ymax=total_starting_treatment+sd_total_starting_treatment), width=.2) + 
  labs(title = "Number of people starting treatment for bowel cancer", x = "Month", y = "Number starting treatment") + 
  scale_y_continuous(limit = c(0, 300)) + 
  scale_color_manual(values=c( "dodgerblue3", "firebrick2", "limegreen"))


# 4. Plotting proportion of referrals within target in one line; red points= lockdowns 
#   (September = regional lockdowns) - dates still to be investigated, not very clear; blue point = change in data recording
ggplot(all_data, aes(Date, prop_within_target, group = 1, label = Date)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Proportion of bowel cancer referrals starting treatment within target", x = "Month", y = "Proportion starting treatment within target") + 
  scale_y_continuous(limit = c(0, 100)) + 
  geom_point(data = all_data %>% filter(Date == "March 2020"), color = "red") + 
  geom_point(data = all_data %>% filter(Date == "December 2020"), color = "blue") + 
  geom_point(data = all_data %>% filter(Date == "September 2020"), color = "red") + 
  geom_point(data = all_data %>% filter(Date == "October 2020"), color = "red")


#### Data tables ####
d2020 <- all_data %>% filter(year >= "2020")

table_data <- d2020 %>%
  select(Date, n_uscr_referrals, total_within_target, total_starting_treatment)

# Calculating percentage change in number of USCR referrals
table_data$uscr_2019 <- round(mean(d2019$n_uscr_referrals),0) # calculating monthly mean for 2019 
table_data$uscr_sd_2019 <- round(sd(d2019$n_uscr_referrals),0) # calculating monthly SD for 2019 
table_data$uscr_prop_reduction <- 
  round(((table_data$uscr_2019-table_data$n_uscr_referrals)/table_data$uscr_2019)*100,0) # calculating percent change 2019 vs 2020

# Calculating percentage change in number of people starting treatment within target
table_data$within_target_2019 <- round(mean(d2019$total_within_target),0) # calculating monthly mean for 2019 
table_data$within_target_sd_2019 <- round(sd(d2019$total_within_target),0) # calculating monthly SD for 2019 
table_data$within_target_prop_reduction <- 
  round(((table_data$within_target_2019-table_data$total_within_target)/table_data$within_target_2019)*100,0) # calculating percent change 2019 vs 2020

# Calculating percentage change in number of people starting treatment in total
table_data$total_starting_treatment_2019 <- round(mean(d2019$total_starting_treatment),0) # calculating monthly mean for 2019 
table_data$total_starting_treatment_sd_2019 <- round(sd(d2019$total_starting_treatment),0) # calculating monthly SD for 2019 
table_data$total_starting_treatment_prop_reduction <- 
  round(((table_data$total_starting_treatment_2019-table_data$total_starting_treatment)/table_data$total_starting_treatment_2019)*100,0) # calculating percent change 2019 vs 2020


############## DATA TABLES:

# Data table for USCR
table_uscr <- table_data %>%
  filter(!is.na(n_uscr_referrals)) %>%
  select(Date, n_uscr_referrals, uscr_prop_reduction)

#Monthly mean 2019 is:
round(mean(d2019$n_uscr_referrals),0)
#SD of Monthly mean 2019 is:
round(sd_n_uscr_referrals)


# Creating April-November 2020 mean
apr_nov_20 <- table_data %>%
  filter(Date >= "April 2020")  %>%
  filter(!is.na(n_uscr_referrals))

#M onthly mean of April-November 2020 is:
round(mean(apr_nov_20$n_uscr_referrals),0)
#SD of Monthly mean 2019 is:
round(sd(apr_nov_20$n_uscr_referrals),0)



# Data table for total starting treatment / within target
table_treat <- table_data %>%
  select(Date, total_starting_treatment, total_starting_treatment_prop_reduction, 
         total_within_target, within_target_prop_reduction)

#Monthly mean 2019 for number starting treatment is:
round(mean(d2019$total_starting_treatment),0)
#SD of monthly mean 2019 for number starting treatment is:
round(sd_total_starting_treatment)


# Number startign treatment / within target:

# Creating April 2020 -November 2021 mean
apr_nov_21 <- table_treat %>%
  filter(Date >= "April 2020")

# Monthly mean of number of people starting treatment April 2020 -November 2021 is:
round(mean(apr_nov_20$total_starting_treatment),0)
# SD of monthly mean of number of people starting treatment April 2020 -November 2021 is:
round(sd(apr_nov_20$total_starting_treatment),0)


# Monthly mean of number of people starting treatment within target April 2020 -November 2021 is:
round(mean(apr_nov_20$total_within_target),0)
# SD of monthly mean of number of people starting treatment within target April 2020 -November 2021 is:
round(sd(apr_nov_20$total_within_target),0)
