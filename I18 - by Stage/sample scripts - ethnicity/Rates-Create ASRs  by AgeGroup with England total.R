# =============================================================================#####
#        INCIDENCE BY ETHNICITY in ENGLAND
#                  2013-17 Incidence
#                  2013-17 Population
#        Are differences from white population changing over age? Do people become aculturated.
#           Author: Christine Delon 
#           Date:   October 2020
#           Confidence Intervals are set to 95%

# =============================================================================#####
#
#
# Prerequisites:  The database data must be loaded first using the "80 Get DB Data for Scipts 81-89" script.
#                 The wb: wb <- loadWorkbook(file=paste0(pIncidenceDataDirectory, "IncidenceChartsTables_i7.xlsx")) 
#
# =============================================================================#####
# Contents=====================================================================#####
#
# PART 1:  Packages and parameters                                             #####
# PART 2:  Load data and filtering                                             #####
# PART 3:  Load cancer types to be exported                                    #####
# PART 4:  Create dataframe for table output                                   #####
# PART 5:  Loop across the cancer types                                        #####
#  PART  6: Get the incidence data for a particular level 1, 3 or 5 cancer type#####
#  PART  7: Carry out the sensitivity analysis                                 #####
#  PART  8: Get the population data                                            #####
#  PART  9: Calculate Crude Rates                                              #####
#  PART 10: Calculate Age Standardised Rates (ASR)                             #####
#  PART 11: Calculate Confidence Intervals                                     #####
#  PART 12: Calculate ASR percentage change and ratio                          #####
#  PART 13: Combine crude rates to ASR summary table                           #####
#  PART 14: Insert data from the loop into the dataframe for output            #####
# PART 15: Clean up after the loop                                             #####
# PART 16: Calculate relationship to reference group                           #####
# PART 17: Output data                                                         #####
# =============================================================================#####


# =============================================================================#####
# PART 1:  Packages and parameters
# =============================================================================#####
# set working directory

# Cleanup

rm(list=ls())

# Libraries needed

library(dplyr)
#library(tidyr)
library(openxlsx)

# Script parameters

pIncidenceDataDirectory <- 
  paste0("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Indicators\\Incidence\\GN0001-England\\",
         "SN0089-2017_Ethnicity\\4_Working\\Incidence by Ethnicity Project\\Raw data\\")
pDataDirectory <- 
  paste0("G:\\Cancer Information\\Statistical Information Team\\Information and Risk\\Risk factor projects\\",
         "Incidence by broad ethnic group\\")
pSafeOutputDirectory <-
  paste0("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Indicators\\Incidence\\GN0001-England\\",
         "SN0089-2017_Ethnicity\\4_Working\\Incidence by Ethnicity Project\\R Output\\")
pOutputDirectory <- 
  paste0("G:\\Cancer Information\\Statistical Information Team\\Information and Risk\\Risk factor projects\\",
         "Incidence by broad ethnic group\\R work\\R Output\\")


# Choose whether to run sensitivity 1, 2 or 3, see end of Part 2 for explanation and data is filtered in Part 7, inside the loop
 sensit <- "sens1"
# sensit <- "sens2"
# sensit <- "sens3"


# Calculate whether rate for each Ethnicity is signficantly greater or lower then the reference group. 
 refgroup <- "White"
# refgroup <- "AllGroupsCombined"


# Choose an age group and filter to it. Choosing "0-90 is not necessary for calculations, this is default,
# but will allow script output to be labelled with right ages automatically
# AgeType <- "0-90+"
# AgeType <- "0-64"
# AgeType <- "65-90+"


# =============================================================================#####
# PART 2:  Load data and filtering                                             #####
# =============================================================================#####

PopulationWeighting <- readRDS(paste0(pDataDirectory, "R work\\Standard Data\\DataR\\PopulationWeighting.rds"))


# Load data cleaned by script 0.
#---------------------------------------------------------------------------------------------------------------#
# Incidence Data
CoreIncEthRawData   <- readRDS(paste0(pIncidenceDataDirectory, "SN0089_CoreIncidenceEthnicity_2017_w_allcombined.rds"))

# 1
IncType      <- "2013-17 incidence data"
NoOfYrs      <- 5   # use this in case I change to three years vs five years, or even just one year.
CoreIncEth   <- CoreIncEthRawData %>%
  filter(IncidenceYear %in% c(2013, 2014, 2015, 2016, 2017)) %>%    # filter down to five years
  group_by(CancerSiteLevel5Code, CancerSiteLevel5Desc,
           CancerSiteLevel3Code, CancerSiteLevel3Desc,
           CancerSiteLevel1Code,CancerSiteLevel1Desc,
           Gender, AgeRangeCode, EthnicGroup) %>%   # group by everything except Year and Incidence Count
  summarise(IncidenceCount = sum(IncidenceCount)) %>% 
  ungroup()


# Population Data
# 1
PopType      <- "2013-17 census aged on"
CorePopEth  <- readRDS(paste0(pDataDirectory,"Raw Data\\CorePopEthAgedOn11_18_w_allcombined.rds"))
CorePopEth$EthnicGroup[which(CorePopEth$EthnicGroup == "TotalPopulation")] <- "AllGroupsCombined"
CorePopEth   <- CorePopEth %>%
  filter(Year %in% c(2013, 2014, 2015, 2016, 2017)) %>%            # filter down to five years
  group_by(Gender, AgeRangeCode, AgeRangeDesc, EthnicGroup) %>%    # sum up to one big year
  summarise(PopulationCount = sum(PopulationCount)) %>%
  ungroup()


# # Filter down to the relevent age group. 0-90= i.e. no filtering is default. 
# if(AgeType == "0-64"){
#   CoreIncEth <- CoreIncEth %>% filter(AgeRangeCode %in% c(  "0-04", "05-09", "10-14", "15-19",
#                                                             "20-24", "25-29", "30-34", "35-39", 
#                                                             "40-44", "45-49", "50-54", "55-59", "60-64"))
#   CorePopEth <- CorePopEth %>% filter(AgeRangeCode %in% c(  "0-04", "05-09", "10-14", "15-19",
#                                                             "20-24", "25-29", "30-34", "35-39", 
#                                                             "40-44", "45-49", "50-54", "55-59", "60-64"))
# } else if (AgeType == "65-90+"){
#   CoreIncEth <- CoreIncEth %>% filter(AgeRangeCode %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"))
#   CorePopEth <- CorePopEth %>% filter(AgeRangeCode %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"))
# }

# Set up age groups to calculate age specific rates

# # 20 year age bands
# CoreIncEth <- CoreIncEth %>% 
#   mutate(AgeGroup = ifelse(AgeRangeCode %in%        c("0-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"),  "0-39", 
#                            ifelse(AgeRangeCode %in%                                    c("40-44", "45-49", "50-54", "55-59"), "40-59",
#                                   ifelse(AgeRangeCode %in%                             c("60-64", "65-69", "70-74", "75-79"), "60-79",
#                                                                                                                               "80+"))))

# 10 year age bands
CoreIncEth <- CoreIncEth %>% 
  mutate(
    AgeGroup = ifelse(AgeRangeCode %in%        c("0-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"),  "0-39",
                      ifelse(AgeRangeCode %in%                                    c("40-44", "45-49"),                   "40-49",
                             ifelse(AgeRangeCode %in%                             c("50-54", "55-59"),                   "50-59",
                                  ifelse(AgeRangeCode %in%                        c("60-64", "65-69"),                   "60-69",
                                         ifelse(AgeRangeCode %in%                 c("70-74", "75-79"),                   "70-79",
                                                                                                                         "80+"))))))


# review groupings
# table(CoreIncEth$AgeRangeCode,CoreIncEth$AgeGroup)

#---------------------------------------------------------------------------------------------------------------#

# Sensitivity analysis
# The Ethnic group 'Not known' dwarfs each ethnic group, 
# There are three options
# 1 Drop not known
# 2 Distribute not known
# 3 Distribute not known AND other

# IF there is a not known but NO other ethnic group with cases for a given ICD-10 code/gender/age
# then the value gets dropped
# Therefore incorporate this step AFTER the data is all summed up to level 3 (or 5 for leukaemias) so this doesn't happen

#---------------------------------------------------------------------------------------------------------------#


# =============================================================================#####
# PART 3:  Load cancer types to be exported
# =============================================================================#####

wb <- loadWorkbook(file=paste0(pDataDirectory, "R work\\Standard Data\\DataR\\IncidenceChartsTables_iETH.xlsx"))
CancerTypes <- read.xlsx(wb, "ChartTableSpecification", colNames=TRUE)
CancerTypes <- CancerTypes %>% filter(ChartOrTable=="Number of New Cases, Crude and European Age-Standardised (AS) Incidence Rates per 100,000 Population, UK")
rm(wb)


# =============================================================================#####
# PART 4:  Create dataframe for table output
# ============================================================#####

TableOutput <- data.frame(matrix(ncol=13)) %>% 
  filter(is.null(X1))

colnames(TableOutput) <- c("CancerType", "ICD10code", "Gender", "EthnicGroup", "AgeGroup",
                           "Cases",      "AveCases",  "CrudeRate", "ASR",  "LCI",  "UCI",
                           "ASRpercentchange", "ASRratio")

# =============================================================================#####
# PART 5:  Loop across the cancer types
# =============================================================================#####

# Everything after this section is executed in a loop, once for each cancer type
# Or run the following line manually to set one single cancer type (e.g. for debugging)

# ct <- 1

# If skipping the loop, to run just one graph, the matching closing bracket is down in part 12 in the very last line, 400ish
# So to don't worry about skipping closing brackets as you trouble shoot your way down. 

for(ct in 1:nrow(CancerTypes)) {
  cancerTypeCode            <- CancerTypes[ct, "CancerTypeCode"]
  cancerTypeIsStandard      <- CancerTypes[ct, "IsStandard"]
  cancerTypeExtraCriteria   <- CancerTypes[ct, "ExtraCriteria"]
  cancerTypeLevel           <- CancerTypes[ct, "Level"]
  cancerTypeIncludeMale     <- CancerTypes[ct, "Male"]
  cancerTypeIncludeFemale   <- CancerTypes[ct, "Female"]
  cancerTypeIncludePerson   <- CancerTypes[ct, "Person"]
  cancerTypePopulationTitle <- CancerTypes[ct, "PopulationTitle"]
  cancerTypeDescription     <- CancerTypes[ct, "CancerTypeDescription"]
  cancerTypeCodeFormatted   <- CancerTypes[ct, "CancerTypeCodeFormatted"]
  cancerTypeTokenCode       <- CancerTypes[ct, "TokenCode"]
  TokenCodeSheetName        <- CancerTypes[ct, "TokenCodeSheetName"]
  
  
  # =============================================================================#####
  # PART 6: Get the incidence data for a particular level 1, 3 or 5 cancer type  #####
  # =============================================================================#####
  
  # Filter the incidence data to this cancer type, aggregate and calculate the value for the current year.
  # A couple of different ways to aggregate the data, as described in the IncidenceByAgeCancerTypes spreadsheet
  
  # CancerTypyeIsStandard, pulls out cancer types that need special treatment. 
  # e.g. brain with C and D codes, 
  # For incidence All cancer is not standard as it excludes C44. 
  
  # This also aggregates the five year data into one year though that step is now already done above. 
  IncEth <- CoreIncEth
  
  if(cancerTypeIsStandard) {
    if(cancerTypeLevel==3) {
      incidenceSummary <- IncEth %>%
        filter(CancerSiteLevel3Code == cancerTypeCode) %>%
        group_by(AgeRangeCode, Gender, EthnicGroup, AgeGroup) %>%
        summarise(IncidenceCount = sum(IncidenceCount, na.rm = TRUE)) %>%
        ungroup()
    }
    if(cancerTypeLevel==5) {
      incidenceSummary <- IncEth %>%
        filter(CancerSiteLevel5Code == cancerTypeCode) %>%
        group_by(AgeRangeCode, Gender, EthnicGroup, AgeGroup) %>%
        summarise(IncidenceCount = sum(IncidenceCount, na.rm = TRUE)) %>%
        ungroup()
    }
  } else { #if cancer type is not standard
    
    # these don't map to a single value in a single level of the Cancer Site dimension so they need more work.
    # i.e. brain and 'all cancers'. 
    
    if((cancerTypeCode=="C00-C97 ex. C44")&&(is.na(cancerTypeExtraCriteria)||nchar(cancerTypeExtraCriteria)==0)) {
      incidenceSummary <- IncEth %>%
        filter(CancerSiteLevel1Code == "C00-C97" & CancerSiteLevel3Code != "C44") %>%
        group_by(AgeRangeCode, Gender, EthnicGroup, AgeGroup) %>%
        summarise(IncidenceCount = sum(IncidenceCount, na.rm = TRUE)) %>%
        ungroup()
      
    } else if(cancerTypeCode=="C70-C72, C75.1-C75.3, D32-D33, D35.2-D35.4, D42-D43, D44.3-D44.5") {
      incidenceSummary <- IncEth %>%
        filter(CancerSiteLevel3Code == "C70-C72, C75.1-C75.3" | CancerSiteLevel3Code == "D32-D33, D35.2-D35.4, D42-D43, D44.3-D44.5") %>%
        group_by(AgeRangeCode, Gender, EthnicGroup, AgeGroup) %>%
        summarise(IncidenceCount = sum(IncidenceCount, na.rm = TRUE)) %>%
        ungroup()
    }
  }
  # Add in Persons
  
  if(cancerTypeIncludePerson) {
    personsSummary <- incidenceSummary %>%
      group_by(AgeRangeCode, EthnicGroup, AgeGroup) %>%
      summarise(IncidenceCount = sum(IncidenceCount)) %>%
      ungroup() %>%
      mutate(Gender = "Persons")
    
    incidenceSummary <- bind_rows(incidenceSummary, personsSummary)
    rm(personsSummary)
  }
  
  
  # =============================================================================#####
  # PART 7: Carry out the sensitivity analysis                                   #####
  # =============================================================================#####
  
  # Sensitivity analysis
  # The Ethnic group 'Not known' dwarfs each ethnic group, 
  # There are three options
  # 1 Drop not known
  # 2 Distribute not known
  # 3 Distribute not known AND other
  
  # IF there is a not known but NO other ethnic group with cases for a given ICD-10 code/gender/age
  # then the value gets dropped
  # Therefore incorporate this AFTER the data is all summed up. 
  incidenceSummarySaved <- incidenceSummary
  # incidenceSummarySaved -> incidenceSummary
  
  # Sensitivity, sens1, sens2 or sens3 is defined in PART 1:  Packages and parameters 

  
  if(sensit=="sens1"){
    # 1
    SensitType   <- "Drop not known"
    incidenceSummary <- incidenceSummarySaved %>%
      filter(EthnicGroup != "Not known")
    
    } else if(sensit=="sens2"){
      # 2
      SensitType   <- "Distribute not known"
      incidenceSummary <- incidenceSummarySaved %>%
        group_by(Gender, AgeRangeCode) %>%
        mutate(IncCountOld     = IncidenceCount,
               DistributeCases = IncidenceCount[which(EthnicGroup == "Not known")],
               IncPerc         = ifelse(IncidenceCount == 0, 0,
                                        ifelse((EthnicGroup == "Not known" | EthnicGroup == "AllGroupsCombined"),0,
                                               IncCountOld/(sum(IncCountOld)
                                                               -IncCountOld[which(EthnicGroup == "Not known")]
                                                               -IncCountOld[which(EthnicGroup == "AllGroupsCombined")]
                                                               ))),
               IncidenceCount  = ifelse(IncCountOld == 0, 0, IncCountOld+IncPerc*DistributeCases)) %>%
        filter(EthnicGroup != "Not known") %>%
        select(-IncPerc, -DistributeCases, -IncCountOld)
      
      } else if(sensit=="sens3"){
        # 3
        SensitType   <- "Distribute not known AND other"
        incidenceSummary5 <- incidenceSummarySaved %>%
          group_by(Gender, AgeRangeCode) %>%
          mutate(IncCountOld     = IncidenceCount,
                 DistributeCases = (IncidenceCount[which(EthnicGroup == "Not known")]
                                    + IncidenceCount[which(EthnicGroup == "Other")]),
                 IncPerc         = ifelse(IncidenceCount == 0, 0,
                                          ifelse((EthnicGroup == "Not known" | EthnicGroup == "Other" | EthnicGroup == "AllGroupsCombined"),0,
                                                 IncidenceCount/(sum(IncidenceCount)
                                                                 -IncidenceCount[which(EthnicGroup == "Not known")]
                                                                 -IncidenceCount[which(EthnicGroup == "Other")]
                                                                 -IncidenceCount[which(EthnicGroup == "AllGroupsCombined")]))),
                 IncidenceCount  = ifelse(IncidenceCount == 0, 0, IncidenceCount+IncPerc*DistributeCases)) %>%
          filter(EthnicGroup != "Not known" & EthnicGroup != "Other") %>%
          select(-IncPerc, -DistributeCases, -IncCountOld)
        }
  
  
  # =============================================================================#####
  # PART 8: Get the population data                                              #####
  # =============================================================================#####
  
  PopEth <- CorePopEth
  
  #Aggregate, this creates the five year sum, already done above but no harm repeating
  populationSummary <- PopEth %>%
    group_by(AgeRangeCode, Gender, EthnicGroup) %>%
    summarise(PopulationCount = sum(PopulationCount, na.rm = TRUE)) %>%
    ungroup()
  
  
  if(cancerTypeIncludePerson) {
    #Persons 
    personsSummary <- populationSummary %>%
      group_by(AgeRangeCode, EthnicGroup) %>%
      summarise(PopulationCount = sum(PopulationCount)) %>% 
      ungroup() %>%
      mutate(Gender = "Persons") %>% 
      select(AgeRangeCode, Gender, EthnicGroup, PopulationCount)
    personsSummary$Gender      <- as.character(personsSummary$Gender)
    
    
    populationSummary <- bind_rows(populationSummary, personsSummary)
    rm(personsSummary)
  }
  
  # populationSummary$AgeRangeCode <- as.character(populationSummary$AgeRangeCode)
  # populationSummary$EthnicGroup  <- as.character(populationSummary$EthnicGroup)
  
  # =============================================================================#####
  # PART 9: Calculate Crude Rates                                                #####
  # =============================================================================#####
  
  
  # Calculate the crude rate for ethnic group and Gender combination
  
  # Join data
  crudeSummary <- inner_join(incidenceSummary, populationSummary, by=c("AgeRangeCode", "Gender", "EthnicGroup"))
  
  # crude rate calculation
  crudeSummary <- crudeSummary %>% 
    group_by(Gender, EthnicGroup, AgeGroup) %>%
    summarise(Cases     = sum(IncidenceCount),
              AveCases  = sum(IncidenceCount)/NoOfYrs,
              CrudeRate = sum(IncidenceCount, na.rm=TRUE) / sum(PopulationCount)*100000) %>%
    ungroup()
  
  
  # =============================================================================#####
  # PART 10: Calculate Age Standardised Rates (ASR)                              #####
  # =============================================================================#####
  
  # Calculate the ASRs for each Incidence Year and Gender combination
  
  # Join data
  asrSummary <- inner_join(incidenceSummary, populationSummary, by=c("AgeRangeCode", "Gender", "EthnicGroup"))
  asrSummary <- inner_join(asrSummary, PopulationWeighting, by="AgeRangeCode")
  
  # ASR calculation
  asrSummary <- asrSummary %>% 
    mutate(Numerator=ifelse(PopulationCount==0, NA, IncidenceCount*100000/PopulationCount*PopulationWeighting))
  asrSummary <- asrSummary %>%
    group_by(Gender, EthnicGroup, AgeGroup) %>%
    summarise(Cases    = sum(IncidenceCount),
              AveCases = sum(IncidenceCount)/NoOfYrs,
              ASR      = sum(Numerator, na.rm=TRUE) / sum(PopulationWeighting)) %>%
    ungroup()
  
  # =============================================================================#####
  # PART 11:  Calculate Confidence Intervals                                     #####
  # =============================================================================#####
  
  # qnorm(0.975) = 1.959964 i.e. half of a  5% confidence interval
  # qnorm(0.90)  = 1.644854 i.e. half of a 10% confidence interval
  # qnorm(0.995) = 2.575829 i.e. half of a 99% confidence interval
  
  # Calculate LCI. If there are no cases, return NAN, if the LCI is below zero, return zero, other wise just give the LCI.
  asrSummary <- asrSummary %>% 
    mutate(LCI = ifelse(Cases==0, NaN,
                        ifelse((ASR-(1.96*(ASR/sqrt(Cases))))<0, 0,
                               ASR-(1.96*(ASR/sqrt(Cases))))))
  
  # Calculate UCI. If there are no cases, return NAN, other wise just give the UCI
  asrSummary <- asrSummary %>% 
    mutate(UCI = ifelse(Cases==0, NaN,
                        ASR+(1.96*(ASR/sqrt(Cases)))))
  
  # =============================================================================#####
  # PART 12: Calculate ASR percentage change and ratio                           #####
  # =============================================================================#####
  
  # See Part 1 where I define refgroup as either White or AllGroupsCombined, see environment too. 
  
  asrSummary <- asrSummary %>% 
    group_by(Gender, AgeGroup) %>% 
    mutate (ASR_Ref          = ASR[which(EthnicGroup == refgroup)],
            ASRpercentchange = ((ASR-ASR_Ref)/ASR_Ref)*100,
            ASRratio         = ASR/ASR_Ref) %>% 
    ungroup() %>%
    select(-ASR_Ref)
  
  
  # =============================================================================#####
  # PART 13: Combine crude rates to ASR summary table                            #####
  # =============================================================================#####
  
  asrSummary <- inner_join(asrSummary, crudeSummary, by=c("Gender", "EthnicGroup","AgeGroup", "Cases", "AveCases"))
  rm(crudeSummary)
  
  # =============================================================================#####
  # PART 14: Insert data from the loop into the dataframe for output             #####
  # =============================================================================#####
  
  
  asrSummary <- asrSummary %>%
    mutate(CancerType = cancerTypeDescription,
           ICD10code  = cancerTypeCodeFormatted) %>%
    #rearrange column order
    select(colnames(TableOutput))
  
  TableOutput <- rbind(TableOutput, asrSummary) 
  
  # This is the close bracket for looping to make tables for every cancer.
  # No need to use when trouble shooting and running just one cancer. 
}

# =============================================================================#####
# PART 15: Clean up after the loop                                             #####
# =============================================================================#####

rm(cancerTypeCode, cancerTypeCodeFormatted, cancerTypeDescription, cancerTypeExtraCriteria,
   cancerTypeIncludeFemale, cancerTypeIncludeMale, cancerTypeIncludePerson,
   cancerTypeIsStandard, cancerTypeLevel, cancerTypePopulationTitle, cancerTypeTokenCode, TokenCodeSheetName,
   ct)

rm(asrSummary, incidenceSummary, incidenceSummarySaved, populationSummary)
rm(CoreIncEth, CorePopEth, PopEth)

# =============================================================================#####
# PART 16: Calculate relationship to reference group                           #####
# =============================================================================#####

TableOutputWLabels <- TableOutput %>% 
  mutate(IncType    = IncType,
         PopType    = PopType,
         SensitType = SensitType,
         AgeType    = "See AgeGroup")

# Calculate whether rate for each Ethnicity is signficantly greater or lower then the reference group. 
# Which is defined in part 1, as either White or AllGroupsCombined, see also environment

TableOutputWLabels <- TableOutputWLabels %>%
  group_by(CancerType, ICD10code, Gender) %>%
  mutate(SigLower    = ifelse(UCI<= LCI[which(EthnicGroup == refgroup)], "sig lower",  "-"), 
         SigHigher   = ifelse(LCI>= UCI[which(EthnicGroup == refgroup)], "sig higher", "-"), 
         SigSummary1 = ifelse(EthnicGroup == refgroup, "ref group",
                               ifelse(SigLower == "-" & SigHigher == "-", "-", "sig")),
         SigSummary2 = ifelse(SigLower == "sig lower", -1, 
                              ifelse(SigHigher == "sig higher", 1, 0))) %>%
  ungroup()


# =============================================================================#####
# PART 17: Output data                                                         #####
# =============================================================================#####

write.table(TableOutputWLabels,
            #paste0(pSafeOutputDirectory, "IncByEthByAge_20Yr_13-17_CI95_rg-", refgroup, "_", sensit, ".csv"),
            paste0(pSafeOutputDirectory, "IncByEthByAge_10Yr_13-17_CI95_rg-", refgroup, "_", sensit, ".csv"),
            quote=T, sep=",", row.names=F, na = "")


rm(TableOutput, TableOutputWLabels)
rm(IncType, PopType, sensit, SensitType, AgeType, NoOfYrs, refgroup)

# =============================================================================#####
#                                      FIN                                     #####
# =============================================================================#####
