###
# Get Ethnicity and Population data
# =================
# 
# The Ethnicity data is cleaned by alteryx, but instead of being loaded to the database, it is saved as a csv
# This script reads the national ethnicity incidence data from the csv file.
#
# The Population data is the main challenge for this project. 
# Variations on the population are described and produced below. 
#
# The European 2013 population weighting data is trivil and was copied into the folder from the webplishing 
# script output. 
~
# The data is saved into RDS files for MUCH quicker opening than csv files.
#
# It is ESSENTIAL to save the Incidence RDS file in the Alteryx folder together with the source data 
# so all the protected data is in the same safe place and can all be deleted at the right time
#
# Prerequisites:  None


#####=============================================================================#####
#       PART 1:  Packages and parameters                                          #####
#####=============================================================================#####

# Cleanup
rm(list=ls())

# Libraries
library(dplyr)


# Script parameters 
# SAFE    data directory for sensitive incidence data
pIncidenceDataDirectory <- 
  paste0("G:\\Cancer Information\\Cancer Stats\\Alteryx\\Indicators\\Incidence\\GN0001-England\\",
         "SN0089-2017_Ethnicity\\4_Working\\Incidence by Ethnicity Project\\Raw data\\")
# Project data directory for non-sensitive population data
pDataDirectory <- 
  paste0("G:\\Cancer Information\\Statistical Information Team\\Information and Risk\\Risk factor projects\\",
         "Incidence by broad ethnic group\\")

#####=============================================================================#####
#      PART 2:  Load ethnicity incidence data from the cleaned csv file           #####
#####=============================================================================#####

# This query loads and populates a large data frame (1.2 million rows) so takes a minute or so to execute (and longer on wi-fi!).
CoreIncidenceEthnicity   <- read.csv(paste0(pIncidenceDataDirectory, "SN0089_Incidence_2017_eth_cleaned.csv"),
                                     stringsAsFactors = FALSE)

# Because cancer data is only at level 5 there is no data in most CancerSiteKey rows, 
# e.g. C02 has lots of data but C020, C021 are empty. Also we will be selecting based on level 5 at the lowest
# Therefore cut out CancerSiteKey (level 6) and Cancer Site Code (also level 6, what even is this?).

CoreIncidenceEthnicity <- CoreIncidenceEthnicity %>%
  group_by(CancerSiteLevel5Code, CancerSiteLevel5Desc, CancerSiteLevel3Code, CancerSiteLevel3Desc,
           CancerSiteLevel1Code, CancerSiteLevel1Desc,
           IncidenceYear, Gender, AgeRangeCode, EthnicGroup) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  ungroup

saveRDS(CoreIncidenceEthnicity,
        file=paste0(pIncidenceDataDirectory, "SN0089_CoreIncidenceEthnicity_2017.rds"))

# Calculate total England cancer cases (all ethnicities combined and add this on).
CoreIncidenceEthnicityTotal <- CoreIncidenceEthnicity %>%
  group_by(CancerSiteLevel5Code, CancerSiteLevel5Desc, CancerSiteLevel3Code, CancerSiteLevel3Desc,
           CancerSiteLevel1Code, CancerSiteLevel1Desc,
           IncidenceYear, Gender, AgeRangeCode) %>%
  summarise(IncidenceCount = sum(IncidenceCount)) %>%
  mutate(EthnicGroup = "AllGroupsCombined") %>% 
  ungroup

#Have not made sure all columns are in same order as should be safe enough.

CoreIncidenceEthnicityTotal <- rbind(CoreIncidenceEthnicity, CoreIncidenceEthnicity2)

saveRDS(CoreIncidenceEthnicityTotal,
        file=paste0(pIncidenceDataDirectory, "SN0089_CoreIncidenceEthnicity_2017_w_allcombined.rds"))



#####=============================================================================#####
#      PART 3:  Load Population Weighting Data                                    #####
#####=============================================================================#####

# This is a small table, 2 columns 19 rows, produced by
# script 80 or 90 in the webpublising scripts and copied over for this project. 

#####=============================================================================#####
#      PART 4, 5, 6: Population Data                                              #####
#####=============================================================================#####

#####=============================================================================#####
#      PART 4A: Nomis Census Data 2011                                            #####
#      PART 4A: Nomis Census Data 2011 x5 to mimic five year data                 #####
#####=============================================================================#####

#___PART 4B: Nomis Census Data 2011___________________________________________________

# This is the gold standard ethnicity data
# Cleaned with Alteryx
CorePopEth11 <- read.csv(paste0(pDataDirectory,
                                "Raw Data\\Nomis Census 2011 population data\\NOMIS 2011 Pop data Long.csv"),
                         stringsAsFactors = FALSE)


names(CorePopEth11)[names(CorePopEth11) == "Sex"] <- "Gender"

# Note this data only has ages up to 85+ not 90+ put this info in the file name.
saveRDS(CorePopEth11,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEth11_85+.rds"))


# Calculate total England population (all ethnicities combined and add this on).
CorePopEth11Total <- CorePopEth11 %>%
  group_by(Gender, AgeRangeCode) %>%
  summarise(PopulationCount = sum(PopulationCount)) %>%
  mutate(EthnicGroup = "AllGroupsCombined") %>% 
  ungroup

#Have not made sure all columns are in same order as should be safe enough.
CorePopEth11Total <- rbind(CorePopEth11, CorePopEth11Total)

saveRDS(CoreIncidenceEthnicityTotal,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEth11_85+_w_allcombinedxxx.rds"))


#___PART 4B: Nomis Census Data 2011 multiplied by five to mimic five year data___

CorePopEth11x5 <- CorePopEth11 %>% 
  mutate(PopulationCount = PopulationCount*5)

# Note this data only has ages up to 85+ not 90+ put this info in the file name.
saveRDS(CorePopEth11x5,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEth11x5_85+.rds"))

#####=============================================================================#####
#      PART 5: ONS Aged On Census Data 2011-18                                    #####
#####=============================================================================#####

CorePopEthAgedOn11_18 <- read.csv(paste0(pDataDirectory,
                                    "Raw Data\\",
                                    "ONS 2011 Aged On to 2012-2018 population data\\",
                                    "The final cleaned population to use 2020-9-17\\",
                                    "ONS_AgedOn_Pop_2011-2018.csv"),
                             stringsAsFactors = FALSE)

CorePopEthAgedOn11_18 <- CorePopEthAgedOn11_18 %>%
  select(-CountryCode, -CountryName)

CorePopEthAgedOn11_18Total <- CorePopEthAgedOn11_18

saveRDS(CorePopEthAgedOn11_18Total,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18_w_allcombined___.rds"))


CorePopEthAgedOn11_18 <- CorePopEthAgedOn11_18%>% 
  filter(EthnicGroup != "TotalPopulation")
  
saveRDS(CorePopEthAgedOn11_18,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18_w-o___.rds"))

# Can check files by importing them if you want
# CoreIncEth <- readRDS(paste0(pIncidenceDataDirectory, "CoreIncidenceEthnicity_2017.rds"))
# PopWeight  <- readRDS(paste0(pIncidenceDataDirectory, "PopulationWeighting.rds"))

#####=============================================================================#####
#      PART 6: ONS Aged On Census Data 2011-18 Sensitivity Analysis, Perturb Data #####
#  6A: Reallocate x% (10%) of population 0-4 and 90+ from white to all others proportionatley #####
#  6B: Reallocate x% (10%) of population 0-4 and 90+ from all others to white     #####
#  6C: Increase Asian population in under 65s (10%) and over 65s (5%) taking these from all others proportionately #####
#  6D: Assume Asian, Black, Mixed and Other Groups are NOT Ageing                 #####
#  6X: Clean up                                                                   #####
#####=============================================================================#####

#===PART 6A: Reallocate x% (10%) of population 0-4 and 90+ from white to all others proportionatley=#####
#Reallocate 10% to these age groups
PercA <- 0.1
AgesA <- c("0-04", "90+")

ReallocateA <- CorePopEthAgedOn11_18Total %>%
  group_by(Gender, Year, AgeRangeCode, AgeRangeDesc) %>%
  mutate(OldPopCount = PopulationCount,
         White10Perc = PercA*PopulationCount[which(EthnicGroup == "White")],
         Proportion  = ifelse(EthnicGroup %in% c("Asian", "Black", "MixedMultiple", "Other"),
                              PopulationCount/sum(PopulationCount[EthnicGroup %in% c("Asian", "Black", "MixedMultiple", "Other")]),
                              0),
         NewPopCount = ifelse(EthnicGroup == "TotalPopulation", PopulationCount,
                              ifelse(EthnicGroup == "White", PopulationCount-White10Perc,
                                     PopulationCount+(Proportion*White10Perc))),
         # Only apply redistributed population to some age groups, otherwise use undistributed population
         PopulationCount = ifelse(AgeRangeCode %in% AgesA,
                                  NewPopCount,
                                  OldPopCount),
         PercentDif  = (NewPopCount - OldPopCount)/OldPopCount,
         Checksum1   = sum(NewPopCount[which(EthnicGroup != "TotalPopulation")]),
         Checksum2   = Checksum1 - OldPopCount[which(EthnicGroup == "TotalPopulation")],
         Checksum3   = (Checksum2 <0.06 & Checksum2 > -0.06)
         ) %>%
  ungroup

if(sum(ReallocateA$Checksum3==TRUE) != nrow(ReallocateA)){stop("not all checksums are TRUE")}

# Export Raw data to check methods and review effect of changes
write.table(ReallocateA, 
            paste0(pDataDirectory, "Populations\\", "ReAllocateA.csv"),
            quote=T, sep=",", row.names=F, na = "")

# Remove columns which are useful for checking method and numbers, but not needed for analysis.
ReallocateA<- ReallocateA%>% 
  select(-OldPopCount, -White10Perc, -Proportion, -NewPopCount, -PercentDif,
         -Checksum1, -Checksum2, -Checksum3)

write.table(ReallocateA, 
            paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateA.csv"),
            quote=T, sep=",", row.names=F, na = "")
saveRDS(ReallocateA,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateA.rds"))

rm(ReallocateA, PercA, AgesA)
  
#===PART 6B: Reallocate x% (10%) of population 0-4 and 90+ from all others to white ========#####

PercB <- 0.1
AgesB <- c("0-04", "90+")
# AgesB <- "allAges" 
#Note commented out PopulationCount ifelse loop so that PopulationCount = NewPopCount,

ReallocateB <- CorePopEthAgedOn11_18Total %>%
  group_by(Gender, Year, AgeRangeCode, AgeRangeDesc) %>%
  mutate(OldPopCount = PopulationCount,
         AllXPerc    = PercB*PopulationCount,
         NewPopCount = ifelse(EthnicGroup == "TotalPopulation", OldPopCount,
                              ifelse(EthnicGroup == "White",
                                     OldPopCount+sum(AllXPerc[which(EthnicGroup %in% c("Asian", "Black", "MixedMultiple", "Other"))]),
                                                               OldPopCount-AllXPerc)),
         # Only apply redistributed population to some age groups, otherwise use undistributed population
         PopulationCount = ifelse(AgeRangeCode %in% AgesB,    # comment out this if else
                                  NewPopCount,
                                  OldPopCount),               # and comment out this line to get B for all ages. 
         PercentDif  = (NewPopCount - OldPopCount)/OldPopCount,
         Checksum1   = sum(NewPopCount[which(EthnicGroup != "TotalPopulation")]),
         Checksum2   = Checksum1 - OldPopCount[which(EthnicGroup == "TotalPopulation")],
         Checksum3   = (Checksum2 <0.06 & Checksum2 > -0.06)
  ) %>%
  ungroup

if(sum(ReallocateB$Checksum3==TRUE) != nrow(ReallocateB)){stop("not all checksums are TRUE")}

# Export Raw data to check methods and review effect of changes
write.table(ReallocateB, 
            paste0(pDataDirectory, "Populations\\", "ReAllocateB.csv"),
            #paste0(pDataDirectory, "Populations\\", "ReAllocateB_AllAgesReall_",PercB*100, "P.csv"),
            quote=T, sep=",", row.names=F, na = "")

# Remove columns which are useful for checking method and numbers, but not needed for analysis.
ReallocateB<- ReallocateB%>% 
  select(-OldPopCount, -AllXPerc, -NewPopCount, -PercentDif,
         -Checksum1, -Checksum2, -Checksum3)

write.table(ReallocateB, 
            paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateB.csv"),
           # paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateB_AllAgesReall_",PercB*100, "P.csv"),
            quote=T, sep=",", row.names=F, na = "")
saveRDS(ReallocateB,
        file=paste0(pDataDirectory, "Raw Data\\", 
                    "CorePopEthAgedOn11_18ReAllocateB.rds"
                    #"CorePopEthAgedOn11_18ReAllocateB_AllAgesReall_",PercB*100, "P.rds"
                    ))

rm(ReallocateB, PercB, AgesB)

#===PART 6C: Increase Asian population in under 65s (10%) and over 65s (5%) taking these from all others proportionately=#####

PercCyoung <- 0.1   # percentage change for younger age groups
PercCold   <- 0.05  # percentage chane for older age groups
AgesCyoung <- c( "0-04", "05-09", "10-14", "15-19",     # Age groups changed by younger percentage
                "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49", "50-54", "55-59",
                "60-64")
AgesCold   <- c(         "65-69",                       # Age groups changed by older percentage
                "70-74", "75-79",
                "80-84", "85-89", "90+")
#                                                       # Note no need to list all age groups, any not listed here will remain the same.


ReallocateC <- CorePopEthAgedOn11_18Total %>%
  group_by(Gender, Year, AgeRangeCode, AgeRangeDesc) %>%
  mutate(OldPopCount      = PopulationCount,
         AsianXPercyoung  = PercCyoung*PopulationCount[which(EthnicGroup == "Asian")],
         AsianXPercold    = PercCold  *PopulationCount[which(EthnicGroup == "Asian")],
         Proportion       = ifelse(EthnicGroup %in% c("White", "Black", "MixedMultiple", "Other"),
                                   PopulationCount/sum(PopulationCount[EthnicGroup %in% c("White", "Black", "MixedMultiple", "Other")]),
                                   0),
         NewPopCountyoung = ifelse(EthnicGroup == "TotalPopulation", OldPopCount,
                                   ifelse(EthnicGroup == "Asian", OldPopCount + AsianXPercyoung,
                                                                  OldPopCount -(AsianXPercyoung*Proportion))),
         NewPopCountold   = ifelse(EthnicGroup == "TotalPopulation", OldPopCount,
                                   ifelse(EthnicGroup == "Asian", OldPopCount + AsianXPercold,
                                                                  OldPopCount -(AsianXPercold*Proportion))),
         # Only apply redistributed population to some age groups, otherwise use undistributed population (in this case "some" equalls "all". )
         PopulationCount = ifelse(AgeRangeCode %in% AgesCyoung,
                                  NewPopCountyoung,
                                  ifelse(AgeRangeCode %in% AgesCold,
                                         NewPopCountold,
                                         OldPopCount)),
         PercentDif  = (PopulationCount - OldPopCount)/OldPopCount,
         Checksum1   = sum(PopulationCount[which(EthnicGroup != "TotalPopulation")]),
         Checksum2   = Checksum1 - OldPopCount[which(EthnicGroup == "TotalPopulation")],
         Checksum3   = (Checksum2 <0.06 & Checksum2 > -0.06)
  ) %>%
  ungroup

if(sum(ReallocateC$Checksum3==TRUE) != nrow(ReallocateC)){stop("not all checksums are TRUE")}

# Export Raw data to check methods and review effect of changes
write.table(ReallocateC, 
            paste0(pDataDirectory, "Populations\\", "ReAllocateC.csv"),
            quote=T, sep=",", row.names=F, na = "")

# Remove columns which are useful for checking method and numbers, but not needed for analysis.
ReallocateC <- ReallocateC%>% 
  select(-OldPopCount, -AsianXPercyoung, -AsianXPercold,
         -Proportion, -NewPopCountyoung, -NewPopCountold,
         -PercentDif,
         -Checksum1, -Checksum2, -Checksum3)

write.table(ReallocateC, 
            paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateC.csv"),
            quote=T, sep=",", row.names=F, na = "")
saveRDS(ReallocateC,
        file=paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateC.rds"))

rm(ReallocateC, PercCyoung, PercCold, AgesCyoung, AgesCold)

#===PART 6D: Assume Asian, Black, Mixed and Other Groups are NOT Ageing ==========#####

# For each Group_By section Take 2011 population, subtract the excess and add it to the white group.
AgesD <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")

ReallocateD <- CorePopEthAgedOn11_18Total %>%
  group_by(Gender, AgeRangeCode, AgeRangeDesc, EthnicGroup) %>%
  mutate(OldPopCount = PopulationCount,
         Pop2011     = PopulationCount[which(Year == 2011)]) %>%
  ungroup() %>%
  group_by(Gender, Year, AgeRangeCode, AgeRangeDesc) %>%
  mutate(ExcessPopCount = PopulationCount - Pop2011,
         NewPopCount = ifelse(EthnicGroup == "TotalPopulation", OldPopCount,
                              ifelse(EthnicGroup == "White",
                                     OldPopCount+sum(ExcessPopCount[which(EthnicGroup %in% c("Asian", "Black", "MixedMultiple", "Other"))]),
                                     Pop2011)),
         # Only apply redistributed population to some age groups, otherwise use undistributed population
         PopulationCount = ifelse(AgeRangeCode %in% AgesD,
                                  NewPopCount,
                                  OldPopCount),
         PercentDif  = (NewPopCount - OldPopCount)/OldPopCount,
         Checksum1   = sum(NewPopCount[which(EthnicGroup != "TotalPopulation")]),
         Checksum2   = Checksum1 - OldPopCount[which(EthnicGroup == "TotalPopulation")],
         Checksum3   = (Checksum2 <0.06 & Checksum2 > -0.06)
         ) %>%
  ungroup

# Check that Checksum3 is all TRUE
if(sum(ReallocateD$Checksum3==TRUE) != nrow(ReallocateD)){stop("not all checksums are TRUE")}

# Export Raw data to check methods and review effect of changes
write.table(ReallocateD, 
            paste0(pDataDirectory, "Populations\\", "ReAllocateD.csv"),
            quote=T, sep=",", row.names=F, na = "")

# Remove columns which are useful for checking method and numbers, but not needed for analysis.
ReallocateD<- ReallocateD %>% 
  select(-OldPopCount, -Pop2011, -ExcessPopCount, -NewPopCount, -PercentDif,
         -Checksum1, -Checksum2, -Checksum3)

write.table(ReallocateD, 
            paste0(pDataDirectory, "Raw Data\\", "CorePopEthAgedOn11_18ReAllocateD.csv"),
            quote=T, sep=",", row.names=F, na = "")
saveRDS(ReallocateD,
        file=paste0(pDataDirectory, "Raw Data\\", 
                    "CorePopEthAgedOn11_18ReAllocateD.rds"
        ))

rm(ReallocateD, AgesD)


#===PART 6X: Clean up                                                             #####

rm(CorePopEthAgedOn11_18, CorePopEthAgedOn11_18Total)
rm(pDataDirectory, pIncidenceDataDirectory)

# ================================================================================#####
#                                        FIN                                      #####
# ================================================================================#####
