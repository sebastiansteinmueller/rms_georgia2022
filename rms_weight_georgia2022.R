############################################ START ######################################################

############################################ rms_weight_georgia2022.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, stats@unhcr.org
#### Project: RMS Georgia Pilot 2022
#### Description: Weight, calibrate, adjust population and create survey object from clean data for the RMS Pilot in the Georgia Tbilisi Administered Territory


rm(list=ls()) # clear workspace


#### I. Read data, packages etc #####


### packages

library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(rlang)
library(openxlsx)


### functions
# add as required


### load clean data created with rms_process_[countryyear]

load("data/rms_clean_georgia2022.RData")


### load ASR 2021 cleaned data for post-stratification
load("data/asr_2020-2021_20220517.RData")

rm(list=c("hst", "idp", "oth", "ref", "ret", "roc", "rsd", "sta", "uasc", "vda", "countries")) # remove tables not needed



#### II. Compare ASR and survey data #####

## calculate post-stratification counts from ASR end-2021 demographic table for refugees/asylum-seekers in Georgia

dem <- dem %>%
  filter(year == 2021, asylum_iso3 == "GEO", populationType %in% c("REF", "ASY"), totalEndYear >0) %>% # ROC (refugee-like) are in Abkhazia according to PSR internal notes
  mutate(
    checkFemale = rowSums(select(.,female_0_4:female_12_17, female_18_59, female_60), na.rm = T), # check age categories add up to totals
    checkMale = rowSums(select(.,male_0_4:male_12_17, male_18_59, male_60), na.rm = T),
    checkTotalEndYear =  rowSums(select(.,female_0_4:female_12_17, female_18_59, female_60,
                                        male_0_4:male_12_17, male_18_59, male_60),
                                 na.rm = T)
    ) %>%
  mutate(
    checkFemaleDiff = checkFemale-female,
    checkMaleDiff = checkMale-male,
    checkTotalEndYearDiff = checkTotalEndYear-totalEndYear
  )

# check
  summary(dem$checkFemaleDiff)
  summary(dem$checkMaleDiff)
  summary(dem$checkTotalEndYearDiff) # all OK, 0 differences

t.ori.dem <- dem %>%
    mutate(origin_iso3 = ifelse(origin == "UKN", "98", as.character(origin_iso3))) %>% # origin ISO3 = 98 for unknowns
    group_by(origin_iso3) %>%
    summarise_at(vars(female_0_4:female_12_17, female_18_59, female_60,
                      male_0_4:male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>%
    ungroup() %>%
    mutate(totalEndYear = rowSums(select(.,`female_0_4`:`male_60`), na.rm = T)) %>%
    arrange(desc(totalEndYear))

t.ori.dem.adult <- dem %>%
  mutate(origin_iso3 = ifelse(origin == "UKN", "98", as.character(origin_iso3))) %>% # origin ISO3 = 98 for unknowns
  group_by(origin_iso3) %>%
  summarise_at(vars(female_18_59, female_60,
                   male_18_59, male_60), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(totalEndYear = rowSums(select(.,`female_18_59`:`male_60`), na.rm = T)) %>%
  arrange(desc(totalEndYear))

## compare to survey data

# selected adult respondent (should be random sample of adult survey population)
t.adultind.refori.dem <- hh %>%
  filter(citizenship != "GEO") %>%
  unite("ageSex", R02, R03cat2, remove = T) %>%
  group_by(citizenship, ageSex) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = ageSex, values_from = n) %>%
  mutate(total = rowSums(select(.,`Female_18-59`:`Male_60+`), na.rm = T)) %>%
  select(citizenship, `Female_18-59`, `Female_60+`, `Male_18-59`, `Male_60+`, total) %>%
  arrange(desc(total))

# all non-national HH members


#### III. Define weights / post-stratification distributions #####


#### IV. Make survey objects #####

### DEFINE YOUR SURVEY OBJECTS HERE ###

## household design with all interviewed HHs
hh.design <- hh %>%
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`) %>% # srvyr needs clean names
  as_survey_design() # <- DEFINE YOUR HH SURVEY OBJECT HERE

## household design only with HHs with at least one non-national member
hhref.design <- hh %>%
  filter(allNationals == 0) %>%
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`) %>% # srvyr needs clean names
  as_survey_design() # <- DEFINE YOUR HH SURVEY OBJECT HERE


## household member design with all interviewed HHs
ind.design <- s1 %>%
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`, parent_index = `_parent_index`) %>% # srvyr needs clean names
  as_survey_design(ids = `parent_index`) # <- DEFINE YOUR INDIVIDUAL SURVEY OBJECT HERE

## household member design only with non-nationals
indref.design <- s1 %>%
  filter(citizenship != "GEO") %>% # ADJUST COUNTRY CODE
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`, parent_index = `_parent_index`) %>% # srvyr needs clean names
  as_survey_design(ids = `parent_index`) # <- DEFINE YOUR INDIVIDUAL SURVEY OBJECT HERE


#### V. Write to Rdata for analysis #####

save(hh, s1, hh.design, hhref.design, ind.design, indref.design, file = "data/rms_clean_weighted_georgia2022.RData")


############################################ END ######################################################
