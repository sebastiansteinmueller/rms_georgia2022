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
library(survey)
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

rm(list=c("hst", "idp", "oth", "ref", "ret", "roc", "rsd", "sta", "uasc", "vda")) # remove tables not needed


# merge regions of origin to data files

hh <- hh %>%
  left_join(countries %>%
              select(iso3, hcr_region) %>%
              rename(originHcrRegion = "hcr_region") %>%
              mutate(iso3 = as.character(iso3)),
            by = c("origin" = "iso3")) %>%
  mutate(originHcrRegion = case_when(
            is.na(originHcrRegion) ~ "Unknown",
            !(is.na(originHcrRegion)) ~ as.character(originHcrRegion)
      )
  )
table(hh$originHcrRegion, useNA = "ifany")

s1 <- s1 %>%
  left_join(countries %>%
              select(iso3, hcr_region) %>%
              rename(originHcrRegion = "hcr_region") %>%
              mutate(iso3 = as.character(iso3)),
            by = c("origin" = "iso3")) %>%
  mutate(originHcrRegion = case_when(
    is.na(originHcrRegion) ~ "Unknown",
    !(is.na(originHcrRegion)) ~ as.character(originHcrRegion)
    )
  )
table(s1$originHcrRegion, useNA = "ifany")


#### II. Compare ASR and survey data and define IRRS-compliant populations #####

### check which individuals and HHs are (all-) refugees/asylum-seekers
# for Georgia: the sampling frame was refugee/asylum-seeker/humanitarian status holder heads of HH
# we therefore assume that individual respondents and HH members are REF/ASY as long as they don't hold Georgian citizenship
# in other surveys with different sampling frames, the reason for the last migration to the country and the visa/resident permit might also be considered for population definition
# individuals with Georgian citizenship and HHs with only Georgians will be removed from the analyis datasets / survey objects

# adult individuals
t.adultind.ori <- hh %>%
  group_by(origin) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(n))  # 13% (n=44) respondents to individual interviews are Georgian (fix this in next form version, only interview refugees for individual interviews) - should be removed from the analysis

# HH members
t.hhm.ori <- s1 %>%
  group_by(origin) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(n)) # 23 % (n=196) HH members are Georgian (OK, it's plausible and to be expected that refugees live with nationals including former refugees now naturalised)

# HHs
t.hh.nat <- hh %>%
  group_by(allNationals) %>% # variable describing whether all HH members are nationals
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) # 2% (n=8) HHs consist of Georgian nationals only - should be removed from the analysis



### calculate post-stratification counts from ASR end-2021 demographic table for refugees/asylum-seekers in Georgia

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


demori <- dem %>%
    mutate(origin_iso3 = ifelse(origin == "UKN", "98", as.character(origin_iso3))) %>% # origin ISO3 = 98 for unknowns
    group_by(origin_iso3) %>%
    summarise_at(vars(female_0_4:female_12_17, female_18_59, female_60,
                      male_0_4:male_12_17, male_18_59, male_60), ~sum(., na.rm = T)) %>%
    ungroup() %>%
    mutate(totalEndYear = rowSums(select(.,`female_0_4`:`male_60`), na.rm = T)) %>%
    arrange(desc(totalEndYear)) %>%
    left_join(countries %>%
              select(iso3, hcr_region) %>%
              rename(originHcrRegion = "hcr_region") %>%
              mutate(iso3 = as.character(iso3)),
            by = c("origin_iso3" = "iso3")) %>%
    mutate(originHcrRegion = case_when(
                is.na(originHcrRegion) ~ "Unknown",
                !(is.na(originHcrRegion)) ~ as.character(originHcrRegion)
          )
    )
table(demori$originHcrRegion, useNA = "ifany")

## selected adult respondent (random sample of adult survey population)

# ASR
t.dem.ori.adult <- dem %>%
  mutate(origin_iso3 = ifelse(origin == "UKN", "98", as.character(origin_iso3))) %>% # origin ISO3 = 98 for unknowns
  group_by(origin_iso3) %>%
  summarise_at(vars(female_18_59, female_60,
                   male_18_59, male_60), ~sum(., na.rm = T)) %>%
  ungroup() %>%
  mutate(totalEndYear = rowSums(select(.,`female_18_59`:`male_60`), na.rm = T)) %>%
  arrange(desc(totalEndYear))

# compare to survey data
t.adultind.ori.agesex <- hh %>%
  filter(citizenship != "GEO") %>% # remove nationals
  to_factor() %>%
  unite("ageSex", R02, R03cat2, remove = T) %>%
  group_by(citizenship, ageSex) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = ageSex, values_from = n) %>%
  mutate(total = rowSums(select(.,`Female_18-59`:`Male_60+`), na.rm = T)) %>%
  select(citizenship, `Female_18-59`, `Female_60+`, `Male_18-59`, `Male_60+`, total) %>%
  arrange(desc(total))


# only broad age groups available in population data; 60+ bracket very few (both pop and survey), would lead to instability
# raking would be an option, but would ignore significant sex imbalance between origins
# therefore post-stratification on joint origin/sex distribution, but collapsing most of the origins


t.adultrefind.ori <- hh %>%
  filter(origin != "GEO") %>%
  group_by(origin) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(n))

t.dem.ori <- demori %>%
  group_by(origin_iso3) %>%
  summarise(n = sum(totalEndYear)) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(n))

t.adultrefind.ori <- hh %>%
  filter(origin != "GEO") %>%
  group_by(origin, R02) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = R02, values_from = n) %>%
  ungroup() %>%
  mutate(n = rowSums(select(., Female, Male), na.rm = T)) %>%
  mutate(percFemale = Female/n,
         percMale = Male/n) %>%
  arrange(desc(n))

t.adultrefind.demori <- demori %>%
  select(origin_iso3, originHcrRegion, female_18_59, female_60, male_18_59, male_60 ) %>%
  mutate(Female = rowSums(select(., female_18_59, female_60), na.rm=T),
         Male = rowSums(select(., male_18_59, male_60), na.rm=T)) %>%
  mutate(n = rowSums(select(., Female, Male), na.rm = T)) %>%
  mutate(percFemale = Female/n,
         percMale = Male/n) %>%
  arrange(desc(n))



  # all non-national HH members


#### III. Define weights / post-stratification distributions and variables #####

hh <- hh %>% # define variable originSex.ps for post-stratification of individual adult interviews by origin and sex
  mutate(origin.ps = case_when(
              origin == "IRN" ~ "IRN",
              origin == "IRQ" ~ "IRQ",
              origin == "RUS" ~ "RUS",
              origin == "UKR" ~ "UKR",
              !(origin %in% c("IRN", "IRQ", "RUS", "UKR")) & originHcrRegion == "Middle East and North Africa" ~ "OtherMENA",
              !(origin %in% c("IRN", "IRQ", "RUS", "UKR")) & originHcrRegion != "Middle East and North Africa" ~ "OtherNotMENA"
    )
  ) %>%
  unite("originSex.ps", origin.ps, R02, remove = F)

table(hh$origin.ps, useNA = "ifany")
table(hh$originSex.ps, useNA = "ifany")

t.adultrefind.demori.ps <- t.adultrefind.demori %>%
  select(originHcrRegion, origin_iso3, Female, Male) %>%
  pivot_longer(cols = c(Female, Male), names_to = "R02") %>%
  mutate(origin.ps = case_when(
    origin_iso3 == "IRN" ~ "IRN",
    origin_iso3 == "IRQ" ~ "IRQ",
    origin_iso3 == "RUS" ~ "RUS",
    origin_iso3 == "UKR" ~ "UKR",
    !(origin_iso3 %in% c("IRN", "IRQ", "RUS", "UKR")) & originHcrRegion == "Middle East and North Africa" ~ "OtherMENA",
    !(origin_iso3 %in% c("IRN", "IRQ", "RUS", "UKR")) & originHcrRegion != "Middle East and North Africa" ~ "OtherNotMENA"
    )
  ) %>%
  unite("originSex.ps", origin.ps, R02, remove = F)


PS.AdultInd <- t.adultrefind.demori.ps %>%
  group_by(originSex.ps) %>%
  summarise(Freq = sum(value))

pscheck.adultind <- as.data.frame(table(hh[hh$origin!="GEO",]$originSex.ps, useNA = "ifany"))

N.AdultInd <-  sum(PS.AdultInd$Freq) # total population size for individual adult interviews

#### IV. Make survey objects #####

### DEFINE YOUR SURVEY OBJECTS HERE ###


## individual interview design
indref.auxdesign <- hh %>%
  filter(origin != "GEO") %>%
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`) %>% # srvyr needs clean names
  mutate(dweightAdultInd = N.AdultInd/n(), # design weights (inverse selection probabilities) under assumed SRSWOR
         fpcAdultInd = n()/N.AdultInd) %>% # finite population correction
  as_survey_design(ids = NULL, #  <- DEFINE YOUR HH SURVEY OBJECT HERE
                   strata = NULL,
                   weights = dweightAdultInd,
                   fpc = fpcAdultInd) #
# check:
indref.auxdesign.agecheck <- indref.auxdesign %>%
  group_by(R03cat2) %>%
  summarise(n = survey_total(),
            perc = survey_mean())
prop.table(table(hh[hh$origin != "GEO",]$R03cat2)) # OK, no adjustment in point estimate for srswor

# post-stratify individual design to ASR end-2021 population counts by origin and sex:
indref.design <- postStratify(design = indref.auxdesign,
                                 strata = ~originSex.ps,
                                 population = PS.AdultInd) %>%
  as_survey()


# checks:
# 1) survey totals over post-stratification variable same as ASR (population) counts?
t.indref.originSex <- indref.design %>%
  group_by(originSex.ps) %>%
  summarise(n = survey_total(),
          perc = survey_mean())
# compare to ASR:
t.indref.originSex
PS.AdultInd # OK (totals match)

# 2) check other variables (does stratification change distributions?)
t.indref.saf01 <- indref.design %>%
  group_by(SAF01SDG) %>%
  summarise(n = survey_total(),
            perc = survey_mean())

# compare to unweighted:
t.indref.saf01
prop.table(table(hh[hh$origin!="GEO",]$SAF01SDG)) # OK (minor changes)


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
hhm.design <- s1 %>%
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`, parent_index = `_parent_index`) %>% # srvyr needs clean names
  as_survey_design(ids = `parent_index`) # <- DEFINE YOUR INDIVIDUAL SURVEY OBJECT HERE

## household member design only with non-nationals
hhmref.design <- s1 %>%
  filter(citizenship != "GEO") %>% # ADJUST COUNTRY CODE
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`, parent_index = `_parent_index`) %>% # srvyr needs clean names
  as_survey_design(ids = `parent_index`) # <- DEFINE YOUR INDIVIDUAL SURVEY OBJECT HERE


#### V. Write to Rdata for analysis #####

save(hh, s1, hh.design, hhref.design, hhm.design, hhmref.design, indref.design, file = "data/rms_clean_weighted_georgia2022.RData")


############################################ END ######################################################
