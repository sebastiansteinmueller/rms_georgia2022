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


#### II. Define weights / post-stratification distributions #####

# add



#### III. Make survey objects #####

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


#### IV. Write to Rdata for analysis #####

save(hh, s1, hh.design, hhref.design, ind.design, indref.design, file = "data/rms_clean_weighted_georgia2022.RData")


############################################ END ######################################################
