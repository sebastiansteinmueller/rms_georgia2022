############################################ START ######################################################

############################################ rms_tables_georgia2022.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, stats@unhcr.org
#### Project: RMS Georgia Pilot 2022
#### Description: Create survey object and indicator tables for COMPASS with clean data from the RMS Pilot in the Georgia Tbilisi Administered Territory


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

### options
#options(scipen = 999)

### functions
source("rmsfunctions.R")

## colours (https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/strategic/documents/english/brand-book/UNHCR-Brand%20Book.pdf)
unhcrPaletteBlue <- c("#0072BC", "#338EC9", "#66AAD7", "#99C7E4", "#CCE3F2")
unhcrPaletteBlack <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")
unhcrPaletteYellow <- c("#FAEB00", "#FBEF33", "#FCF366", "#FDF799", "#FEFBCC")
unhcrPaletteWhite <- c("#FFFFFF")
unhcrPaletteRed <- c("#E73451")
unhcrPaletteGreen <- c("#00AB92")


### clean data created with rms_process_[countryyear]

load("data/rms_clean_georgia2022.RData")



#### II. Make survey objects ##### 

### DEFINE YOUR SURVEY OBJECTS HERE ###
hh.design <- hh %>% 
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`) %>% # srvyr needs clean names 
  as_survey_design() # <- DEFINE YOUR HH SURVEY OBJECT HERE

ind.design <- s1 %>% 
  to_factor() %>% # convert all labelled objects to factors
  rename(index = `_index`, parent_index = `_parent_index`) %>% # srvyr needs clean names 
  as_survey_design(ids = `parent_index`) # <- DEFINE YOUR INDIVIDUAL SURVEY OBJECT HERE

#### III. Indicator tables ##### 


### By demographics of all HH members

t.gender <- rmstable(R02x, ind.design %>% mutate(R02x = R02), R02, R03cat, DISABILITY3, studyunit = "Individual")

t.age <- rmstable(R03catx, ind.design %>% mutate(R03catx = R03cat), R02, R03cat, DISABILITY3, studyunit = "Individual")

t.disability <- rmstable(DISABILITY3x, ind.design %>% mutate(DISABILITY3x = DISABILITY3), R02, R03cat, DISABILITY3, studyunit = "Individual")

t.hhsize <- rmstable(hhsizecat, hh.design, headHHR02, headHHR03, hhdisability3, studyunit = "Household")


### Core impact 2.2, residing in safe and secure settlements
# t.out2.2 <- rmstable(settlements, hh.design, R02, R03cat, DISABILITY3)


### Core impact 2.3, access to health services
t.imp2.3 <- rmstable(healthaccess, hh.design, R02, R03cat, DISABILITY3, indicatorname = "Core impact 2.3", studyunit = "Adult individual")


### Core impact 3.3, Feeling safe walking alone at night, SDG 16.1.4 
t.imp3.3 <- rmstable(SAF01SDG, hh.design, R02, R03cat, DISABILITY3, indicatorname = "Core impact 3.3", studyunit = "Adult individual")


### Core outcome 1.3, legally recognized identity documents or credentials
t.out1.3 <- rmstable(documents,ind.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 1.3", studyunit = "Individual")


### Core outcome 8.2, primary reliance on clean fuels and technology, SDG 7.1.2
t.out8.2 <- rmstable(cookingfuel, ind.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 8.2", studyunit = "Individual")


### Core outcome 13.2, self-reported change in income
t.out13.2 <- rmstable(INC01, hh.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 8.2", studyunit = "Adult individual")


#### IV. Merge indicator tables and write to excel ##### 

t.rms <- bind_rows(
            t.gender,
            t.age,
            t.disability,
            t.hhsize,
            t.imp2.3,
            t.imp3.3,
            t.out1.3,
            t.out8.2,
            t.out13.2
  ) 


write.xlsx(t.rms, "output/RMS indicator tables_Georgia 2022.xlsx")

############################################ END ######################################################