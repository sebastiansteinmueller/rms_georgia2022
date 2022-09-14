############################################ START ######################################################

############################################ rms_tables_georgia2022.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, stats@unhcr.org
#### Project: RMS Georgia Pilot 2022
#### Description: Create indicator tables for COMPASS with clean weighted/calibrated data from the RMS Pilot in the Georgia Tbilisi Administered Territory


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
options(warn = 0)

### functions
source("rmsfunctions.R")

## colours (https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/strategic/documents/english/brand-book/UNHCR-Brand%20Book.pdf)
unhcrPaletteBlue <- c("#0072BC", "#338EC9", "#66AAD7", "#99C7E4", "#CCE3F2")
unhcrPaletteBlack <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC")
unhcrPaletteYellow <- c("#FAEB00", "#FBEF33", "#FCF366", "#FDF799", "#FEFBCC")
unhcrPaletteWhite <- c("#FFFFFF")
unhcrPaletteRed <- c("#E73451")
unhcrPaletteGreen <- c("#00AB92")


### clean and weighted data created with rms_process_[countryyear] and rms_weight_[countryyear]

load("data/rms_clean_weighted_georgia2022.RData")





#### III. Indicator tables #####


### By demographics of all HH members

t.hhsize <- rmstable(hhsizecat, hh.design, headHHR02, headHHR03, hhdisability3, studyunit = "Households (HH size includes Georgian household members)")

t.hhm.gender <- rmstable(R02x, hhmref.design %>% mutate(R02x = R02), R02, R03cat, DISABILITY3, studyunit = "Household member")

t.hhm.age <- rmstable(R03catx, hhmref.design %>% mutate(R03catx = R03cat), R02, R03cat, DISABILITY3, studyunit = "Household member")

t.hhm.disability <- rmstable(DISABILITY3x, hhmref.design %>% mutate(DISABILITY3x = DISABILITY3), R02, R03cat, DISABILITY3, studyunit = "Household member")


### By demographics of adult individuals interviewed
t.ind.gender <- rmstable(R02x, indref.design %>% mutate(R02x = R02), R02, R03cat, DISABILITY3, studyunit = "Adult individual")

t.ind.age <- rmstable(R03catx, indref.design %>% mutate(R03catx = R03cat), R02, R03cat, DISABILITY3, studyunit = "Adult individual")

t.ind.disability <- rmstable(DISABILITY3x, indref.design %>% mutate(DISABILITY3x = DISABILITY3), R02, R03cat, DISABILITY3, studyunit = "Adult individual")


### Core impact 2.2, (residing in safe and secure settlements) with access to basic facilities
t.imp2.2 <- rmstable(basicFacilities, hhmref.design, R02, R03cat, DISABILITY3, indicatorname = "Core impact 2.2", studyunit = "Household member")


### Core impact 2.3, access to health services
t.imp2.3 <- rmstable(healthaccess, indref.design, R02, R03cat, DISABILITY3, indicatorname = "Core impact 2.3", studyunit = "Adult individual")


### Core impact 3.3, Feeling safe walking alone at night, SDG 16.1.4
t.imp3.3 <- rmstable(SAF01SDG, indref.design, R02, R03cat, DISABILITY3, indicatorname = "Core impact 3.3", studyunit = "Adult individual")


### Core outcome 1.3, legally recognized identity documents or credentials
t.out1.3 <- rmstable(documents, hhmref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 1.3", studyunit = "Household member")


### Core outcome 8.2, primary reliance on clean fuels and technology, SDG 7.1.2
t.out8.2 <- rmstable(cookingfuel, hhmref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 8.2", studyunit = "Household member")

### Core outcome 12.1, basic drinking water services
t.out12.1 <- rmstable(basicDrinkingWater, hhmref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 12.1", studyunit = "Household member")

### Core outcome 12.2, basic sanitation facility
t.out12.2 <- rmstable(basicSanitation, hhmref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 12.2", studyunit = "Household member")

### Core outcome 13.1, bank account
t.out13.1 <- rmstable(banking, indref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 13.1", studyunit = "Adult individual")

### Core outcome 13.2, self-reported change in income
t.out13.2 <- rmstable(INC01, indref.design, R02, R03cat, DISABILITY3, indicatorname = "Core outcome 13.2", studyunit = "Adult individual")


### Core outcome 13.3, unemployment rate
t.out13.3 <- rmstable(employmentStatus, indref.design %>% filter(labourForce == 1), R02, R03cat, DISABILITY3, indicatorname = "Core outcome 13.3", studyunit = "Adult individual")

### (no indicator) Labour force participation rate
t.labourForce <- rmstable(labourForce, indref.design %>% filter(workingAge == 1), R02, R03cat, DISABILITY3, indicatorname = "Labour force participation rate", studyunit = "Adult individual")


### Core outcome 13.3, unemployment rate
t.out13.3 <- rmstable(employmentStatus, indref.design %>% filter(labourForce == 1), R02, R03cat, DISABILITY3, indicatorname = "Core outcome 13.3", studyunit = "Adult individual")

#### IV. Merge indicator tables and write to excel #####

t.rms <- bind_rows(
            t.hhsize,
            t.hhm.gender,
            t.hhm.age,
            t.hhm.disability,
            t.imp2.2,
            t.out1.3,
            t.out8.2,
            t.ind.gender,
            t.ind.age,
            t.ind.disability,
            t.imp2.3,
            t.imp3.3,
            t.out12.1,
            t.out12.2,
            t.out13.1,
            t.out13.2,
            t.labourForce,
            t.out13.3
  )


write.xlsx(t.rms, "output/RMS indicator tables_Georgia 2022.xlsx")

############################################ END ######################################################
