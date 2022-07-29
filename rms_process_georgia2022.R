############################################ START ######################################################

############################################ rms_process_georgia2022.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, stats@unhcr.org
#### Project: RMS Georgia Pilot 2022
#### Description: Check, clean and process data incl creating variables for RMS Pilot data in the Georgia Tbilisi Administered Territory


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

### functions
# source function file if required


### load dataset from kobo

koboid <- "ae5bvmJUXEsxk6G9xuSYyW" # <- CHANGE TO YOUR KOBO FORM ID (see readme on https://github.com/dickoa/robotoolbox for setting up robotoolbox)
asset <- kobo_asset(koboid)
dat <-  kobo_submissions(asset)

hh <- dat$main
s1a <- dat$S1A
s1b <- dat$S1B

rm(list="dat")


#### II. Check and process data, create indicator variables #####

dim(hh)
dim(s1a)
dim(s1b)



### merge

## create unique variables indid and indid2 to merge between s1 variables to hh individual interviews

# check if merging variable is unique
sum(duplicated(s1a %>% filter(!is.na(adult_age)) %>% unite(indid, c(`_parent_index`, adult_age)) %>% select(indid) )) # OK, unique

hh <- hh %>%
  unite(indid, c(`_index`, selected_adult), remove = F) # indid: merge selected adult HH roster information to individual level interview


s1a <- s1a %>%
  unite(indid, c(`_parent_index`, adult_age), remove = F) %>% # indid: merge selected adult HH roster information to individual level interview
  unite(indid2, c(`_parent_index`, R01, R03), remove = F) # indid2: all HH roster individuals: merging variable to merge s1a - s1b

sum(duplicated(s1a$indid2)) # OK, unique


s1b <- s1b %>%
  unite(indid2, c(`_parent_index`, name_individual, ind_age_year), remove = F) # auxiliary merging variable to merge s1a - s1b

sum(duplicated(s1b$indid2)) # OK, unique


dim(s1a)
dim(s1b)
s1 <- s1a %>%
  left_join(s1b %>% select(c("indid2", colnames(s1b)[!(colnames(s1b) %in% colnames(s1a))])), by = "indid2")
dim(s1)


### checks



## check unique head of household
# View(s1 %>% group_by(`_parent_index`) %>% filter(sum(R02_rel == "1") == 0)) # three HHs with no head
# View(s1 %>% group_by(`_parent_index`) %>% filter(sum(R02_rel == "1") > 1)) # two HHs with more than one head


## create unique head of household variable:

# 1) if none or more than one head of household, assign oldest HH member (oldest head of household) as head of household

table(s1$R02_rel)

s1.oldestHHM <- s1 %>% # find oldest HH member per HH
  group_by(`_parent_index`) %>%
  slice_max(R03, with_ties = F) %>%
  ungroup() %>%
  mutate(oldestHHM = 1) %>%
  select(`_index`, oldestHHM)

s1.oldestHead <- s1 %>% # find oldest head of household per HH
  filter(R02_rel == "1") %>%
  group_by(`_parent_index`) %>%
  slice_max(R03, with_ties = F) %>%
  ungroup() %>%
  mutate(oldestHead = 1) %>%
  select(`_index`, oldestHead)

s1 <- s1 %>%
  left_join(s1.oldestHHM) %>%
  left_join(s1.oldestHead) %>%
  group_by(`_parent_index`) %>%
  mutate(
    householdHead = case_when(
      sum(R02_rel == "1") == 1 & R02_rel == "1" ~ 1,
      sum(R02_rel == "1") == 1 & R02_rel != "1" ~ 0,
      sum(R02_rel == "1") == 0 & oldestHHM == 1 ~ 1,
      sum(R02_rel == "1") == 0 & is.na(oldestHHM) ~ 0,
      sum(R02_rel == "1") > 1 & R02_rel == "1" & oldestHead == 1 ~ 1,
      sum(R02_rel == "1") > 1 &R02_rel == "1" & is.na(oldestHead) ~ 0,
      sum(R02_rel == "1") > 1 & R02_rel != "1"  ~ 0
    )
  ) %>%
  mutate(
    householdHead = labelled(householdHead,
                           labels = c(
                             "Head of household" = 1,
                             "Not head of household" = 0
                           ),
                           label = "Head of household")
  ) %>%
  ungroup()

## check head of household again
# View(s1 %>% group_by(`_parent_index`) %>% filter(sum(householdHead == 1) != 1)) # OK, no HHs with other than 1 head

# check age range of eligible adults for individual interviews
range(s1 %>% select(`_parent_index`, adult_age, R03) %>% filter(!is.na(adult_age)) %>% select(R03)) # ERROR in Georgia xlsform: only adults 19+ selected



### demographics and table headers


## create categorical age and variable yes/no for disability; create further summary and discretisized variables for indicators and table headers
# https://www.washingtongroup-disability.com/fileadmin/uploads/wg/Documents/WG_Document__5C_-_Analytic_Guidelines_for_the_WG-SS__Stata_.pdf

s1 <- s1 %>% # demographics / disaggregation variables
  mutate(
    R03cat = cut(R03, # UNHCR age brackets
                 breaks = c(-1, 4, 11, 17, 24, 49, 59, Inf),
                  labels = c(1, 2, 3, 4, 5, 6, 7)),
    R03cat2 = cut(R03, # UNHCR broad age brackets
                 breaks = c(-1, 4, 11, 17, 59, Inf),
                 labels = c(1, 2, 3, 4, 5))
  ) %>%
  mutate(
    R03cat = labelled(R03cat, labels =
                        c("0-4"=1, "5-11"=2, "12-17"=3, "18-24"=4, "25-49"=5, "50-59"=6, "60+"=7),
                        label = "UNHCR fine age brackets"),
    R03cat2 = labelled(R03cat2,
                       labels = c("0-4"=1, "5-11"=2, "12-17"=3, "18-59"=4, "60+"=5),
                       label = "UNHCR broad age brackets (18-59)")
  ) %>%
  mutate( # primary citizenship from REF01 and REF02
    citizenship = case_when(
                REF01 == "1" ~ "GEO",
                REF01 %in% c("2", "98") ~ as.character(REF02),
                REF01 == "99" ~ "99"
        )
  ) %>%
  mutate(citizenship = labelled(citizenship,
                                labels = val_labels(s1$REF02),
                                label = var_label(s1$REF02))
  ) %>%
  mutate(originAux = case_when( # create auxiliary variable to assign origin defined as imputed citizenship: country of birth if citizenship not recorded and for foreign-born stateless persons, most common origin of all other HH members for native-born stateless people and those with still unknown origin"
                !(citizenship %in% c("77", "98", "99")) ~ as.character(citizenship),
                citizenship %in% c("77", "98", "99") & !(REF06 %in% c("77", "98", "99", "GEO")) ~ as.character(REF06), # adjust country code
                citizenship %in% c("77", "98", "99") & REF06 %in% c("77", "98", "99", "GEO") ~ NA_character_, # adjust country code
        )
  ) %>%
  mutate( # disability identifier variables according to Washington Group standards
    disaux1_234 = DIS01 %in% c("2","3","4"), # indicator variables for all 6 domains with value TRUE if SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_234 = DIS02 %in% c("2","3","4"),
    disaux3_234 = DIS03 %in% c("2","3","4"),
    disaux4_234 = DIS04 %in% c("2","3","4"),
    disaux5_234 = DIS05 %in% c("2","3","4"),
    disaux6_234 = DIS06 %in% c("2","3","4"),

    disaux1_34 = DIS01 %in% c("3","4"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_34 = DIS02 %in% c("3","4"),
    disaux3_34 = DIS03 %in% c("3","4"),
    disaux4_34 = DIS04 %in% c("3","4"),
    disaux5_34 = DIS05 %in% c("3","4"),
    disaux6_34 = DIS06 %in% c("3","4")
  ) %>%
  mutate(
    disSum234 = rowSums(select(., disaux1_234, disaux2_234 , disaux3_234 , disaux4_234 , disaux5_234 , disaux6_234)), # count number of TRUE indicator variables over 6 domains
    disSum34 = rowSums(select(., disaux1_34, disaux2_34 , disaux3_34 , disaux4_34 , disaux5_34 , disaux6_34)) # count number of TRUE indicator variables over 6 domains

  ) %>%
  mutate(
    DISABILITY1 = case_when( # : the level of inclusion is at least one domain/question is coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum234 >= 1 ~ 1,
      disSum234 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY2 = case_when( # : the level of inclusion is at least two domains/questions are coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL or any 1 domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL
      disSum234 >= 2 | disSum34 >=1  ~ 1,
      disSum234 < 2 & disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum34 >= 1 ~ 1,
      disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY4 = case_when( # : the level of inclusion is at least one domain/question is coded CANNOT DO AT ALL.
      DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4" ~ 1,
      !(DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4") & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY1 = labelled(DISABILITY1,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 1"),
    DISABILITY2 = labelled(DISABILITY2,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 2"),
    DISABILITY3 = labelled(DISABILITY3,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 3"),
    DISABILITY4 = labelled(DISABILITY4,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 4")
  )


# check:
# View(s1 %>% select(DIS01, DIS02, DIS03, DIS04, DIS05, DIS06, disSum234, disSum34, DISABILITY1, DISABILITY2, DISABILITY3, DISABILITY4)) # OK


### indicators in individual dataset  ###

## outcome 1.3, documents/credentials
s1 <- s1 %>%
  mutate( # auxiliary variables
    doc5plus  = case_when(
      REG01a == "1" | REG01b == "1" | REG01h == "1" | REG01i == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1" ~ 1,
      !(REG01a == "1" | REG01b == "1" | REG01h == "1" | REG01i == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1") ~ 0
    ),
    doc04  = case_when(
      REG04a == "1" | REG04f == "1" | REG04g == "1" | REG04c == "1" | REG04e == "1"  ~ 1,
      !(REG04a == "1" | REG04f == "1" | REG04g == "1" | REG04c == "1" | REG04e == "1") ~ 0
    )
  ) %>%
  mutate(
    documents = case_when(
     R03 < 5 ~ doc04,
     R03 >= 5 ~ doc5plus
    )
  ) %>%
  mutate(
    documents = labelled(documents,
      labels = c(
        "Does not have documents or credentials" = 0,
        "Has documents or credentials" = 1
      ),
      label = "Valid identify documents or credentials"
    )
  )


# check:
# View(s1 %>% select(R03cat, REG01a, REG01b, REG01h, REG01i, REG01d, REG01e, REG01f, doc5plus, REG04a, REG04f, REG04g, REG04e, doc04, documents) %>% arrange(documents)) # OK



### indicators in HH dataset ###


## outcome 13.3, unemployment
# Standard: https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/normativeinstrument/wcms_230304.pdf
# variable derivation: https://www.ilo.org/ilostat-files/LFS/ILO_CAPI_LFS_VARIABLE_DERIVATION_GUIDE%20(A1V3).pdf
hh <- hh %>%
  left_join(s1 %>% select(indid, R03), by = "indid") %>% # need age variable from HH member dataset for working age population definition
  mutate(
    workingAge = case_when( # ADJUST BY COUNTRY CONTEXT. ILO recommends 15+, in Georgia only 18+ interviewed for labour force
      R03 < 18 ~ 0,
      R03 >= 18 ~ 1
    ),
    atWork = case_when( # currently in paid employment?
      UNEM01 == "1"  ~ 1, # paid employed
      UNEM01 == "2" & UNEM02 == "2" & UNEM03 == "2" ~ 2, # no current work -> establish temporary absence (UNEM04, UNEM05)
      UNEM01 == "2" & (UNEM02 == "1" | UNEM03 == "1") ~ 3 # own business / paid family job -> establish work in agriculture/fishing ( UNEM07)
    )
  ) %>%
  mutate(
    tempAbs = case_when( # temporarily absent from paid employment?
      atWork == 2 & UNEM04 == "1" ~ 1, # paid employed
      atWork == 2 & UNEM04 == "2" & UNEM05 == "1" ~ 3, # temporary absence from own business / paid family job -> establish work in agriculture/fishing (UNEM07)
      atWork == 2 & UNEM04 == "2" &  UNEM05 == "2" ~ 2 # no work, no temporary absence -> establish work in agriculture/fishing (UNEM06)
    )
  ) %>%
  mutate(
    agriFish = case_when(
      atWork == 3 & UNEM07 == "3" ~ 1, # own business or family paid job not in agriculture/fishing -> paid employed
      atWork == 3 & UNEM07 %in% c("1", "2") ~ 2, # own business or family paid job in agriculture/fishing -> establish destination of production (UNEM08)
      tempAbs == 3 & UNEM07 == "3" ~ 1, # own business or family paid job not in agriculture/fishing -> paid employed
      tempAbs == 3 & UNEM07 %in% c("1", "2") ~ 2, # own business or family paid job in agriculture/fishing -> establish destination of production (UNEM08)
      tempAbs == 2 & UNEM06 == "3" ~ 0, # not paid employed
      tempAbs == 2 & UNEM06 %in% c("1", "2") ~ 2 # own business or family paid job in agriculture/fishing -> establish destination of production (UNEM08)
    )
  ) %>%
  mutate(
    agriFishDest = case_when(
      agriFish == 2 & UNEM08 %in% c("3", "4") ~ 0, # only/mainly for family use -> not paid employed
      agriFish == 2 & UNEM08 %in% c("1", "2") ~ 1 # only/mainly for sale -> paid employed
    )
  ) %>%
  mutate(
    employed = case_when( # variable for paid employment
      (agriFishDest == 0 | agriFish ==0) & UNEM09 == "1" & UNEM10 == "1" ~ 0, # not paid employed but looking and available -> unemployed in the labour force
      atWork == 1 | tempAbs == 1 | agriFish == 1 | agriFishDest == 1 ~ 1, # paid employed in the labour force
      (agriFishDest == 0 | agriFish == 0) & (UNEM09 != "1" | UNEM10 != "1") ~ NA_real_ # not paid employed, not job-seeking and/or not available to start work -> outside the labour force
    )
  ) %>%
  mutate(
    unemployed = case_when( # variable for unemployment
      employed == 1 ~ 0,
      employed == 0 & UNEM09 == "1" & UNEM10 == "1" ~ 1,
      employed == 0 & (UNEM09 != "1" | UNEM10 != "1") ~ NA_real_ # not in employment, not job-seeking and/or not available to start work -> outside the labour force
    )
  ) %>%
  mutate(
    labourForce = case_when(
      workingAge == 1 & (!(employed == 1 | unemployed == 1) | is.na(employed) | is.na(unemployed)) ~ 0,
      workingAge == 1 & (employed == 1 | unemployed == 1) ~ 1, # working or looking for work and available
      workingAge == 0 ~ NA_real_
    )
  ) %>%
  mutate(
    workingAge = labelled(workingAge,
                           labels = c(
                             "Not working-age" = 0,
                             "Working-age" = 1
                           ),
                           label = "Working-age population"
                  ),
    labourForce = labelled(labourForce,
                          labels = c(
                            "Outside the labour force" = 0,
                            "In the labour force" = 1
                          ),
                          label = "Labour force"
                  ),
    employmentStatus = labelled(unemployed,
                          labels = c(
                            "Employed" = 0,
                            "Unemployed" = 1
                           ),
                        label = "Employment status"
    )
  )

# check

table(hh$labourForce, hh$employed, useNA = "ifany")
table(hh$labourForce, hh$unemployed, useNA = "ifany")

table(hh$employed, hh$unemployed, useNA = "ifany")


View(hh %>% select(workingAge, UNEM01:UNEM10, atWork, tempAbs, agriFish, agriFishDest, labourForce, employed, unemployed) %>% arrange(labourForce, employed, unemployed))


## impact 2.3, access to health services
hh <- hh %>%
  mutate(
    healthaccess = case_when(
      HACC03 == "1" ~ 0, # non-access includes also respondents who did access health services in HACC01, but mentioned they could not access other health services in HACC03
      HACC03 == "2" & HACC01 == "1" ~ 1,
      HACC03 == "2" & HACC01 == "2" ~ NA_real_ # note: denominator are only those who needed health services. If did not use or need health services, they should not be counted, i.e. NA
    )
  ) %>%
  mutate(
    healthaccess = labelled(healthaccess,
                         labels = c(
                           "No" = 0,
                           "Yes" = 1
                         ),
                         label = "Received needed health services in the last 3 months"
                    )
    )



## outcome 8.2, primary reliance on clean fuels and technology
hh <- hh %>%
  mutate(
    cookingfuel = case_when(
      COOK01 == "1" & (COOK03 %in% c("1", "2", "3", "4", "5") | (COOK03 %in% c("6", "96") & COOK04 %in% c("1", "2", "3", "22"))) ~ 1, # see https://mics.unicef.org/files?job=W1siZiIsIjIwMTcvMDIvMDMvMTYvMjcvMjUvNTk5L1BpY3RvcmlhbHNfV0hPX0hvdXNlaG9sZF9FbmVyZ3lfVXNlX0NhdGFsb2d1ZV9TZXB0ZW1iZXJfMjAxNl8ucGRmIl1d&sha=57b4a452fcc0ac88
      COOK01 == "1" & (COOK03 %in% c("7", "8", "9", "10") | (COOK03 %in% c("6", "96") & !(COOK04 %in% c("1", "2", "3", "22")))) ~ 0,
      COOK01 == "2" ~ NA_real_
    )
  ) %>%
  mutate(
    cookingfuel = labelled(cookingfuel,
                            labels = c(
                              "Polluting" = 0,
                              "Clean" = 1
                            ),
                            label = "Primary cooking fuel and technology"
    )

  )



## impact 3.3, feeling safe walking alone

hh <- hh %>%
  mutate(SAF01SDG = case_when( # create variable for Subject safety feeling indicator https://unstats.un.org/sdgs/metadata/files/Metadata-16-01-04.pdf
      SAF01 %in% c(1,2) ~ 1,
      SAF01 %in% c(3,4) ~ 2,
      SAF01 %in% c(97) ~ 97,
      SAF01 %in% c(98, 99) ~ 98
    )
  ) %>%
  mutate(
    SAF01SDG = labelled(SAF01SDG,
             labels = c(
               "Very or fairly safe" = 1,
               "Very or bit unsafe" = 2,
               "I never walk alone after dark" = 97,
               "Unknown/Prefer not to respond" = 98
             ),
             label = "SDG 16.1.4: How safe do you feel walking alone in your area/neighbourhood after dark?")
  )





## remove auxiliary variables
s1 <- s1 %>%
  select(-c(disaux1_234:disSum34), -doc5plus, -doc04)


## HH-head demographics / aggregate HH level disability for table headers for HH level indicators

s1.hhlevel <- s1 %>%
  group_by(`_parent_index`) %>%
  summarise(
    hhsize = n(), # hh size
    hhdisability3aux = sum(DISABILITY3 == 1), # number of disabled household members
    hhNationals = sum(citizenship == "GEO"), # number of National citizens in HH (adjust national code for other RMS)
    hhadults = sum(R03 >=18) # number of adults in HH
  ) %>%
  ungroup() %>%
  mutate(hhdisability3 = case_when( #  disability at HH level (at least one disabled HH member (DISABILITY3 from WG) vs none)
    hhdisability3aux == 0 ~ 0,
    hhdisability3aux >0 ~ 1
    )
  ) %>%
  mutate(hhdisability3 = labelled(hhdisability3,
                                  labels = c(
                                    "Without disability" = 0,
                                    "With disability" = 1
                                  ),
                                  label = "Washington Group disability identifier 3 for household level")
  ) %>%
  mutate(
    hhsizecat = cut(hhsize, # categorical hh size
                    breaks = c(-1, 1, 3, 5, Inf),
                    labels = c("1", "2-3", "4-5", "6+"))
  ) %>%
  mutate(
    allNationalsAux = hhsize - hhNationals
  ) %>%
  mutate(
    allNationals = case_when(
      allNationalsAux == 0 ~ 1,
      allNationalsAux > 0 ~ 0
    )
  ) %>%
  mutate(allNationals = labelled(allNationals,
                                  labels = c(
                                    "At least one non-national in HH" = 0,
                                    "All HH members nationals" = 1
                                  ),
                                  label = "National citizens in HH")
  ) %>%
  left_join( # add sex and age of head of household
    s1 %>%
      filter(householdHead == 1) %>%
      select(`_parent_index`, R02, R03, R03cat, R03cat2, citizenship, REF06),
    by = "_parent_index"
  ) %>%
  rename(
    headHHR02 = R02,
    headHHR03 = R03,
    headHHR03cat = R03cat,
    headHHR03cat2 = R03cat2,
    headHHcitizenship = citizenship,
    headHHREF06 = REF06
  ) %>%
select(-hhdisability3aux, -allNationalsAux)

# check unique HHs
sum(duplicated(s1.hhlevel$`_parent_index`)) # OK, 0

# most frequent non-national origin nationality/DOB in HH
s1.HHorigin <- s1 %>%
  mutate(originAux2 = ifelse(originAux == "GEO", NA_character_, originAux)) %>%
  filter(!is.na(originAux2)) %>%
  group_by(`_parent_index`, originAux2) %>%
  summarise(n= n()) %>%
  slice_max(originAux2, with_ties = F) %>%
  ungroup()


# merge to HH dataset
hh <- hh %>%
  left_join(s1.hhlevel,
            by = c("_index" = "_parent_index")
  ) %>%
  left_join(s1.HHorigin %>% select(`_parent_index`, originAux2 ),
            by = c("_index" = "_parent_index")
  )




## merge s1 variables of selected individual (this can be someone other than head of household) to hh

dim(hh)
dim(s1)
hh <- hh %>%
  left_join(s1 %>% select(indid, R01, R02, R02_rel, R03cat, R03cat2, R06, citizenship, REF06, countrybirth, REF15, REF16, documents, IDP01, DIS01:DISABILITY4),
            by = "indid")
dim(hh)


## merge indicator hh variables to be expressed as population % to s1, and most frequent origin to update origin variable

dim(hh)
dim(s1)
s1 <- s1 %>%
  left_join(hh %>% select(`_index`, cookingfuel, originAux2, hhadults), by = c("_parent_index" = "_index"))
dim(s1)


## finalise imputing origin variable for missing/unknown nationality and stateless people
s1 <- s1 %>%
  mutate(origin = case_when(
            !(is.na(originAux)) ~ as.character(originAux),
            is.na(originAux) & !is.na(originAux2) ~ as.character(originAux2),
            is.na(originAux) & is.na(originAux2) & REF06 != "GEO" ~ as.character(REF06),
            is.na(originAux) & is.na(originAux2) & REF06 == "GEO" ~ as.character(citizenship),
            )
         ) %>%
  mutate(origin = labelled(origin,
         labels = val_labels(s1$citizenship),
         label = "Origin (nationality where available, country of birth for foreign-born stateless persons)")
         )

hh <- hh %>%
  left_join(s1 %>% select(indid, origin),
            by = "indid")

#### III. Remove personal identifiers #####

hh <- hh %>%
  select(-c(adult_name, fam_name1:fam_name9, namechild2less, women_name_b_total, originAux, originAux2, atWork, tempAbs, agriFish, agriFishDest, employed, unemployed))

s1 <- s1 %>%
  select(-c(R01, indid2, R05b, calculation_002, AgeMonths, calculation3, calculation4, R04, adult, name_individual, ind_age_month, originAux, originAux2))



#### IV. Write to Rdata for weighting and post-stratification #####

save(hh, s1, file = "data/rms_clean_georgia2022.RData")


############################################ END ######################################################
