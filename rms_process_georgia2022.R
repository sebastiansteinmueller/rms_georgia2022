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
                  labels = c("0-4", "5-11", "12-17", "18-24", "25-49", "50-59", "60+")),
    R03cat2 = cut(R03, # UNHCR age brackets
                 breaks = c(-1, 4, 11, 17, 59, Inf),
                 labels = c("0-4", "5-11", "12-17", "18-59", "60+"))
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


### indicators in individual dataset

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


### indicators in HH dataset

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
    hhNationals = sum(citizenship == "GEO") # number of National citizens in HH (adjust national code for other RMS)
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

# merge to HH dataset

hh <- hh %>%
  left_join(s1.hhlevel,
            by = c("_index" = "_parent_index")
  )




## merge s1 variables of selected individual (this can be someone other than head of household) to hh

dim(hh)
dim(s1)
hh <- hh %>%
  left_join(s1 %>% select(indid, R01, R02, R02_rel, R03cat, R03cat2, R06, citizenship, REF06, countrybirth, REF15, REF16, documents, IDP01, DIS01:DISABILITY4),
            by = "indid")
dim(hh)


## merge indicator hh variables to be expressed as population % to s1

dim(hh)
dim(s1)
s1 <- s1 %>%
  left_join(hh %>% select(`_index`, cookingfuel), by = c("_parent_index" = "_index"))
dim(s1)


#### III. Remove personal identifiers #####

hh <- hh %>%
  select(-c(adult_name, fam_name1:fam_name9, namechild2less, women_name_b_total))

s1 <- s1 %>%
  select(-c(R01, indid2, R05b, calculation_002, AgeMonths, calculation3, calculation4, R04, adult, name_individual, ind_age_month))



#### IV. Write to Rdata for weighting and post-stratification #####

save(hh, s1, file = "data/rms_clean_georgia2022.RData")


############################################ END ######################################################
