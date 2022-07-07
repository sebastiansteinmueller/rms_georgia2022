### function rmstable 
### creates table with headers for gender, age, disability on categorical indicator variable x on survey object design
###
### pass arguments x, design, gender, age unquoted (dplyr). pass indicatorname and studyunit as strings
###
### arguments
### x: categorical indicator variable
### design: design object of class tbl_svy (see srvyr package)
### gender: gender or sex variable with labels Female and Male
### age: categorical age variable with UNHCR age categories and labels in format "0-4" (see in code below for exact age brackets needed)
### indicatorname (optional): name of the indicator
### studyunit (optional): unit of analysis, e.g. household, individual etc
### disability: variable indicating disability with labels "Without disability", "With disability"
###
### begin function rmstable
rmstable <- function(x, design, gender, age, disability, indicatorname = "", studyunit = ""){
  library(tidyverse)
  library(rlang)
  library(srvyr)
  
  design <- design %>% 
    filter(!(is.na({{x}})))
  
  t.total <- design %>% 
    group_by({{x}}) %>%
    summarise(n = survey_total(),
              unwn = unweighted(n()),
              `%` = survey_mean(vartype = "ci", proportion = T)) 
  
  t.gen <- design %>%
    group_by({{gender}}, {{x}}) %>%
    summarise(n = survey_total(),
              unwn = unweighted(n()),
              `%` = survey_mean(vartype = "ci", proportion = T)) %>%
    pivot_wider(names_from = {{gender}}, id_cols = {{x}}, values_from = c(n, unwn, `%`, `%_low`, `%_upp`))
  
  t.age <- design %>%
    group_by({{age}}, {{x}}) %>%
    summarise(n = survey_total(),
              unwn = unweighted(n()),
              `%` = survey_mean(vartype = "ci", proportion = T)) %>%
    pivot_wider(names_from = {{age}}, id_cols = {{x}}, values_from = c(n, unwn, `%`, `%_low`, `%_upp`)) 
  
  t.dis <- design %>%
    group_by({{disability}}, {{x}}) %>%
    summarise(n = survey_total(),
              unwn = unweighted(n()),
              `%` = survey_mean(vartype = "ci", proportion = T)) %>%
    pivot_wider(names_from = {{disability}}, id_cols = {{x}}, values_from = c(n, unwn, `%`, `%_low`, `%_upp`))
  
  t.full <- tibble(
    "Indicator" = indicatorname,
    "Unit of analysis" = studyunit, 
    "Question" =  as.character(var_label(design$variables %>% select({{x}}))),
    t.total %>% select({{x}})
  ) %>%
    mutate(Question = replace(Question, c(2:dim(t.total)[1]), ""),
           Indicator = replace(Indicator, c(2:dim(t.total)[1]), ""),
           `Unit of analysis` = replace(`Unit of analysis`, c(2:dim(t.total)[1]), "")) %>%
    left_join(t.total %>% select({{x}}, n, `%`, `%_low`, `%_upp`), by = as_name(enquo(x))) %>%
    left_join(t.gen %>%  select({{x}}, `%_Female`, `%_low_Female`, `%_upp_Female`, `%_Male`, `%_low_Male`, `%_upp_Male`),  by =  as_name(enquo(x))) %>%
    left_join(t.age %>%  select({{x}}, 
                                any_of(c("%_0-4", "%_low_0-4", "%_upp_0-4",
                                         "%_5-11", "%_low_5-11", "%_upp_5-11",
                                         "%_12-17", "%_low_12-17", "%_upp_12-17",
                                         "%_18-24", "%_low_18-24", "%_upp_18-24", 
                                         "%_25-49", "%_low_25-49", "%_upp_25-49", 
                                         "%_50-59", "%_low_50-59", "%_upp_50-59", 
                                         "%_60+", "%_low_60+", "%_upp_60+"))),  
              by =  as_name(enquo(x))) %>%
    left_join(t.dis %>% select({{x}}, 
                                        `%_Without disability`, `%_low_Without disability`, `%_upp_Without disability`, 
                                        `%_With disability`, `%_low_With disability`, `%_upp_With disability`),  
              by =  as_name(enquo(x))) %>% 
    rename(Answers = as_name(enquo(x)),
           Total = `%`) %>% 
    rename_all(~str_replace_all(.,"%_",""))
  
  
  t.wbase <- t.unwbase <- as_tibble(data.frame(matrix(nrow=0, ncol=dim(t.full)[2])))
  colnames(t.wbase) <- colnames(t.unwbase) <- colnames(t.full)
  
  t.wbase <- tibble(
    "Indicator" = "",
    "Unit of analysis" = "",
    "Question" = "",
    "Answers" = "SAMPLE SIZE WEIGHTED TO POPULATION",
    n = sum(t.total$n, na.rm=T),
    `Total` = sum(t.total$n, na.rm=T),
    `Female` = sum(t.gen$n_Female, na.rm=T),
    `Male` =  sum(t.gen$n_Male, na.rm=T),
    `0-4` = sum(t.age$`n_0-4`, na.rm=T),
    `5-11` = sum(t.age$`n_5-11`, na.rm=T),
    `12-17` = sum(t.age$`n_12-17`, na.rm=T),
    `18-24` = sum(t.age$`n_18-24`, na.rm=T),
    `25-49` = sum(t.age$`n_25-49`, na.rm=T),
    `50-59` = sum(t.age$`n_50-59`, na.rm=T),
    `60+` = sum(t.age$`n_60+`, na.rm=T), 
    `Without disability` = sum(t.dis$`n_Without disability`, na.rm=T),
    `With disability` = sum(t.dis$`n_With disability`, na.rm=T)
  ) 
  
  
  t.unwbase <- tibble(
    "Indicator" = "",
    "Unit of analysis" = "",
    "Question" = "",
    "Answers" = "UNWEIGHTED SAMPLE SIZE",
    n = sum(t.total$unwn, na.rm=T),
    `Total` = sum(t.total$unwn, na.rm=T),
    `Female` = sum(t.gen$unwn_Female, na.rm=T),
    `Male` =  sum(t.gen$unwn_Male, na.rm=T),
    `0-4` = sum(t.age$`unwn_0-4`, na.rm=T),
    `5-11` = sum(t.age$`unwn_5-11`, na.rm=T),
    `12-17` = sum(t.age$`unwn_12-17`, na.rm=T),
    `18-24` = sum(t.age$`unwn_18-24`, na.rm=T),
    `25-49` = sum(t.age$`unwn_25-49`, na.rm=T),
    `50-59` = sum(t.age$`unwn_50-59`, na.rm=T),
    `60+` = sum(t.age$`unwn_60+`, na.rm=T), 
    `Without disability` = sum(t.dis$`unwn_Without disability`, na.rm=T),
    `With disability` = sum(t.dis$`unwn_With disability`, na.rm=T)
  ) 
  
  t.full <- t.full %>% 
    bind_rows(t.wbase %>% select(any_of(colnames(t.full)))) %>%
    bind_rows(t.unwbase %>% select(any_of(colnames(t.full))))
  
  return(t.full)
  
}
### end function rmstable