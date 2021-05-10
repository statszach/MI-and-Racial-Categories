###################
#### Tidy Data ####
###################

## Grab correct survey weights from each dataset.

#1. Filter for state or territory.
#Core: Guam = 66, Tennessee = 47, Oregon = 41
#V1: Ohio = 39


llcp2018_tidy2 <- llcp2018_tidy2 %>% filter(`_STATE` == 66 | `_STATE` == 47 | `_STATE` ==  39 | `_STATE` == 41)


llcp2018core <- llcp2018 %>% filter(`_STATE` == 66 | `_STATE` == 47 | `_STATE` == 41) 
llcp2018v1 <- llcp2018v1 %>% filter(`_STATE` == 39)

#2. Select for variables of interest and weight
# Need Race, Sex, 4 Anx/Dep Var, Survey data
# Also rename weight variable to be consistent

brfss2018core_formerge <- llcp2018core %>% rename(., finalweight = `_LLCPWT`) %>%
  select(`_STATE`,
         SEX1,
         `_RACE`,
         ADPLEAS1,
         ADDOWN1,
         FEELNERV,
         STOPWORY,
         finalweight,
         `_PSU`,
         `_STSTR`)

brfss2018v1_formerge <- llcp2018v1 %>% rename(., finalweight = `_LCPWTV1`) %>%
  select(`_STATE`,
         SEX1,
         `_RACE`,
         ADPLEAS1,
         ADDOWN1,
         FEELNERV,
         STOPWORY,
         finalweight,
         `_PSU`,
         `_STSTR`)

BRFSS2018_FinalWeight <- rbind(brfss2018core_formerge, brfss2018v1_formerge)


## Filtering Out Unusable Data

BRFSS2018_FilterMissing <- BRFSS2018_FinalWeight %>% mutate(ADPLEAS1 = na_if(ADPLEAS1, "7"),
                                                            ADPLEAS1 = na_if(ADPLEAS1, "9"),
                                                            ADDOWN1 = na_if(ADDOWN1, "7"),
                                                            ADDOWN1 = na_if(ADDOWN1, "9"),
                                                            FEELNERV = na_if(FEELNERV, "7"),
                                                            FEELNERV = na_if(FEELNERV, "9"),
                                                            STOPWORY = na_if(STOPWORY, "7"),
                                                            STOPWORY = na_if(STOPWORY, "9"),
                                                            SEX1 = na_if(SEX1, "7"),
                                                            SEX1 = na_if(SEX1, "9"),
                                                            `_RACE` = na_if(`_RACE`, "9"))


## Also want to filter out any cases where they answered none of the anx/dep questions
## 1. Make new variable checking number of missing responses

table(BRFSS2018_FilterMissing$ADPLEAS1, useNA = "always")

#    1     2     3     4  <NA> 
# 7960  1341   506   631 15075 

BRFSS2018_FilterMissing <- BRFSS2018_FilterMissing %>% mutate(
  ADPLEAS1_Miss = dplyr::if_else(is.na(ADPLEAS1), 1, 0),
  ADDOWN1_Miss = dplyr::if_else(is.na(ADDOWN1), 1, 0),
  FEELNERV_Miss = dplyr::if_else(is.na(FEELNERV), 1, 0),
  STOPWORY_Miss = dplyr::if_else(is.na(STOPWORY), 1, 0),
  RACE_Miss = dplyr::if_else(is.na(`_RACE`), 1, 0))

table(BRFSS2018_FilterMissing$ADPLEAS1_Miss, useNA = "always")

# 0     1  <NA> 
# 10438 15075     0 

## 2. Sum up missing variables, if = 4, filter out

BRFSS2018_FilterMissing <- BRFSS2018_FilterMissing %>% mutate(
  Measure_Sum_Miss = ADPLEAS1_Miss + ADDOWN1_Miss + FEELNERV_Miss + STOPWORY_Miss
)

table(BRFSS2018_FilterMissing$Measure_Sum_Miss, useNA = "always")

#    0     1     2     3     4  <NA> 
#10107   422    99    48 14837     0 

BRFSS2018_FilterMissing2 <- BRFSS2018_FilterMissing %>% filter(Measure_Sum_Miss < 4) 

table(BRFSS2018_FilterMissing2$Measure_Sum_Miss, useNA = "always")

## 3. Also filter out if race is NA

table(BRFSS2018_FilterMissing2$RACE_Miss, useNA = "always")

#     0     1  <NA> 
# 10458   218     0 

BRFSS2018_FilterMissing2 <- BRFSS2018_FilterMissing2 %>% filter(RACE_Miss < 1) 

table(BRFSS2018_FilterMissing2$RACE_Miss, useNA = "always")

#     0  <NA> 
# 10458     0

## Checking Missing

table(is.na(BRFSS2018_FilterMissing2))

# FALSE   TRUE 
# 166571    757 
# (757 / (166571 + 757))*100
# 0.45%


## Need to filter out American Indian/Alaska Native and Other, Non-Hispanic due to model convergence issues

table(BRFSS2018_FilterMissing2$`_RACE`)

#         2    3    4    5    6    7    8 
# 7610  553  105  565  560   64  398  603 

BRFSS2018_FilterMissing2 <- BRFSS2018_FilterMissing2 %>% filter(`_RACE` == 1 |
                                                                  `_RACE` == 2 |
                                                                  `_RACE` == 4 |
                                                                  `_RACE` == 5 |
                                                                  `_RACE` == 7 |
                                                                  `_RACE` == 8 )

# 1    2    4    5    7    8 
# 7610  553  565  560  398  603 