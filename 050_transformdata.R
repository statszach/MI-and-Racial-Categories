########################
#### TRANSFORM DATA ####
########################

## Create Race Variables

BRFSS2018_Transform <- BRFSS2018_FilterMissing2 %>% mutate(race_2cat = dplyr::if_else(`_RACE` == 1, 1, 2))

table(BRFSS2018_Transform$race_2cat)

#    1    2 
# 7610 2679

BRFSS2018_Transform$race_2cat <- factor(BRFSS2018_Transform$race_2cat,
                                        levels = c(1,2),
                                        labels = c("White",
                                                   "Non-White"))

table(BRFSS2018_Transform$race_2cat)

# White Non-White 
# 7610      2679  


BRFSS2018_Transform$race <- factor(BRFSS2018_Transform$`_RACE`,
                                   levels = c(1, 2, 4, 5, 7, 8),
                                   labels = c("White, Non-Hispanic",
                                              "Black, Non-Hispanic",
                                              "Asian, Non-Hispanic",
                                              "Native Hawaiian or other Pacific Islander",
                                              "Multiracial, Non-Hispanic",
                                              "Hispanic"))

table(BRFSS2018_Transform$race)

# White, Non-Hispanic                       Black, Non-Hispanic                       Asian, Non-Hispanic 
#               14690                                      1002                                       612 
# Native Hawaiian or other Pacific Islander                 Multiracial, Non-Hispanic                                  Hispanic 
#                                       564                                       539                                       731 

## Highly skewed -- taking random subset of 3000 and 1000 of White, Non-Hispanic 

set.seed(10003000)

BRFSS2018_WhiteOnlyRand1000 <- BRFSS2018_Transform %>% filter(race == "White, Non-Hispanic") %>% sample_n(size = 1000, replace = TRUE)
BRFSS2018_WhiteOnlyRand3000 <- BRFSS2018_Transform %>% filter(race == "White, Non-Hispanic") %>% sample_n(size = 3000, replace = TRUE)

BRFSS2018_NonWhiteOnly <- BRFSS2018_Transform %>% filter(race != "White, Non-Hispanic")

## Subset for analysis variables

BRFSS2018_Analysis_1000 <- rbind(BRFSS2018_WhiteOnlyRand1000, BRFSS2018_NonWhiteOnly) %>% 
  select(race, race_2cat, ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY, SEX1, finalweight, `_PSU`, `_STSTR`)

table(BRFSS2018_Analysis_1000$race)

# White, Non-Hispanic                       Black, Non-Hispanic                       Asian, Non-Hispanic 
# 1000                                      1002                                       612 
# Native Hawaiian or other Pacific Islander                 Multiracial, Non-Hispanic                                  Hispanic 
# 564                                       539                                       731 

BRFSS2018_Analysis_3000 <- rbind(BRFSS2018_WhiteOnlyRand3000, BRFSS2018_NonWhiteOnly) %>% 
  select(race, race_2cat, ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY, SEX1, finalweight, `_PSU`, `_STSTR`)


table(BRFSS2018_Analysis_3000$race_2cat)

# White Non-White 
# 3000      3448 