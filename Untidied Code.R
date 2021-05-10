##########################
#### Loading Packages ####
##########################

library(haven)
library(tidyverse)
library(psych)
library(lavaan)
library(survey)
library(lavaan.survey)
library(semTools)
library(mirt)
library(MBESS)

equiv_chi=function(alpha=.05,chi,df,m,N_sample,popRMSEA=.08){
  Fml<-chi/(N_sample-m)
  popep<-(df*popRMSEA^2)/m
  popdelt<-(N_sample-m)*popep
  pval<-pchisq(chi, df, ncp=popdelt, lower.tail=TRUE)   
  res<-data.frame(chi,Fml,popep,popdelt,pval)
  res
}

options(survey.lonely.psu="adjust")


######################
#### Loading Data ####
######################

llcp2018 <- read_sas("C:/Users/zkunicki/Documents/Research/TWOCATEGORYRACEISWRONG/TwoCatRaceIsWrong/llcp2018.sas7bdat", NULL)
llcp2018v1 <- read_sas("C:/Users/zkunicki/Documents/Research/TWOCATEGORYRACEISWRONG/TwoCatRaceIsWrong/llcp18v1.sas7bdat", NULL)

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

###########################
#### DESCRIPTIVE TABLE ####
###########################

table1data <- BRFSS2018_Analysis %>% select(race, race_2cat, SEX1, ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY) 

table1 <- table1data %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                      all_categorical() ~ "{n} ({p}%)"))

options(gtsummary.print_engine = "kable")

print(table1)

describe(table1data)

#            vars     n mean   sd median trimmed mad min max range  skew kurtosis   se
# ADPLEAS1      4 17744 1.39 0.83      1    1.17   0   1   4     3  2.15     3.57 0.01
# ADDOWN1       5 17897 1.37 0.78      1    1.17   0   1   4     3  2.21     4.10 0.01
# FEELNERV      6 17853 1.49 0.85      1    1.28   0   1   4     3  1.80     2.30 0.01
# STOPWORY      7 17847 1.48 0.90      1    1.25   0   1   4     3  1.85     2.23 0.01

omegadata <- BRFSS2018_Analysis %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)

omega(omegadata) #0.87

ci.reliability(omegadata, B = 1000) #.84, 95% CI .84, .85

#####################
#### OMEGA BY RC ####
#####################

omegadata_white <- BRFSS2018_Analysis %>% filter(race=="White, Non-Hispanic") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_white, B = 1000) #.85, .84, .85

omegadata_black <- BRFSS2018_Analysis %>% filter(race=="Black, Non-Hispanic") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_black, B = 1000) #.82, .79, .85

omegadata_asian <- BRFSS2018_Analysis %>% filter(race=="Asian, Non-Hispanic") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_asian, B = 1000) #.80, .75, .85

omegadata_pacific <- BRFSS2018_Analysis %>% filter(race=="Native Hawaiian or other Pacific Islander") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_pacific, B = 1000) #.73, .67, .79

omegadata_multi <- BRFSS2018_Analysis %>% filter(race=="Multiracial, Non-Hispanic") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_multi, B = 1000) #.86, .83, .89

omegadata_hispanic <- BRFSS2018_Analysis %>% filter(race=="Hispanic") %>% select(ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY)
ci.reliability(omegadata_hispanic, B = 1000) #.86, .83, .89

################################
#### CREATING SURVEY DESIGN ####
################################

design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                    nest = TRUE, data = BRFSS2018_Analysis)
#############
#### CFA ####
#############

## Build Model

cfa_model <- 'f1 =~ ADPLEAS1 + ADDOWN1 + FEELNERV + STOPWORY'

## Full Sample

cfa_fullsample <- cfa(cfa_model, BRFSS2018_Analysis_1000, ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_fullsample, fit.measure = T, standardized = T)

cfa_fullsample_survey <- lavaan.survey(cfa_fullsample, design)

summary(cfa_fullsample_survey, fit.measure =T)

## by RC

cfa_asianonly <- cfa(cfa_model, omegadata_asian)
cfa_asianonly.survey <- lavaan.survey(cfa_asianonly, design)
summary(cfa_asianonly.survey, fit.measure = T, standardized = T)

cfa_blackonly <- cfa(cfa_model, omegadata_black)
cfa_blackonly.survey <- lavaan.survey(cfa_blackonly, design)
summary(cfa_blackonly.survey, fit.measure = T, standardized = T)

cfa_hispaniconly <- cfa(cfa_model, omegadata_hispanic)
cfa_hispaniconly.survey <- lavaan.survey(cfa_hispaniconly, design)
summary(cfa_hispaniconly.survey, fit.measure = T, standardized = T)

cfa_multionly <- cfa(cfa_model, omegadata_multi)
cfa_multionly.survey <- lavaan.survey(cfa_multionly, design)
summary(cfa_multionly.survey, fit.measure = T, standardized = T)

cfa_pacificonly <- cfa(cfa_model, omegadata_pacific)
cfa_pacificonly.survey <- lavaan.survey(cfa_pacificonly, design)
summary(cfa_pacificonly.survey, fit.measure = T, standardized = T)

cfa_whiteonly <- cfa(cfa_model, omegadata_white)
cfa_whiteonly.survey <- lavaan.survey(cfa_whiteonly, design)
summary(cfa_whiteonly.survey, fit.measure = T, standardized = T)

###########################################################
#### SECTION i. RACIAL/ETHNIC CATEGORIES ARE IMPRECISE ####
###########################################################

# In this section, we compare White and non-white participants.

## Overall model

cfa_race2cat <- cfa(cfa_model, BRFSS2018_Analysis_3000, ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat, fit.measures = T, standardized = T)

cfa_race2cat_survey <- lavaan.survey(cfa_race2cat, design)

summary(cfa_race2cat_survey, fit.measure =T)

## Configural Model

cfa_race2cat.configural <- cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race_2cat", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.configural, fit.measures = T, standardized = T)

cfa_race2cat.configural.survey <- lavaan.survey(cfa_race2cat.configural, design)

summary(cfa_race2cat.configural.survey, fit.measures = T, standardized = TRUE)

# Good fit, proceed to metric

cfa_race2cat.metric <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race_2cat", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.metric, fit.measures = T, standardized = T)

cfa_race2cat.metric.survey <- lavaan.survey(cfa_race2cat.metric, design)

summary(cfa_race2cat.metric.survey, fit.measures = T, standardized = T)

compareFit(cfa_race2cat.configural.survey, cfa_race2cat.metric.survey)

# No sig difference chisq (3) = .53, p = .91

equiv_chi(0.05, .53, 3, 2, 6448, .05)

#    chi          Fml   popep popdelt         pval
# 1 0.53 8.222153e-05 0.00375 24.1725 1.460203e-06

# sig difference, proceed to scalar

## Scalar ##

cfa_race2cat.scalar <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race_2cat", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.scalar, fit.measures = T)

cfa_race2cat.scalar.survey <- lavaan.survey(cfa_race2cat.scalar, design)

summary(cfa_race2cat.scalar.survey, fit.measures = T, standardized = T)

compareFit(cfa_race2cat.metric.survey, cfa_race2cat.scalar.survey)

# sig difference, chisq (3) = 20.48, p = .004

equiv_chi(0.05, 20.48, 3, 2, 6448, .05)

#     chi         Fml   popep popdelt      pval
# 1 20.48 0.003177164 0.00375 24.1725 0.2727035

# non sig, scalar not achieved. proceed to partial

## Partial Scalar ##

cfa_race2cat.scalar.partial1 <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race_2cat", group.equal = c("loadings", "intercepts"),
                                   group.partial = c("STOPWORY ~ 1"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))


summary(cfa_race2cat.scalar.partial1, fit.measures = T)

cfa_race2cat.scalar.partial1.survey <- lavaan.survey(cfa_race2cat.scalar.partial1, design)

summary(cfa_race2cat.scalar.partial1.survey, fit.measures = T)

compareFit(cfa_race2cat.metric.survey, cfa_race2cat.scalar.partial1.survey)

# When freeing STOPWORY, sig difference (p = .05)
equiv_chi(.05, 5.96, 2, 2, 6448, .05)

# When freeing STOPWORY, partial achieved

## Sensitivity Analysis ##

cfa_race2cat.configural.1000 <- cfa(cfa_model, BRFSS2018_Analysis_1000, group = "race_2cat", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.configural.1000, fit.measures = T, standardized = T)

cfa_race2cat.configural.1000.survey <- lavaan.survey(cfa_race2cat.configural.1000, design)

summary(cfa_race2cat.configural.1000.survey, fit.measures = T, standardized = T)

# Good fit, proceed to metric

cfa_race2cat.metric.1000 <-cfa(cfa_model, BRFSS2018_Analysis_1000, group = "race_2cat", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.metric.1000, fit.measures = T, standardized = T)

cfa_race2cat.metric.1000.survey <- lavaan.survey(cfa_race2cat.metric.1000, design)

compareFit(cfa_race2cat.configural.1000.survey, cfa_race2cat.metric.1000.survey)

# no sig difference, proceed to scalar

cfa_race2cat.scalar.1000 <-cfa(cfa_model, BRFSS2018_Analysis_1000, group = "race_2cat", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.scalar.1000, fit.measures = T, standardized = T)

cfa_race2cat.scalar.1000.survey <- lavaan.survey(cfa_race2cat.scalar.1000, design)

compareFit(cfa_race2cat.metric.1000.survey, cfa_race2cat.scalar.1000.survey)

# Sig difference

equiv_chi(0.05, 19.72, 3, 2, 4448, .05)

# Non sig, proceed to partial scalar

## Partial Scalar ##

cfa_race2cat.scalar1000.partial1 <-cfa(cfa_model, BRFSS2018_Analysis_1000, group = "race_2cat", group.equal = c("loadings", "intercepts"),
                                       group.partial = c("FEELNERV ~ 1"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_race2cat.scalar1000.partial1, fit.measures = T)

cfa_race2cat.scalar1000.partial1.survey <- lavaan.survey(cfa_race2cat.scalar1000.partial1, design)

compareFit(cfa_race2cat.metric.1000.survey, cfa_race2cat.scalar1000.partial1.survey)

# freeing FEELNERV p = .05

equiv_chi(0.05, 5.92, 3, 2, 4448, .05)

# p = .02
###########################################################################################################################################
## In this section, we do pairwise comparisons of White participants and Black participants, White and Asian, White and Multiracial, etc ##
###########################################################################################################################################

## Asian v White

## Create Survey Designs

asianwhite_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="White, Non-Hispanic" | race == "Asian, Non-Hispanic")

asianwhite_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = asianwhite_data)

## Configural

cfa_asianwhite.configural <- cfa(cfa_model, asianwhite_data, group = "race", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_asianwhite.configural, fit.measures = T, standardized = T) 

cfa_asianwhite.configural.survey <- lavaan.survey(cfa_asianwhite.configural, asianwhite_design)

summary(cfa_asianwhite.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_asianwhite.metric <-cfa(cfa_model, asianwhite_data, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_asianwhite.metric, fit.measures = T, standardized = T)

cfa_asianwhite.metric.survey <- lavaan.survey(cfa_asianwhite.metric, asianwhite_design)

summary(cfa_asianwhite.metric.survey, fit.measures = T, standardized = TRUE)

compareFit(cfa_asianwhite.configural.survey, cfa_asianwhite.metric.survey)

# No sig difference chisq (2) = .47 p = .78

equiv_chi(0.05, .47, 2, 2, 3612, .05)

# sig diff, go to scalar

## Scalar ##

cfa_asianwhite.scalar <-cfa(cfa_model, asianwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_asianwhite.scalar, fit.measures = T, standardized = T)

cfa_asianwhite.scalar.survey <- lavaan.survey(cfa_asianwhite.scalar, asianwhite_design)

summary(cfa_asianwhite.scalar.survey, fit.measures = T, standardized = TRUE)

compareFit(cfa_asianwhite.metric.survey, cfa_asianwhite.scalar.survey)

# no sig diff, chisq (2) = .52, p = .77

equiv_chi(0.05, .52, 2, 2, 3612, .05)

# sig diff, could proceed but will stop here

## Black vs White

## Create Survey Designs

blackwhite_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="White, Non-Hispanic" | race == "Black, Non-Hispanic")

blackwhite_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = blackwhite_data)

## Configural

cfa_blackwhite.configural <- cfa(cfa_model, blackwhite_data, group = "race", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_blackwhite.configural, fit.measures = T, standardized = T) 

cfa_blackwhite.configural.survey <- lavaan.survey(cfa_blackwhite.configural, blackwhite_design)

summary(cfa_blackwhite.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_blackwhite.metric <-cfa(cfa_model, blackwhite_data, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_blackwhite.metric, fit.measures = T, standardized = T)

cfa_blackwhite.metric.survey <- lavaan.survey(cfa_blackwhite.metric, blackwhite_design)

summary(cfa_blackwhite.metric.survey, fit.measures = T, standardized = TRUE)


compareFit(cfa_blackwhite.configural.survey, cfa_blackwhite.metric.survey)

# No sig difference chisq (2) = .65 p = .72

equiv_chi(0.05, .65, 2, 2, 4002, .05)

# sig diff, go to scalar

## Scalar ##

cfa_blackwhite.scalar <-cfa(cfa_model, blackwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_blackwhite.scalar, fit.measures = T, standardized = T)

cfa_blackwhite.scalar.survey <- lavaan.survey(cfa_blackwhite.scalar, blackwhite_design)

summary(cfa_blackwhite.scalar.survey, fit.measures = T, standardized = TRUE)


compareFit(cfa_blackwhite.metric.survey, cfa_blackwhite.scalar.survey)

# sig diff, chisq (2) = 19.86, p = .77

equiv_chi(0.05, 19.86, 2, 2, 4002, .05)

# no sig diff, go to scalar

## Partial Scalar ##

cfa_blackwhite.scalar.partial1 <-cfa(cfa_model, blackwhite_data, group = "race", group.equal = c("loadings", "intercepts"),
                                     group.partial = c("ADDOWN1 ~ 1"))


summary(cfa_blackwhite.scalar.partial1, fit.measures = T, standardized = T)

cfa_blackwhite.scalar.partial1.survey <- lavaan.survey(cfa_blackwhite.scalar.partial1, blackwhite_design)

compareFit(cfa_blackwhite.metric.survey, cfa_blackwhite.scalar.partial1.survey)

equiv_chi(0.05, 6.22, 2, 2, 4002, .05)

summary(cfa_blackwhite.scalar.partial1.survey, fit.measures = T, standardized = TRUE)


## Native Hawaiian or other Pacific Islander vs White ##

## Create Survey Designs

hawaiiwhite_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="White, Non-Hispanic" | race == "Native Hawaiian or other Pacific Islander")

hawaiiwhite_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                nest = TRUE, data = hawaiiwhite_data)

## Configural

cfa_hawaiiwhite.configural <- cfa(cfa_model, hawaiiwhite_data, group = "race", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hawaiiwhite.configural, fit.measures = T, standardized = T) 

cfa_hawaiiwhite.configural.survey <- lavaan.survey(cfa_hawaiiwhite.configural, hawaiiwhite_design)

summary(cfa_hawaiiwhite.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hawaiiwhite.metric <-cfa(cfa_model, hawaiiwhite_data, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hawaiiwhite.metric, fit.measures = T, standardized = T)

cfa_hawaiiwhite.metric.survey <- lavaan.survey(cfa_hawaiiwhite.metric, hawaiiwhite_design)

summary(cfa_hawaiiwhite.metric.survey, fit.measures = T, standardized = TRUE)

compareFit(cfa_hawaiiwhite.configural.survey, cfa_hawaiiwhite.metric.survey)

# No sig difference chisq (2) = .65 p = .72

equiv_chi(0.05, .51, 2, 2, 3564, .05)

# sig diff, go to scalar

## Scalar ##

cfa_hawaiiwhite.scalar <-cfa(cfa_model, hawaiiwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hawaiiwhite.scalar, fit.measures = T, standardized = T)

cfa_hawaiiwhite.scalar.survey <- lavaan.survey(cfa_hawaiiwhite.scalar, hawaiiwhite_design)

summary(cfa_hawaiiwhite.scalar.survey, fit.measures = T, standardized = T)

compareFit(cfa_hawaiiwhite.metric.survey, cfa_hawaiiwhite.scalar.survey)

# sig diff, chisq (2) = 14.72, p = <.001

equiv_chi(0.05, 14.72, 2, 2, 3564, .05)

# no sig diff, go to partial scalar

## Partial Scalar ##

cfa_hawaiiwhite.scalar.partial1 <-cfa(cfa_model, hawaiiwhite_data, group = "race", group.equal = c("loadings", "intercepts"),
                                      group.partial = c("ADDOWN1 ~ 1"))

summary(cfa_hawaiiwhite.scalar.partial1, fit.measures = T, standardized = T)

cfa_hawaiiwhite.scalar.partial1.survey <- lavaan.survey(cfa_hawaiiwhite.scalar.partial1, hawaiiwhite_design)

summary(cfa_hawaiiwhite.scalar.partial1.survey, fit.measures = T, standardized = T)

compareFit(cfa_hawaiiwhite.metric.survey, cfa_hawaiiwhite.scalar.partial1.survey)

## Multiracial vs White ##

## Create Survey Designs

multiwhite_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="White, Non-Hispanic" | race == "Multiracial, Non-Hispanic")

multiwhite_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = multiwhite_data)

## Configural

cfa_multiwhite.configural <- cfa(cfa_model, multiwhite_data, group = "race", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_multiwhite.configural, fit.measures = T, standardized = T) 

cfa_multiwhite.configural.survey <- lavaan.survey(cfa_multiwhite.configural, multiwhite_design)

summary(cfa_multiwhite.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_multiwhite.metric <-cfa(cfa_model, multiwhite_data, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_multiwhite.metric, fit.measures = T, standardized = T)

cfa_multiwhite.metric.survey <- lavaan.survey(cfa_multiwhite.metric, multiwhite_design)

summary(cfa_multiwhite.metric.survey, fit.measures = T, standardized = TRUE)

compareFit(cfa_multiwhite.configural.survey, cfa_multiwhite.metric.survey)

# No sig difference chisq (2) = 2.29 p = .32

equiv_chi(0.05, 2.29, 3, 2, 3539, .05)

# sig diff, go to scalar

## Scalar ##

cfa_multiwhite.scalar <-cfa(cfa_model, multiwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_multiwhite.scalar, fit.measures = T, standardized = T)

cfa_multiwhite.scalar.survey <- lavaan.survey(cfa_multiwhite.scalar, multiwhite_design)

summary(cfa_multiwhite.scalar.survey, fit.measures = T, standardized = TRUE)

compareFit(cfa_multiwhite.metric.survey, cfa_multiwhite.scalar.survey)

equiv_chi(0.05, 2.51, 2, 2, 3564, .05)

## Scalar met

## Hispanic vs White ##

## Create Survey Designs

hispanicwhite_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="White, Non-Hispanic" | race == "Hispanic")

hispanicwhite_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                  nest = TRUE, data = hispanicwhite_data)

## Configural

cfa_hispanicwhite.configural <- cfa(cfa_model, hispanicwhite_data, group = "race", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hispanicwhite.configural, fit.measures = T, standardized = T) 

cfa_hispanicwhite.configural.survey <- lavaan.survey(cfa_hispanicwhite.configural, hispanicwhite_design)

summary(cfa_hispanicwhite.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hispanicwhite.metric <-cfa(cfa_model, hispanicwhite_data, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hispanicwhite.metric, fit.measures = T, standardized = T)

cfa_hispanicwhite.metric.survey <- lavaan.survey(cfa_hispanicwhite.metric, hispanicwhite_design)

summary(cfa_hispanicwhite.metric.survey, fit.measures = T, standardized = TRUE)


compareFit(cfa_hispanicwhite.configural.survey, cfa_hispanicwhite.metric.survey)

# No sig difference chisq (2) = 5.48 p = .06

equiv_chi(0.05, 2.29, 2, 2, 3731, .05)

# sig diff, go to scalar

## Scalar ##

cfa_hispanicwhite.scalar <-cfa(cfa_model, hispanicwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hispanicwhite.scalar, fit.measures = T, standardized = T)

cfa_hispanicwhite.scalar.survey <- lavaan.survey(cfa_hispanicwhite.scalar, hispanicwhite_design)

summary(cfa_hispanicwhite.scalar.survey, fit.measures = T, standardized = T)

compareFit(cfa_hispanicwhite.metric.survey, cfa_hispanicwhite.scalar.survey)

# sig diff, chisq (2) = 7.34, p = <.001

equiv_chi(0.05, 7.34, 2, 2, 3564, .05)


###########################################################
#### SECTION ii. WHITE AS THE DEFAULT COMPARISON GROUP ####
###########################################################

## Omnibus Test ##

cfa_allrace.omnibus <- cfa(cfa_model, BRFSS2018_Analysis_3000)

cfa_allrace.omnibus.survey <- lavaan.survey(cfa_allrace.omnibus, design)

summary(cfa_allrace.omnibus.survey, fit.measures = T)

## Configural 

cfa_allrace.configural <- cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race")

summary(cfa_allrace.configural, fit.measures = T, standardized = T)

# Add survey design

cfa_allrace.configural.survey <- lavaan.survey(cfa_allrace.configural, design)

summary(cfa_allrace.configural.survey, fit.measures = T, standardized = T)

## Metric -- restrict loadings, All Race/Ethnicity

cfa_allrace.metric <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race", group.equal = "loadings", ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_allrace.metric, fit.measures = T, standardized = T)

# Add survey design

cfa_allrace.metric.survey <- lavaan.survey(cfa_allrace.metric, design)

summary(cfa_allrace.metric.survey, fit.measures = T)

compareFit(cfa_allrace.configural.survey, cfa_allrace.metric.survey)

equiv_chi(0.05, 9.7, 15, 6, 4448, .05)

# Metric achieved, proceed to scalar

cfa_allrace.scalar <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_allrace.scalar, fit.measures = T, standardized = T)

# Add survey design

cfa_allrace.scalar.survey <- lavaan.survey(cfa_allrace.scalar, design)

summary(cfa_allrace.scalar.survey, fit.measures = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.scalar.survey)

equiv_chi(0.05, 32.48, 15, 6, 4448, .05)

# Scalar not acheived, proceed to partial scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, BRFSS2018_Analysis_3000, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "FEELNERV~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, design)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)

equiv_chi(0.05, 15.03, 10, 5, 4448, .05)

# Achieved

#######################################################################################################################
## In this section, we do pairwise comparisons of Black participants and Asian participants, Black and Hispanic, etc ##
#######################################################################################################################

## Black v Asian

## Create Survey Designs

asianblack_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Black, Non-Hispanic" | race == "Asian, Non-Hispanic")

asianblack_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = asianblack_data)

## Configural

cfa_asianblack.configural <- cfa(cfa_model, asianblack_data, group = "race")

summary(cfa_asianblack.configural, fit.measures = T, standardized = T) 

cfa_asianblack.configural.survey <- lavaan.survey(cfa_asianblack.configural, asianblack_design)

summary(cfa_asianblack.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_asianblack.metric <-cfa(cfa_model, asianblack_data, group = "race", group.equal = "loadings")

summary(cfa_asianblack.metric, fit.measures = T, standardized = T)

cfa_asianblack.metric.survey <- lavaan.survey(cfa_asianblack.metric, asianblack_design)

compareFit(cfa_asianblack.configural.survey, cfa_asianblack.metric.survey)

## Scalar

cfa_asianblack.scalar <-cfa(cfa_model, asianblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_asianblack.scalar, fit.measures = T, standardized = T)

cfa_asianblack.scalar.survey <- lavaan.survey(cfa_asianblack.scalar, asianblack_design)

compareFit(cfa_asianblack.metric.survey, cfa_asianblack.scalar.survey)

equiv_chi(0.05, 7.89, 3, 2, 1614, .05)

## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, asianblack_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "STOPWORY~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, asianblack_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)


## Black v Native Hawaiian

## Create Survey Designs

hawaiiblack_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Black, Non-Hispanic" | race == "Native Hawaiian or other Pacific Islander")

hawaiiblack_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                nest = TRUE, data = hawaiiblack_data)

## Configural

cfa_hawaiiblack.configural <- cfa(cfa_model, hawaiiblack_data, group = "race")

summary(cfa_hawaiiblack.configural, fit.measures = T, standardized = T) 

cfa_hawaiiblack.configural.survey <- lavaan.survey(cfa_hawaiiblack.configural, hawaiiblack_design)

summary(cfa_hawaiiblack.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hawaiiblack.metric <-cfa(cfa_model, hawaiiblack_data, group = "race", group.equal = "loadings")

summary(cfa_hawaiiblack.metric, fit.measures = T, standardized = T)

cfa_hawaiiblack.metric.survey <- lavaan.survey(cfa_hawaiiblack.metric, hawaiiblack_design)

compareFit(cfa_hawaiiblack.configural.survey, cfa_hawaiiblack.metric.survey)

## Scalar

cfa_hawaiiblack.scalar <-cfa(cfa_model, hawaiiblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hawaiiblack.scalar, fit.measures = T, standardized = T)

cfa_hawaiiblack.scalar.survey <- lavaan.survey(cfa_hawaiiblack.scalar, hawaiiblack_design)

compareFit(cfa_hawaiiblack.metric.survey, cfa_hawaiiblack.scalar.survey)

equiv_chi(0.05, 12.62, 3, 2, 1566, .05)

## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, hawaiiblack_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "ADDPLEAS1~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hawaiiblack_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)


## Black v Multiracial

## Create Survey Designs

multiblack_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Black, Non-Hispanic" | race == "Multiracial, Non-Hispanic")

multiblack_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = multiblack_data)

## Configural

cfa_multiblack.configural <- cfa(cfa_model, multiblack_data, group = "race")

summary(cfa_multiblack.configural, fit.measures = T, standardized = T) 

cfa_multiblack.configural.survey <- lavaan.survey(cfa_multiblack.configural, multiblack_design)

summary(cfa_multiblack.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_multiblack.metric <-cfa(cfa_model, multiblack_data, group = "race", group.equal = "loadings")

summary(cfa_multiblack.metric, fit.measures = T, standardized = T)

cfa_multiblack.metric.survey <- lavaan.survey(cfa_multiblack.metric, multiblack_design)

compareFit(cfa_multiblack.configural.survey, cfa_multiblack.metric.survey)

## Scalar

cfa_multiblack.scalar <-cfa(cfa_model, multiblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multiblack.scalar, fit.measures = T, standardized = T)

cfa_multiblack.scalar.survey <- lavaan.survey(cfa_multiblack.scalar, multiblack_design)

compareFit(cfa_multiblack.metric.survey, cfa_multiblack.scalar.survey)

equiv_chi(0.05, 8.46, 3, 2, 1541, .05)

## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, multiblack_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "STOPWORY~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, multiblack_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)

## Black v Hispanic

## Create Survey Designs

hispanicblack_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Black, Non-Hispanic" | race == "Hispanic")

hispanicblack_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                  nest = TRUE, data = hispanicblack_data)

## Configural

cfa_hispanicblack.configural <- cfa(cfa_model, hispanicblack_data, group = "race")

summary(cfa_hispanicblack.configural, fit.measures = T, standardized = T) 

cfa_hispanicblack.configural.survey <- lavaan.survey(cfa_hispanicblack.configural, hispanicblack_design)

summary(cfa_hispanicblack.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hispanicblack.metric <-cfa(cfa_model, hispanicblack_data, group = "race", group.equal = "loadings")

summary(cfa_hispanicblack.metric, fit.measures = T, standardized = T)

cfa_hispanicblack.metric.survey <- lavaan.survey(cfa_hispanicblack.metric, hispanicblack_design)

compareFit(cfa_hispanicblack.configural.survey, cfa_hispanicblack.metric.survey)

## Scalar

cfa_hispanicblack.scalar <-cfa(cfa_model, hispanicblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicblack.scalar, fit.measures = T, standardized = T)

cfa_hispanicblack.scalar.survey <- lavaan.survey(cfa_hispanicblack.scalar, hispanicblack_design)

compareFit(cfa_hispanicblack.metric.survey, cfa_hispanicblack.scalar.survey)

equiv_chi(0.05, 14.09, 3, 2, 1733, .05)

## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, hispanicblack_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "ADPLEAS1~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hispanicblack_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)


## Hispanic vs Multiracial

hispanicmulti_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Multiracial, Non-Hispanic" | race == "Hispanic")

hispanicmulti_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                  nest = TRUE, data = hispanicmulti_data)

## Configural

cfa_hispanicmulti.configural <- cfa(cfa_model, hispanicmulti_data, group = "race")

summary(cfa_hispanicmulti.configural, fit.measures = T, standardized = T) 

cfa_hispanicmulti.configural.survey <- lavaan.survey(cfa_hispanicmulti.configural, hispanicmulti_design)

summary(cfa_hispanicmulti.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hispanicmulti.metric <-cfa(cfa_model, hispanicmulti_data, group = "race", group.equal = "loadings")

summary(cfa_hispanicmulti.metric, fit.measures = T, standardized = T)

cfa_hispanicmulti.metric.survey <- lavaan.survey(cfa_hispanicmulti.metric, hispanicmulti_design)

compareFit(cfa_hispanicmulti.configural.survey, cfa_hispanicmulti.metric.survey)

## Scalar

cfa_hispanicmulti.scalar <-cfa(cfa_model, hispanicmulti_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicmulti.scalar, fit.measures = T, standardized = T)

cfa_hispanicmulti.scalar.survey <- lavaan.survey(cfa_hispanicmulti.scalar, hispanicmulti_design)

compareFit(cfa_hispanicmulti.metric.survey, cfa_hispanicmulti.scalar.survey)

equiv_chi(0.05, 2.18, 3, 2, 1270, .05)

## Hispanic vs Native Hawaiian

hispanichawaii_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Native Hawaiian or other Pacific Islander" | race == "Hispanic")

hispanichawaii_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                   nest = TRUE, data = hispanichawaii_data)

## Configural

cfa_hispanichawaii.configural <- cfa(cfa_model, hispanichawaii_data, group = "race")

summary(cfa_hispanichawaii.configural, fit.measures = T, standardized = T) 

cfa_hispanichawaii.configural.survey <- lavaan.survey(cfa_hispanichawaii.configural, hispanichawaii_design)

summary(cfa_hispanichawaii.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hispanichawaii.metric <-cfa(cfa_model, hispanichawaii_data, group = "race", group.equal = "loadings")

summary(cfa_hispanichawaii.metric, fit.measures = T, standardized = T)

cfa_hispanichawaii.metric.survey <- lavaan.survey(cfa_hispanichawaii.metric, hispanichawaii_design)

compareFit(cfa_hispanichawaii.configural.survey, cfa_hispanichawaii.metric.survey)

## Scalar

cfa_hispanichawaii.scalar <-cfa(cfa_model, hispanichawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanichawaii.scalar, fit.measures = T, standardized = T)

cfa_hispanichawaii.scalar.survey <- lavaan.survey(cfa_hispanichawaii.scalar, hispanichawaii_design)

compareFit(cfa_hispanichawaii.metric.survey, cfa_hispanichawaii.scalar.survey)

equiv_chi(0.05, 7.93, 3, 2, 1295, .05)



## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, hispanichawaii_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "ADPLEAS1~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hispanichawaii_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)

## Hispanic vs Asian

hispanicasian_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Asian, Non-Hispanic" | race == "Hispanic")

hispanicasian_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                  nest = TRUE, data = hispanicasian_data)

## Configural

cfa_hispanicasian.configural <- cfa(cfa_model, hispanicasian_data, group = "race")

summary(cfa_hispanicasian.configural, fit.measures = T, standardized = T) 

cfa_hispanicasian.configural.survey <- lavaan.survey(cfa_hispanicasian.configural, hispanicasian_design)

summary(cfa_hispanicasian.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_hispanicasian.metric <-cfa(cfa_model, hispanicasian_data, group = "race", group.equal = "loadings")

summary(cfa_hispanicasian.metric, fit.measures = T, standardized = T)

cfa_hispanicasian.metric.survey <- lavaan.survey(cfa_hispanicasian.metric, hispanicasian_design)

compareFit(cfa_hispanicasian.configural.survey, cfa_hispanicasian.metric.survey)

## Scalar

cfa_hispanicasian.scalar <-cfa(cfa_model, hispanicasian_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicasian.scalar, fit.measures = T, standardized = T)

cfa_hispanicasian.scalar.survey <- lavaan.survey(cfa_hispanicasian.scalar, hispanicasian_design)

compareFit(cfa_hispanicasian.metric.survey, cfa_hispanicasian.scalar.survey)

equiv_chi(0.05, .51, 3, 2, 1343, .05)

## Multiracial vs Asian

multiasian_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Asian, Non-Hispanic" | race == "Multiracial, Non-Hispanic")

multiasian_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                               nest = TRUE, data = multiasian_data)

## Configural

cfa_multiasian.configural <- cfa(cfa_model, multiasian_data, group = "race")

summary(cfa_multiasian.configural, fit.measures = T, standardized = T) 

cfa_multiasian.configural.survey <- lavaan.survey(cfa_multiasian.configural, multiasian_design)

summary(cfa_multiasian.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_multiasian.metric <-cfa(cfa_model, multiasian_data, group = "race", group.equal = "loadings")

summary(cfa_multiasian.metric, fit.measures = T, standardized = T)

cfa_multiasian.metric.survey <- lavaan.survey(cfa_multiasian.metric, multiasian_design)

compareFit(cfa_multiasian.configural.survey, cfa_multiasian.metric.survey)

## Scalar

cfa_multiasian.scalar <-cfa(cfa_model, multiasian_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multiasian.scalar, fit.measures = T, standardized = T)

cfa_multiasian.scalar.survey <- lavaan.survey(cfa_multiasian.scalar, multiasian_design)

compareFit(cfa_multiasian.metric.survey, cfa_multiasian.scalar.survey)

equiv_chi(0.05, 1.90, 3, 2, 1151, .05)


## Multiracial vs Native Hawaiian

multihawaii_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Native Hawaiian or other Pacific Islander" | race == "Multiracial, Non-Hispanic")

multihawaii_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                nest = TRUE, data = multihawaii_data)

## Configural

cfa_multihawaii.configural <- cfa(cfa_model, multihawaii_data, group = "race")

summary(cfa_multihawaii.configural, fit.measures = T, standardized = T) 

cfa_multihawaii.configural.survey <- lavaan.survey(cfa_multihawaii.configural, multihawaii_design)

summary(cfa_multihawaii.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_multihawaii.metric <-cfa(cfa_model, multihawaii_data, group = "race", group.equal = "loadings")

summary(cfa_multihawaii.metric, fit.measures = T, standardized = T)

cfa_multihawaii.metric.survey <- lavaan.survey(cfa_multihawaii.metric, multihawaii_design)

compareFit(cfa_multihawaii.configural.survey, cfa_multihawaii.metric.survey)

## Scalar

cfa_multihawaii.scalar <-cfa(cfa_model, multihawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multihawaii.scalar, fit.measures = T, standardized = T)

cfa_multihawaii.scalar.survey <- lavaan.survey(cfa_multihawaii.scalar, multihawaii_design)

compareFit(cfa_multihawaii.metric.survey, cfa_multihawaii.scalar.survey)

equiv_chi(0.05, 8.56, 3, 2, 1103, .05)

## Partial Scalar

cfa_allrace.partialscalar1 <-cfa(cfa_model, multihawaii_data, group = "race", group.equal = c("loadings", "intercepts"),
                                 group.partial = "STOPWORY~1")

summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)

cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, multihawaii_design)

summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)

compareFit(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)


## Asian vs Native Hawaiian

asianhawaii_data <- BRFSS2018_Analysis_3000 %>% 
  filter(race =="Native Hawaiian or other Pacific Islander" | race == "Asian, Non-Hispanic")

asianhawaii_design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                                nest = TRUE, data = asianhawaii_data)

## Configural

cfa_asianhawaii.configural <- cfa(cfa_model, asianhawaii_data, group = "race")

summary(cfa_asianhawaii.configural, fit.measures = T, standardized = T) 

cfa_asianhawaii.configural.survey <- lavaan.survey(cfa_asianhawaii.configural, asianhawaii_design)

summary(cfa_asianhawaii.configural.survey, fit.measures = T, standardized = TRUE)

## Metric

cfa_asianhawaii.metric <-cfa(cfa_model, asianhawaii_data, group = "race", group.equal = "loadings")

summary(cfa_asianhawaii.metric, fit.measures = T, standardized = T)

cfa_asianhawaii.metric.survey <- lavaan.survey(cfa_asianhawaii.metric, asianhawaii_design)

compareFit(cfa_asianhawaii.configural.survey, cfa_asianhawaii.metric.survey)

## Scalar

cfa_asianhawaii.scalar <-cfa(cfa_model, asianhawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_asianhawaii.scalar, fit.measures = T, standardized = T)

cfa_asianhawaii.scalar.survey <- lavaan.survey(cfa_asianhawaii.scalar, asianhawaii_design)

compareFit(cfa_asianhawaii.metric.survey, cfa_asianhawaii.scalar.survey)

equiv_chi(0.05, 5.08, 3, 2, 1176, .05)