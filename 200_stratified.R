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

anova(cfa_asianwhite.configural.survey, cfa_asianwhite.metric.survey)

# No sig difference chisq (2) = .47 p = .78

equiv_chi(0.05, .47, 2, 2, 3612, .05)

# sig diff, go to scalar

# Sample size is 2854 + 577 

RDR(431, 423, 7, 4, 2, 3431)

# .03

## Scalar ##

cfa_asianwhite.scalar <-cfa(cfa_model, asianwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_asianwhite.scalar, fit.measures = T, standardized = T)

cfa_asianwhite.scalar.survey <- lavaan.survey(cfa_asianwhite.scalar, asianwhite_design)

summary(cfa_asianwhite.scalar.survey, fit.measures = T, standardized = TRUE)

anova(cfa_asianwhite.metric.survey, cfa_asianwhite.scalar.survey)

# no sig diff, chisq (2) = .52, p = .77

equiv_chi(0.05, .52, 2, 2, 3612, .05)

# sig diff, could proceed but will stop here

RDR(441, 431, 10, 3, 2, 3431)

# .02

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


anova(cfa_blackwhite.configural.survey, cfa_blackwhite.metric.survey)

# No sig difference chisq (2) = .65 p = .72

equiv_chi(0.05, .65, 2, 2, 4002, .05)

# sig diff, go to scalar

# Sample size is 2854 + 949

RDR(287, 283, 7, 4, 2, 3803)

#.01

## Scalar ##

cfa_blackwhite.scalar <-cfa(cfa_model, blackwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_blackwhite.scalar, fit.measures = T, standardized = T)

cfa_blackwhite.scalar.survey <- lavaan.survey(cfa_blackwhite.scalar, blackwhite_design)

summary(cfa_blackwhite.scalar.survey, fit.measures = T, standardized = TRUE)


anova(cfa_blackwhite.metric.survey, cfa_blackwhite.scalar.survey)

# sig diff, chisq (2) = 19.86, p = .77

equiv_chi(0.05, 19.86, 2, 2, 4002, .05)

# no sig diff, go to scalar

RDR(374, 287, 10, 7, 2, 3803)

#.12

## Partial Scalar ##

cfa_blackwhite.scalar.partial1 <-cfa(cfa_model, blackwhite_data, group = "race", group.equal = c("loadings", "intercepts"),
                                     group.partial = c("ADDOWN1 ~ 1"))


summary(cfa_blackwhite.scalar.partial1, fit.measures = T, standardized = T)

cfa_blackwhite.scalar.partial1.survey <- lavaan.survey(cfa_blackwhite.scalar.partial1, blackwhite_design)

compareFit(cfa_blackwhite.metric.survey, cfa_blackwhite.scalar.partial1.survey)

equiv_chi(0.05, 6.22, 2, 2, 4002, .05)

summary(cfa_blackwhite.scalar.partial1.survey, fit.measures = T, standardized = TRUE)

anova(cfa_blackwhite.metric.survey, cfa_blackwhite.scalar.partial1.survey)

RDR(358, 287, 9, 7, 2, 3803)

#.13

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

anova(cfa_hawaiiwhite.configural.survey, cfa_hawaiiwhite.metric.survey)

# No sig difference chisq (2) = .65 p = .72

equiv_chi(0.05, .51, 2, 2, 3564, .05)

# sig diff, go to scalar

# sample size is 2854 + 530

RDR(284, 264, 7, 4, 2, 3384)

#.06

## Scalar ##

cfa_hawaiiwhite.scalar <-cfa(cfa_model, hawaiiwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hawaiiwhite.scalar, fit.measures = T, standardized = T)

cfa_hawaiiwhite.scalar.survey <- lavaan.survey(cfa_hawaiiwhite.scalar, hawaiiwhite_design)

summary(cfa_hawaiiwhite.scalar.survey, fit.measures = T, standardized = T)

anova(cfa_hawaiiwhite.metric.survey, cfa_hawaiiwhite.scalar.survey)

# sig diff, chisq (2) = 14.72, p = <.001

equiv_chi(0.05, 14.72, 2, 2, 3564, .05)

# no sig diff, go to partial scalar

RDR(323, 284, 10, 7, 2, 3384)

#.08

## Partial Scalar ##

cfa_hawaiiwhite.scalar.partial1 <-cfa(cfa_model, hawaiiwhite_data, group = "race", group.equal = c("loadings", "intercepts"),
                                      group.partial = c("ADDOWN1 ~ 1"))

summary(cfa_hawaiiwhite.scalar.partial1, fit.measures = T, standardized = T)

cfa_hawaiiwhite.scalar.partial1.survey <- lavaan.survey(cfa_hawaiiwhite.scalar.partial1, hawaiiwhite_design)

summary(cfa_hawaiiwhite.scalar.partial1.survey, fit.measures = T, standardized = T)

anova(cfa_hawaiiwhite.metric.survey, cfa_hawaiiwhite.scalar.partial1.survey)

RDR(317, 284, 9, 7, 2, 3384)

#.10

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

anova(cfa_multiwhite.configural.survey, cfa_multiwhite.metric.survey)

# No sig difference chisq (2) = 2.29 p = .32

equiv_chi(0.05, 2.29, 3, 2, 3539, .05)

# sig diff, go to scalar

# Sample size is 2854 + 511

RDR(418, 412, 7, 4, 2, 3365)

#.02

## Scalar ##

cfa_multiwhite.scalar <-cfa(cfa_model, multiwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_multiwhite.scalar, fit.measures = T, standardized = T)

cfa_multiwhite.scalar.survey <- lavaan.survey(cfa_multiwhite.scalar, multiwhite_design)

summary(cfa_multiwhite.scalar.survey, fit.measures = T, standardized = TRUE)

anova(cfa_multiwhite.metric.survey, cfa_multiwhite.scalar.survey)

equiv_chi(0.05, 2.51, 2, 2, 3564, .05)

RDR(426, 418, 10, 7, 2, 3365)

#.03

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


anova(cfa_hispanicwhite.configural.survey, cfa_hispanicwhite.metric.survey)

# No sig difference chisq (2) = 5.48 p = .06

equiv_chi(0.05, 2.29, 2, 2, 3731, .05)

# sig diff, go to scalar

# Sample size is 2854 + 685

RDR(326, 300, 7, 4, 2, 3539)

#.07

## Scalar ##

cfa_hispanicwhite.scalar <-cfa(cfa_model, hispanicwhite_data, group = "race", group.equal = c("loadings", "intercepts"), ordered = c("ADPLEAS1", "ADDOWN1", "FEELNERV", "STOPWORY"))

summary(cfa_hispanicwhite.scalar, fit.measures = T, standardized = T)

cfa_hispanicwhite.scalar.survey <- lavaan.survey(cfa_hispanicwhite.scalar, hispanicwhite_design)

summary(cfa_hispanicwhite.scalar.survey, fit.measures = T, standardized = T)

anova(cfa_hispanicwhite.metric.survey, cfa_hispanicwhite.scalar.survey)

# sig diff, chisq (2) = 7.34, p = <.001

equiv_chi(0.05, 7.34, 2, 2, 3564, .05)

RDR(336, 326, 10, 7, 2, 3539)

#.04