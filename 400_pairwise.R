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

anova(cfa_asianblack.configural.survey, cfa_asianblack.metric.survey)

# Sample size is 949 + 577

RDR(197, 195, 7, 4, 2, 1526) # Set to 0 because value is negative (see preprint and Dudgeon, 2004)

## Scalar

cfa_asianblack.scalar <-cfa(cfa_model, asianblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_asianblack.scalar, fit.measures = T, standardized = T)

cfa_asianblack.scalar.survey <- lavaan.survey(cfa_asianblack.scalar, asianblack_design)

anova(cfa_asianblack.metric.survey, cfa_asianblack.scalar.survey)

equiv_chi(0.05, 7.89, 3, 2, 1614, .05)

RDR(229, 197, 10, 7, 2, 1526)

# 0.11

## Partial Scalar

# cfa_asianblack.partialscalar1 <-cfa(cfa_model, asianblack_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "STOPWORY~1")
# 
# summary(cfa_asianblack.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_asianblack.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, asianblack_design)
# 
# summary(cfa_asianblack.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_asianblack.metric.survey, cfa_asianblack.partialscalar1.survey)
# 
# RDR(229, 197, 10, 9, 2, 1526)

#.20

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

anova(cfa_hawaiiblack.configural.survey, cfa_hawaiiblack.metric.survey)

# sample size is 949 + 530

RDR(58, 36, 7, 4, 2, 1479)

#.09

## Scalar

cfa_hawaiiblack.scalar <-cfa(cfa_model, hawaiiblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hawaiiblack.scalar, fit.measures = T, standardized = T)

cfa_hawaiiblack.scalar.survey <- lavaan.survey(cfa_hawaiiblack.scalar, hawaiiblack_design)

anova(cfa_hawaiiblack.metric.survey, cfa_hawaiiblack.scalar.survey)

equiv_chi(0.05, 12.62, 3, 2, 1566, .05)

RDR(99, 58, 10, 7, 2, 1479)

#.13

## Partial Scalar

# cfa_hawaiiblack.partialscalar1 <-cfa(cfa_model, hawaiiblack_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "ADDPLEAS1~1")
# 
# summary(cfa_hawaiiblack.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_hawaiiblack.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hawaiiblack_design)
# 
# summary(cfa_hawaiiblack.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_hawaiiblack.metric.survey, cfa_hawaiiblack.partialscalar1.survey)

# RDR not calculated due to lavaan error (zero/negative variance)

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

anova(cfa_multiblack.configural.survey, cfa_multiblack.metric.survey)

# Sample size is 949 + 511

RDR(186, 184, 7, 4, 2, 1460) # Set to 0
 

## Scalar

cfa_multiblack.scalar <-cfa(cfa_model, multiblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multiblack.scalar, fit.measures = T, standardized = T)

cfa_multiblack.scalar.survey <- lavaan.survey(cfa_multiblack.scalar, multiblack_design)

anova(cfa_multiblack.metric.survey, cfa_multiblack.scalar.survey)

equiv_chi(0.05, 8.46, 3, 2, 1541, .05)

RDR(207, 186, 10, 7, 2, 1460) 
# .09


## Partial Scalar

# cfa_allrace.partialscalar1 <-cfa(cfa_model, multiblack_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "STOPWORY~1")
# 
# summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, multiblack_design)
# 
# summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)

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

anova(cfa_hispanicblack.configural.survey, cfa_hispanicblack.metric.survey)

# Sample size is 949 + 685

RDR(91, 73, 7, 4, 2, 1634) 

# .08

## Scalar

cfa_hispanicblack.scalar <-cfa(cfa_model, hispanicblack_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicblack.scalar, fit.measures = T, standardized = T)

cfa_hispanicblack.scalar.survey <- lavaan.survey(cfa_hispanicblack.scalar, hispanicblack_design)

anova(cfa_hispanicblack.metric.survey, cfa_hispanicblack.scalar.survey)

equiv_chi(0.05, 14.09, 3, 2, 1733, .05)

RDR(128, 91, 10, 7, 2, 1634) 

# .12

## Partial Scalar

# cfa_allrace.partialscalar1 <-cfa(cfa_model, hispanicblack_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "ADPLEAS1~1")
# 
# summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hispanicblack_design)
# 
# summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)
# 

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

anova(cfa_hispanicmulti.configural.survey, cfa_hispanicmulti.metric.survey)

# sample size is 685 + 511

RDR(215, 201, 7, 4, 2, 1196)

# .07

## Scalar

cfa_hispanicmulti.scalar <-cfa(cfa_model, hispanicmulti_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicmulti.scalar, fit.measures = T, standardized = T)

cfa_hispanicmulti.scalar.survey <- lavaan.survey(cfa_hispanicmulti.scalar, hispanicmulti_design)

anova(cfa_hispanicmulti.metric.survey, cfa_hispanicmulti.scalar.survey)

equiv_chi(0.05, 2.18, 3, 2, 1270, .05)

RDR(221, 215, 10, 7, 2, 1196)

#.04

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

anova(cfa_hispanichawaii.configural.survey, cfa_hispanichawaii.metric.survey)

# sample size is 685 + 530

RDR(89, 53, 7, 3, 2, 1215)

#.11

## Scalar

cfa_hispanichawaii.scalar <-cfa(cfa_model, hispanichawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanichawaii.scalar, fit.measures = T, standardized = T)

cfa_hispanichawaii.scalar.survey <- lavaan.survey(cfa_hispanichawaii.scalar, hispanichawaii_design)

anova(cfa_hispanichawaii.metric.survey, cfa_hispanichawaii.scalar.survey)

equiv_chi(0.05, 7.93, 3, 2, 1295, .05)

RDR(116, 89, 10, 7, 2, 1215)

#.11 

## Partial Scalar

# cfa_allrace.partialscalar1 <-cfa(cfa_model, hispanichawaii_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "ADPLEAS1~1")
# 
# summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, hispanichawaii_design)
# 
# summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)

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

anova(cfa_hispanicasian.configural.survey, cfa_hispanicasian.metric.survey)

# sample size is 685 + 577

RDR(237, 212, 7, 4, 2, 1262)

#.11

## Scalar

cfa_hispanicasian.scalar <-cfa(cfa_model, hispanicasian_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_hispanicasian.scalar, fit.measures = T, standardized = T)

cfa_hispanicasian.scalar.survey <- lavaan.survey(cfa_hispanicasian.scalar, hispanicasian_design)

anova(cfa_hispanicasian.metric.survey, cfa_hispanicasian.scalar.survey)

equiv_chi(0.05, .51, 3, 2, 1343, .05)

RDR(240, 237, 10, 7, 2, 1262)

# 0


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

anova(cfa_multiasian.configural.survey, cfa_multiasian.metric.survey)

# Sample size is 511 + 577

RDR(327, 324, 7, 4, 2, 1088)

#0

## Scalar

cfa_multiasian.scalar <-cfa(cfa_model, multiasian_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multiasian.scalar, fit.measures = T, standardized = T)

cfa_multiasian.scalar.survey <- lavaan.survey(cfa_multiasian.scalar, multiasian_design)

anova(cfa_multiasian.metric.survey, cfa_multiasian.scalar.survey)

equiv_chi(0.05, 1.90, 3, 2, 1151, .05)

RDR(335, 327, 10, 7, 2, 1088)

# .06

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

anova(cfa_multihawaii.configural.survey, cfa_multihawaii.metric.survey)

# Sample size is 511 + 530

RDR(192, 164, 7, 4, 2, 1041)

#.13

## Scalar

cfa_multihawaii.scalar <-cfa(cfa_model, multihawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_multihawaii.scalar, fit.measures = T, standardized = T)

cfa_multihawaii.scalar.survey <- lavaan.survey(cfa_multihawaii.scalar, multihawaii_design)

anova(cfa_multihawaii.metric.survey, cfa_multihawaii.scalar.survey)

equiv_chi(0.05, 8.56, 3, 2, 1103, .05)

RDR(220, 192, 10, 7, 2, 1041)

#.13

## Partial Scalar

# cfa_allrace.partialscalar1 <-cfa(cfa_model, multihawaii_data, group = "race", group.equal = c("loadings", "intercepts"),
#                                  group.partial = "STOPWORY~1")
# 
# summary(cfa_allrace.partialscalar1, fit.measures = T, standardized = T)
# 
# cfa_allrace.partialscalar1.survey <- lavaan.survey(cfa_allrace.partialscalar1, multihawaii_design)
# 
# summary(cfa_allrace.partialscalar1.survey, fit.measures = T, standardized = T)
# 
# anova(cfa_allrace.metric.survey, cfa_allrace.partialscalar1.survey)


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

anova(cfa_asianhawaii.configural.survey, cfa_asianhawaii.metric.survey)

# Sample size is 577 + 530

RDR(193, 175, 7, 4, 2, 1107)

#.10

## Scalar

cfa_asianhawaii.scalar <-cfa(cfa_model, asianhawaii_data, group = "race", group.equal = c("loadings", "intercepts"))

summary(cfa_asianhawaii.scalar, fit.measures = T, standardized = T)

cfa_asianhawaii.scalar.survey <- lavaan.survey(cfa_asianhawaii.scalar, asianhawaii_design)

anova(cfa_asianhawaii.metric.survey, cfa_asianhawaii.scalar.survey)

equiv_chi(0.05, 5.08, 3, 2, 1176, .05)

RDR(218, 193, 10, 7, 2, 1107)

#.12