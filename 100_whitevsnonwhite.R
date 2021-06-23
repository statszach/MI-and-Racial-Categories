################################
#### CREATING SURVEY DESIGN ####
################################

design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                    nest = TRUE, data = BRFSS2018_Analysis_3000)
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

anova(cfa_race2cat.configural.survey, cfa_race2cat.metric.survey)

RDR(T_A = 451.58, T_B = 443.63, df_A = 7, df_B = 4, G = 2, N = 3000)
# 0.03

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

anova(cfa_race2cat.metric.survey, cfa_race2cat.scalar.survey)

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