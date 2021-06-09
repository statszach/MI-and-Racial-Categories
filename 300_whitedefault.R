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
