###########################
#### DESCRIPTIVE TABLE ####
###########################

table1data <- BRFSS2018_Analysis_3000 %>% select(race, race_2cat, SEX1, ADPLEAS1, ADDOWN1, FEELNERV, STOPWORY,
                                            finalweight, `_STSTR`, `_PSU`) 

table1design <- svydesign(ids = ~ `_PSU`, weights = ~finalweight, strata = ~`_STSTR`, 
                    nest = TRUE, data = table1data)

table1 <- table1design %>% tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                      all_categorical() ~ "{n} ({p}%)"),
                                          by = race)

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