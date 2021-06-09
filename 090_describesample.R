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

