library(haven) # import data
library(tidyverse) # data management
library(psych) # descriptives and EFA
library(lavaan) # CFA/MI
library(survey) # survey data
library(lavaan.survey) # survey CFA/MI
library(semTools) # survey CFA/MI
library(MBESS) # reliability
library(gtsummary) #tables

equiv_chi=function(alpha=.05,chi,df,m,N_sample,popRMSEA=.08){
  Fml<-chi/(N_sample-m)
  popep<-(df*popRMSEA^2)/m
  popdelt<-(N_sample-m)*popep
  pval<-pchisq(chi, df, ncp=popdelt, lower.tail=TRUE)   
  res<-data.frame(chi,Fml,popep,popdelt,pval)
  res
} # for Counsell et al equivalence test

RDR = function(x2_r, x2_pop, df_r, df_pop, n, k){
  
  noncent_pop = x2_pop - df_pop
   
  noncent_r = x2_r - df_r
  
  delta_F = abs(noncent_r - noncent_pop) / n
  
  RDR = sqrt(delta_F / df_r) * sqrt(k)
  
  RDR
}



options(survey.lonely.psu="adjust") # For using survey data