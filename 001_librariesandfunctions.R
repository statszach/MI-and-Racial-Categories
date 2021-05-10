library(haven) # import data
library(tidyverse) # data management
library(psych) # descriptives and EFA
library(lavaan) # CFA/MI
library(survey) # survey data
library(lavaan.survey) # survey CFA/MI
library(semTools) # survey CFA/MI
library(MBESS) # reliability

equiv_chi=function(alpha=.05,chi,df,m,N_sample,popRMSEA=.08){
  Fml<-chi/(N_sample-m)
  popep<-(df*popRMSEA^2)/m
  popdelt<-(N_sample-m)*popep
  pval<-pchisq(chi, df, ncp=popdelt, lower.tail=TRUE)   
  res<-data.frame(chi,Fml,popep,popdelt,pval)
  res
} # for Counsell et al equivalence test

options(survey.lonely.psu="adjust") # For using survey data