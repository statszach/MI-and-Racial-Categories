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


# T_A is the larger Chi-square value
# 
# T_B is the smaller Chi-square value
# 
# Df_A and Df_B are the corresponding degrees of freedom
# 
# G is the number of groups

RDR = function(T_A, T_B, df_A, df_B, G, N){

T_chisqdiff=T_A-T_B

DF_chisqdiff=df_A-df_B

RDR=sqrt(G*(T_chisqdiff-DF_chisqdiff)/((N-G)*DF_chisqdiff))

RDR
}

options(survey.lonely.psu="adjust") # For using survey data