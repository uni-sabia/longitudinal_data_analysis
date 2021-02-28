library(survival)
library(eha)

# Data of time at which an event happend
TIME <- as.numeric(c(8,9,10,11,11,12))
EVENT <- as.numeric(c(1,0,1,1,1,0))
DATA = data.frame(TIME,EVENT)

# R functions that calculates the "hazard rate".
## Hazard rate is the potential of the event occurring, defined as number of events/sum of time.
OUTPUT<-phreg(Surv(DATA$TIME,DATA$EVENT)~1,dist="weibull", shape=1)
exp(-coef(OUTPUT))

# Second dataset, grouped by sex
TIME <- as.numeric(c(8,9,10,11,11,12,8,10))
EVENT <- as.numeric(c(1,0,1,1,1,0,1,1))
SEX <- as.factor(c(0,0,1,1,1,0,1,1))
DATA = data.frame(TIME,EVENT,SEX)

# Estimate the model, grouped by sex
phreg(Surv(DATA$TIME,DATA$EVENT) ~ DATA$SEX, dist="weibull", shape=1)

# Change the reference group
DATA$SEX <- relevel(DATA$SEX , ref="1")
phreg(Surv(DATA$TIME,DATA$EVENT)~DATA$SEX, dist="weibull", shape=1)

# Cox model
## Cox model does not specifcy the baseline hazard
coxph(Surv(DATA$TIME,DATA$EVENT) ~ DATA$SEX)

##########
### Group work
library(haven)
library(descr)
library(survival)
library(eha)
library(questionr) # allows you to look up variables
library(ggplot2)
library(dplyr)
library(tidyverse)

# Data 
## The DHS Program (Demographic and Health Surveys)
## Country of interest: Guatemala

## Data Preparation
dhs_raw <- read_dta("data/02_DHS/GUATEMALA_F.DTA")

## Find which variables are relevant for our analysis
### Full list 
makeVlist <- function(x) {
  labels <- sapply(x, function(x) attr(x,"label"))
  tibble(name= names(labels), label=labels)
}
variables <- makeVlist(dhs_raw)
variables ## Are there variables that are omitted in our analysis? 

### Look up by a keyword
lookfor(dhs_raw, "child")

### Variables of interest
# v012 = respondents' age
# v016 = date of interview
# v106 = highest education level 
# v201 = total children ever born
# v212 = age of respondent at 1st birth



