# Assignment 3 
# Event history modeling 

library(haven)
library(descr)
library(survival)
library(eha)
library(questionr) # allows you to look up variables
library(ggplot2)
library(dplyr)
library(tidyverse)
library(survminer)
library(broom)

# Data 
## The DHS Program (Demographic and Health Surveys)
## Country of interest: Guatemala

## Data Preparation
dhs_raw <- read_dta("data/02_DHS/GUATEMALA_F.DTA")
nrow(dhs_raw)

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

## Limit the investigation to the ages 25-49
dhs_1 <- dhs_raw %>% filter(v012 >= 25 & v012 <= 49)
nrow(dhs_1)

## Delete respondents with missing information for level of education (v106)
## Also delete respondents who are still in school
dhs_2 <- dhs_1 %>% filter(v106>0 & v106 <=3)
nrow(dhs_2)

## Create event, time, birth cohorts, education level
## v_201: number of children (no NAs)
## v_010: respondent's birth year
## v_007: year of interview
## v_212: age of respondent at 1st birth

dhs <- dhs_2 %>% mutate(event = case_when(
        v201 == 0 ~ 0,
        v201 > 0 ~ 1,
        TRUE ~ -1)) %>%
  mutate(time = case_when(
    event == 0 ~ v007 - v010 -1 ,
    event == 1 ~ v212,
    TRUE ~ -1)) %>%
  filter(time!=-1, event!=-1) %>% # Delete missing values
  mutate(cohort = case_when(
    (v012 >= 25 & v012 <= 35) ~ "Young",
    (v012 >= 36 & v012 <= 49) ~ "Old"
  )) %>% mutate(cohort=as.factor(cohort), education = as.factor(v106))
nrow(dhs)

# Exercise 3.2
# a) Estimate a survival function by cohort. 
sur_cohort <- survfit(Surv(dhs$time, dhs$event) ~ dhs$cohort)
sur_cohort_tidy <- tidy(sur_cohort)
print(sur_cohort_tidy) 

plot1 <- ggplot(sur_cohort_tidy, aes(x=time, y=estimate, group=strata)) +
  geom_step(aes(color=strata)) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Survival Probability") +
  scale_x_continuous(name= "Age") + 
  scale_color_manual(name="Cohort", labels=c("Young", "Old"), 
                     values=c("green", "orange"))
plot1
ggsave("figures/3_surv_cohort.png", plot1, width=5, height=3)

# a) Estimate a survival function by education level. 
sur_edu <- survfit(Surv(dhs$time, dhs$event) ~ dhs$education)
sur_edu_tidy <- tidy(sur_edu)
print(sur_edu_tidy) 

plot2 <- ggplot(sur_edu_tidy, aes(x=time, y=estimate, group=strata)) +
  geom_step(aes(color=strata)) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Survival Probability") +
  scale_x_continuous(name= "Age") + 
  scale_color_manual(name="Education Level",labels=c("Low", "Medium", "Higher"), 
                                      values=c("red", "green", "blue"))
plot2
ggsave("figures/3_surv_edu.png", plot2, width=5, height=3)

# Exercise 3.3
# Sample statistics
## Subjects

crosstab(dhs$education, dhs$cohort, prop.c=T)
freq(dhs$education)

## Events
sum(dhs$event)
aggregate(x= dhs$event,
          by=list(dhs$cohort),
          FUN=sum)

# Exposure
sum(dhs$time)
aggregate(dhs$time,
          by= list(dhs$cohort),
          FUN = sum)

# Exercise 3.4 Estimate cox models
## Control for cohort, with young as reference
dhs$cohort <- relevel(dhs$cohort, ref="Young")
coxph(Surv(time, event) ~ cohort, data=dhs)

# Control for cohort and education
dhs$education <- relevel(dhs$education, ref="3")
coxph(Surv(time, event) ~ cohort + education, data=dhs)
