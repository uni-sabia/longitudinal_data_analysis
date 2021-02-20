# Exercise 2

## Load libraries
library(haven)
library(desc)
library(survival)
library(dplyr)
library(ggplot2)
library(broom)
library(survminer)
options(scipen=999)

# 1. Data Preparation

### Read in the data (European Social Survey 2018/19)
ess_raw <- read_dta("data/01_ESS/ESS9e03.dta", encoding="latin1")

### Subset data by variables of interest;
### Select Netherlands, for 1930-1989 cohort
ess <- ess_raw %>% 
  select(idno, cntry, yrbrn, gndr, inwyys, lvpntyr, evmar, maryr, pdjobev, pdempyr, bthcld, fcldbrn) %>%
  filter(cntry=="NL", yrbrn <= 1989 & yrbrn >=1930) %>% 
  labelled::remove_labels() 
nrow(ess)

### Delete mssing information 
#### The censored cases have NAs in the year of which the event occured . 
#### We would lose the censored cases if we eliminated all NAs. 
#### Therefore, we will replace NAs with 0 for the concerned variables and then perform analysis.
ess$maryr[is.na(ess$maryr)] <- 0
ess$pdempyr[is.na(ess$pdempyr)] <- 0
ess$fcldbrn[is.na(ess$fcldbrn)] <- 0

ess <- na.omit(ess)

# Our final sample size is 500. 
nrow(ess)

### Create event function
create_event <- function(x, variable){
  
  if (variable == "lvpntyr"){
     x <- x %>% filter(lvpntyr != 1111) 
     e <- x %>% select(idno, vars= !!variable) %>%
       mutate(event = case_when(
         vars == 0 ~ 0,
         vars > 0 ~ 1)) %>% select(idno, event)
     x <- merge(x, e, by="idno")
  } else {
    e <- x %>% select(idno, vars= !!variable) %>%
      mutate(event = case_when(
        vars == 2 ~ 0,
        vars == 1 ~ 1)) %>% select(idno, event)
    x <- merge(x, e, by="idno")
  }
  x
}

### Create time function

create_time <- function(x, variable){
  time <- x %>% select(idno, yrbrn, vars = !!variable, inwyys) %>%
    mutate(age= vars-yrbrn - 1,
           age_censored = inwyys-yrbrn-1) %>%
    select(idno, age, age_censored)
  x <- merge(x, time, by="idno") 
  x <- x %>% mutate(time = case_when(
    event==1 ~ age,
    event==0 ~ age_censored)) %>%
    mutate(time=as.numeric(time))
  x
}

### Create cohort function
create_groups <- function(x) {
  x <- x %>% mutate(cohort = case_when(
      yrbrn >= 1930 & yrbrn <= 1949 ~ "1930-49",
      yrbrn >= 1950 & yrbrn <= 1969 ~ "1950-69",
      yrbrn >= 1970 & yrbrn <= 1989 ~ "1970-89"
  )) %>%
    mutate(gender = case_when(
      gndr == 2 ~ "Female",
      gndr == 1 ~ "Male"
    )) %>%  mutate(cohort = factor(cohort), 
                   gender = factor(gender)) 
  x
}

### Construct an event history 
# leaving home
ess_home <- ess %>% create_event("lvpntyr") %>% create_time("lvpntyr") %>% create_groups()

# marriage
ess_mar <- ess %>% create_event("evmar") %>% create_time("maryr") %>% create_groups()

# first job
ess_job <- ess %>% create_event("pdjobev") %>% create_time("pdempyr") %>% create_groups()

# first child  
ess_chl <- ess %>% create_event("bthcld") %>% create_time("fcldbrn") %>% create_groups()


# 2.1 Estimate survival functions for the age at which people have first child
## a) By cohort
table(ess_chl$event)
survival1 <- survfit(Surv(ess_chl$time, ess_chl$event) ~ ess_chl$cohort)
survival_tidy1 <- tidy(survival1)
print(survival1) 

plot1 <- ggplot(survival_tidy1, aes(x=time, y=estimate, group=strata)) +
  geom_step(aes(color=strata)) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Survival Probability") +
  scale_x_continuous(name= "Time") +
  scale_color_manual(name="Cohort", labels=c("1930-49", "1950-69", "1970-1989"), 
                     values=c("green", "orange", "purple"))

ggsave("figures/chl_cohort.png", plot1, width=5, height=3)


## b) By gender
survival2 <- survfit(Surv(ess_chl$time, ess_chl$event) ~ ess_chl$gender)
survival_tidy2 <- tidy(survival2)
print(survival2)
plot2 <- ggplot(survival_tidy2, aes(x=time, y=estimate, group=strata)) +
  geom_step(aes(color=strata)) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Survival Probability") +
  scale_x_continuous(name= "Age") +
  scale_color_manual(name="Gender", labels=c("Female", "Male"), values=c("red", "blue"))

ggsave("figures/chl_gender.png", plot2, width=5, height=3.5)

### Perform log-rank test (Table 3)
diff_chl_gender <- survdiff(Surv(ess_chl$time, ess_chl$event) ~ ess_chl$gender)
diff_chl_gender

# 2. Replicate Table 1 
## Leaving home by gender and cohort
home_srv <- survfit(Surv(time, event) ~ gender+cohort, data=ess_home)
print(home_srv)

## First job by gender and cohort
job_srv <- survfit(Surv(time, event) ~ gender+cohort, data=ess_job)
print(job_srv)

## First marriage by gender and cohort
mar_srv <- survfit(Surv(time, event) ~ gender+cohort, data=ess_mar)
print(mar_srv)

## First child by gender and cohort
child_srv <- survfit(Surv(time, event) ~ gender+cohort, data=ess_chl)
print(child_srv)
