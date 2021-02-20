# Perform Kaplan-Meier function to calculate mediance age of leaving home in Europe

## Load libraries
library(haven)
library(desc)
library(survival)
library(dplyr)
library(ggplot2)
library(broom)
library(survminer)

### Read in the data (European Social Survey 2018/19)
ess_raw <- read_dta("data/01_ESS/ESS9e03.dta", 
                    encoding="latin1")

### Select variables of interest
ess <- ess_raw %>% 
  select(yrbrn, cntry, inwyys, gndr, evmar, maryr) 

### Limit the investigation to the cohorts born 1920 to 1999
ess_year <- ess %>% filter(yrbrn <= 1999 & yrbrn >=1920) 

# Limiting the year reduces the sample size to 47,323.
nrow(ess_year)

### Select country (sweden)
ess_sweden <- ess_year %>% 
  filter(cntry=="SE") 

# Our final sample size is 1,477
nrow(ess_sweden)

### Create event function

create_event <- function(x){ 
  x <- x %>% mutate(event = 
                      case_when(c==2 ~ 0,
                                evmar==1 ~ 1)) %>%
    mutate(event = as.numeric(event))
}

### Create time function

create_time <- function(x){
    x <- x %>% mutate(age1 = maryr-yrbrn-1,
                      age2 = inwyys-yrbrn-1) %>%
      mutate(time = case_when(
        event==1 ~ age1,
        event==0 ~ age2)) %>%
      mutate(time=as.numeric(time))
  
}

### Create cohort function

create_cohort <- function(x) {
  x <- x %>% mutate(cohort = case_when(
    yrbrn > 1919 & yrbrn < 1940 ~ "1920-39",
    yrbrn > 1939 & yrbrn < 1960 ~ "1940-59",
    yrbrn > 1959 & yrbrn < 1980 ~ "1960-79",
    yrbrn > 1979 & yrbrn < 2000 ~ "1980-99"
  ))
}

ess_sweden <- ess_sweden %>% create_event() %>% create_time %>% create_cohort()

# Survival functions

survival1 <- survfit(Surv(ess_sweden$time, ess_sweden$event) ~ ess_sweden$gndr)
plot(survival1, col=c("red", "blue"))
legend("topright", legend=c("Female", "Male"),
       col=c("red", "blue"), lwd=4, cex=0.8)

summary(survival1) 

survival2 <- survfit(Surv(ess_sweden$time, ess_sweden$event) ~ ess_sweden$gndr+ess_sweden$cohort)

summary(survival1) 
