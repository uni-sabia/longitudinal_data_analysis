# Perform Kaplan-Meier function to calculate mediance age of leaving home in Europe

## Load libraries
library(haven)
library(desc)
library(survival)
library(dplyr)

# Exercise 1.1

### Read in the data (European Social Survey 2018/19)
ess_raw <- read_dta("data/01_ESS/ESS9e03.dta", 
                    encoding="latin1")

# Original sample size is 49,519.
nrow(ess_raw)

### Select variables of interest
ess <- ess_raw %>% 
  select(lvpntyr, yrbrn, cntry, inwyys, gndr) 

### Limit the investigation to the cohorts born 1920 to 1999
ess_year <- ess %>% filter(yrbrn <= 1999 & yrbrn >=1920) 

# Limiting the year reduces the sample size to 47,323.
nrow(ess_year)

### Delete cases with missing data 
ess_no_na <- ess_year %>% 
  na.omit() # delete cases with missing data 

# Deleting rows with missing values reduces the sample size to 45,664
nrow(ess_no_na)

### Exclude the people who have never lived with a parent.
ess_exc <- ess_no_na %>% filter(lvpntyr != 1111)

# Excluding subjects who are not at risk reduces the sample size to 45,374
nrow(ess_exc)

### Select country and gender 
ess_germany_women <- ess_exc %>% 
  filter(cntry=="DE") %>% # Select Germany
  filter(gndr==2) # Select women

# The final sample has 1,072 observations.
nrow(ess_germany_women)

### Create an event variable
# The following function creates an event variable that takes the value of 0 if the person has not left home yet, and 1 if the person has left home. 

create_event <- function(x){ 
  x <- x %>% mutate(event = 
                      case_when(lvpntyr==0 ~ 0,
                                lvpntyr>0 ~ 1)) %>%
    mutate(event = as.numeric(event))
  x
}

# There are 1,020 events and 52 censored cases in our analysis. 
ess_germany_women <- create_event(ess_germany_women)
table(ess_germany_women$event)

### Create a time variable (age)
# The following function creates a time variable whose values are the age at which the person left home. For the censored cases, the time variable is the age at which the interview took place. 

create_time <- function(x){
  x <- x %>% mutate(age1 = lvpntyr-yrbrn-1,
                    age2 = inwyys-yrbrn-1) %>%
    mutate(time = case_when(
      event==1 ~ age1,
      event==0 ~ age2)) %>%
    mutate(time=as.numeric(time))
}

ess_germany_women <- create_time(ess_germany_women)

# Exercise 1.2 

## a) Estimate a survival function for your sample. 

survival <- survfit(Surv(ess_germany_women$time, ess_germany_women$event) ~ 1)

plot(survival) # Step-function
summary(survival) # Overview of the table

## b) Make comparisons to other groups 
ess_germany_men <- ess_exc %>% 
  filter(cntry=="DE") %>% # Select Germany
  filter(gndr==1) # Select men

# The final sample has 1,124 observations.
nrow(ess_germany_men)

# Create event and time variables
ess_germany_men <- ess_germany_men %>% create_event() %>% create_time()

survival_men <- survfit(Surv(ess_germany_men$time, ess_germany_men$event) ~ 1)

plot(survival_men) # Step-function
summary(survival_men) # Overview of the table


ess_spain_women <- ess_exc %>%
  filter(cntry=="ES") %>% # Select Spain
  filter(gndr==2) # Select women

# The final sample has 754 observations
nrow(ess_spain_women)

