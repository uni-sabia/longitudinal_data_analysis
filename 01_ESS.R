library(haven)
library(desc)
library(survival)

# Research question: how long does it take for people to leave home for the first time in Sweden? 

# Read in the data (European Social Survey 2018/19)
df1 <- read_dta("data/01_ESS/ESS9e03.dta", 
                encoding="latin1")

# Select variables relevant to leaving home
df2 <- subset(df1, select=c(yrbrn, lvpntyr, inwyys, cntry, gndr))
str(df2)


# Subset data for Sweden
sweden <- subset(df2, df2$cntry=="SE")
head(sweden)

# Inspect variables
attributes(sweden$lvpntyr)

# Explore data
table(df2$lvpntyr) 
## 6349 people have not left home (0)
## 355 people have never lived with a parent (1111)
## No missing value

# Exclude cases of people who have never lived with parents
sweden <- subset(sweden, (sweden$lvpntyr !=1111))

# Create a new variable for event 
sweden$event[sweden$lvpntyr==0] <- 0 ## 0 if the person has not left home yet
sweden$event[sweden$lvpntyr>0] <- 1 ## 1 if the person has left home 
sweden$event <- as.numeric(sweden$event)

# Create a new variable for age when leaving home
sweden$age1 <- -1 # Account for the lost year when calculating age by subtracting years from years
sweden$age1 <- (sweden$lvpntyr-sweden$yrbrn) # year of leaving home - year of birth

# Create a new variable for age when censoring happened 
## Refers to cases of people dropping out of the experiment, death, etc. 
sweden$age2 <- -1
sweden$age2 <- (sweden$inwyys-sweden$yrbrn) # start of interview - birth year

# Create a new variable for time
sweden$time <- -1 
sweden$time[sweden$event == 1] <- sweden$age1[sweden$event==1] ## For people that left home, put the age when leaving home

sweden$time[sweden$event == 0] <- sweden$age2[sweden$event==0] ## For people who are censored, put the age of censored
sweden$time <- as.numeric(sweden$time)

# Kaplan-meier survival function
survival01 <- survfit(Surv(sweden$time, sweden$event) ~ 1)

plot(survival01)

summary(survival01)
