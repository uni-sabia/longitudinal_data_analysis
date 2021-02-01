library(haven)
library(dplyr)

# Read in the data (European Social Survey 2018/19)
df1 <- read_dta("data/01_ESS/ESS9e03.dta", 
                encoding="latin1")

## Subset the data for employment, gender and age
df2 <- df1 %>% select(idno, cntry, evpdemp, gndr, agea)

# Examine/browse your data
head(df2)
## Subset data for Germany
germany <- df2 %>% filter(cntry=="DE")
## Inspect variables
sapply(germany, attributes)

# Select responents who are in working age and eliminate NAs
germany_w <- germany %>% filter(agea <= 65 & agea >= 16) %>% 
  na.omit()

# Generate new variables
germany_w <- germany_w %>% mutate(gender=case_when(gndr==2 ~ "Female",
                                                   gndr==1 ~ "Male")) 
germany_w$gender <- factor(germany_w$gender, levels=c("Male","Female"))

# Sample size and analysis 

## How large is the sample size? 
nrow(germany_w) #1794

## How many females/males are included in your sample?
table(germany_w$gender)

## How many are wokring/not working?
table(germany_w$evpdemp) 

## What is the proportion of women and men working vs. not working? 
prop.table(table(germany_w$evpdemp, germany_w$gender), 2)
