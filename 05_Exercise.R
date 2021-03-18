library(haven)
library(stringr)
library(dplyr)
library(stargazer)

# German Socioeconomic Panel Data
gsep_raw <- readRDS("data/03_gsep/Exercise05.rds") %>%
  janitor::clean_names()

# Exercise 5.1.
## Create a balanced dataset

### Reassign values for emp
gsep_raw$emp <- as.numeric(substr(gsep_raw$emp, 1,1))
gsep_raw$mig <- as.factor(substr(gsep_raw$mig, 1,1))
gsep_raw$edu <- as.factor(substr(gsep_raw$edu, 1,1))

### Choose only the people who are either looking for employment or employed
gsep <- gsep_raw %>% filter(emp %in% c(0,1))

### Drop respondents that contribute only 1 person-year
gsep <- gsep %>% arrange(id, year) %>%
  group_by(id) %>%
  mutate(spell=row_number()) %>%
  mutate(spell_max=max(spell)) %>%
  filter(spell_max == 2) %>% 
  select(-spell, -spell_max)


nrow(gsep)/2 # 14968 subjects
nrow(gsep) # 29936 subjects

# Exercise 5.2 
## OLS regression

model1_a <- lm(satis ~ emp + year + sex + agec, data=gsep)
model1_b <- lm(satis ~ emp + year + sex + agec + mig, data=gsep)
model1_c <- lm(satis ~ emp + year + sex + agec + mig + edu, data=gsep)

## Print results
stargazer(model1_a, model1_b, model1_c, 
          type= "text")

# Exercise 5.3
## Diff-in-Diff
gsep <- gsep %>% group_by(id) %>%
  mutate(satis_lag = lag(satis), 
         emp_lag = lag(emp)) %>% ungroup() %>%
  mutate(diff_satis = satis - satis_lag,
         diff_emp = emp - emp_lag)

model2 <- lm(diff_satis ~ diff_emp, data=gsep)

stargazer(model2, type="text")

# Exercise 5.4

gsep_mig <- gsep %>%
  filter(mig==2)
gsep_nat <- gsep %>%
  filter(mig==1)

model3 <- lm(diff_satis ~ diff_emp, data=gsep_mig) 
model4 <- lm(diff_satis ~ diff_emp, data=gsep_nat)

stargazer(model3, model4, type="text")

# Exercise 5.6

## Preparing data for diff in diff analysis
gsep <- gsep %>% mutate(
  fam = as.numeric(fam),
  year = as.numeric(year)
) %>% group_by(id) %>%
  mutate(fam_lag = lag(fam)) %>%
  ungroup() %>%
  mutate(diff_fam = fam - fam_lag)

## OLS models
model5_1 <- lm(satis ~ fam + year, data=gsep)
model5_2 <- lm(satis ~ fam + year + emp + sex + mig + agec + edu, data=gsep)

## DiD 
model5_3 <- lm(diff_satis ~ diff_fam + emp + year, data=gsep)

stargazer(model5_1, model5_2, model5_3, type="text")
