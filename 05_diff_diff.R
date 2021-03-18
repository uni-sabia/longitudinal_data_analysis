# Diff in Diff
library(dplyr)

ID <-as.numeric(c(1,2,2,3,3,4,4,4))
YEAR <-as.numeric(c(1984,1984,1985,1984,1985,1984,1985,1987))
SATIS <-as.numeric(c(5,6,8,1,2,2,3,1))
EMP <-as.numeric(c(0,0,1,0,0,0,0,1))
DATA = data.frame(ID,YEAR,SATIS,EMP)

# Arrange them by id and descending year variable
data <- DATA %>% arrange(ID, YEAR) %>%
  group_by(ID) %>%
  mutate(YEAR_lag = lag(YEAR), # So that we can identify subjects that participated extra time
         spell=row_number())  %>%
  filter(YEAR==YEAR_lag+1 | spell==1) # Eliminate rows that are out of the timeframe 

balanced_data <- data %>% 
  arrange(ID,YEAR) %>%
  group_by(ID) %>%
  mutate(spell_new = row_number(),
         spell_max = max(spell)) %>%
  filter(spell_max !=1) # Eliminate rows that participated in the study only once. 

#######
# Consider a new, balanced dataset.
ID <-as.numeric(c(1,1,2,2,3,3,4,4))
YEAR <-as.numeric(c(1984,1985,1984,1985,1984,1985,1984,1985))
SATIS <-as.numeric(c(5,9,4,8,3,4,1,2))
MARRIED <-as.numeric(c("0","1","0","1","0","0","0","0"))
DATA01 <- data.frame(ID, YEAR, SATIS, MARRIED)

# Diff in Diff estimation in R
diff_data <- DATA01 %>% 
  arrange(ID, YEAR) %>%
  group_by(ID) %>%
  mutate(satis_lag = lag(SATIS),
         married_lag = lag(MARRIED)) %>%
  ungroup() %>%
  mutate(diff_satis = SATIS-satis_lag,
         diff_married = MARRIED-married_lag)

diff_ols <- lm(diff_data$diff_satis ~ diff_data$diff_married)
diff_ols
