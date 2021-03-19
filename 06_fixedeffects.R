# Least squared dummy variables

#Packages
library(plm)
library(dplyr)
options(scipen=999)

#Insert data
ID <-as.numeric(c(1,1,2,2,3,3,4,4))
YEAR <-as.numeric(c(1984,1985,1984,1985,1984,1985,1984,1985))
SATIS <-as.numeric(c(5,6,6,8,6,8,5,5))
EMP <-as.numeric(c(0,1,0,1,0,1,0,0))
DATA01 = data.frame(ID,YEAR,SATIS,EMP) 

#OLS
lm(DATA01$SATIS ~ DATA01$EMP)

#DiD
DATA01<- DATA01 %>%
  arrange(ID, YEAR) %>%
  group_by(ID) %>%
  mutate(EMP_L = lead(EMP),
         SATIS_L = lead(SATIS))

DATA01$SATIS_NEW <- (DATA01$SATIS_L -DATA01$SATIS)
DATA01$EMP_NEW <- (DATA01$EMP_L-DATA01$EMP)

lm(DATA01$SATIS_NEW ~ DATA01$EMP_NEW)

#LDSV => Control for ID to account for individual differences
lm(DATA01$SATIS ~ DATA01$EMP+as.factor(DATA01$ID))


#FE
DATA01$YEAR <-as.factor(DATA01$YEAR)
plm(SATIS ~ EMP, data=DATA01, index=c("ID","YEAR"), model = "within")

#Insert data
ID <-as.numeric(c(1,1,1,2,2,2))
YEAR <-as.numeric(c(1984,1985,1986,1984,1985,1986))
SATIS <-as.numeric(c(10,6,9,10,7,8))
EMP <-as.numeric(c(0,1,1,0,1,1))
DATA01 = data.frame(ID,YEAR,SATIS,EMP)

#FE
plm(SATIS ~ EMP, data=DATA01, model = "within", index="ID")

