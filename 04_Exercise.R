# This week's exercise is event history modeling
# Data prep

library(haven)
library(eha)
library(survival)
library(dplyr)
library(stargazer)
library(survminer)
library(lmtest)

gsep <- readRDS("data/03_gsep/Exercise04.rds")

# Exercise 4-2. Piecewise constant model 

## Create time variables
gsep_00 <- gsep %>% mutate(
  END = TIME,
  BEGIN = 0
)

## Create time intervals (Surv)
gsep_00$Surv <- with(gsep_00, Surv(BEGIN, END, EVENT==1))

## Episode Splitting: Cut the time into 4 intervals 
gsep_00 <- survSplit(Surv ~., data=gsep_00, cut=c(15, 30, 45), episode = "AGEC")

## Piecewise constant model
## = Proportional hazards model with parametric baseline hazard(s)
### Control for unemployment duration and migration background

model_00 <- phreg(Surv~as.factor(AGEC)+MIG, dist="weibull", shape=1, data=gsep_00)
summary(model_00)
stargazer(model_00, type="text", out="table1.html", apply.coef=exp, t.auto = F, p.auto=F)

## Cox model
model_01 <- coxph(Surv(TIME, EVENT) ~ MIG, data=gsep)
summary(model_01)

### Print results of two models
stargazer(model_00, model_01, type="text", out="table1.html", apply.coef=exp, t.auto = F, p.auto=F)

# Exercise 4-3. Multivariate Cox models 
model_02 <- coxph(Surv(TIME, EVENT) ~ SEX+MIG, data=gsep)
model_03 <- coxph(Surv(TIME, EVENT) ~ SEX+MIG+EDU, data=gsep)
stargazer(model_02, model_03, type="text", out="table1.html", apply.coef=exp, t.auto = F, p.auto=F)

## Log likelihood test (compare goodness of fit)
lrtest(model_02, model_03)

# Exercise 4-4. Cox-model with interaction
## Create an interaction term
gsep$INT <- interaction(gsep$SEX, gsep$MIG)
levels(gsep$INT)

## Check if each group has enough observations
table(gsep$INT)

## cox estimation
model_04 <- coxph(Surv(TIME, EVENT) ~ INT, data=gsep)
ggforest(model_04, data=NULL, main="Hazard Ratio")


# 4-5. Proportionality assumption 
## Interact the baseline hazard with another variable
gsep_00$AGEC <- as.factor(gsep_00$AGEC)
gsep_00$PROP <- interaction(gsep_00$MIG, gsep_00$AGEC)

table(gsep_00$PROP)

model_proptest <- phreg(Surv ~ PROP, dist="weibull", shape=1, data=gsep_00)

## Prep dataset for graphing
output1 <- as.data.frame(model_proptest$coef)
names(output1) <- "coef"
coefnames <- rownames(output1)
rownames(output1) <- NULL
output2 <- cbind(coefnames, output1)

## Calculate hazard ratios and confidence levels
output2$expcoef <- exp(model_proptest$coef)
output2$se <- sqrt(diag(summary(model_proptest)$var))
#Lower limit
output2$logL <- output2$coef - 1.96*output2$se
output2$L <- exp(output2$logL)
# Upper limit
output2$logU <- output2$coef + 1.96*output2$se
output2$U <- exp(output2$logU)
output3 <- subset(output2, coefnames!="log(scale)")

## Create a reference category
row <- as.numeric(c(1,1,1,1,1,1,1,1))
output4 <- rbind(row, output3)

## Separate labels for AGEC and SEX
output4$DURATION <- c("(0,15]", "(0,15]", "(15,30]", "(15,30]", "(30,45]", "(30,45]", "(45,60]", "(45,60]")
output4$DURATION <- as.factor(output4$DURATION)
output4$GENDER <- c(rep(c("native","migrant"),4))
output4$GENDER <- as.factor(output4$GENDER)

## ggplot
ggplot(output4, aes(x = DURATION, y=expcoef, color= GENDER))+
  geom_point(aes(color=GENDER), position=position_dodge(width=0.1)) +
  geom_path(aes(x = as.numeric(DURATION),color = GENDER), position=position_dodge(width=0.1)) +
  geom_errorbar(aes(ymin=L, ymax=U), width=.05, position=position_dodge(width=0.1)) +
  ylim(0,1) +
  ylab("Hazard ratio")+
  xlab("Months in unemployment") +
  scale_color_manual(values=c("red", "blue")) +
  theme_bw()

# 4-6. Interaction between sex and family, holding education and baseline hazard constant
gsep_00$INT_ <- interaction(gsep_00$SEX, gsep_00$FAM)
model_05 <- coxph(Surv(TIME, EVENT) ~ INT_+EDU+AGEUN, data=gsep_00)

stargazer(model_05, type="text", out="table1.html", apply.coef=exp, t.auto = F, p.auto=F)
ggforest(model_05, data=NULL, main="Hazard Ratio")
