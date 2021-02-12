#Prepare an example dataset

id <- c(1,2,3,4,5,6)
time <- c(8,9,10,11,11,12)
event <- c(1,0,1,1,1,0)
data <- data.frame(id,time,event)
head(data)

# Install and load "survival package"
install.packages("survival")
library(survival)

# Calculate Kaplan-meier Survival estimate
survival01 <- survfit(Surv(data$time, data$event) ~ 1)

plot(survival01) # Step-function
summary(survival01) # Overview of the table
