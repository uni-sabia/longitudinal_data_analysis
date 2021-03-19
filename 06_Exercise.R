library(haven)
library(dplyr)
library(plm)
library(sjlabelled)
library(ggplot2)
library(table1)
library(stargazer)

# 1. Data Prep
share_raw <- read_dta("data/05_EasyShare/EasyShare.dta") 
  

## Select variables
share <- share_raw %>% 
  filter(country %in% c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23), 
         age > 50 & age < 80,
         isced1997_r > 0 & isced1997_r < 10,
         mar_stat > 0,
         eurod >= 0,
         ac002d1 >=0) %>% 
  mutate(id=as.factor(mergeid),
         wave=as.factor(wave),
         eurod=as.numeric(eurod),
         vol=as.factor(
           ifelse(ac002d1==0, "0-No volunteering", "1-Volunteering")),
         gender=as.factor(
           ifelse(female==0, "2-Man", "1-Woman")),
         fam=as.factor(
           case_when(
           mar_stat %in% c(1,2,3) ~ "1-Married",
           mar_stat %in% c(4, 5) ~ "2-Single/divorced",
           mar_stat == 6 ~ "2-Widowed")),
         country=as.factor(
           substring(as_label(country), first=4)),
         edu = as.factor(isced1997_r),
         age = as.numeric(age)
         ) %>%
  select(id, wave, eurod, vol, gender, fam, country, edu, age) %>%
  na.omit()

## Number of subjects
nrow(share) 
nrow(share)/2 

## Distribution of the outcome variable (eurod)
dist_data <- share %>% group_by(vol, eurod) %>%
  summarize(freq=n()) %>%
  mutate(share=freq/sum(freq)) %>% as.data.frame()

distribution <- ggplot(dist_data, aes(x=as.factor(eurod), y=share, fill=vol)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Distribution of depression levels",
       subtitle = "by volunteer activity",
       x = "Depression (EURO_d)",
       y = "%",
       fill = "") +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("blue", "red"))

ggsave("figures/6_graph1.png", distribution, width=8, height =5)

## Summary statistics
table1::label(share$eurod) <- "Depression"
table1::label(share$age) <- "Age"
table1::label(share$fam) <- "Marital status"
table1::label(share$gender) <- "Gender"
table1::table1(~eurod + age + fam + gender, data=share)

# 3. OLS models 

model1 <- lm(eurod ~ vol + gender + wave + country + age, data=share)
model2 <- lm(eurod ~ vol + gender + wave + country + age + fam, data=share)
model3 <- lm(eurod ~ vol + gender + wave + country + age + fam + edu, data=share)

stargazer(model1, model2, model3, type="text")

# 4. Fixed Effects 

model4 <- plm(eurod ~ vol + wave, data=share, index=c("id", "wave"))
model5 <- plm(eurod ~ vol + wave + fam, data=share, index=c("id", "wave"))

stargazer(model4, model5, type="text")
