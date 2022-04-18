# loading ----
library(flexsurv)
library(survival)
library(MASS)
library(gtsummary)
library(tidyverse)
library(patchwork)
library(xtable)
library(survminer)# for log rank visualization
data = as_tibble(read.table("Menopause.dat", sep = "\t", header = F))
colnames(data) = c("id", "intake_age", "menopause_age",
                   "menopause", "race", "education")

data = data %>% 
  mutate(menopause_time = menopause_age-intake_age) %>% 
  mutate(race = factor(race, 
                       levels = c(0,1,2), 
                       labels = c("white","black", "other")),
         education = factor(education, levels = 0:3,
                            labels = c("post graduate",
                                       "college graduate",
                                       "some college",
                                       "high school or less"))
  ) %>% 
  mutate(race = factor(race, levels = c("other","black","white")))

# data_other = data %>% 
#   mutate(race = factor(race, levels = c("other","black","white")))





# Model ----

## start1: meno_time ----
png(filename = "pres_vis/loglog_ph_assump.png", width = 1000, height = 1000)
par(mfrow = c(2,2))
menopause_tim_surv = Surv(data$menopause_time, data$menopause)
plot(survfit(menopause_tim_surv~data$race),
     col = c("red","blue","purple"),
     fun = "cloglog",
     main = "meno_time~race",
     xlab = "log(menopause time)",
     ylab = "log(-log(s)")
legend(0.02, -1, 
       cex = 1,
       legend = c("other", "black", "white"), 
       fill = c("red","blue","purple"))
plot(survfit(menopause_tim_surv~data$education),
     col = c("red","blue","purple","black"),
     fun = "cloglog",
     main = "meno_time~education",
     xlab = "log(menopause time)",
     ylab = "log(-log(s)")
legend(0.02, -1, 
       cex = 1,
       legend = c("post graduate","college graduate","some college","high school or less"),
       fill = c("red","blue","purple","black"))

## start2: meno_age  ----
menopause_age_surv = Surv(data$menopause_age, data$menopause)
plot(survfit(menopause_age_surv~data$race),
     col = c("red","blue","purple"),
     fun = "cloglog",
     main = "meno_age~race",
     xlab = "log(menopause age)",
     ylab = "log(-log(s)")
legend(45, 0, 
       cex = 1,
       legend =c("other", "black", "white"), 
       fill = c("red","blue","purple"))
plot(survfit(menopause_age_surv~data$education),
     col = c("red","blue","purple","black"),
     fun = "cloglog",
     main = "meno_age~education",
     xlab = "log(menopause age)",
     ylab = "log(-log(s)")
legend(52, -3, 
       cex = 1,
       legend = c("post graduate","college graduate","some college","high school or less"), 
       fill = c("red","blue","purple","black"))
dev.off()


