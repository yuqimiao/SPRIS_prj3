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
  ) 

# survival plot
## models
exp1 <- flexsurvreg(Surv(menopause_time, menopause) ~ 1, data = data,
                    dist = "exp")  # S(t)=e^{-rate*t} 
KM1=survfit(Surv(menopause_time, menopause)~1, data = data, conf.type='log')
exp2 <- flexsurvreg(Surv(intake_age,menopause_age, menopause) ~ 1, data = data,dist = "exp")  # S(t)=e^{-rate*t} 
KM2=survfit(Surv(intake_age, menopause_age, menopause)~1, data = data, conf.type='log')
### shift----
shift = min(data$intake_age)
# shift = min(data$menopause_age)
data_shift = data %>% 
  mutate(menopause_age_shift = menopause_age-shift)
exp2_shift <- flexsurvreg(Surv(menopause_age_shift, menopause) ~ 1,
                          data = data_shift,
                          dist = "exp") 

weib2_shift <- flexsurvreg(Surv(menopause_age_shift, menopause) ~ 1,
                          data = data_shift,
                          dist = "weibull") 

## 2*2 ----

par(mfrow = c(2,2), mar = rep(2,4))
plot(exp1, xlab = "menopause time", ylab = "survival probability",
     main = "Exponential survival curve of menapause time")
abline(h=0.5, col = "blue")
plot(KM1, conf.int = T, mark.time = TRUE,xlab="menopause time", ylab="survival probability", main="K-M curve of menapause time")
abline(h=0.5, col = "blue")
plot(exp2, xlab = "menopause age", ylab = "survival probability",
     main = "Exponential survival curve of menapause age")
abline(h=0.5, col = "blue")
plot(KM2, conf.int = T, mark.time = TRUE,xlab="menopause age", ylab="survival probability", main="K-M curve of menapause age")
abline(h=0.5, col = "blue")

## 1*3 ----
par(mfrow = c(3,1), mar = rep(2,4))
plot(exp2, xlab = "menopause age shift", ylab = "survival probability",main = "Exponential survival curve of menapause age")
plot(exp2_shift, xlab = "menopause age shift", ylab = "survival probability",main = "Exponential survival curve of shifted menapause age")
plot(weib2_shift, xlab = "menopause age shift", ylab = "survival probability",main = "Weilbull survival curve of shifted menapause age")

# median menopause table ----
exp1_int_sum = summary(exp1, type = "quantile", quantiles = 0.5)
print(KM1)
exp1_int = round(c(exp1_int_sum[[1]][[2]]+shift,
                   exp1_int_sum[[1]][[3]]+shift,
                   exp1_int_sum[[1]][[4]]+shift),2)
km1_int = c(NA,NA,NA)
# median menopause age 
exp2_int_sum = summary(exp2, type = "quantile", quantiles = 0.5)
exp2_shift_int_sum = summary(exp2_shift, type = "quantile", quantiles = 0.5)
weib2_shift_int_sum = summary(weib2_shift, type = "quantile", quantiles = 0.5)

print(KM2)
km2_int = c(55.7, 55, 56.6)
exp2_int = round(c(exp2_int_sum[[1]][[2]]+shift,
                   exp2_int_sum[[1]][[3]]+shift,
                   exp2_int_sum[[1]][[4]]+shift),2)
exp2_shift_int = round(c(exp2_shift_int_sum[[1]][[2]]+shift,
                         exp2_shift_int_sum[[1]][[3]]+shift,
                         exp2_shift_int_sum[[1]][[4]]+shift),2)
weib2_shift_int = round(c(weib2_shift_int_sum[[1]][[2]]+shift,
                          weib2_shift_int_sum[[1]][[3]]+shift,
                          weib2_shift_int_sum[[1]][[4]]+shift),2)
## tib 
median_surv_tib = tibble(starting_time = c("menopause_time", "menopause_time", 
                         "menopause_age", "menopause_age","menopause_age", "menopause_age"),
       type = c("exponential", "KM", "exponential", "KM", "exponential_shift", "Weilbull_shift"),
       median_survival = c(list(exp1_int),list(km1_int),
                           list(exp2_int),list(km2_int),
                           list(exp2_shift_int),list(weib2_shift_int))
       ) %>% 
  mutate(median_survival = map(median_survival, function(x) {
    tibble(measure = c("median", "95%L", "95%U"),
           value = x) %>% 
      pivot_wider(names_from = measure,
                  values_from = value)
      
  })) %>% 
  unnest(median_survival)

xtable(median_surv_tib, caption = "Median survival time table")

## KM survival table
sum_km1 = summary(KM1)
sum_km2 = summary(KM2)

# dev.off()
png(filename = "pres_vis/number_risk.png")
par(mfrow = c(2,1), mar = rep(4,4))
plot(sum_km1$time, sum_km1$n.risk,
     main = "Number of risk as a function of menopause time", 
     ylab = "Number as risk",
     xlab = "Menopause time")
plot(sum_km2$time, sum_km2$n.risk,
     main = "Number of risk as a function of menopause age", 
     ylab = "Number as risk",
     xlab = "Menopause age")
dev.off()

km1_tib = as_tibble(unclass(sum_km1)[c(1:8, 14,15)])
km2_tib = as_tibble(unclass(sum_km2)[c(1:8, 14,15)])

xtable(km1_tib, caption = "KM table using menopause time")
xtable(km2_tib, caption = "KM table using menopause age")

