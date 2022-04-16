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
## fit1: meno_time ----
fit1 = coxph(Surv(menopause_time, menopause)~race+education+intake_age,
             data=data)

## fit2: meno_age ~ race ----
fit2=coxph(Surv(time = intake_age, 
                time2 = menopause_age, 
                event = menopause)~race,
           data=data)


## fit3: meno_age ~ race + education ----

fit3=coxph(Surv(time = intake_age, 
                time2 = menopause_age, 
                event = menopause)~race+education,
           data=data)


# model tidy -- others as reference ----
texreg::texreg(list(fit1,fit2,fit3))

# result ----
## Test race ----
## Test whether race is siginificant
anova(fit2)
## Test whether race siginificant given education
anova(fit3)
## compare model
comp23 = anova(fit2, fit3)
xtable::xtable(broom::tidy(comp23), caption = "Anova for Model1: Model 1: ~ race, Model 2: ~ race + education")

## baseline ----
S0=survfit(fit3,newdata=data.frame(race="white", education = "post graduate")) 
# dev.off()
png(filename = "pres_vis/baseline.png")
plot(S0$time,S0$surv,xlab='time',ylim=c(0,1),ylab='Estimated Survival Rate',type='s',lty=1)
dev.off()

## PH assumption ----
cz1 <- cox.zph(fit1)
xtable(cz1$table, caption = "Assumption check for Model1")
dev.off()
png(filename = "pres_vis/fit1_diag.png")
par(mfrow = c(3,1))
plot(cz1)
dev.off()

cz3 <- cox.zph(fit3)
print(cz3)

xtable(cz3$table, caption = "Assumption check for Model3")
# dev.off()
png(filename = "pres_vis/fit3_diag.png")
par(mfrow = c(3,1))
plot(cz3)
dev.off()


## plot models ----
g2 = ggsurvplot(survfit(Surv(time = intake_age, 
                             time2 = menopause_age, 
                             event = menopause)~race,
                        data=data), data=data, conf.int = T)
ggsave(filename = "pres_vis/rank_surv.png")
g3 = ggsurvplot(survfit(Surv(time = intake_age, 
                             time2 = menopause_age, 
                             event = menopause)~race+education,
                        data=data), data=data, conf.int = T)






