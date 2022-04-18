rm(list=ls())
library(dplyr)
library(tidyr)
library(bannerCommenter)
library(survival)
library(ggplot2)
library(survminer)
library(gtsummary)


dat = read.delim("~/Desktop/DrPH Courseworks/2022 Spring/SPRIS/Assignment 3/Menopause.dat", 
           header=F)
names(dat) = c("id", "intake_age", "menopause_age", "menopause", "race", "education")
dat = dat %>%
  mutate(
    race = case_when(
      race == 0 ~ "White",
      race == 1 ~ "Black",
      race == 2 ~ "Other"
    ),
    education = case_when(
      education == 0 ~ "Post-graduate",
      education == 1 ~ "College Graduate",
      education == 2 ~ "Some College",
      education == 3 ~ "High School or Less"
    ),
    education = factor(education,
                        levels=c("Post-graduate",
                          "College Graduate",
                          "Some College",
                          "High School or Less")),
    menopause_time = menopause_age - intake_age
  )

# banner("Descriptive")
#################################################################
##                         Descriptive                         ##
#################################################################
dat %>% 
  select(-c(id)) %>% 
  mutate(
    menopause = case_when(
    menopause == 0 ~ "No",
    menopause == 1 ~  "Yes"))%>%
  select(race, education, intake_age, menopause, menopause_age, menopause_time)%>%
  tbl_summary(by = menopause,
              digits = all_continuous() ~ 1,
              label = list(
                race ~ "Race",
                education ~ "Education",
                intake_age ~ "Intake Age",
                menopause_age ~ "Menopause Age",
                menopause_time ~ "Time on Study"
              )) %>%
  bold_labels()%>%
  add_overall(last=TRUE)%>%
  #add_p()%>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Menopause**")%>%
  as_gt() 

# median age at entry was 47 among those who censored (50 in event)
# median follow up time is 4.4 for censored (1.9 for even)

dat %>% 
  select(-c(id)) %>% 
  mutate(
    menopause = case_when(
      menopause == 0 ~ "No",
      menopause == 1 ~  "Yes"))%>%
  tbl_strata(
    strata = race,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = menopause),
    .header = "**{strata}**"
  )%>%
  bold_labels()

# banner("(A)")
#################################################################
##                             (A)                             ##
#################################################################
# using different time scale --> different interpretation
# since it is observational study, unlike others that an interventsion is carried out
# in this study, the interest is in the study of menopause process

## 1a. assuming exponential (constant hazard), estimate median with 95% CI and interpret

# https://stats.stackexchange.com/questions/159044/weibull-survival-model-in-r
# survreg's scale = 1/(rweibull shape), exponential shape = 1
# survreg's intercept = log(rweibull scale)

mod1 = survreg(Surv(menopause_time, menopause) ~ 1, data =  dat, 
               dist = "exponential")
mod1
confint(mod1)
summary(mod1)
summary(mod1)$coef
summary(mod1)$scale
exp(-summary(mod1)$coef) # lambda
# plot to check eponential assumption log(-cumulative hazard) vs log(time)
# rweibull(10000, 1, 1/exp(summary(mod1)$coef))%>%hist
# rexp(10000, rate = exp(summary(mod1)$coef))%>%hist
# by day 12, half of the participants still have not experienced menopause since enrollment
log(2)/exp(-summary(mod1)$coef)
ciTools::add_quantile(dat, mod1, p=0.5, alpha = 0.05, names = NULL, 
                yhatName = "median_pred")

# log -cumulative hazard
# https://www.itl.nist.gov/div898/handbook/apr/section2/apr222.htm#:~:text=Similar%20to%20probability%20plots%2C%20cumulative,of%20the%20i%2Dth%20failure.
temp = dat[c("menopause_time", "menopause")]
temp = temp[order(temp$menopause_time),]
temp$rank = 1:dim(temp)[1]
temp$reverse_rank = dim(temp)[1]+1-temp$rank
temp$hazard = NA
temp$hazard[which(temp$menopause==1)]=1/temp$reverse_rank[which(temp$menopause==1)]
temp$cumhaz = cumsum(ifelse(is.na(temp$hazard), 0, temp$hazard)) + temp$hazard*0
ggplot(temp[!is.na(temp$cumhaz),], aes(x=log(menopause_time), 
                 y=log(cumhaz)))+
  geom_point()+
  geom_line(color="red")+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("log of time")+
  ylab("log(H(t))")+
  theme_bw()+
  ggtitle("Log Cumulative Hazard plot")



## 1b. show table of estimate and graph using KM, estimate median and compare with 1a
mod2 = survfit(Surv(menopause_time, menopause) ~ 1, data =  dat)
mod2 # More than 50% ppl still havae not experienced the event by the last event/censor
ptime2 = summary(mod2)
ggsurvplot(mod2,
           risk.table = T,
           surv.median.line = "none")

# https://www.usu.edu/math/jrstevens/biostat/projects2013/pres_LeftTruncation.pdf
## 2. manopause_age as outcome, PH model, quasi-independent
mod3 = coxph(Surv(menopause_time, menopause) ~ race + education + intake_age,
             data = dat)
summary(mod3)
cox.zph(mod3)
basehaz(mod3)
plot(cox.zph(mod3), var="intake_age")
ggcoxdiagnostics(mod3, type = "martingale")
ggcoxdiagnostics(mod3, type = "deviance")
ggcoxdiagnostics(mod3, type = "schoenfeld")

# banner("(B)")
#################################################################
##                             (B)                             ##
#################################################################

## 3. show table of estimate and graph using KM, median, compare with 2
mod4 = survfit(Surv(intake_age, menopause_age, menopause) ~ 1, data = dat)
mod4
summary(mod4)
ggsurvplot(mod4,
           risk.table = T,
           surv.median.line = "hv", 
           ggtheme =theme_bw())

par(mfrow=c(1,2))
plot(mod2$time, mod2$n.risk,
     main = "Right Censored",
     ylab = "Number at risk",
     xlab = "Menopause time (time on study)")
plot(mod4$time, mod4$n.risk,
     main = "Left Truncated Right Censored",
     ylab = "Number at risk",
     xlab = "Menopause age")
png(file="/Users/wf2213/Desktop/saving_plot2.png",
    width=600, height=350)



## 4. test if race is related to manopause_age and conclude
mod5 = survfit(Surv(menopause_time, menopause) ~ race, data = dat)
ggsurvplot(mod5,
           risk.table = T,
           surv.median.line = "hv", 
           ggtheme =theme_bw())


mod5 = survfit(Surv(menopause_age, menopause) ~ race, data = dat)
ggsurvplot(mod5,
           risk.table = F,
           conf.int = T,
           surv.median.line = "hv", 
           ggtheme =theme_bw())

# crossing of curves
surv_diff <- survdiff(Surv(menopause_age, menopause) ~ race, data = dat)
surv_diff
# cox model
mod6 = coxph(Surv(intake_age, menopause_age, menopause) ~ race+education, data = dat)
summary(mod6)
cox.zph(mod6)
plot(cox.zph(mod6,transform=rank),se=F,var="race")
ggcoxdiagnostics(mod6, type = "martingale")
ggcoxdiagnostics(mod6, type = "deviance")
ggcoxdiagnostics(mod6, type = "schoenfeld")


## 5a. test if race significant predictor for manopause_age after education
dat$race = relevel(as.factor(dat$race), ref="Other")
mod7 = coxph(Surv(intake_age, menopause_age, menopause) ~ race + education, data = dat)
plot(cox.zph(mod7,transform=rank),se=F,var="race")

## 5b. provide point estimate with 95% CI
summary(mod7)
## 5b. estimate bsaseline survival function
base_surv = basehaz(mod7) %>%
  mutate(
   bsl_hz = hazard*summary(mod7)$coef[2,2]*summary(mod7)$coef[5,2],
   bsl_surv = exp(-bsl_hz)
  )
## 5d. check PH assumption
cox.zph(mod7)
resid(mod7, type="martingale")%>%plot
resid(mod7, type="deviance")%>%plot



dat = dat %>%
  mutate(
    intake_age1 = intake_age - min(intake_age),
    menopause_age1 = menopause_age - min(intake_age)
  )
mod7_1 = coxph(Surv(intake_age1, menopause_age1, menopause) ~ 
                 race + education, data = dat)
plot(cox.zph(mod7,transform=rank),se=F,var="race")


texreg::texreg(list(mod3, mod7, mod7_1))


# othre as ref
# linearity of 
# http://dwoll.de/rexrepos/posts/survivalCoxPH.html

banner("EDA plots")
#################################################################
##                          EDA plots                          ##
#################################################################
dat$menopause_age_cat = quantcut(dat$menopause_age, q=4)
dat$intake_age_cat = quantcut(dat$intake_age, q=4)

p1=ggplot(dat, aes(x=menopause_time, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Time On Study")+
  facet_grid(race~education)
p2=ggplot(dat, aes(x=menopause_age, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Menopause Age")+
  facet_grid(race~education)
ggsave("/Users/wf2213/Desktop/race_edu_age.png", height = 6, width = 8)
p3=ggplot(dat, aes(x=intake_age, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Intake Age")+
  facet_wrap(intake_age_cat~.)
p = ggarrange(p1,p2,p3,common.legend = T, labels="AUTO", 
              legend = "top")
#ggsave("/Users/wf2213/Desktop/age_dist.png", 
#       height = 6, width = 8)

# shifted model
exp2 <- flexsurvreg(Surv(menopause_age, menopause) ~ 1, data = dat,dist = "exp")  # S(t)=e^{-rate*t} 
plot(exp2, xlab = "menopause age", ylab = "survival probability",
     main = "KM and exponential estimates of survival curve")
# median menopause time ----
## summary(exp1) #get the estimate and ci tab
summary(exp2, type = "quantile", quantiles = 0.5) # Not realistic
# shift model----
shift = min(dat$intake_age)
# shift = min(data$menopause_age)
data_shift = dat %>% 
  mutate(menopause_age_shift = menopause_age-shift)
exp2_shift <- flexsurvreg(Surv(menopause_age_shift, menopause) ~ 1,
                          data = data_shift,
                          dist = "weibull")  # S(t)=e^{-rate*t} 
plot(exp2_shift, xlab = "menopause age shift", ylab = "survival probability",main = "KM and exponential estimates of survival curve")
# median menopause time ----
## summary(exp1) #get the estimate and ci tab
tmp = summary(exp2_shift, type = "quantile", quantiles = 0.5)
c(tmp[[1]][[2]]+shift,
  tmp[[1]][[3]]+shift,
  tmp[[1]][[4]]+shift)

# exp
exp3_shift <- flexsurvreg(Surv(menopause_age_shift, menopause) ~ 1,
                          data = data_shift,
                          dist = "exponential")  # S(t)=e^{-rate*t} 
plot(exp3_shift, xlab = "menopause age shift", ylab = "survival probability",main = "KM and exponential estimates of survival curve")
# median menopause time ----
## summary(exp1) #get the estimate and ci tab
tmp = summary(exp3_shift, type = "quantile", quantiles = 0.5)
c(tmp[[1]][[2]]+shift,
  tmp[[1]][[3]]+shift,
  tmp[[1]][[4]]+shift)






ggplot(dat, aes(x=menopause_age, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Menopause Age")+
  facet_grid(race~education)


ggplot(dat, aes(x=menopause_time, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Time on Study")+
  facet_grid(race~education)

ggplot(dat, aes(x=intake_age, fill=as.factor(menopause))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(name = "Status",
                    breaks = c(0,1),
                    labels = c("Cencorsed", "Menopause"),
                    values=c("#69b3a2", "#404080"))+
  xlab("Time on Study")+
  facet_grid(race~education)
