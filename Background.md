# Background



# Methods and analysis

* For survival age onset as event, there are 2 starting points can be considered: first is the follow up time or time on study; Second is the time at birth;[2]

  * If we take the recruitment time as the starting point:

    * The intake age is considered as a baseline confounder and is added in the model. 

    * The hazared ratio model

    * **Model 1**

    * $$
      \begin{aligned}
      t_i &= x_i-v_i\\
      h_i(t) &= h_0(t)exp(\eta_i)\\
      \eta_i&=\beta_1I(race=black)_i\\
      &+\beta_2I(race=others)_i\\
      &+\beta_3I(education=college graduate)_i\\
      &+\beta_4I(education=some college)_i\\
      &+\beta_5I(education=high school or less)_i\\
      &+\beta_6v_i
      \end{aligned}
      $$

  * If we take the age at birth at the starting time

    * Because an individual must have no menopause to a sufficient age $V_i$ to enter the retirement community, and all individuals who menopaused prior to the recruitment time were not included in this study, the menopause age considered in this study are left-truncated.[1]
    * For the left truncated data, the hazard ratio is estimated conditionally:

    $$
    h(t|Z_i,X_i>V_i) = \frac{P(X_i=t|Z_i,X_i>V_i)}{P(X_i\geq t|Z_i,X_i>V_i)}
    $$

    * Where X is the event time, V is the left truncation time, so all the event is happened conditionally

    * Quasi-independent: If the event age X and the entry age V are conditionally independent, given the covariates Z, then a simple calculation shows that the conditional hazard $h(t | Z, X > V)$ and the unconditional hazard rate, $h(t |Z)$ are equivalent (Andersen, et al., 1993)[1]. By assuming queasi-independence, there is no further need to adjust the intake age in the covariates of coxPH.

      * But might be violated from our data since the intake_time is correlated with the event time?

    * The hazard ratio model

    * **Model 2** (only race)
      $$
      \begin{aligned}
      h_i(x| v_i) &= h_0(x|v_i)exp(\eta_i)\\
      \eta_i&=\beta_1I(race=black)_i\\
      &+\beta_2I(race=others)_i
      \end{aligned}
      $$
      
    
    * **Model 3**
    
    * $$
      \begin{aligned}
      h_i(x| v_i) &= h_0(x|v_i)exp(\eta_i)\\
      \eta_i&=\beta_1I(race=black)_i\\
      &+\beta_2I(race=others)_i\\
      &+\beta_3I(education=college graduate)_i\\
      &+\beta_4I(education=some college)_i\\
      &+\beta_5I(education=high school or less)_i
      \end{aligned}
      $$

  



# Results

## Check the assumption of PH model

* The PH assumption requires the hazard ratio is constant over time,  consequently, the relative risk between subject should also be constant if there is no time-dependent covariants included.
* We first check the log-log KM curve using 2 starting points. As in Fig ??, the stratified log-log KM curve of both variables are slightly non-paralleled in both time measurements, indicating a potential violation of the cox pH assumption [3].  To further test the assumption objectively, a score test of for addition of the time-dependent term is contected for each variable and also globally. From Table ??,  all the time-dependent terms are insignificant, even though there is local crossing of the stratified KM curves. Thus we conclude that there is no evidence to reject the PH assumption for cox model 1 and model 3. We also plot the time-dependent coeffitients against time for all the variables, and the horizontal and flat lines further verifies the validation of PH assumption in models.

## Survival curve comparison using different starting point

* combine exp and KM for both starting point Fig??
* 3  plots for parametric fitting for menopause age
  * exp no truncation
  * exp trunc
  * weib truc
* Compare exp trunc and KM median survival
* combine km table for both starting point, tab??



## Race effect on menopause age

The first model only takes race as covariates to evaluate the main effect without considering confounding effect. From the **model1** output, we can see that there is significant difference on menopause age between black and white, but no significant difference between white and others; To further compare the difference between black and others, a new **model1'** is fitted with others as reference and No significance between race others and black is detected.

To further check the confounder effect of education to the relationship between race and menopause age, **model2** is fitted to get the conditional effect of race given education level.  Even though from the anova using loglikelihood ratio test didn't show siginificant advantage of model 2, the relative risk ratio of menapause age of black v.s. white becomes larger after adjusting the educational level (2.50(1.30, 4.82) after adjusting, 2.14(1.16, 3.95) without adjusting). We conclude that education level is a potential confounder of the relationship between race and menopause age. Controling for education level, the relative risk of menopause age is 2.64(0.98,7.09) for a plack patient v.s. other ethnicity patient， and an estimate of the baseline survival function for White non-Hispanic patients with Post- graduate education is in Fig ??.

Only show race ggsurv plot(g2) ?

# Conclusion and discussion



# Resource

[1] Klein and Moeschberger, *Survival Analysis*. Secion 9.4

[2] Kim et al., “Cox Proportional Hazards Models with Left Truncation and Time-Varying Coefficient.”

[3] Kleinbaum and Klein, *Survival Analysis*.

## Check list

- [ ] menopause time
  - [x] exp survival mdian suvival time, interprete and CI
  - [x] KM survival mdian suvival time, interprete and CI
  - [x] Fit PH
    - [x] interprete
    - [x] check assumption
- [x] menopause age
  - [x] exp survival mdian suvival time, interprete and CI
  - [x] KM survival mdian suvival time, interprete and CI
  - [x] Fit PH
    - [x] race only for whether 3 race groups are equivalent
    - [x] race+education
      - [x] test model selection, whether education is useful by LR 
      - [x] race significance after adjusting education
      - [ ] point and 95% CI, interpretation of RR of Black v.s. other
      - [ ] s0 estimates for white and post
      - [ ] check PH assumption

### Table appendix





## Pres

## Compare between time on study and time at age

### survival curve

* KM 
* exp
* median survival time

2*2 

* for time at age
  * exp
  * truncated exp (time at age)
  * truncated weibull (time at age)

* median survival time
* survival table

### PH model

* tab1: coefficient table with race+education but time at age has no intake_age; 
* tab2: race v.s. race+education ?(95% CI)
* tab3: anova table for  tab2 for confounder effect
  * ref as others

### Baseline survival 

### PH assumption plot



# Appendix



## Plots

meno_age_curv.png

surve_curve2*2.png

rank_surv.png

baseline.png

## Table

### Median survival table

```latex
\begin{table}[ht]
\centering
\begin{tabular}{rllrrr}
  \hline
 & starting\_time & type & median & 95\%L & 95\%U \\ 
  \hline
1 & menopause\_time & exponential & 56.59 & 54.15 & 59.49 \\ 
  2 & menopause\_time & KM &  &  &  \\ 
  3 & menopause\_age & exponential & 224.64 & 188.98 & 272.48 \\ 
  4 & menopause\_age & KM & 55.70 & 55.00 & 56.60 \\ 
  5 & menopause\_age & exponential\_shift & 68.75 & 63.64 & 74.28 \\ 
  6 & menopause\_age & Weilbull\_shift & 55.93 & 55.07 & 56.89 \\ 
   \hline
\end{tabular}
\caption{Median survival time table} 
\end{table}
```



### Model1

(fit2)

```latex

\begin{table}
\begin{center}
\begin{tabular}{l c}
\hline
 & Model 1 \\
\hline
raceblack   & $0.76^{*}$ \\
            & $(0.31)$   \\
raceother   & $-0.10$    \\
            & $(0.43)$   \\
\hline
AIC         & $664.45$   \\
R$^2$       & $0.01$     \\
Max. R$^2$  & $0.83$     \\
Num. events & $75$       \\
Num. obs.   & $380$      \\
Missings    & $0$        \\
PH test     & $0.50$     \\
\hline
\multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
\end{tabular}
\caption{Model1: menopause age over race}
\label{table:coefficients}
\end{center}
\end{table}
```

### Model1'

(fit2_other)

```latex
\begin{table}
\begin{center}
\begin{tabular}{l c}
\hline
 & Model 1 \\
\hline
raceblack   & $0.87$   \\
            & $(0.50)$ \\
racewhite   & $0.10$   \\
            & $(0.43)$ \\
\hline
AIC         & $664.45$ \\
R$^2$       & $0.01$   \\
Max. R$^2$  & $0.83$   \\
Num. events & $75$     \\
Num. obs.   & $380$    \\
Missings    & $0$      \\
PH test     & $0.50$   \\
\hline
\multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
\end{tabular}
\caption{Model1': menopause age over race}
\label{table:coefficients}
\end{center}
\end{table}
```

### Model2

(fit3)

```latex
\begin{table}
\begin{center}
\begin{tabular}{l c}
\hline
 & Model 1 \\
\hline
raceblack                    & $0.92^{**}$ \\
                             & $(0.33)$    \\
raceother                    & $-0.05$     \\
                             & $(0.43)$    \\
educationcollege graduate    & $-0.66^{*}$ \\
                             & $(0.32)$    \\
educationsome college        & $0.00$      \\
                             & $(0.31)$    \\
educationhigh school or less & $-0.66$     \\
                             & $(0.41)$    \\
\hline
AIC                          & $663.86$    \\
R$^2$                        & $0.03$      \\
Max. R$^2$                   & $0.83$      \\
Num. events                  & $75$        \\
Num. obs.                    & $380$       \\
Missings                     & $0$         \\
PH test                      & $0.67$      \\
\hline
\multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
\end{tabular}
\caption{Model2 menopause age over race adjusting education}
\label{table:coefficients}
\end{center}
\end{table}
```

### Model2'

(fit3_other)

```latex
\begin{table}
\begin{center}
\begin{tabular}{l c}
\hline
 & Model 1 \\
\hline
raceblack                    & $0.97$      \\
                             & $(0.50)$    \\
racewhite                    & $0.05$      \\
                             & $(0.43)$    \\
educationcollege graduate    & $-0.66^{*}$ \\
                             & $(0.32)$    \\
educationsome college        & $0.00$      \\
                             & $(0.31)$    \\
educationhigh school or less & $-0.66$     \\
                             & $(0.41)$    \\
\hline
AIC                          & $663.86$    \\
R$^2$                        & $0.03$      \\
Max. R$^2$                   & $0.83$      \\
Num. events                  & $75$        \\
Num. obs.                    & $380$       \\
Missings                     & $0$         \\
PH test                      & $0.67$      \\
\hline
\multicolumn{2}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
\end{tabular}
\caption{Statistical models}
\label{table:coefficients}
\end{center}
\end{table}

```

### Compare model 12



```latex
\begin{table}[ht]
\centering
\begin{tabular}{rrrrr}
  \hline
 & logLik & statistic & df & p.value \\ 
  \hline
1 & -330.22 &  &  &  \\ 
  2 & -326.93 & 6.59 &   3 & 0.09 \\ 
   \hline
\end{tabular}
\caption{Anova for Model 1:age over race, Model 2: age over race + education} 
\end{table}
```

### Compare model12 fitting

```latex
\begin{table}[ht]
\centering
\begin{tabular}{rrrrrr}
  \hline
 & coef & exp(coef) & se(coef) & z & Pr($>$$|$z$|$) \\ 
  \hline
raceblack & 0.76 & 2.15 & 0.31 & 2.45 & 0.01 \\ 
  raceother & -0.10 & 0.90 & 0.43 & -0.24 & 0.81 \\ 
   \hline
\end{tabular}
\caption{Model1 fitting} 
\end{table}
```



```latex
\begin{table}[ht]
\centering
\begin{tabular}{rrrrrr}
  \hline
 & coef & exp(coef) & se(coef) & z & Pr($>$$|$z$|$) \\ 
  \hline
raceblack & 0.92 & 2.50 & 0.33 & 2.74 & 0.01 \\ 
  raceother & -0.05 & 0.95 & 0.43 & -0.12 & 0.90 \\ 
  educationcollege graduate & -0.66 & 0.52 & 0.32 & -2.05 & 0.04 \\ 
  educationsome college & 0.00 & 1.00 & 0.31 & 0.01 & 0.99 \\ 
  educationhigh school or less & -0.66 & 0.52 & 0.41 & -1.62 & 0.10 \\ 
   \hline
\end{tabular}
\caption{Model2 fitting} 
\end{table}
```



### Schoenfeld table for models

model 1 (cz1)

```latex
\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & chisq & df & p \\ 
  \hline
race & 0.49 & 2.00 & 0.78 \\ 
  education & 1.37 & 3.00 & 0.71 \\ 
  intake\_age & 0.45 & 1.00 & 0.50 \\ 
  GLOBAL & 1.98 & 6.00 & 0.92 \\ 
   \hline
\end{tabular}
\caption{Assumption check for Model1} 
\end{table}
```



```latex
\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & chisq & df & p \\ 
  \hline
race & 1.87 & 2.00 & 0.39 \\ 
  education & 0.92 & 3.00 & 0.82 \\ 
  GLOBAL & 3.17 & 5.00 & 0.67 \\ 
   \hline
\end{tabular}
\caption{Assumption check for Model3} 
\end{table}
```

