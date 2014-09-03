<!--pandoc
t: html
default-image-extension: png
template: templates/html.template
toc-depth: 4
toc:

t: latex
s:
S:
latex-engine: xelatex
template: templates/latex.template
default-image-extension: pdf
toc-depth: 4
toc:
-->



# Competition and Selection

## Accounting for behavioral measurement uncertainty


```r
library(MASS) # MASS clashes with dplyr... so always load first
library(pander) # pander clashes with dplyr... so always load first
library(ggplot2)
library(grid)
library(dplyr)
set.alignment('right', row.names = 'left')
load("data/analyses_data/sel_grads_mcmc.RData")
fitness <- read.table(file = "data/fitness+competition.csv", sep = ',',
  header = TRUE, stringsAsFactors = FALSE)
load("data/analyses_data/fit_raneff_data.RData")
```

We will examine the effect of competition on selection in two general steps.

  1. Is there an interaction between competition and behavior on fitness?
  2. Are there nonlinear effects of behavior on fitness?


### Correlations between selection gradients and competition
First a plot of the relationship between selection gradients and competition. The two study areas were pooled to calculate selection gradients for each year. Therefore we need to calculate competition for the combined study areas. Competition is the number of offspring produced during the year divided by the number of offspring that survived to spring (i.e. recruited into the population).


```r
competition_year <- fitness %>%
  filter(grid_year != "SU2008") %>%
  select(Year, competition) %>%
  unique() %>%
  group_by(Year, add = FALSE) %>%
  summarise(mean_competition = mean(competition))
  
n_year <- filter(fit_raneff_data, type == "blup") %>%
  group_by(Year, add = FALSE) %>% summarise(n = n())
competition_year <- left_join(competition_year, n_year, by = "Year")
competition_year$Year <- as.character(competition_year$Year)

load("data/analyses_data/sel_grads_mcmc.RData")
sel_grads_mcmc_comp <- left_join(
  filter(sel_grads_mcmc, standardization == "SD"), competition_year,
  by = "Year")
save(sel_grads_mcmc_comp, competition_year,
  file = "data/analyses_data/sel_grads_mcmc_comp.RData")
```


```r
load("data/analyses_data/sel_grads_mcmc_comp.RData")

cor_sgrad_comp <- function(x){
  v <- x$variable[1]
  ct <- cor.test(x$post_mode, x$mean_competition)
  data.frame(variable = v, est = ct$estimate, lower = ct$conf.int[1],
    upper = ct$conf.int[2], stringsAsFactors = FALSE)
}

mcmc_cor <- sel_grads_mcmc_comp %>%
  group_by(variable, add = FALSE) %>%
  do(cor_sgrad_comp(.))
mcmc_cor[ ,2:4] <- round(mcmc_cor[ ,2:4], digits = 2)
mcmc_cor$print <- paste(mcmc_cor$est,
  " (", mcmc_cor$lower, ", ", mcmc_cor$upper, ")", sep = "")
```
#### Aggression and Competition


```r
p <- ggplot(data = filter(sel_grads_mcmc_comp, variable == "Aggression"),
  aes(x = mean_competition, y = post_mode))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Aggression")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_text(size = 10),
  panel.border = element_rect(linetype = "solid", colour = "black"),
  plot.title = element_text(size = 10))
p <- p + geom_text(data = filter(mcmc_cor, variable == "Aggression"),
  aes(x = 6.5, y = 1.1, label = paste("Correlation = ", print, sep = '')),
  size = 2.5)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/05_agg_comp_mcmc) 

#### Activity and Competition

```r
p <- ggplot(data = filter(sel_grads_mcmc_comp, variable == "Activity"),
  aes(x = mean_competition, y = post_mode))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Activity")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_text(size = 10),
  panel.border = element_rect(linetype = "solid", colour = "black"),
  plot.title = element_text(size = 10))
p <- p + ylim(c(-1, 1.5))
p <- p + geom_text(data = filter(mcmc_cor, variable == "Activity"),
  aes(x = 6.1, y = 1.5, label = paste("Correlation = ", print, sep = '')),
  size = 2.5)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/05_act_comp_mcmc) 

#### Docility and Competition

```r
p <- ggplot(data = filter(sel_grads_mcmc_comp, variable == "Docility"),
  aes(x = mean_competition, y = post_mode))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Docility")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_text(size = 10),
  panel.border = element_rect(linetype = "solid", colour = "black"),
  plot.title = element_text(size = 10))
p <- p + geom_text(data = filter(mcmc_cor, variable == "Docility"),
  aes(x = 6.1, y = 0.65, label = paste("Correlation = ", print, sep = '')),
  size = 2.5)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/05_doc_comp_mcmc) 

### Effect of competition on linear selection (glmms)

```r
load("data/analyses_data/fit_raneff_data.RData")
library(dplyr)
fit_raneff_data <- tbl_df(fit_raneff_data)
library(lme4)

# Model with interactions between competition and the behavioral traits. 
# grid_year and ID are random effects.
arsLinearCompetition <- function(dat){
  ars_linear_comp <- glmer(
  	ars_all ~ 
  	competition_s + 
  	aggression_s + 
  	competition_s:aggression_s + 
  	activity_s + 
  	competition_s:activity_s + 
  	docility_s +
  	competition_s:docility_s +
    (1|Grid) + (1|ID),
  	data = dat, family = poisson, control=glmerControl(optimizer="bobyqa")
  	)
  random_effect_variances <- VarCorr(ars_linear_comp)
  data.frame(t(summary(ars_linear_comp)$coefficients[ ,"Estimate"]),
  ID = random_effect_variances$ID[1], Grid = random_effect_variances$Grid[1])
}

library(foreach)
```

```
## foreach: simple, scalable parallel programming from Revolution Analytics
## Use Revolution R for scalability, fault tolerance and more.
## http://www.revolutionanalytics.com
```

```r
library(doMC)
```

```
## Loading required package: iterators
## Loading required package: parallel
```

```r
ncores = 12
registerDoMC(cores = ncores)

batches <- data.frame(start = seq(1, 1000, round(1000/ncores))[1:ncores])
batches$stop <- c(batches$start[2:length(batches$start)] - 1, 1000)

start_time <- Sys.time()
ars_linear_comp_posterior <- foreach(i = 1:ncores, .combine = rbind) %dopar% {
  results <- fit_raneff_data %>%
    filter(type == "raneff", itt %in% batches$start[i]:batches$stop[i]) %>%
    group_by(itt, add = FALSE) %>%
    do(arsLinearCompetition(.))
}
run_time <- Sys.time() - start_time
print(run_time)
```

```
## Time difference of 2.539 mins
```

```r
save(ars_linear_comp_posterior,
  file = "data/analyses_data/ars_linear_comp_posterior.RData")
```


```r
load("data/analyses_data/ars_linear_comp_posterior.RData")
library(MCMCglmm)
library(lme4)
library(data.table)
```

```
## data.table 1.9.2  For help type: help("data.table")
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```

```r
getPosteriorParams <- function(x){
  require(MCMCglmm)
  dat_mcmc <- mcmc(x)
  pm <- posterior.mode(dat_mcmc)
  hpd <- HPDinterval(dat_mcmc, prob = 0.9)
  pm_table <-  format(round(pm , digits = 2), digits = 1, nsmall = 2,
    scientific = FALSE)
  hpd_table <- format(round(hpd, digits = 2), digits = 1, nsmall = 2,
    scientific = FALSE)
  pm_hpd_table <- data.frame(cbind(pm_table, hpd_table))
  pm_hpd_table$pm_hpd <- paste(
    pm_table, " (", hpd_table[ ,1], ", ", hpd_table[ ,2], ")", sep = ''
    )
  pm_hpd_table$sig[sign(hpd[ ,1]) == sign(hpd[ ,2])] <- "*"
  pm_hpd_table$sig[sign(hpd[ ,1]) != sign(hpd[ ,2])] <- " "
  pm_hpd_table$space[sign(hpd[ ,1]) == sign(hpd[ ,2])] <- "*"
  pm_hpd_table$space[sign(hpd[ ,1]) != sign(hpd[ ,2])] <- "&nbsp;"
  pm_hpd_table$pm_hpd <- paste(pm_hpd_table$pm_hpd, pm_hpd_table$sig, sep = "")
  return(pm_hpd_table)
}

linear_hpd_ars <- getPosteriorParams(ars_linear_comp_posterior %>%
  ungroup() %>%
  select(Intercept = X.Intercept., Competition = competition_s,
    Aggression = aggression_s, Activity = activity_s, docility = docility_s,
    "Competition x Aggression" = competition_s.aggression_s,
    "Competition x Activity" = competition_s.activity_s,
    "Competition x Docility" = competition_s.docility_s)
    )
```

#### Interaction between competition and linear selection results

The effect of competition on linear selection on female behavioral traits for annual reproductive success. Posterior modes are given with highest posterior density intervals in parentheses.


```r
pandoc.table(linear_hpd_ars %>% select(pm_hpd), justify="right")
```


----------------------------------------------------
                        &nbsp;                pm_hpd
------------------------------ ---------------------
                 **Intercept** -1.01 (-1.11, -0.93)*

               **Competition** -3.27 (-3.65, -3.09)*

                **Aggression**   0.36 ( 0.13, 0.63)*

                  **Activity**   -0.12 (-0.37, 0.15)

                  **docility** -0.18 (-0.38, -0.06)*

  **Competition x Aggression**   1.30 ( 0.50, 1.99)*

    **Competition x Activity**   -0.45 (-1.26, 0.32)

    **Competition x Docility** -0.54 (-1.13, -0.09)*
----------------------------------------------------

### Competition and nonlinear selection (glmms)


```r
load("data/analyses_data/fit_raneff_data.RData")
library(lme4)

arsNonlinearResults <- function(dat){
  ars_model  <- glmer(ars_all ~ aggression_s*competition_s +
    activity_s*competition_s + docility_s*competition_s +
    aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s +
    I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
    (1 | grid_year) + (1|ID), data = dat, family = poisson,
    control=glmerControl(optimizer="bobyqa"))
  kpd_model  <- glmer(kprod   ~ aggression_s*competition_s +
    activity_s*competition_s + docility_s*competition_s +
    aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s +
    I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
    (1 | grid_year) + (1|ID), data = dat, family = poisson,
    control=glmerControl(optimizer="bobyqa"))
  ows_model  <- glmer(prop    ~ aggression_s*competition_s +
    activity_s*competition_s + docility_s*competition_s +
    aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s +
    I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
    (1 | grid_year) + (1|ID), data = dat, weights = kprod, family = binomial,
    control=glmerControl(optimizer="bobyqa"))

  ars_vc <- VarCorr(ars_model)
  ars_t <- data.table(fitness = "ars",
    t(summary(ars_model)$coefficients[ ,"Estimate"]), ID = ars_vc$ID[1],
    grid_year = ars_vc$grid_year[1])
  kpd_vc <- VarCorr(kpd_model)
  kpd_t <- data.table(fitness = "kpd",
    t(summary(kpd_model)$coefficients[ ,"Estimate"]), ID = kpd_vc$ID[1],
    grid_year = kpd_vc$grid_year[1])
  ows_vc <- VarCorr(ows_model)
  ows_t <- data.table(fitness = "ows",
    t(summary(ows_model)$coefficients[ ,"Estimate"]), ID = ows_vc$ID[1],
    grid_year = ows_vc$grid_year[1])
  rbind(rbind(ars_t, kpd_t), ows_t)
}


library(foreach)
library(doMC)
ncores = 12
registerDoMC(cores = ncores)

batches <- data.frame(start = seq(1, 1000, round(1000/ncores))[1:ncores])
batches$stop <- c(batches$start[2:length(batches$start)] - 1, 1000)

start_time <- Sys.time()
nonlinear_mcmc <- foreach(i = 1:ncores, .combine = rbind) %dopar% {
  results <- fit_raneff_data %>%
    filter(type == "raneff", itt %in% batches$start[i]:batches$stop[i]) %>%
    group_by(itt, add = FALSE) %>%
    do(arsNonlinearResults(.))
}
run_time <- Sys.time() - start_time
print(run_time)
```

```
## Time difference of 20.4 mins
```

```r
save(nonlinear_mcmc, file = "data/analyses_data/nonlinear_mcmc_models.RData")
```


```r
load("data/analyses_data/nonlinear_mcmc_models.RData")

pm_hpd_ars <- getPosteriorParams(
  nonlinear_mcmc[nonlinear_mcmc$fitness =="ars", 3:18])
pm_hpd_ows <- getPosteriorParams(
  nonlinear_mcmc[nonlinear_mcmc$fitness =="ows", 3:18])
pm_hpd_kpd <- getPosteriorParams(
  nonlinear_mcmc[nonlinear_mcmc$fitness =="kpd", 3:18])

nonlinear_results_mcmc <- data.frame(ARS = pm_hpd_ars$pm_hpd,
  OWS = pm_hpd_ows$pm_hpd, Fecundity = pm_hpd_kpd$pm_hpd)
  
row.names(nonlinear_results_mcmc) <- c("Intercept", "Aggression",
 "Competition", "Activity", "Docility", "Aggression^2", "Activity^2",
 "Docility^2", "Aggression x Competition", "Activity x Competition",
 "Docility x Competition", "Aggression x Activity",
 "Aggression^2 x Competition", "Activity^2 x Competition",
 "Docility^2 x Competition", "Agg. x Act. x Competition"
 )
```

### Nonlinear results
The effect of competition on linear and nonlinear selection on female behavioral traits for annual reproductive success. Posterior modes are given with highest posterior density intervals in parentheses.


```r
pandoc.table(nonlinear_results_mcmc[c(1,3,2,4:16), ], 
  split.tables = 160)
```


-----------------------------------------------------------------------------------------------
&nbsp;                                            ARS                   OWS           Fecundity
------------------------------- --------------------- --------------------- -------------------
**Intercept**                   -0.76 (-1.18, -0.42)* -1.84 (-2.35, -1.42)* 1.34 ( 1.28, 1.41)*

**Competition**                 -2.58 (-3.98, -1.72)* -2.41 (-4.21, -1.27)* -0.06 (-0.19, 0.09)

**Aggression**                    0.45 ( 0.08, 0.82)*   0.59 ( 0.17, 1.13)* -0.04 (-0.09, 0.02)

**Activity**                      -0.17 (-0.53, 0.17)   -0.06 (-0.68, 0.24)  0.03 (-0.05, 0.06)

**Docility**                    -0.29 (-0.55, -0.13)* -0.29 (-0.54, -0.03)* 0.04 ( 0.00, 0.07)*

**Aggression^2**                  -0.12 (-0.51, 0.15)   -0.28 (-0.70, 0.16)  0.02 (-0.05, 0.05)

**Activity^2**                     0.00 (-0.36, 0.23)   -0.07 (-0.42, 0.31) -0.02 (-0.07, 0.02)

**Docility^2**                    -0.15 (-0.33, 0.04)   -0.20 (-0.41, 0.05)  0.01 (-0.03, 0.04)

**Aggression x Competition**      1.71 ( 0.48, 2.66)*   2.16 ( 0.55, 3.53)* -0.03 (-0.16, 0.09)

**Activity x Competition**        -0.72 (-1.77, 0.35)   -0.24 (-1.70, 1.10) -0.03 (-0.15, 0.08)

**Docility x Competition**      -0.70 (-1.55, -0.26)*   -0.45 (-1.13, 0.49) -0.04 (-0.14, 0.02)

**Aggression x Activity**          0.14 (-0.22, 0.67)    0.21 (-0.29, 0.84)  0.02 (-0.06, 0.08)

**Aggression^2 x                  -0.27 (-1.36, 0.56)   -0.35 (-1.98, 0.70)  0.03 (-0.11, 0.14)
Competition**                                                                                  

**Activity^2 x Competition**      -0.06 (-1.07, 0.68)   -0.11 (-1.32, 1.00)  0.00 (-0.11, 0.11)

**Docility^2 x Competition**      -0.51 (-1.05, 0.15)   -0.34 (-1.29, 0.19) -0.02 (-0.11, 0.05)

**Agg. x Act. x Competition**      0.44 (-0.75, 1.92)    0.52 (-1.08, 2.57)  0.01 (-0.20, 0.16)
-----------------------------------------------------------------------------------------------

## Ignoring behavioural uncertainty

```r
load("data/analyses_data/sel_grads_blup.RData")
load("data/analyses_data/fit_raneff_data.RData")
```

### Correlations between selection gradients and competition

```r
competition_year <- fitness %>%
  filter(grid_year != "SU2008") %>%
  select(Year, competition) %>%
  unique() %>%
  group_by(Year, add = FALSE) %>%
  summarise(mean_competition = mean(competition))
  
n_year <- filter(fit_raneff_data, type == "blup") %>%
  group_by(Year, add = FALSE) %>%
  summarise(n = n())
competition_year <- left_join(competition_year, n_year, by = "Year")
competition_year$Year <- as.character(competition_year$Year)

load("data/analyses_data/sel_grads_blup.RData")
sel_grads_blup_competition <- left_join(
  filter(sel_grads_blup, standardization == "SD"), competition_year,
  by = "Year")
save(sel_grads_blup_competition, competition_year,
  file = "data/analyses_data/sel_grads_blup_competition.RData")
```


```r
load("data/analyses_data/sel_grads_blup_competition.RData")

cor_sgrad_comp <- function(x){
  v <- x$variable[1]
  ct <- cor.test(x$coefficients, x$mean_competition)
  data.frame(variable = v, est = ct$estimate, lower = ct$conf.int[1],
    upper = ct$conf.int[2], stringsAsFactors = FALSE)
}

sg.comp <- sel_grads_blup_competition %>%
  group_by(variable, add = FALSE) %>%
  do(cor_sgrad_comp(x=.))
sg.comp[ ,2:4] <- round(sg.comp[ ,2:4], digits = 2)
sg.comp$print <- paste(sg.comp$est, " (", sg.comp$lower, ", ",
  sg.comp$upper, ")", sep = "")
```

#### Aggression and Competition


```r
p <- ggplot(data = filter(sel_grads_blup_competition,
  variable == "Aggression"), aes(x = mean_competition, y = coefficients))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Aggression")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), strip.background = element_blank(), strip.text = element_text(size = 10), panel.border = element_rect(linetype = "solid", colour = "black"), plot.title = element_text(size = 10))
p <- p + geom_text(data = filter(sg.comp, variable == "Aggression"),
  aes(x = 6, y = 1.5, label = paste("r = ", print, sep = '')), size = 2.5)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/05_agg_comp_blup) 

#### Activity and Competition


```r
p <- ggplot(data = filter(sel_grads_blup_competition, variable == "Activity"), 
  aes(x = mean_competition, y = coefficients))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Activity")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_text(size = 10),
  panel.border = element_rect(linetype = "solid", colour = "black"),
  plot.title = element_text(size = 10))
p <- p + geom_text(data = filter(sg.comp, variable == "Activity"),
  aes(x = 6, y = 2.2, label = paste("r = ", print, sep = '')), size = 2.5)
  pdf(file = "test.pdf", width = 2.17, height = 2.03)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

#### Docility and Competition


```r
p <- ggplot(data = filter(sel_grads_blup_competition, variable == "Docility"),
  aes(x = mean_competition, y = coefficients))
p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.2)
p <- p + geom_point()
p <- p + theme_bw(base_size = 10)
p <- p + scale_x_continuous(breaks = c(3,4,5,6,7,8,9))
p <- p + ylab("Selection Gradient")
p <- p + ggtitle("Docility")
p <- p + xlab("Juvenile Competition")
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_text(size = 10),
  panel.border = element_rect(linetype = "solid", colour = "black"),
  plot.title = element_text(size = 10))
p <- p + geom_text(data = filter(sg.comp, variable == "Docility"),
  aes(x = 6, y = 1, label = paste("r = ", print, sep = '')), size = 2.5)
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/05_doc_comp_blup) 

### Effect of competition on linear selection

```r
load("data/analyses_data/fit_raneff_data.RData")
library(dplyr)
fit_raneff_data <- tbl_df(fit_raneff_data)
library(lme4)
fit_raneff_data$oID <- 1:nrow(fit_raneff_data)

ars_linear_model  <- glmer(ars_all ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | grid_year) +
  (1|ID) + (1|oID), data = filter(fit_raneff_data, type == "blup"),
  family = poisson, control=glmerControl(optimizer="bobyqa"))
kpd_linear_model  <- glmer(kprod ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | grid_year) +
  (1|ID) + (1|oID), data = filter(fit_raneff_data, type == "blup"),
  family = poisson, control=glmerControl(optimizer="bobyqa"))
ows_linear_model  <- glmer(prop ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | grid_year) +
  (1|ID) + (1|oID), data = filter(fit_raneff_data, type == "blup"),
  weights = kprod, family = binomial, control=glmerControl(optimizer="bobyqa"))

save(ars_linear_model, kpd_linear_model, ows_linear_model,
  file = "data/analyses_data/ars_linear_blup_models.RData")
```

Models with observation level random effect.


```r
load("data/analyses_data/fit_raneff_data.RData")
library(dplyr)
fit_raneff_data <- tbl_df(fit_raneff_data)
library(lme4)

fit_raneff_data$oID <- 1:nrow(fit_raneff_data)

ars_linear_model_  <- glmer(ars_all ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | oID) +
  (1 | grid_year) + (1|ID), data = filter(fit_raneff_data, type == "blup"),
  family = poisson, control=glmerControl(optimizer="bobyqa"))
kpd_linear_model_  <- glmer(kprod   ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | oID) +
  (1 | grid_year) + (1|ID), data = filter(fit_raneff_data, type == "blup"), 
  family = poisson, control=glmerControl(optimizer="bobyqa"))
ows_linear_model_  <- glmer(prop    ~ aggression_s*competition_s +
  activity_s*competition_s + docility_s*competition_s + (1 | oID) +
  (1 | grid_year) + (1|ID), data = filter(fit_raneff_data, type == "blup"), 
  weights = kprod, family = binomial, control=glmerControl(optimizer="bobyqa"))

save(ars_linear_model, kpd_linear_model, ows_linear_model,
  file = "data/analyses_data/ars_linear_blup_models.RData")
```


```r
load("data/analyses_data/ars_linear_blup_models.RData")
library(lme4)

getLmerParams <- function(x){
  coefs <- summary(x)$coefficients
  coef.table <- data.frame(format(coefs[ ,1:3], digits = 1, nsmall = 2, 
    scientific = FALSE))
  coef.table$pval[coefs[ ,4] > 0.001] <- format(coefs[coefs[ ,4] > 0.001, 4], 
    digits = 1, nsmall = 2)
  coef.table$pval[coefs[ ,4] < 0.001] <- "< 0.001"
  coef.table$coefs <- paste(coef.table$Estimate, " ±", coef.table$Std..Error, 
    sep = '')
  return(coef.table)
}

ars_linear_blup_results <- getLmerParams(ars_linear_model)
row.names(ars_linear_blup_results) <- c("Intercept", "Aggression",
  "Competition", "Activity", "Docility", "Aggression x Competition",
  "Activity x Competition", "Docility x Competition")
names(ars_linear_blup_results) <- c("Estimate", "SE", "Z", "P", "Est ± se")
```

#### Interaction between competition and linear selection results


```r
pandoc.table(ars_linear_blup_results[c(1,3,2,4:8), c(5,4,3)],
  justify = "right", split.tables = 160)
```


---------------------------------------------------------
                        &nbsp;     Est ± se       P     Z
------------------------------ ------------ ------- -----
                 **Intercept** -1.10 ± 0.22 < 0.001 -5.09

               **Competition** -3.62 ± 0.64 < 0.001 -5.64

                **Aggression**  0.74 ± 0.21 < 0.001  3.49

                  **Activity** -0.32 ± 0.20    0.11 -1.61

                  **Docility** -0.27 ± 0.16    0.10 -1.67

  **Aggression x Competition**  2.62 ± 0.64 < 0.001  4.07

    **Activity x Competition** -1.36 ± 0.61    0.03 -2.21

    **Docility x Competition** -0.77 ± 0.49    0.11 -1.58
---------------------------------------------------------

### Competition and nonlinear selection


```r
load("data/analyses_data/fit_raneff_data.RData")
library(lme4)
fit_blups_data <- filter(fit_raneff_data, type == "blup")
fit_blups_data$oID <- 1:nrow(fit_blups_data)

ars_nl_model <- glmer(ars_all ~ aggression_s*competition_s + 
  activity_s*competition_s + docility_s*competition_s + 
  aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s + 
  I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
  (1 | grid_year) + (1|ID) + (1|oID),
  data = fit_blups_data, family = poisson,
  control=glmerControl(optimizer="bobyqa"))

ars_nl_model <- glmer(ars_all ~ aggression_s*competition_s + 
  activity_s*competition_s + docility_s*competition_s + 
  aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s + 
  I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
  (1 | grid_year) + (1|ID),
  data = fit_blups_data, family = poisson,
  control=glmerControl(optimizer="bobyqa"))
  
kpd_nl_model <- glmer(kprod ~ aggression_s*competition_s + 
  activity_s*competition_s + docility_s*competition_s + 
  aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s + 
  I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
  (1 | grid_year) + (1|ID) + (1|oID),
  data = fit_blups_data, family = poisson,
  control=glmerControl(optimizer="bobyqa"))
  
ows_nl_model <- glmer(prop ~ aggression_s*competition_s + 
  activity_s*competition_s + docility_s*competition_s + 
  aggression_s*activity_s*competition_s + I(aggression_s^2)*competition_s + 
  I(activity_s^2)*competition_s + I(docility_s^2)*competition_s +
  (1 | grid_year) + (1|ID) + (1|oID),
  data = fit_blups_data, weights = kprod,
  family = binomial,
  control=glmerControl(optimizer="bobyqa"))

save(ars_nl_model,kpd_nl_model, ows_nl_model,
  file = "data/analyses_data/nl.blup_models.RData")

fit_raneff_data %>%
  ungroup() %>%
  summarise(
    mean_ars = mean(ars_all, na.rm = TRUE),
    var_ars = var(ars_all, na.rm = TRUE),
    mean_kpd = mean(kprod, na.rm = TRUE),
    var_kpd = var(kprod, na.rm = TRUE)
    )
```

```
## Source: local data frame [1 x 4]
## 
##   mean_ars var_ars mean_kpd var_kpd
## 1   0.8784   1.255    3.902   4.372
```

#### Format results of nonlinear selection for table

```r
load("data/analyses_data/nl.blup_models.RData")

coef_p_ars <- getLmerParams(ars_nl_model)
coef_p_ows <- getLmerParams(ows_nl_model)
coef_p_kpd <- getLmerParams(kpd_nl_model)

term_names <- c("Intercept", "Aggression",
 "Competition", "Activity", "Docility", "Aggression^2", "Activity^2",
 "Docility^2", "Aggression x Competition", "Activity x Competition",
 "Docility x Competition", "Aggression x Activity",
 "Aggression^2 x Competition", "Activity^2 x Competition",
 "Docility^2 x Competition", "Agg. x Act. x Competition"
 )

row.names(coef_p_ars) <- term_names
row.names(coef_p_ows) <- term_names
row.names(coef_p_kpd) <- term_names

names(coef_p_ars) <- c("Estimate", "SE", "Z", "P", "Est ± se")
names(coef_p_ows) <- c("Estimate", "SE", "Z", "P", "Est ± se")
names(coef_p_kpd) <- c("Estimate", "SE", "Z", "P", "Est ± se")
```

### Nonlinear results
The effect of competition on linear and nonlinear selection on female behavioral traits for annual reproductive success. Posterior modes are given with highest posterior density intervals in parentheses.

#### ARS

```r
pandoc.table(coef_p_ars[c(1,3,2,4:16), c(5,3,4)],
 split.tables = 160)
```


----------------------------------------------------------
            &nbsp;                Est ± se     Z      P   
------------------------------- ------------ ----- -------
         **Intercept**          -0.68 ± 0.31 -2.23  0.025 

        **Competition**         -2.24 ± 0.91 -2.46  0.014 

        **Aggression**          0.96 ± 0.31  3.06   0.002 

         **Activity**           -0.66 ± 0.28 -2.35  0.019 

         **Docility**           -0.51 ± 0.23 -2.18  0.029 

       **Aggression^2**         -0.65 ± 0.32 -2.07  0.039 

        **Activity^2**          -0.30 ± 0.26 -1.16  0.247 

        **Docility^2**          -0.18 ± 0.12 -1.48  0.139 

 **Aggression x Competition**   3.40 ± 0.92  3.68  < 0.001

  **Activity x Competition**    -2.45 ± 0.84 -2.90  0.004 

  **Docility x Competition**    -1.41 ± 0.69 -2.04  0.041 

   **Aggression x Activity**    1.01 ± 0.43  2.35   0.019 

       **Aggression^2 x         -1.86 ± 0.96 -1.93  0.053 
         Competition**                                    

 **Activity^2 x Competition**   -0.81 ± 0.77 -1.05  0.295 

 **Docility^2 x Competition**   -0.63 ± 0.39 -1.61  0.107 

 **Agg. x Act. x Competition**  2.62 ± 1.32  1.99   0.046 
----------------------------------------------------------

#### OWS

```r
pandoc.table(coef_p_ows[c(1,3,2,4:16), c(5,3,4)],
 split.tables = 160)
```


----------------------------------------------------------
            &nbsp;                Est ± se     Z      P   
------------------------------- ------------ ----- -------
         **Intercept**          -1.92 ± 0.39 -4.87 < 0.001

        **Competition**         -2.48 ± 1.18 -2.11  0.035 

        **Aggression**          1.33 ± 0.42  3.17   0.001 

         **Activity**           -0.82 ± 0.38 -2.14  0.032 

         **Docility**           -0.49 ± 0.29 -1.70  0.088 

       **Aggression^2**         -1.02 ± 0.43 -2.36  0.018 

        **Activity^2**          -0.37 ± 0.36 -1.03  0.304 

        **Docility^2**          -0.18 ± 0.15 -1.17  0.242 

 **Aggression x Competition**   4.75 ± 1.28  3.70  < 0.001

  **Activity x Competition**    -2.89 ± 1.20 -2.40  0.016 

  **Docility x Competition**    -0.78 ± 0.90 -0.86  0.388 

   **Aggression x Activity**    1.40 ± 0.60  2.35   0.019 

       **Aggression^2 x         -3.11 ± 1.36 -2.29  0.022 
         Competition**                                    

 **Activity^2 x Competition**   -1.17 ± 1.10 -1.06  0.291 

 **Docility^2 x Competition**   -0.50 ± 0.50 -0.98  0.325 

 **Agg. x Act. x Competition**  3.95 ± 1.86  2.13   0.034 
----------------------------------------------------------

#### Fecundity

```r
pandoc.table(coef_p_kpd[c(1,3,2,4:16), c(5,3,4)],
 split.tables = 160)
```


-------------------------------------------------------------
            &nbsp;                 Est ± se      Z       P   
------------------------------- -------------- ------ -------
         **Intercept**          1.362 ± 0.091  14.937 < 0.001

        **Competition**         -0.017 ± 0.161 -0.106  0.92  

        **Aggression**          -0.058 ± 0.049 -1.176  0.24  

         **Activity**           0.030 ± 0.050  0.594   0.55  

         **Docility**           0.030 ± 0.041  0.724   0.47  

       **Aggression^2**         0.036 ± 0.053  0.672   0.50  

        **Activity^2**          -0.064 ± 0.051 -1.248  0.21  

        **Docility^2**          0.005 ± 0.026  0.209   0.83  

 **Aggression x Competition**   -0.067 ± 0.117 -0.579  0.56  

  **Activity x Competition**    0.008 ± 0.088  0.086   0.93  

  **Docility x Competition**    -0.116 ± 0.087 -1.325  0.19  

   **Aggression x Activity**    0.024 ± 0.075  0.319   0.75  

       **Aggression^2 x         0.153 ± 0.149  1.029   0.30  
         Competition**                                       

 **Activity^2 x Competition**   0.079 ± 0.145  0.545   0.59  

 **Docility^2 x Competition**   -0.067 ± 0.055 -1.218  0.22  

 **Agg. x Act. x Competition**  -0.252 ± 0.240 -1.051  0.29  
-------------------------------------------------------------

#### Plot of quadratic interaction

```r
library(effects)
```

```
## Loading required package: colorspace
## 
## Attaching package: 'effects'
## 
## The following object is masked from 'package:car':
## 
##     Prestige
```

```r
library(ggplot2)

g.ows <- glm(prop ~ aggression_s * competition_s + activity_s * 
    competition_s + docility_s * competition_s + aggression_s:activity_s * 
    competition_s + I(aggression_s^2) * competition_s, data = filter(fit_raneff_data, 
    type == "blup"), weights = kprod, family = binomial)
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
e.ows <- effect(term = "aggression_s:competition_s:activity_s", 
    mod = g.ows)

e <- Effect(c("aggression_s", "activity_s", "competition_s"), 
    g.ows, xlevels = list(aggression_s = 100, activity_s = 2, 
        competition_s = 2))

s <- summary(e, type = "link")
se <- s$effect
su <- s$upper
sl <- s$lower
d <- as.data.frame(se)
du <- as.data.frame(su)
dl <- as.data.frame(sl)
names(d) <- c("la.lc", "ha.lc", "la.hc", "ha.hc")
names(du) <- c("la.lc", "ha.lc", "la.hc", "ha.hc")
names(dl) <- c("la.lc", "ha.lc", "la.hc", "ha.hc")

plot_d <- data.frame(Aggression = as.numeric(rep(row.names(s$effect), 
    12)), OWS = c(d$la.lc, d$ha.lc, d$la.hc, d$ha.hc, du$la.lc, 
    du$ha.lc, du$la.hc, du$ha.hc, dl$la.lc, dl$ha.lc, dl$la.hc, 
    dl$ha.hc), Competition = rep(rep(c("Low\nCompetition", "High\nCompetition"), 
    each = 200), 3), Activity = rep(rep(c("Low", "High"), each = 100), 
    6), type = rep(c("main", "upper", "lower"), each = 400))
plot_d$env <- paste(plot_d$Competition, plot_d$Activity, sep = ".")

quad_plot <- ggplot(plot_d, aes(x = Aggression, y = OWS)) + geom_line(aes(alpha = Activity, 
    linetype = type, size = type)) + facet_wrap(~Competition) + 
    scale_alpha_discrete(range = c(1, 0.3)) + scale_linetype_manual(values = c(2, 
    1, 2)) + scale_size_manual(values = c(0.3, 1, 0.3)) + ylab("Offspring\nOverwinter Survival") + 
    xlab("Aggression") + theme_bw(base_size = 10) + theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), panel.background = element_blank(), 
    panel.border = element_rect(linetype = "solid", colour = "black"), 
    axis.ticks = element_blank(), axis.text = element_text(size = 10), 
    legend.key = element_blank(), strip.background = element_blank()) + 
    guides(linetype = FALSE, size = FALSE, alpha = guide_legend(override.aes = list(size = 1))) + 
    theme(legend.position = c(0.75, 0.25), legend.background = element_blank(), 
        legend.key.size = unit(0.4, "cm")) + theme(plot.margin = unit(c(0.1, 
    0.1, 0.1, 0.1), "cm")) + theme(axis.text.x = element_blank(), 
    axis.text.y = element_blank())

pdf("figure/05_quad_print.pdf", width = 3.14, height = 2)
quad_plot
dev.off()
```

```
## pdf 
##   2
```

```r
quad_plot
```

![](figure/05_quad) 

#### Tile plot of interaction

```r
n = 12
e <- Effect(c("aggression_s", "activity_s", "competition_s"), 
    g.ows, xlevels = list(aggression_s = n, activity_s = n, competition_s = 2))
s <- summary(e, type = "response")
de <- as.data.frame(s$effect)
de_lc <- de[, 1:n]
de_hc <- de[, (n + 1):(2 * n)]

fix.names <- function(x) {
    a <- unlist(lapply(strsplit(names(x), split = "\\."), "[[", 
        1))
    b <- unlist(lapply(strsplit(names(x), split = "\\."), "[[", 
        2))
    out <- paste(a, b, sep = ".")
    return(out)
}

d_hc <- data.frame(Aggression = rep(row.names(de_hc), n), Activity = rep(fix.names(de_hc), 
    each = n), OWS = as.vector(as.matrix(de_hc)))
d_hc$Aggression <- as.numeric(as.character(d_hc$Aggression))
d_hc$Activity <- as.numeric(as.character(d_hc$Activity))

d_lc <- data.frame(Aggression = rep(row.names(de_lc), n), Activity = rep(fix.names(de_lc), 
    each = n), OWS = as.vector(as.matrix(de_lc)))
d_lc$Aggression <- as.numeric(as.character(d_lc$Aggression))
d_lc$Activity <- as.numeric(as.character(d_lc$Activity))

tile_plot <- ggplot(d_lc, aes(x = Aggression, y = Activity, z = OWS)) + 
    geom_tile(aes(alpha = OWS), fill = "black", size = 0) + scale_alpha_continuous(range = c(1, 
    0)) + ylab("Activity") + xlab("Aggression") + theme_bw(base_size = 10) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(linetype = "solid", 
            colour = "black"), axis.ticks = element_blank(), 
        legend.key = element_blank(), strip.background = element_blank()) + 
    guides(linetype = FALSE, size = FALSE, alpha = FALSE) + theme(plot.margin = unit(c(0.1, 
    0.1, 0.1, 0.1), "cm"))

pdf("figure/05_tile_print.pdf", width = 3.14, height = 3.14)
tile_plot
dev.off()
```

```
## pdf 
##   2
```


```r
tile_plot
```

![](figure/05_tile) 
