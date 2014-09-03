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


# Temporal Selection Gradients


```r
library(MASS) # MASS clashes with dplyr... so always load first
library(pander) # pander clashes with dplyr... so always load first
```

```
## 
## Attaching package: 'pander'
## 
## The following object is masked from 'package:knitr':
## 
##     pandoc
```

```r
set.alignment('right', row.names = 'left')
library(MCMCglmm)
```

```
## Loading required package: Matrix
## Loading required package: coda
## Loading required package: lattice
## Loading required package: ape
```

```r
library(arm)
```

```
## Loading required package: lme4
## Loading required package: Rcpp
## 
## arm (Version 1.7-07, built: 2014-8-27)
## 
## Working directory is /home/ryan/projects/2014-female-selection
## 
## 
## Attaching package: 'arm'
## 
## The following object is masked from 'package:ape':
## 
##     balance
## 
## The following object is masked from 'package:coda':
## 
##     traceplot
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:MASS':
## 
##     select
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(grid)

# Load Data
load("data/analyses_data/fit_raneff_data.RData")
fit_raneff_data$Year <- as.character(fit_raneff_data$Year)
```

##Does 'Year' significantly improve models of selection?
The significance of interaction terms between Year and the behavioral traits on fitness.


```r
# GLMMs to test whether selection fluctuates across years.
library(lme4)
# Models with interactions between year and the behavioral traits
fit_raneff_data_blup <- filter(fit_raneff_data, type == "blup")

fit_raneff_data_blup %>%
  group_by(Grid, Year, add = FALSE) %>%
  summarise(n(), var(ars_all))
```

```
## Source: local data frame [16 x 4]
## Groups: Grid
## 
##    Grid Year n() var(ars_all)
## 1    KL 2003   4       0.0000
## 2    KL 2004   8       0.7857
## 3    KL 2005  19       1.6550
## 4    KL 2006  24       0.4275
## 5    KL 2007  21       1.1000
## 6    KL 2008  29       0.5369
## 7    KL 2009  24       0.7373
## 8    KL 2010  22       1.2121
## 9    SU 2003  14       1.3022
## 10   SU 2004  18       0.8007
## 11   SU 2005  31       1.9828
## 12   SU 2006  24       0.3025
## 13   SU 2007  19       1.0526
## 14   SU 2008  16       0.0000
## 15   SU 2009  11       0.8727
## 16   SU 2010  12       2.4470
```

```r
# grid_years with no variation in fitness need to be removed
fit_raneff_data_blup <- filter(fit_raneff_data_blup,
  !(grid_year %in% c("KL2003", "SU2008")))

# Need to also remove Grid Years with very low sample sizes.
fit_raneff_data_blup %>%
  group_by(Grid, Year, add = FALSE) %>%
  summarise(n(), var(ars_all))
```

```
## Source: local data frame [14 x 4]
## Groups: Grid
## 
##    Grid Year n() var(ars_all)
## 1    KL 2004   8       0.7857
## 2    KL 2005  19       1.6550
## 3    KL 2006  24       0.4275
## 4    KL 2007  21       1.1000
## 5    KL 2008  29       0.5369
## 6    KL 2009  24       0.7373
## 7    KL 2010  22       1.2121
## 8    SU 2003  14       1.3022
## 9    SU 2004  18       0.8007
## 10   SU 2005  31       1.9828
## 11   SU 2006  24       0.3025
## 12   SU 2007  19       1.0526
## 13   SU 2009  11       0.8727
## 14   SU 2010  12       2.4470
```

```r
fit_raneff_data_blup <- filter(fit_raneff_data_blup,
  !(grid_year %in% c("KL2004")))

fit_raneff_data_blup$oID <- 1:nrow(fit_raneff_data_blup)
fit_raneff_data_blup <- droplevels(fit_raneff_data_blup)

ars_year <- glmer(ars_all ~ Year + Grid + activity_s + activity_s:Year +
  aggression_s + aggression_s:Year + docility_s + docility_s:Year +
  (1|ID) + (1|oID), data = fit_raneff_data_blup, family = poisson, 
  control=glmerControl(optimizer="bobyqa"))
```

```
## Warning: maxfun < 10 * length(par)^2 is not recommended.
```

```r
save(ars_year, file = "data/analyses_data/ars_year.RData")
```


```r
load("data/analyses_data/ars_year.RData")
# Test fit of models. Does the addition of year:traits improve the fit?
library(car)
```

```
## 
## Attaching package: 'car'
## 
## The following object is masked from 'package:arm':
## 
##     logit
```

```r
library(lme4)
aod <- Anova(ars_year, type = 2)
aod <- data.frame(aod)
aod$Chisq <- round(aod$Chisq ,digits = 2)

# Function to convert small p-values into 'P < X'
p.format <- function(x){ 
	out <- signif(x, digits = 2)
  out[x < 0.005]  <- "< 0.005"
	out[x < 0.001]  <- "< 0.001"
	out[x < 0.0001] <- "< 0.0001"
	return(out)
}
# Format p values
aod[ ,3] <- p.format(aod[ ,3])
row.names(aod) <- c("Year", "Grid", "Activity", "Aggression", "Docility",
  "Year x Activity", "Year x Aggression", "Year x Docility")
pandoc.table(aod, caption = 
  "The effect of year on selection for female behavioral traits through annual
reproductive success. Significance was calculated with Wald chisq tests from
a type II analysis of deviance. GLMMs were fitted with identity as a random
effect and assumed a Poisson error distribution.")
```


-------------------------------------------------
&nbsp;                    Chisq   Df   Pr..Chisq.
----------------------- ------- ---- ------------
**Year**                   38.1    7     < 0.0001

**Grid**                   1.07    1          0.3

**Activity**               0.34    1         0.56

**Aggression**             0.34    1         0.56

**Docility**               0.98    1         0.32

**Year x Activity**       22.92    7      < 0.005

**Year x Aggression**     22.84    7      < 0.005

**Year x Docility**       10.33    7         0.17
-------------------------------------------------

Table: The effect of year on selection for female behavioral traits through annual
reproductive success. Significance was calculated with Wald chisq tests from
a type II analysis of deviance. GLMMs were fitted with identity as a random
effect and assumed a Poisson error distribution.

## Accounting for random effect uncertainty
### Calculate selection gradients
Calculate selection coefficients for each of the 1000 samples of the posterior distribution of random effects.


```r
# Function to get posterior distribution of selection gradients
x <- fit_raneff_data %>% filter(type == "raneff" & Year == 2003 & itt == 1)
seCoefMCMC <- function(x){
  mod <- lm(rel_ars ~ aggression + activity + docility, data = x)
  mod_sd <-  lm(rel_ars ~ aggression_sy + activity_sy + docility_sy, data = x)
  res <- c(as.list(coef(mod)[-1]), as.list(coef(mod_sd)[-1]))
  res$Year <- x$Year[1]
  res$itt <- x$itt[1]
  return(data.frame(res, stringsAsFactors = FALSE))
}

start_time <- Sys.time()
sel_grads_mcmc_post <- fit_raneff_data %>%
  filter(type == "raneff") %>%
  group_by(itt, Year, add = FALSE) %>%
  do(seCoefMCMC(.))
print(paste("Approx. run time: ", format(Sys.time() - start_time)))
```

```
## [1] "Approx. run time:  57.5 secs"
```

```r
save(sel_grads_mcmc_post,
  file = "data/analyses_data/sel_grads_mcmc_post.RData")
```


```r
load("data/analyses_data/sel_grads_mcmc_post.RData")
x <- sel_grads_mcmc_post %>% filter(Year == "2003")
getCred <- function(x, sig = 0.05){
  require(MCMCglmm)
  mcmc_data <- x %>% ungroup() %>% select(aggression, activity, docility, aggression_sy, activity_sy, docility_sy) %>% mcmc(.)
  pm  <- posterior.mode(mcmc_data)
  int <- HPDinterval(mcmc_data, prob = 1 - sig)
  tbl_df(data.frame(
    Year      = x$Year[1],
    variable  = c("Aggression", "Activity", "Docility", "Aggression", 
    "Activity", "Docility"),
     standardization = c("None", "None", "None", "SD", "SD", "SD"),
    post_mode = pm,
    lower     = int[ ,"lower"],
    upper     = int[ ,"upper"],
    stringsAsFactors = FALSE
    ))
}

getCred(sel_grads_mcmc_post %>% filter(Year == "2004"))
```

```
## Source: local data frame [6 x 6]
## 
##               Year   variable standardization post_mode    lower   upper
## aggression    2004 Aggression            None  -0.30391 -0.76486 0.14086
## activity      2004   Activity            None   0.03346 -0.31909 0.66905
## docility      2004   Docility            None   0.05488 -0.06433 0.11222
## aggression_sy 2004 Aggression              SD  -0.24452 -0.80615 0.07615
## activity_sy   2004   Activity              SD   0.01654 -0.35851 0.62341
## docility_sy   2004   Docility              SD   0.20932 -0.25839 0.51523
```

```r
sel_grads_mcmc <- sel_grads_mcmc_post %>%
  group_by(Year, add = FALSE) %>%
  do(getCred(x = ., sig = 0.05))

sel_grads_mcmc$upper_sig_star <- ""
sel_grads_mcmc$lower_sig_star <- ""
sel_grads_mcmc$upper_sig_star[sel_grads_mcmc$post_mode > 0 &
  sel_grads_mcmc$lower > 0] <- "*"
sel_grads_mcmc$lower_sig_star[sel_grads_mcmc$post_mode < 0 &
  sel_grads_mcmc$upper < 0] <- "*"

save(sel_grads_mcmc, sel_grads_mcmc_post, getCred,
  file = "data/analyses_data/sel_grads_mcmc.RData")
```


```r
load("data/analyses_data/sel_grads_mcmc.RData")

N <- fit_raneff_data %>%
  filter(type == "blup") %>%
  group_by(Year, add = FALSE) %>%
  summarise(n(), doc_mean = mean(docility, na.rm = TRUE))

# Format for table
sgt <- sel_grads_mcmc
sgt$sig_star <- ""
sgt$sig_star[sgt$post_mode > 0 & sgt$lower > 0] <- "*"
sgt$sig_star[sgt$post_mode < 0 & sgt$upper < 0] <- "*"
sgt$post_mode <- format(round(sgt$post_mode, digits = 2), digits = 1,
  nsmall = 2)
sgt$lower     <- format(round(sgt$lower,     digits = 2), digits = 1,
  nsmall = 2)
sgt$upper     <- format(round(sgt$upper,     digits = 2), digits = 1,
  nsmall = 2)
sgt$coef      <- paste(sgt$post_mode, " (", sgt$lower, " to ", sgt$upper,")",
  sgt$sig_star, sep = '')

sgt_agg <- filter(sgt, variable == "Aggression", standardization == "None")
sgt_act <- filter(sgt, variable == "Activity",   standardization == "None")
sgt_doc <- filter(sgt, variable == "Docility",   standardization == "None")

sgt_agg_sd <- filter(sgt, variable == "Aggression", standardization == "SD")
sgt_act_sd <- filter(sgt, variable == "Activity",   standardization == "SD")
sgt_doc_sd <- filter(sgt, variable == "Docility",   standardization == "SD")


doc_post_mode <- sel_grads_mcmc %>% filter(standardization == "None" & variable == "Docility") 
doc_post_mode$post_mode_m <- doc_post_mode$post_mode * N$doc_mean
doc_post_mode$post_mode_m <- format(round(doc_post_mode$post_mode_m, digits = 2), digits = 1, nsmall = 2)

pandoc.table(
  data.frame(Year = N$Year, N = N[ ,2], Aggression = sgt_agg$coef,
    Acitivity = sgt_act$coef, Docility = sgt_doc$coef
  ),
  caption = "Non-standardized selection gradients (accounting for behavioural uncertainty)."
  )
```


--------------------------------------------------------
  Year   N            Aggression               Acitivity
------ --- --------------------- -----------------------
  2003  18  0.03 (-0.92 to 0.84)    0.45 (-0.17 to 1.32)

  2004  26 -0.30 (-0.76 to 0.14)    0.03 (-0.32 to 0.67)

  2005  50 -0.18 (-0.45 to 0.10)    0.13 (-0.16 to 0.40)

  2006  48  0.39 (-0.04 to 0.79)    0.08 (-0.39 to 0.45)

  2007  40  0.00 (-0.35 to 0.36)    0.11 (-0.22 to 0.42)

  2008  45  0.44 (-0.12 to 0.95)   -0.26 (-0.75 to 0.40)

  2009  35  0.12 (-0.33 to 0.58) -0.58 (-0.93 to -0.02)*

  2010  34  0.06 (-0.30 to 0.35)    0.03 (-0.26 to 0.30)
--------------------------------------------------------

Table: Non-standardized selection gradients (accounting for behavioural uncertainty). (continued below)

 
-----------------------
               Docility
-----------------------
  -0.08 (-0.26 to 0.06)

   0.05 (-0.06 to 0.11)

   0.03 (-0.02 to 0.07)

  -0.04 (-0.12 to 0.00)

   0.00 (-0.05 to 0.05)

  -0.02 (-0.08 to 0.05)

-0.06 (-0.11 to -0.01)*

-0.05 (-0.08 to -0.02)*
-----------------------

```r
pandoc.table(
  data.frame(Year = N$Year, N = N[ ,2], Aggression = sgt_agg_sd$coef,
    Acitivity = sgt_act_sd$coef, Docility = sgt_doc_sd$coef
  ),
  caption = "SD-standardized selection gradients (accounting for behavioural uncertainty)."
  )
```


------------------------------------------------------
 Year   N       Aggression             Acitivity      
------ --- --------------------- ---------------------
 2003  18  0.03 (-0.76 to 0.71)  0.79 (-0.24 to 1.38) 

 2004  26  -0.24 (-0.81 to 0.08) 0.02 (-0.36 to 0.62) 

 2005  50  -0.19 (-0.47 to 0.07) 0.16 (-0.12 to 0.41) 

 2006  48  0.39 (-0.04 to 0.72)  0.08 (-0.37 to 0.44) 

 2007  40  -0.01 (-0.32 to 0.34) 0.09 (-0.23 to 0.42) 

 2008  45  0.46 (-0.06 to 0.95)  -0.18 (-0.72 to 0.40)

 2009  35  0.16 (-0.29 to 0.59)  -0.55 (-0.92 to 0.01)

 2010  34  -0.04 (-0.27 to 0.32) 0.04 (-0.25 to 0.30) 
------------------------------------------------------

Table: SD-standardized selection gradients (accounting for behavioural uncertainty). (continued below)

 
-----------------------
       Docility        
-----------------------
 -0.38 (-1.12 to 0.31) 

 0.21 (-0.26 to 0.52)  

 0.10 (-0.09 to 0.34)  

-0.25 (-0.58 to -0.03)*

 0.01 (-0.23 to 0.23)  

 -0.08 (-0.37 to 0.25) 

-0.30 (-0.53 to -0.05)*

-0.23 (-0.35 to -0.10)*
-----------------------

```r
pandoc.table(
  data.frame(Year = N$Year, N = N[ ,2], mean_trait = N$doc_mean, Docility = doc_post_mode$post_mode_m
  ),
  caption = "Mean standardized selection gradients (accounting for behavioural uncertainty)."
  )
```


----------------------------------
 Year   N   mean_trait   Docility 
------ --- ------------ ----------
 2003  18     17.29       -1.46   

 2004  26     17.31        0.95   

 2005  50     17.25        0.55   

 2006  48     16.77       -0.70   

 2007  40     16.77        0.05   

 2008  45      16.9       -0.27   

 2009  35     16.79       -0.97   

 2010  34     17.19       -0.87   
----------------------------------

Table: Mean standardized selection gradients (accounting for behavioural uncertainty).

### Plots

```r
load("data/analyses_data/sel_grads_mcmc_post.RData")
library(ggplot2)
library(dplyr)

sel_grads_mcmc <- sel_grads_mcmc_post %>%
  group_by(Year, add = FALSE) %>% do(getCred(x = ., sig = 0.05))

sel_grads_mcmc$upper_sig_star <- ""
sel_grads_mcmc$lower_sig_star <- ""
sel_grads_mcmc$upper_sig_star[sel_grads_mcmc$post_mode > 0 &
  sel_grads_mcmc$lower > 0] <- "*"
sel_grads_mcmc$lower_sig_star[sel_grads_mcmc$post_mode < 0 &
  sel_grads_mcmc$upper < 0] <- "*"

p <- ggplot(data = filter(sel_grads_mcmc, standardization == "SD"),
  aes(x = Year, y = post_mode, group = variable))
p <- p + geom_hline(yintercept = 0, size = 0.25) # Line at y = 0
p <- p + geom_errorbar(aes(ymax = upper, ymin = lower),
  position = position_dodge(width = 0.5), width = 0.4, size = 0.4)
# Houle data percentiles
p <- p + geom_hline(yintercept = c(0.2975, -0.2975), linetype = 2, size = 0.4)
p <- p + geom_point(aes(shape = variable, fill = variable),
  position = position_dodge(width = 0.5), size = 3)
p <- p + scale_shape_manual(name = "B", values = c(24, 21, 22))
p <- p + scale_fill_manual(name = "B", values = c("white", "black", "white"))
p <- p + scale_color_manual(name = "B", values = c("black", "black", "black"))
p <- p + xlab("Year") 
p <- p + ylab("Posterior Mode ± 0.95 Credible Interval")
p <- p + theme_bw(base_size = 10)
p <- p + theme(legend.position = c(0.92, 0.86),
  legend.background = element_blank(), legend.key.size = unit(0.4, "cm"))
p <- p + theme(legend.title = element_text(family = "Helvetica",
                                 face = "plain", size = 18))
p <- p + theme(legend.key = element_blank())
p <- p + theme(strip.background = element_blank())
p <- p + theme(panel.grid.minor = element_blank(),
  panel.grid.major = element_blank())
p <- p + theme(panel.border = element_blank())
p <- p + theme(axis.line = element_line(color = "black"))
p <- p + geom_text(aes(x = Year, y = upper, group = variable,
  label = upper_sig_star), vjust = -0.3,
  position = position_dodge(width = 0.5), size = 5)
p <- p + geom_text(aes(x = Year, y = lower, group = variable,
  label = lower_sig_star), vjust = 1.3,
  position = position_dodge(width = 0.5), size = 5)
p_sel_grad_MCMC <- p + ylim(c(-1.1,1.4))

pdf(file = "figure/04_sg_mcmc_SD_print.pdf", width = 4.33, height = 3)
p_sel_grad_MCMC
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
## Warning: Removed 1 rows containing missing values (geom_text).
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
p_sel_grad_MCMC
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
## Warning: Removed 1 rows containing missing values (geom_text).
```

![SD Standardized Selection Gradients](figure/04_sg_mcmc_SD) 


```r
load("data/analyses_data/sel_grads_mcmc_post.RData")
library(ggplot2)
library(dplyr)

sel_grads_mcmc <- sel_grads_mcmc_post %>%
  group_by(Year, add = FALSE) %>% do(getCred(x = ., sig = 0.05))

sel_grads_mcmc$upper_sig_star <- ""
sel_grads_mcmc$lower_sig_star <- ""
sel_grads_mcmc$upper_sig_star[sel_grads_mcmc$post_mode > 0 &
  sel_grads_mcmc$lower > 0] <- "*"
sel_grads_mcmc$lower_sig_star[sel_grads_mcmc$post_mode < 0 &
  sel_grads_mcmc$upper < 0] <- "*"

p <- ggplot(data = filter(sel_grads_mcmc, standardization == "None"),
  aes(x = Year, y = post_mode, group = variable)
)
p <- p + geom_hline(yintercept = 0, size = 0.25) # Line at y = 0
p <- p + geom_errorbar(aes(ymax = upper, ymin = lower),
  position = position_dodge(width = 0.5), width = 0.4, size = 0.4)
# Houle data percentiles
p <- p + geom_hline(yintercept = c(0.2975, -0.2975), linetype = 2, size = 0.4)
p <- p + geom_point(aes(shape = variable, fill = variable),
  position = position_dodge(width = 0.5), size = 3)
p <- p + scale_shape_manual(name = "B", values = c(24, 21, 22))
p <- p + scale_fill_manual(name = "B", values = c("white", "black", "white"))
p <- p + scale_color_manual(name = "B", values = c("black", "black", "black"))
p <- p + xlab("Year") 
p <- p + ylab("Posterior Mode ± 0.95 Credible Interval")
p <- p + theme_bw(base_size = 10)
p <- p + theme(legend.position = c(0.92, 0.86),
  legend.background = element_blank(), legend.key.size = unit(0.4, "cm"))
p <- p + theme(legend.title = element_text(family = "Helvetica",
                                 face = "plain", size = 18))
p <- p + theme(legend.key = element_blank())
p <- p + theme(strip.background = element_blank())
p <- p + theme(panel.grid.minor = element_blank(),
  panel.grid.major = element_blank())
p <- p + theme(panel.border = element_blank())
p <- p + theme(axis.line = element_line(color = "black"))
p <- p + geom_text(aes(x = Year, y = upper, group = variable,
  label = upper_sig_star), vjust = -0.3,
  position = position_dodge(width = 0.5), size = 5)
p <- p + geom_text(aes(x = Year, y = lower, group = variable,
  label = lower_sig_star), vjust = 1.3,
  position = position_dodge(width = 0.5), size = 5)
p_sel_grad_MCMC <- p + ylim(c(-1.1,1.4))

pdf(file = "figure/04_sg_mcmc_NS_print.pdf", width = 4.33, height = 3)
p_sel_grad_MCMC
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
p_sel_grad_MCMC
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

![Non-standardized Selection Gradients](figure/04_sg_mcmc_NS) 

### Correlations


```r
sel_grads_mcmc_flat <- data.frame(
  Aggression = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Aggression")$post_mode,
  Agg_upper  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Aggression")$upper,
  Agg_lower  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Aggression")$lower,
  Activity   = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Activity")$post_mode,
  Act_upper  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Activity")$upper,
  Act_lower  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Activity")$lower,
  Docility   = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Docility")$post_mode,
  Doc_upper  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Docility")$upper,
  Doc_lower  = filter(sel_grads_mcmc, standardization == "SD",
   variable == "Docility")$lower
)

cor.behav <- function(x, y){
  ct <- cor.test(x, y)
  out <- data.frame(est = ct$estimate, lower = ct$conf.int[1],
    upper = ct$conf.int[2], stringsAsFactors = FALSE)
  out <- round(out, digits = 2)
  out$print <- paste(out$est, " (", out$lower, ", ", out$upper, ")", sep = "")
}

cor_agg_act <- cor.behav(sel_grads_mcmc_flat$Aggression, 
  sel_grads_mcmc_flat$Activity)
cor_agg_doc <- cor.behav(sel_grads_mcmc_flat$Aggression, 
  sel_grads_mcmc_flat$Docility)
cor_doc_act <- cor.behav(sel_grads_mcmc_flat$Docility, 
  sel_grads_mcmc_flat$Activity)
```

#### Aggression and Activity


```r
p <- ggplot(data = sel_grads_mcmc_flat, aes(x = Activity, y = Aggression))
p <- p + geom_point()
p <- p + ylab("Aggression")
p <- p + xlab("Activity")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbarh(aes(xmin = Act_lower, xmax = Act_upper),
  height = 0.07, size = 0.2)
p <- p + geom_errorbar(aes(ymin = Agg_lower, ymax = Agg_upper),
  width = 0.07, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = 0.25, y = 1.2,
  label = paste("r = ", cor_agg_act, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_agg_act_mcmc) 

#### Aggression and Docility


```r
p <- ggplot(data = sel_grads_mcmc_flat, aes(y = Aggression, x = Docility))
p <- p + geom_point()
p <- p + ylab("Aggression")
p <- p + xlab("Docility")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbarh(aes(xmin = Doc_lower, xmax = Doc_upper),
  height = 0.07, size = 0.2)
p <- p + geom_errorbar(aes(ymin = Agg_lower, ymax = Agg_upper),
  width = 0.03, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = -0.3, y = 1.2,
  label = paste("r = ", cor_agg_doc, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_agg_doc_mcmc) 

#### Activity and Docility


```r
p <- ggplot(data = sel_grads_mcmc_flat, aes(x = Activity, y = Docility))
p <- p + geom_point()
p <- p + xlab("Activity")
p <- p + ylab("Docility")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbar(aes(ymin = Doc_lower, ymax = Doc_upper),
  width = 0.07, size = 0.2)
p <- p + geom_errorbarh(aes(xmin = Act_lower, xmax = Act_upper),
  height = 0.03, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = 0, y = 0.6,
  label = paste("r = ", cor_doc_act, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_act_doc_mcmc) 

### Summary Stats


```r
library(dplyr)
sign_change <- function(x){
  # sign changes calculated as the number of changes in direction between 
  # successive years relative to n−1
  s <- sign(x)
  sum(s[1:(length(s)-1)] != s[-1])/(length(s)-1)
}
se <- function(lower, upper){
  (upper - lower) / 3.92
}

sum_stats_mcmc <- sel_grads_mcmc %>%
filter(standardization == "SD") %>%
group_by(variable, add = FALSE) %>%
summarise(
  mean_abs_b = mean(abs(post_mode)),
  abs_mean_b = abs(mean(post_mode)),
  sd_b = sd(post_mode),
  mean_se_b = mean(se(lower, upper)),
  freq_sign = sign_change(post_mode),
  mean_cv = mean(se(lower, upper) / abs(post_mode))
  )
sum_stats_mcmc[ ,2:6] <- round(sum_stats_mcmc[ ,2:6], 2)
pandoc.table(sum_stats_mcmc)
```


-------------------------------------------------------
 variable   mean_abs_b   abs_mean_b   sd_b   mean_se_b 
---------- ------------ ------------ ------ -----------
 Activity      0.24         0.05      0.37     0.23    

Aggression     0.19         0.07      0.25     0.22    

 Docility      0.2          0.12      0.21     0.16    
-------------------------------------------------------

Table: Table continues below

 
---------------------
 freq_sign   mean_cv 
----------- ---------
   0.29       3.307  

   0.71       4.002  

   0.57       1.845  
---------------------

## Ignoring random effect uncertainty
Calculate selection gradients from BLUPs, estimate SE using jackknifing.

### Calculate selection gradients


```r
# Calculate standardized selection gradients
seCoeffLmer <- function(x){
  model <- lm(rel_ars ~ aggression + activity + docility, data = x)
  model_sy <- lm(rel_ars ~ aggression_sy + activity_sy + docility_sy, data = x)
  mod_coefs <- c(coef(model)[-1], coef(model_sy)[-1])
  sim_coefs <- data.frame(coef(sim(model))[ ,-1], coef(sim(model_sy))[ ,-1]) ## Simulated coefficients for calculating uncertainty.
  names(sim_coefs) <- names(mod_coefs)
  sim_CI <- apply(sim_coefs, 2, quantile, prob = c(0.025, 0.975)) #0.95 conf. int.
  
docil_mean_coef <- mod_coefs["docility"] * mean(x$docility, na.rm = TRUE)
  out <- data.frame(
    standardization = c("None","None","None","SD","SD","SD","Mean"),
    Year = as.numeric(rep(as.character(x$Year[1]), 7)),
    variable = c("Aggression", "Activity", "Docility", "Aggression", 
      "Activity", "Docility", "Docility"),
    coefficients = c(mod_coefs, docil_mean_coef),
    lower = c(sim_CI[1, ], 0),
    upper = c(sim_CI[2, ],0)
    )
    return(out)
}

sel_grads_blup <- fit_raneff_data %>%
  filter(type == "blup") %>%
  group_by(Year, add = FALSE) %>%
  do(seCoeffLmer(.))
sel_grads_blup$variable <- as.character(sel_grads_blup$variable)
sel_grads_blup$Year <- as.character(sel_grads_blup$Year)
sel_grads_blup <- tbl_df(sel_grads_blup)
save(sel_grads_blup, file = "data/analyses_data/sel_grads_blup.RData")
```


```r
load("data/analyses_data/sel_grads_blup.RData")
# Format for table
sgt <- sel_grads_blup
sgt$sig_star <- ""
sgt$sig_star[sgt$coefficients > 0 & sgt$lower > 0] <- "*"
sgt$sig_star[sgt$coefficients < 0 & sgt$upper < 0] <- "*"
sgt$coefficients <- format(round(sgt$coefficients, digits = 2),
  digits = 1, nsmall = 2)
sgt$lower <- format(round(sgt$lower, digits = 2),
  digits = 1, nsmall = 2)
sgt$upper <- format(round(sgt$upper, digits = 2),
  digits = 1, nsmall = 2)
sgt$prb <- NA
sgt$coef <- paste(sgt$coefficients,
  " (", sgt$lower, " to ", sgt$upper,")", sgt$sig_star, sep = '')

sgt_agg <- filter(sgt, standardization == "None" & variable == "Aggression")
sgt_act <- filter(sgt, standardization == "None" & variable == "Activity")
sgt_doc <- filter(sgt, standardization == "None" & variable == "Docility")

sgt_agg_sd <- filter(sgt, standardization == "SD" &  variable == "Aggression")
sgt_act_sd <- filter(sgt, standardization == "SD" &  variable == "Activity")
sgt_doc_sd <- filter(sgt, standardization == "SD" &  variable == "Docility")

sgt_doc_ms <- filter(sgt, standardization == "Mean" &  variable == "Docility")

N <- fit_raneff_data %>%
  filter(type == "blup") %>%
  group_by(Year, add = FALSE) %>%
  summarise(n = n(), t_kprod = sum(kprod), t_ars = sum(ars_all), mean_docil = mean(docility, na.rm = TRUE))

pandoc.table(
  data.frame(
    Year = N$Year,
    Aggression = sgt_agg$coef,
    Activity = sgt_act$coef,
    Docility = sgt_doc$coef
  ),
  caption ="Traditional selection gradients (ignoring behavioural uncertainty). Not standardized."
)
```


------------------------------------------------------
 Year        Aggression               Activity        
------ ----------------------- -----------------------
 2003   -0.26 (-1.66 to 0.61)   0.90 (-0.25 to 1.76)  

 2004  -0.75 (-1.36 to -0.05)*  0.57 (-0.18 to 1.35)  

 2005  -0.56 (-1.02 to -0.06)*  0.46 (-0.08 to 0.98)  

 2006   0.98 ( 0.20 to 1.86)*   -0.33 (-1.11 to 0.48) 

 2007   -0.09 (-0.92 to 0.49)   0.19 (-0.53 to 0.88)  

 2008   1.11 (-0.36 to 2.30)    -0.60 (-1.46 to 0.94) 

 2009   0.75 (-0.09 to 1.39)   -1.38 (-2.11 to -0.60)*

 2010   -0.03 (-0.74 to 0.67)   0.12 (-0.54 to 0.77)  
------------------------------------------------------

Table: Traditional selection gradients (ignoring behavioural uncertainty). Not standardized. (continued below)

 
----------------------
       Docility       
----------------------
-0.11 (-0.36 to 0.10) 

 0.07 (-0.13 to 0.24) 

 0.05 (-0.04 to 0.16) 

-0.10 (-0.22 to 0.04) 

 0.01 (-0.11 to 0.13) 

-0.04 (-0.21 to 0.13) 

-0.12 (-0.23 to 0.00)*

-0.05 (-0.12 to 0.04) 
----------------------

```r
pandoc.table(
  data.frame(
    Year = N$Year,
    Aggression = sgt_agg_sd$coef,
    Activity = sgt_act_sd$coef,
    Docility = sgt_doc_sd$coef
  ),
  caption ="Traditional selection gradients (ignoring behavioural uncertainty). SD-standardized."
)
```


------------------------------------------------------
 Year        Aggression               Activity        
------ ----------------------- -----------------------
 2003   -0.17 (-1.25 to 0.57)   0.86 (-0.20 to 1.96)  

 2004   -0.62 (-1.17 to 0.22)   0.49 (-0.37 to 1.29)  

 2005  -0.41 (-0.71 to -0.04)*  0.34 (-0.10 to 0.76)  

 2006   0.66 ( 0.13 to 1.19)*   -0.23 (-0.96 to 0.35) 

 2007   -0.07 (-0.57 to 0.48)   0.15 (-0.44 to 0.63)  

 2008   0.78 ( 0.20 to 1.54)*   -0.44 (-1.64 to 0.39) 

 2009   0.56 ( 0.05 to 1.20)*  -1.01 (-1.84 to -0.47)*

 2010   -0.02 (-0.43 to 0.56)   0.10 (-0.34 to 0.50)  
------------------------------------------------------

Table: Traditional selection gradients (ignoring behavioural uncertainty). SD-standardized. (continued below)

 
-----------------------
       Docility        
-----------------------
 -0.40 (-1.22 to 0.61) 

 0.26 (-0.29 to 1.01)  

 0.19 (-0.11 to 0.54)  

 -0.41 (-0.92 to 0.17) 

 0.03 (-0.47 to 0.52)  

 -0.17 (-0.81 to 0.45) 

-0.56 (-1.07 to -0.06)*

 -0.21 (-0.47 to 0.16) 
-----------------------

```r
pandoc.table(
  data.frame(
    Year = N$Year,
    Docility = sgt_doc_ms$coefficients,
    mean = N$mean_docil
  ),
  caption ="Traditional selection gradients (ignoring behavioural uncertainty). Mean-standardized."
)
```


------------------------
 Year   Docility   mean 
------ ---------- ------
 2003    -1.86    17.29 

 2004     1.25    17.31 

 2005     0.88    17.25 

 2006    -1.72    16.77 

 2007     0.12    16.77 

 2008    -0.63     16.9 

 2009    -2.04    16.79 

 2010    -0.79    17.19 
------------------------

Table: Traditional selection gradients (ignoring behavioural uncertainty). Mean-standardized.

### Plot
**Female Linear Selection Gradients ARS**  
Linear selection gradients ± 95% credible intervals for female behavioral traits on annual reproductive success.


```r
load("data/analyses_data/sel_grads_blup.RData")
sel_grads_blup$post_mode <- sel_grads_blup$coefficients
sel_grads_blup$upper_sig_star <- ""
sel_grads_blup$lower_sig_star <- ""
sel_grads_blup$upper_sig_star[sel_grads_blup$coefficients > 0 & 
  sel_grads_blup$lower > 0] <- "*"
sel_grads_blup$lower_sig_star[sel_grads_blup$coefficients < 0 &
  sel_grads_blup$upper < 0] <- "*"
sel_grads_blup$upper_sig_01_star <- ""
sel_grads_blup$lower_sig_01_star <- ""
sel_grads_blup$upper_sig_01_star[sel_grads_blup$coefficients > 0
  & sel_grads_blup$lower_1 > 0] <- "."
sel_grads_blup$lower_sig_01_star[sel_grads_blup$coefficients < 0
  & sel_grads_blup$upper_1 < 0] <- "."
sel_grads_blup$upper_sig_01_star[sel_grads_blup$coefficients > 0
  & sel_grads_blup$lower > 0] <- ""
sel_grads_blup$lower_sig_01_star[sel_grads_blup$coefficients < 0
  & sel_grads_blup$upper < 0] <- ""

pdf(file = "figure/04_sg_blup_SD_print.pdf", width = 4.33, height = 3)
p <- p_sel_grad_MCMC %+% filter(sel_grads_blup, standardization == "SD")
p <- p + ylab("Coefficient ± 0.95 Confidence Interval")
p <- p + geom_text(aes(x = Year, y = upper, group = variable,
  label = upper_sig_01_star), vjust = -0.3,
  position = position_dodge(width = 0.5), size = 7)
p <- p + geom_text(aes(x = Year, y = lower, group = variable,
  label = lower_sig_01_star), vjust =  0.5,
  position = position_dodge(width = 0.5), size = 7)
p <- p + geom_text(aes(x = Year, y = upper, group = variable,
  label = upper_sig_star), vjust = -0.3,
  position = position_dodge(width = 0.5), size = 5)
p <- p + geom_text(aes(x = Year, y = lower, group = variable,
  label = lower_sig_star), vjust =  1.3,
  position = position_dodge(width = 0.5), size = 5)
p <- p + ylim(c(-2, 2))
```

```
## Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale.
```

```r
p <- p + scale_shape_manual(name = "A", values = c(24, 21, 22))
```

```
## Scale for 'shape' is already present. Adding another scale for 'shape', which will replace the existing scale.
```

```r
p <- p + scale_fill_manual(name = "A", values = c("white", "black", "white"))
```

```
## Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing scale.
```

```r
p <- p + scale_color_manual(name = "A", values = c("black", "black", "black"))
```

```
## Scale for 'colour' is already present. Adding another scale for 'colour', which will replace the existing scale.
```

```r
p
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
p
```

```
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
## ymax not defined: adjusting position using y instead
```

![](figure/04_sg_blup_SD) 

### Correlations

```r
load("data/analyses_data/sel_grads_blup.RData")
sel_grads_blup_flat <- data.frame(
  Aggression = filter(sel_grads_blup, standardization == "SD" &
   variable == "Aggression")$coefficients,
  Agg_upper  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Aggression")$upper,
  Agg_lower  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Aggression")$lower,
  Activity   = filter(sel_grads_blup, standardization == "SD" &
   variable == "Activity")$coefficients,
  Act_upper  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Activity")$upper,
  Act_lower  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Activity")$lower,
  Docility   = filter(sel_grads_blup, standardization == "SD" &
   variable == "Docility")$coefficients,
  Doc_upper  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Docility")$upper,
  Doc_lower  = filter(sel_grads_blup, standardization == "SD" &
   variable == "Docility")$lower
)

cor.behav <- function(x, y){
  ct <- cor.test(x, y)
  out <- data.frame(est = ct$estimate, lower = ct$conf.int[1],
    upper = ct$conf.int[2], stringsAsFactors = FALSE)
  out <- round(out, digits = 2)
  out$print <- paste(out$est, " (", out$lower, ", ", out$upper, ")", sep = "")
}

cor_blup_agg_act <- cor.behav(sel_grads_blup_flat$Aggression,
  sel_grads_blup_flat$Activity)
cor_blup_agg_doc <- cor.behav(sel_grads_blup_flat$Aggression,
  sel_grads_blup_flat$Docility)
cor_blup_doc_act <- cor.behav(sel_grads_blup_flat$Docility,
  sel_grads_blup_flat$Activity)
```

#### Aggression and Activity

```r
p <- ggplot(data = sel_grads_blup_flat, aes(x = Activity, y = Aggression))
p <- p + geom_point()
p <- p + ylab("Aggression")
p <- p + xlab("Activity")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbarh(aes(xmin = Act_lower, xmax = Act_upper),
  height = 0.07, size = 0.2)
p <- p + geom_errorbar(aes(ymin = Agg_lower, ymax = Agg_upper),
  width = 0.07, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = 0.1, y = 1.7,
  label = paste("r = ", cor_blup_agg_act, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_agg_act_blup) 

#### Aggression and Docility

```r
p <- ggplot(data = sel_grads_blup_flat, aes(y = Aggression, x = Docility))
p <- p + geom_point()
p <- p + ylab("Aggression")
p <- p + xlab("Docility")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbarh(aes(xmin = Doc_lower, xmax = Doc_upper),
  height = 0.07, size = 0.2)
p <- p + geom_errorbar(aes(ymin = Agg_lower, ymax = Agg_upper),
  width = 0.03, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = -0.1, y = 1.7,
  label = paste("r = ", cor_blup_agg_doc, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_agg_doc_blup) 

#### Activity and Docility

```r
p <- ggplot(data = sel_grads_blup_flat, aes(x = Activity, y = Docility))
p <- p + geom_point()
p <- p + xlab("Activity")
p <- p + ylab("Docility")
p <- p + theme_bw(base_size = 10)
p <- p + theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), panel.background = element_blank(),
  strip.background = element_blank(), strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + geom_errorbar(aes(ymin = Doc_lower, ymax = Doc_upper),
  width = 0.07, size = 0.2)
p <- p + geom_errorbarh(aes(xmin = Act_lower, xmax = Act_upper),
  height = 0.03, size = 0.2)
p <- p + annotate(geom = "text", size = 2.5, x = 0, y = 1.2,
  label = paste("r = ", cor_blup_doc_act, sep = ''))
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_act_doc_blup) 

### Summary Statistics

```r
library(dplyr)
sign_change <- function(x){
  # sign changes calculated as the number of changes in direction between 
  # successive years relative to n−1
  s <- sign(x)
  sum(s[1:(length(s)-1)] != s[-1])/(length(s)-1)
}
se <- function(lower, upper){
  (upper - lower) / 3.92
}

sum_stats_blup <- sel_grads_blup %>%
filter(standardization == "SD") %>%
group_by(variable, add = FALSE) %>%
summarise(
  mean_abs_b = mean(abs(coefficients)),
  abs_mean_b = abs(mean(coefficients)),
  sd_b = sd(coefficients),
  mean_se_b = mean(se(lower, upper)),
  freq_sign = sign_change(coefficients),
  mean_cv = mean(se(lower, upper) / abs(coefficients))
  )
sum_stats_blup[ ,2:6] <- round(sum_stats_blup[ ,2:6], 2)
pandoc.table(sum_stats_blup)
```


-------------------------------------------------------
  variable   mean_abs_b   abs_mean_b   sd_b   mean_se_b
---------- ------------ ------------ ------ -----------
  Activity         0.45         0.03   0.58        0.36

Aggression         0.41         0.09   0.52         0.3

  Docility         0.28         0.16    0.3        0.28
-------------------------------------------------------

Table: Table continues below

 
---------------------
  freq_sign   mean_cv
----------- ---------
       0.57     1.138

       0.57     2.701

       0.57     1.912
---------------------

## Compare Analytical Frameworks

### Table

```r
load("data/analyses_data/sel_grads_blup.RData")
load("data/analyses_data/sel_grads_mcmc.RData")

sg_blups <- sel_grads_blup %>% filter(standardization == "SD")
sg_mcmc <- sel_grads_mcmc %>% filter(standardization == "SD")

compare_grads <- left_join(select(sg_blups, Year, variable,
  blup_coef = coefficients, blup_upper = upper, blup_lower = lower),
  select(sg_mcmc, Year, variable, mcmc_pm = post_mode,
    mcmc_upper = upper, mcmc_lower = lower), by = c("variable", "Year"))

ct_print <- function(x,y){
  ct <- cor.test(x,y)
  est <- format(ct$estimate, digits = 2)
  ci <- format(ct$conf.int, digits = 2)
  ct <- format(ct, digits = 2)
  paste(est, " (", ci[1], ", ", ci[2] ,")", sep = '')  
}

c_table <- compare_grads %>%
  group_by(variable) %>%
  summarise(cor = cor(blup_coef, mcmc_pm),
    abs_diff = mean((abs(blup_coef - mcmc_pm))),
    mean_mcmc = mean(abs(mcmc_pm)), mean_blup = mean(abs(blup_coef)),
    prop_diff = mean_blup / mean_mcmc, cor_test = ct_print(blup_coef, mcmc_pm),
    lmerGreater = sum(abs(blup_coef) > abs(mcmc_pm))
  )
pandoc.table(c_table)
```


----------------------------------------------------
 variable   cor    abs_diff   mean_mcmc   mean_blup 
---------- ------ ---------- ----------- -----------
 Activity  0.8931   0.2345     0.2374      0.4527   

Aggression 0.9599   0.2312     0.1904      0.4096   

 Docility  0.9489  0.08657     0.1957      0.2766   
----------------------------------------------------

Table: Table continues below

 
-------------------------------------------
 prop_diff      cor_test       lmerGreater 
----------- ----------------- -------------
   1.907    0.89 (0.51, 0.98)       8      

   2.152    0.96 (0.79, 0.99)       7      

   1.413    0.95 (0.74, 0.99)       7      
-------------------------------------------

### Aggression plot

```r
p <- ggplot(filter(compare_grads, variable == "Aggression"),
  aes(x = blup_coef, y = mcmc_pm))
p <- p + geom_point()
p <- p + geom_errorbarh(aes(xmin = blup_lower, xmax = blup_upper),
  height = 0.04, size = 0.2)
p <- p + geom_errorbar(aes(ymin = mcmc_lower, ymax = mcmc_upper),
  width = 0.07, size = 0.2)
p <- p + theme_bw(base_size = 10)
p <- p + theme(plot.title = element_text(size = 10),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), strip.background = element_blank(),
  strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + annotate(geom = "text", size = 2.5, x = 0.1, y = 1.1,
  label = paste("r = ", filter(c_table, variable == "Aggression") %>%
  select(cor_test), sep = ''))
p <- p + ylab("Posterior Modes")
p <- p + xlab("Selection Gradients")
p <- p + ggtitle("Aggression")
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_compare_agg) 

### Activity Plot

```r
p <- ggplot(filter(compare_grads, variable == "Activity"),
  aes(x = blup_coef, y = mcmc_pm))
p <- p + geom_point()
p <- p + geom_errorbarh(aes(xmin = blup_lower, xmax = blup_upper),
  height = 0.04, size = 0.2)
p <- p + geom_errorbar(aes(ymin = mcmc_lower, ymax = mcmc_upper),
  width = 0.07, size = 0.2)
p <- p + theme_bw(base_size = 10)
p <- p + theme(plot.title = element_text(size = 10),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), strip.background = element_blank(),
  strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + annotate(geom = "text", size = 2.5, x = 0, y = 1.5,
  label = paste("r = ", filter(c_table, variable == "Activity") %>%
  select(cor_test), sep = ''))
p <- p + ylab("Posterior Modes")
p <- p + xlab("Selection Gradients")
p <- p + ggtitle("Activity")
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_compare_act) 

### Docility Plot

```r
p <- ggplot(filter(compare_grads, variable == "Docility"),
  aes(x = blup_coef, y = mcmc_pm))
p <- p + geom_point()
p <- p + geom_errorbarh(aes(xmin = blup_lower, xmax = blup_upper),
  height = 0.04, size = 0.2)
p <- p + geom_errorbar(aes(ymin = mcmc_lower, ymax = mcmc_upper),
  width = 0.07, size = 0.2)
p <- p + theme_bw(base_size = 10)
p <- p + theme(plot.title = element_text(size = 10),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), strip.background = element_blank(),
  strip.text = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))
p <- p + ylab("Posterior Modes")
p <- p + xlab("Selection Gradients")
p <- p + annotate(geom = "text", size = 2.5, x = 0, y = 0.7,
  label = paste("r = ", filter(c_table, variable == "Docility") %>%
  select(cor_test), sep = ''))
p <- p + ggtitle("Docility")
p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
```

![](figure/04_compare_doc) 
