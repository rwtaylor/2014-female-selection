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


# Fitness and Standardization

###Load Data


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
set.alignment('right', row.names = 'left')
load("data/analyses_data/raneffs_blups.RData")
# load("data/analyses_data/pca.RData")
fitness <- read.table(file = "data/fitness+competition.csv", sep = ',',
  header = TRUE, stringsAsFactors = FALSE)
fitness <- tbl_df(fitness)
```

## Merge fitness with blups


```r
fitness$ID <- as.character(fitness$ID)
fit_raneff_data <- inner_join(fitness, raneffs_blups, by = "ID")
fit_raneff_data <- fit_raneff_data %>%
  filter(!is.na(docility) & !is.na(aggression) & !is.na(activity))
```

## Relative Fitness
Calculate relative fitness for each year & population combination. Two populations (Grids).  
Three measures of fitness:

1. ars_all = Annual reproductive success over all litters (no. pups that survived overwinter)  
2. kprod   = Fecundity (kids produced)  
3. prop    = Offpsring overwinter survival (proportion of pups produced that survived overwinter)  


```r
# Calculate offspring overwitner survival
fit_raneff_data <- fit_raneff_data %>% mutate(prop = ars_all/kprod)

fit_raneff_data <- fit_raneff_data %>%
  group_by(Grid, Year, itt) %>%
  mutate(rel_ars = ars_all / mean(ars_all),
   rel_kpd = kprod / mean(kprod),
   rel_ows = prop / mean(prop)
   )
```
Now:

* rel_ars = relative ARS
* rel_kpd = relative fecunidty
* felOWS = relative offspring overwinter survival

## Standardize Variables
Standaradized to mean 0 and sd 1. Standardized variables renamed from xxx to xxx_s or xxx_sy (for standardized within year). Standardized within each BLUP itteration.


```r
# Standardize within itteration and year
fit_raneff_data <- fit_raneff_data %>%
  group_by(itt, Year, add = FALSE) %>%
  mutate(
    aggression_sy  = (aggression  - mean(aggression, na.rm = TRUE))  /
      sd(aggression, na.rm = TRUE),
    activity_sy    = (activity    - mean(activity, na.rm = TRUE))    /
      sd(activity, na.rm = TRUE),
    docility_sy    = (docility    - mean(docility, na.rm = TRUE))    /
      sd(docility, na.rm = TRUE),
    competition_sy = (competition - mean(competition, na.rm = TRUE)) /
      sd(competition, na.rm = TRUE)
      )
# Standardize within itteration
fit_raneff_data <- fit_raneff_data %>%
  group_by(itt, add = FALSE) %>%
  mutate(
    aggression_s  = (aggression  - mean(aggression, na.rm = TRUE))  /
      sd(aggression, na.rm = TRUE),
    activity_s    = (activity    - mean(activity, na.rm = TRUE))    /
      sd(activity, na.rm = TRUE),
    docility_s    = (docility    - mean(docility, na.rm = TRUE))    /
      sd(docility, na.rm = TRUE),
    competition_s = (competition - mean(competition, na.rm = TRUE)) /
      sd(competition, na.rm = TRUE)
      )
```

```r
fit_raneff_data %>%
  group_by(itt, add = FALSE) %>%
  summarise(
    v_agg = var(aggression_s, na.rm = TRUE),
    v_act = var(activity_s, na.rm = TRUE),
    v_doc = var(docility_s, na.rm = TRUE)
    ) %>%
  head(.,n=10) %>%
  pandoc.table(.)
```


-----------------------------
  itt   v_agg   v_act   v_doc
----- ------- ------- -------
    0       1       1       1

    1       1       1       1

    2       1       1       1

    3       1       1       1

    4       1       1       1

    5       1       1       1

    6       1       1       1

    7       1       1       1

    8       1       1       1

    9       1       1       1
-----------------------------

```r
fit_raneff_data %>%
  group_by(itt, Year, add = FALSE) %>%
  summarise(
    v_agg = var(aggression_sy, na.rm = TRUE),
    v_act = var(activity_sy, na.rm = TRUE),
    v_doc = var(docility_sy, na.rm = TRUE)
    ) %>%
  head(.,n=10) %>%
  pandoc.table(.)
```


------------------------------------
 itt   Year   v_agg   v_act   v_doc 
----- ------ ------- ------- -------
  0    2003     1       1       1   

  0    2004     1       1       1   

  0    2005     1       1       1   

  0    2006     1       1       1   

  0    2007     1       1       1   

  0    2008     1       1       1   

  0    2009     1       1       1   

  0    2010     1       1       1   

  1    2003     1       1       1   

  1    2004     1       1       1   
------------------------------------

## Sample Sizes

```r
fit_raneff_data %>%
  filter(itt == "1") %>%
  group_by(Grid, Year, add = FALSE) %>%
  summarise(n()) %>%
  pandoc.table(.)
```


-------------------
 Grid   Year   n() 
------ ------ -----
  KL    2003    3  

  KL    2004    6  

  KL    2005   15  

  KL    2006   19  

  KL    2007   20  

  KL    2008   28  

  KL    2009   24  

  KL    2010   16  

  SU    2003   10  

  SU    2004   15  

  SU    2005   26  

  SU    2006   21  

  SU    2007   14  

  SU    2008    9  

  SU    2009    9  

  SU    2010    2  
-------------------

```r
fit_raneff_data %>%
filter(itt == "1") %>%
  group_by(Year, add = FALSE) %>%
  summarise(n()) %>%
  pandoc.table(.)
```


------------
 Year   n() 
------ -----
 2003   13  

 2004   21  

 2005   41  

 2006   40  

 2007   34  

 2008   37  

 2009   33  

 2010   18  
------------


```r
save(fit_raneff_data, file = "data/analyses_data/fit_raneff_data.RData")
```
