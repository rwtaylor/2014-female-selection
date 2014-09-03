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
template: templates/latex_supplementary.template
default-image-extension: pdf
toc-depth: 4
toc:
-->



# Supplementary Material
Supplementary material for:

R. W. Taylor, M. M. Humphries, S. Boutin, J. C. Gorrell, D. W. Coltman, and A. G. McAdam. Selection on female behaviour fluctuates with offspring environment. Journal of Evolutionary Biology, 2014.



Data and code used in the analyses are available on github

    https://github.com/rwtaylor/2014-female-selection

and archived on Zenodo

    http://dx.doi.org/10.5281/zenodo.10908



## Table S1

--------------------------------------------------------------------------------------------
  Behavior   Overall Repeatability   Repeatability Within Years   Repeatability Across Years
---------- ----------------------- ---------------------------- ----------------------------
Aggression      0.39 (0.24 – 0.46)           0.23 (0.16 – 0.42)           0.50 (0.37 – 0.58)

  Activity      0.48 (0.35 – 0.52)           0.33 (0.19 – 0.45)           0.54 (0.43 – 0.62)

  Docility      0.38 (0.33 – 0.40)           0.32 (0.24 – 0.37)           0.40 (0.36 – 0.42)
--------------------------------------------------------------------------------------------

Table: Repeatability within and across years. Repeatabilities were estimated using linear mixed effects models with individual as a random effect. Repeatability across years was estimated after randomly sampling, for each individual, only one behavioural measure per year.

## Table S2

-----------------------------------------------------------
 Study Area   Year   N juveniles   N survive   Competition 
------------ ------ ------------- ----------- -------------
     KL       2003       79           10          7.90     

     KL       2004       64           21          3.05     

     KL       2005       140          42          3.33     

     KL       2006       102          16          6.38     

     KL       2007       103          24          4.29     

     KL       2008       107          12          8.92     

     KL       2009       84           17          4.94     

     KL       2010       138          34          4.06     

     SU       2003       70           15          4.67     

     SU       2004       65           16          4.06     

     SU       2005       109          35          3.11     

     SU       2006       90            7          11.62    

     SU       2007       87           21          4.14     

     SU       2008       67            1          67.00    

     SU       2009       39            5          7.80     

     SU       2010       90           29          3.10     
-----------------------------------------------------------

Table: Variation in competition across years and study areas. N juveniles is the number of juveniles produced in the study area. N survive is the number of juveniles that recruited into the population by surviving overwinter to the next spring. Competition is the ratio of juveniles produced to juveniles recruited.

## Table S3

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

Table: Generalized linear mixed model results showing the interactions between year and the behavioural traits (BLUPs). Significance was calculated with Wald tests from an analysis of deviance. GLMMs were fitted with squirrel identity as a random effect and assumed a Poisson error distribution.

## Table S4

--------------------------------------------------------------------------------
 Year   N       Aggression              Acitivity               Docility        
------ --- --------------------- ----------------------- -----------------------
 2003  18  0.03 (-0.92 to 0.84)   0.45 (-0.17 to 1.32)    -0.08 (-0.26 to 0.06) 

 2004  26  -0.30 (-0.76 to 0.14)  0.03 (-0.32 to 0.67)    0.05 (-0.06 to 0.11)  

 2005  50  -0.18 (-0.45 to 0.10)  0.13 (-0.16 to 0.40)    0.03 (-0.02 to 0.07)  

 2006  48  0.39 (-0.04 to 0.79)   0.08 (-0.39 to 0.45)    -0.04 (-0.12 to 0.00) 

 2007  40  0.00 (-0.35 to 0.36)   0.11 (-0.22 to 0.42)    0.00 (-0.05 to 0.05)  

 2008  45  0.44 (-0.12 to 0.95)   -0.26 (-0.75 to 0.40)   -0.02 (-0.08 to 0.05) 

 2009  35  0.12 (-0.33 to 0.58)  -0.58 (-0.93 to -0.02)* -0.06 (-0.11 to -0.01)*

 2010  34  0.06 (-0.30 to 0.35)   0.03 (-0.26 to 0.30)   -0.05 (-0.08 to -0.02)*
--------------------------------------------------------------------------------

Table: Non-standardized linear selection gradients (β′) for female behavioural traits through annual reproductive success after accounting for behavioural measurement uncertainty (see methods for details). Posterior modes are given with the lower and upper bounds of the 0.95 highest posterior density interval. 
## Table S5

------------------------------------------------------------------------------
 Year   N       Aggression             Acitivity              Docility        
------ --- --------------------- --------------------- -----------------------
 2003  18  0.03 (-0.76 to 0.71)  0.79 (-0.24 to 1.38)   -0.38 (-1.12 to 0.31) 

 2004  26  -0.24 (-0.81 to 0.08) 0.02 (-0.36 to 0.62)   0.21 (-0.26 to 0.52)  

 2005  50  -0.19 (-0.47 to 0.07) 0.16 (-0.12 to 0.41)   0.10 (-0.09 to 0.34)  

 2006  48  0.39 (-0.04 to 0.72)  0.08 (-0.37 to 0.44)  -0.25 (-0.58 to -0.03)*

 2007  40  -0.01 (-0.32 to 0.34) 0.09 (-0.23 to 0.42)   0.01 (-0.23 to 0.23)  

 2008  45  0.46 (-0.06 to 0.95)  -0.18 (-0.72 to 0.40)  -0.08 (-0.37 to 0.25) 

 2009  35  0.16 (-0.29 to 0.59)  -0.55 (-0.92 to 0.01) -0.30 (-0.53 to -0.05)*

 2010  34  -0.04 (-0.27 to 0.32) 0.04 (-0.25 to 0.30)  -0.23 (-0.35 to -0.10)*
------------------------------------------------------------------------------

Table: SD-standardized linear selection gradients (β′) for female behavioural traits through annual reproductive success after accounting for behavioural measurement uncertainty (see methods for details). Posterior modes are given with the lower and upper bounds of the 0.95 highest posterior density interval. 

## Table S6

----------------------------------------
 Year   N   Mean_docility   Mean_stan_B 
------ --- --------------- -------------
 2003  18       17.29          -1.46    

 2004  26       17.31          0.95     

 2005  50       17.25          0.55     

 2006  48       16.77          -0.70    

 2007  40       16.77          0.05     

 2008  45       16.9           -0.27    

 2009  35       16.79          -0.97    

 2010  34       17.19          -0.87    
----------------------------------------

Table: Mean-standardized linear selection gradients (β′) for female docility through annual reproductive success after accounting for behavioural measurement uncertainty (see methods for details). Posterior modes are given with the lower and upper bounds of the 0.95 highest posterior density interval. Activity and aggression are synthetic behavioural measures, with means of zero and so can not be mean standardized.

## Table S7

-----------------------------------------------------------------------------
 Year        Aggression               Activity                Docility       
------ ----------------------- ----------------------- ----------------------
 2003   -0.26 (-1.66 to 0.61)   0.90 (-0.25 to 1.76)   -0.11 (-0.36 to 0.10) 

 2004  -0.75 (-1.36 to -0.05)*  0.57 (-0.18 to 1.35)    0.07 (-0.13 to 0.24) 

 2005  -0.56 (-1.02 to -0.06)*  0.46 (-0.08 to 0.98)    0.05 (-0.04 to 0.16) 

 2006   0.98 ( 0.20 to 1.86)*   -0.33 (-1.11 to 0.48)  -0.10 (-0.22 to 0.04) 

 2007   -0.09 (-0.92 to 0.49)   0.19 (-0.53 to 0.88)    0.01 (-0.11 to 0.13) 

 2008   1.11 (-0.36 to 2.30)    -0.60 (-1.46 to 0.94)  -0.04 (-0.21 to 0.13) 

 2009   0.75 (-0.09 to 1.39)   -1.38 (-2.11 to -0.60)* -0.12 (-0.23 to 0.00)*

 2010   -0.03 (-0.74 to 0.67)   0.12 (-0.54 to 0.77)   -0.05 (-0.12 to 0.04) 
-----------------------------------------------------------------------------

Table: Non-standardized linear selection gradients (β′) ± standard errors for female behavioural traits through annual reproductive success. Selection gradients were estimated from single realized BLUPs of behavioural traits. Significance was assessed using the sim() function in the R package arm.

## Table S8

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

Table: Mean-standardized linear selection gradients (β′) ± standard errors for female docility. Selection gradients were estimated from single realized BLUPs of behavioural traits. Significance was assessed using the sim() function in the R package arm. Activity and aggression are synthetic behavioural measures, with means of zero and so can not be mean standardized.

## Table S9

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

Table: Selection on female aggression and docility, through annual reproductive success, interacted significantly with juvenile offspring competition for vacant territories after accounting for behavioural measurement uncertainty. The generalized linear mixed model for annual reproductive success assumed a Poisson error distribution, while the model for offspring overwinter survival assumed a binomial error distribution weighted by fecundity. Random effects for identity and grid-year were included in both models to account for pseudoreplication. Presented are posterior modes of the distribution of coefficients from 1000 models of 1000 sets of individual random effects of each behavioural trait generated with MCMCglmm.

## Table S10

-------------------------------------------------------------
&nbsp;                                Est ± se      Z       P
------------------------------- -------------- ------ -------
**Intercept**                    1.362 ± 0.091 14.937 < 0.001

**Competition**                 -0.017 ± 0.161 -0.106    0.92

**Aggression**                  -0.058 ± 0.049 -1.176    0.24

**Activity**                     0.030 ± 0.050  0.594    0.55

**Docility**                     0.030 ± 0.041  0.724    0.47

**Aggression^2**                 0.036 ± 0.053  0.672    0.50

**Activity^2**                  -0.064 ± 0.051 -1.248    0.21

**Docility^2**                   0.005 ± 0.026  0.209    0.83

**Aggression x Competition**    -0.067 ± 0.117 -0.579    0.56

**Activity x Competition**       0.008 ± 0.088  0.086    0.93

**Docility x Competition**      -0.116 ± 0.087 -1.325    0.19

**Aggression x Activity**        0.024 ± 0.075  0.319    0.75

**Aggression^2 x                 0.153 ± 0.149  1.029    0.30
Competition**                                                

**Activity^2 x Competition**     0.079 ± 0.145  0.545    0.59

**Docility^2 x Competition**    -0.067 ± 0.055 -1.218    0.22

**Agg. x Act. x Competition**   -0.252 ± 0.240 -1.051    0.29
-------------------------------------------------------------

Table: There was no evidence for selection on female behaviour through fecundity. Results from a generalized linear mixed model of fecundity with individual and grid-year as random effects. The model was fitted with a Poisson error distribution. This model ignores uncertainty around the BLUPs used for each indivdiual's behavioural measure.

## Figure S1

![](final_figures/F6) 

High activity females (black lines) were favoured when they were also aggressive in high competition environments (left panel). But, for low activity females, there was stabilizing selection for low to moderate levels of aggression through offspring overwinter survival (b = –3.11 ± 1.36, Z = –2.29, P = 0.02). This positive correlational selection was not present in low competition environments (right panel). The interaction between aggression, activity and competition was significant for offspring overwinter survival (b = 3.95 ± 1.86, Z = 2.13, P = 0.03). Predicted values from the model for offspring overwinter survival (Table 3) were plotted for the highest and lowest values of competition we measured. Dashed lines represent 95% confidence intervals around the predicted values.

## Figure S2

![](final_figures/F5) 

The predicted relative fitness surface, as a function of female aggression and activity, was saddle shaped when competition among juveniles for vacant territories was high. Darker squares correspond to higher fitness. Fitness peaks (+) occurred for females with high aggression and activity, and for females with low-moderate aggression and low activity. Predicted values from a linear model of relative fitness with the same predictors as the models in Table 3, were plotted for the highest levels of competition measured and mean docility.
