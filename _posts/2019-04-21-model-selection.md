Model Selection
================

Introduction
------------

In this page I will show how to choose variables in the linear model, using a data set about college admission.

This dataset is created for prediction of Graduate Admissions from an Indian perspective.This dataset is inspired by the UCLA Graduate Dataset. The test scores and GPA are in the older format. The dataset is owned by Mohan S Acharya. The dataset contains several parameters which are considered important during the application for Masters Programs.

The parameters included are : 1. GRE Scores ( out of 340 ) 2. TOEFL Scores ( out of 120 ) 3. University Rating ( out of 5 ) 4. Statement of Purpose and Letter of Recommendation Strength ( out of 5 ) 5. Undergraduate GPA ( out of 10 ) 6. Research Experience ( either 0 or 1 ) 7. Chance of Admit ( ranging from 0 to 1 )

``` r
library(leaps)
```

    ## Warning: package 'leaps' was built under R version 3.5.3

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 3.5.3

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Warning: package 'foreach' was built under R version 3.5.3

    ## Loaded glmnet 2.0-16

``` r
rm(list = ls())
admission <- read.csv("Admission.csv")
n <- nrow(admission)
set.seed(123)
chance=admission$Chance.of.Admit
# Fit the entire model with all potential predictors
fit <- lm(chance~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR++CGPA+Research,data=admission)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = chance ~ GRE.Score + TOEFL.Score + University.Rating + 
    ##     SOP + LOR + +CGPA + Research, data = admission)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.266657 -0.023327  0.009191  0.033714  0.156818 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -1.2757251  0.1042962 -12.232  < 2e-16 ***
    ## GRE.Score          0.0018585  0.0005023   3.700 0.000240 ***
    ## TOEFL.Score        0.0027780  0.0008724   3.184 0.001544 ** 
    ## University.Rating  0.0059414  0.0038019   1.563 0.118753    
    ## SOP                0.0015861  0.0045627   0.348 0.728263    
    ## LOR                0.0168587  0.0041379   4.074 5.38e-05 ***
    ## CGPA               0.1183851  0.0097051  12.198  < 2e-16 ***
    ## Research           0.0243075  0.0066057   3.680 0.000259 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05999 on 492 degrees of freedom
    ## Multiple R-squared:  0.8219, Adjusted R-squared:  0.8194 
    ## F-statistic: 324.4 on 7 and 492 DF,  p-value: < 2.2e-16

Best Subset Selection
---------------------

``` r
all.fit <- regsubsets(chance~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR++CGPA+Research,data=admission) #require("leaps")
all <- summary(all.fit)
all$outmat
```

    ##          GRE.Score TOEFL.Score University.Rating SOP LOR CGPA Research
    ## 1  ( 1 ) " "       " "         " "               " " " " "*"  " "     
    ## 2  ( 1 ) "*"       " "         " "               " " " " "*"  " "     
    ## 3  ( 1 ) "*"       " "         " "               " " "*" "*"  " "     
    ## 4  ( 1 ) "*"       " "         " "               " " "*" "*"  "*"     
    ## 5  ( 1 ) "*"       "*"         " "               " " "*" "*"  "*"     
    ## 6  ( 1 ) "*"       "*"         "*"               " " "*" "*"  "*"     
    ## 7  ( 1 ) "*"       "*"         "*"               "*" "*" "*"  "*"

``` r
all$cp 
```

    ## [1] 115.474967  59.638214  29.336587  17.718935   7.427398   6.120850
    ## [7]   8.000000

``` r
all$bic
```

    ## [1] -741.5803 -785.0469 -808.8901 -815.9094 -821.9484 -819.0821 -812.9903

``` r
# Plot: Cp/BIC of best model vs number of variables
plot(all$cp ,xlab="Number of Variables",ylab="Cp", type="l")
k1 <- which.min(all$cp) #minimum Cp
points(k1,all$cp[k1], col="red",cex=2,pch=20)
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/vHxOCbvEmg04d3y8PsMV84u7hyukk0fjedVHUDWXWDQ!/b/dMMAAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
plot(all$bic ,xlab="Number of Variables ", ylab="BIC",type="l")
k2 <- which.min(all$bic) #minimum BIC
points(k2,all$bic[k2], col="red",cex=2,pch=20)
```

![]http://m.qpic.cn/psb?/V12764hq3b2u4E/HCKWQ2HLPe2MLRoQCzqRiBKEywSQ49XFKHKpVi*DYLs!/b/dFQBAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
# Visualze the best model at each Cp/BIC value
plot(all.fit,scale="Cp")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/TLfUSiybn7unj1kha3Z93K5YtXo98O8xtqb3MPpxK70!/b/dFIBAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
plot(all.fit,scale="bic")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/fBl*nBeddrkdCAbofThGRbUkvt7VBU27AyuWbFVd5c0!/b/dDQBAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
coef(all.fit,k1) # best overall model according to Cp
```

    ##       (Intercept)         GRE.Score       TOEFL.Score University.Rating 
    ##      -1.280013756       0.001852835       0.002807191       0.006427866 
    ##               LOR              CGPA          Research 
    ##       0.017287272       0.118999427       0.024353797

``` r
coef(all.fit,k2) # best overall model according to BIC
```

    ##  (Intercept)    GRE.Score  TOEFL.Score          LOR         CGPA 
    ## -1.335701769  0.001889180  0.003017357  0.019320252  0.122979785 
    ##     Research 
    ##  0.025164868

Accoring to Cp, we choose 6 variables: GRE.Score,TOEFL.Score, University.Rating, LOR, CGPA, Research.

Accoring to BIC, we choose 5 variables: GRE.Score,TOEFL.Score, LOR, CGPA, Research.

Forward Selection
-----------------

``` r
null = lm(chance~ 1, GRE.Score+TOEFL.Score+University.Rating+SOP+LOR++CGPA+Research,data=admission)
full = lm(chance~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR++CGPA+Research,data=admission)

# Forward stepwise selection using AIC
fwd.AIC = step(null, scope = formula(full), direction="forward")
```

    ## Start:  AIC=-1943.46
    ## chance ~ 1
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + CGPA               1    8.5494  1.6639 -2848.7
    ## + GRE.Score          1    7.4881  2.7251 -2602.0
    ## + TOEFL.Score        1    6.4740  3.7392 -2443.9
    ## + SOP                1    5.9123  4.3009 -2373.9
    ## + University.Rating  1    4.7138  5.4994 -2251.0
    ## + LOR                1    4.4587  5.7546 -2228.3
    ## + Research           1    3.4140  6.7993 -2144.9
    ## <none>                           10.2132 -1943.5
    ## 
    ## Step:  AIC=-2848.74
    ## chance ~ CGPA
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + GRE.Score          1   0.41948 1.2444 -2992.0
    ## + TOEFL.Score        1   0.40790 1.2559 -2987.4
    ## + LOR                1   0.26374 1.4001 -2933.0
    ## + Research           1   0.19567 1.4682 -2909.3
    ## + SOP                1   0.16351 1.5004 -2898.5
    ## + University.Rating  1   0.11579 1.5481 -2882.8
    ## <none>                           1.6639 -2848.7
    ## 
    ## Step:  AIC=-2991.99
    ## chance ~ CGPA + GRE.Score
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + LOR                1  0.148847 1.0955 -3053.7
    ## + TOEFL.Score        1  0.107996 1.1364 -3035.4
    ## + SOP                1  0.086309 1.1581 -3025.9
    ## + University.Rating  1  0.084631 1.1597 -3025.2
    ## + Research           1  0.028951 1.2154 -3001.8
    ## <none>                           1.2444 -2992.0
    ## 
    ## Step:  AIC=-3053.69
    ## chance ~ CGPA + GRE.Score + LOR
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + TOEFL.Score        1  0.081304 1.0142 -3090.2
    ## + SOP                1  0.071116 1.0244 -3085.2
    ## + University.Rating  1  0.057613 1.0379 -3078.7
    ## + Research           1  0.024025 1.0715 -3062.8
    ## <none>                           1.0955 -3053.7
    ## 
    ## Step:  AIC=-3090.24
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + SOP                1  0.066239 0.94798 -3122.0
    ## + University.Rating  1  0.052644 0.96158 -3114.9
    ## + Research           1  0.026401 0.98782 -3101.4
    ## <none>                           1.01422 -3090.2
    ## 
    ## Step:  AIC=-3122.01
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + SOP
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + Research           1  0.033513 0.91447 -3138.0
    ## + University.Rating  1  0.008956 0.93903 -3124.8
    ## <none>                           0.94798 -3122.0
    ## 
    ## Step:  AIC=-3138.01
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + SOP + Research
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + University.Rating  1 0.0086866 0.90578 -3140.8
    ## <none>                           0.91447 -3138.0
    ## 
    ## Step:  AIC=-3140.78
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + SOP + Research + 
    ##     University.Rating

``` r
summary(fwd.AIC)
```

    ## 
    ## Call:
    ## lm(formula = chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + 
    ##     SOP + Research + University.Rating, data = admission, subset = GRE.Score + 
    ##     TOEFL.Score + University.Rating + SOP + LOR + +CGPA + Research)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.121948 -0.023214  0.004287  0.021061  0.090636 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -1.3157924  0.0948607 -13.871  < 2e-16 ***
    ## CGPA               0.1063072  0.0057318  18.547  < 2e-16 ***
    ## GRE.Score          0.0018467  0.0004282   4.312 1.95e-05 ***
    ## LOR                0.0166697  0.0023122   7.210 2.13e-12 ***
    ## TOEFL.Score        0.0038091  0.0005868   6.491 2.08e-10 ***
    ## SOP                0.0137648  0.0036439   3.778 0.000178 ***
    ## Research           0.0215510  0.0050716   4.249 2.56e-05 ***
    ## University.Rating  0.0057892  0.0026651   2.172 0.030319 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04291 on 492 degrees of freedom
    ## Multiple R-squared:  0.9113, Adjusted R-squared:  0.9101 
    ## F-statistic: 722.2 on 7 and 492 DF,  p-value: < 2.2e-16

We could see according to AIC, the forward selection first choose CGPA because it has smallest RSS. With CGPA, we next add GRE. Score as follow... The final linear model with CGPA + GRE.Score + LOR + TOEFL.Score + SOP + Research + University.Rating has a good fit with relatively high r squared.

``` r
# Forward stepwise selection using BIC
fwd.BIC = step(null, scope = formula(full), direction="forward", k=log(n))
```

    ## Start:  AIC=-1939.25
    ## chance ~ 1
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + CGPA               1    8.5494  1.6639 -2840.3
    ## + GRE.Score          1    7.4881  2.7251 -2593.6
    ## + TOEFL.Score        1    6.4740  3.7392 -2435.4
    ## + SOP                1    5.9123  4.3009 -2365.5
    ## + University.Rating  1    4.7138  5.4994 -2242.6
    ## + LOR                1    4.4587  5.7546 -2219.9
    ## + Research           1    3.4140  6.7993 -2136.5
    ## <none>                           10.2132 -1939.2
    ## 
    ## Step:  AIC=-2840.31
    ## chance ~ CGPA
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + GRE.Score          1   0.41948 1.2444 -2979.3
    ## + TOEFL.Score        1   0.40790 1.2559 -2974.7
    ## + LOR                1   0.26374 1.4001 -2920.4
    ## + Research           1   0.19567 1.4682 -2896.7
    ## + SOP                1   0.16351 1.5004 -2885.8
    ## + University.Rating  1   0.11579 1.5481 -2870.2
    ## <none>                           1.6639 -2840.3
    ## 
    ## Step:  AIC=-2979.35
    ## chance ~ CGPA + GRE.Score
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + LOR                1  0.148847 1.0955 -3036.8
    ## + TOEFL.Score        1  0.107996 1.1364 -3018.5
    ## + SOP                1  0.086309 1.1581 -3009.1
    ## + University.Rating  1  0.084631 1.1597 -3008.3
    ## + Research           1  0.028951 1.2154 -2984.9
    ## <none>                           1.2444 -2979.3
    ## 
    ## Step:  AIC=-3036.83
    ## chance ~ CGPA + GRE.Score + LOR
    ## 
    ##                     Df Sum of Sq    RSS     AIC
    ## + TOEFL.Score        1  0.081304 1.0142 -3069.2
    ## + SOP                1  0.071116 1.0244 -3064.2
    ## + University.Rating  1  0.057613 1.0379 -3057.6
    ## + Research           1  0.024025 1.0715 -3041.7
    ## <none>                           1.0955 -3036.8
    ## 
    ## Step:  AIC=-3069.17
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + SOP                1  0.066239 0.94798 -3096.7
    ## + University.Rating  1  0.052644 0.96158 -3089.6
    ## + Research           1  0.026401 0.98782 -3076.1
    ## <none>                           1.01422 -3069.2
    ## 
    ## Step:  AIC=-3096.73
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + SOP
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## + Research           1  0.033513 0.91447 -3108.5
    ## <none>                           0.94798 -3096.7
    ## + University.Rating  1  0.008956 0.93903 -3095.3
    ## 
    ## Step:  AIC=-3108.51
    ## chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + SOP + Research
    ## 
    ##                     Df Sum of Sq     RSS     AIC
    ## <none>                           0.91447 -3108.5
    ## + University.Rating  1 0.0086866 0.90578 -3107.1

``` r
summary(fwd.BIC)
```

    ## 
    ## Call:
    ## lm(formula = chance ~ CGPA + GRE.Score + LOR + TOEFL.Score + 
    ##     SOP + Research, data = admission, subset = GRE.Score + TOEFL.Score + 
    ##     University.Rating + SOP + LOR + +CGPA + Research)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.118984 -0.023038  0.002536  0.021265  0.086407 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.3145663  0.0952161 -13.806  < 2e-16 ***
    ## CGPA         0.1079295  0.0057044  18.920  < 2e-16 ***
    ## GRE.Score    0.0017899  0.0004290   4.172 3.57e-05 ***
    ## LOR          0.0172462  0.0023055   7.480 3.42e-13 ***
    ## TOEFL.Score  0.0038378  0.0005889   6.517 1.77e-10 ***
    ## SOP          0.0184780  0.0029384   6.288 7.07e-10 ***
    ## Research     0.0216374  0.0050905   4.251 2.55e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04307 on 493 degrees of freedom
    ## Multiple R-squared:  0.9105, Adjusted R-squared:  0.9094 
    ## F-statistic: 835.5 on 6 and 493 DF,  p-value: < 2.2e-16

We could see according to BIC, the forward selection first choose CGPA because it has smallest RSS. With CGPA, we next add GRE. Score as follow... The final linear model with CGPA + GRE.Score + LOR + TOEFL.Score + SOP + Research has a good fit with relatively high r squared.

Ridge Regression
----------------

``` r
x <- model.matrix(chance~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR++CGPA+Research,data=admission)[,-1] 
y <- chance
ridgefit <- glmnet(x,y,alpha=0,lambda=0.01) 
coef(ridgefit)
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s0
    ## (Intercept)       -1.244459677
    ## GRE.Score          0.002136877
    ## TOEFL.Score        0.003269662
    ## University.Rating  0.007694343
    ## SOP                0.005106232
    ## LOR                0.017486694
    ## CGPA               0.096013739
    ## Research           0.024800136

``` r
ridgefit <- glmnet(x,y,alpha=0,lambda=10000) 
coef(ridgefit)
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s0
    ## (Intercept)       7.216277e-01
    ## GRE.Score         1.427720e-07
    ## TOEFL.Score       2.592207e-07
    ## University.Rating 1.201004e-06
    ## SOP               1.373776e-06
    ## LOR               1.387707e-06
    ## CGPA              2.903313e-06
    ## Research          2.186117e-06

``` r
ridgefit <- glmnet(x,y,alpha=0) #fit a range of lambda
ridgefit$lambda # see the lambda's that are being fit
```

    ##   [1] 124.41946087 113.36637889 103.29522225  94.11875941  85.75750823
    ##   [6]  78.13904756  71.19738995  64.87241006  59.10932395  53.85821453
    ##  [11]  49.07359919  44.71403589  40.74176419  37.12237815  33.82452839
    ##  [16]  30.81965051  28.08171771  25.58701531  23.31393539  21.24278963
    ##  [21]  19.35563875  17.63613715  16.06939133  14.64183090  13.34109099
    ##  [26]  12.15590524  11.07600812  10.09204609   9.19549653   8.37859396
    ##  [31]   7.63426277   6.95605591   6.33809907   5.77503982   5.26200120
    ##  [36]   4.79453952   4.36860584   3.98051094   3.62689332   3.30469011
    ##  [41]   3.01111055   2.74361179   2.49987688   2.27779471   2.07544170
    ##  [46]   1.89106518   1.72306816   1.56999554   1.43052146   1.30343788
    ##  [51]   1.18764405   1.08213702   0.98600295   0.89840917   0.81859697
    ##  [56]   0.74587508   0.67961359   0.61923859   0.56422714   0.51410276
    ##  [61]   0.46843129   0.42681714   0.38889988   0.35435109   0.32287152
    ##  [66]   0.29418850   0.26805360   0.24424046   0.22254281   0.20277272
    ##  [71]   0.18475895   0.16834548   0.15339013   0.13976337   0.12734718
    ##  [76]   0.11603401   0.10572587   0.09633347   0.08777547   0.07997774
    ##  [81]   0.07287274   0.06639892   0.06050023   0.05512555   0.05022835
    ##  [86]   0.04576620   0.04170046   0.03799591   0.03462045   0.03154487
    ##  [91]   0.02874251   0.02618910   0.02386254   0.02174265   0.01981110
    ##  [96]   0.01805113   0.01644752   0.01498637   0.01365502   0.01244195

``` r
dim(coef(ridgefit)) 
```

    ## [1]   8 100

``` r
#coef(ridgefit) is a matrix with p rows (number of coefficents)
#and number of columns = number of lambda's (automatically selected)   

# Coefficient Plot
plot(ridgefit,xvar="lambda")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/s54AwYm9tEqHWeWpy*w2b.e061iaj*qhV05vpvrEW3A!/b/dL8AAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
# Cross-validation
cv.out <- cv.glmnet(x,y,alpha=0) #default is 10-fold CV
coef(cv.out,s="lambda.min") #coefficient estimates using min lambda 
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                              1
    ## (Intercept)       -1.227654881
    ## GRE.Score          0.002175946
    ## TOEFL.Score        0.003368139
    ## University.Rating  0.008115049
    ## SOP                0.006002967
    ## LOR                0.017603863
    ## CGPA               0.090814311
    ## Research           0.025020642

``` r
coef(cv.out) 
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                              1
    ## (Intercept)       -1.004106286
    ## GRE.Score          0.002153219
    ## TOEFL.Score        0.003598681
    ## University.Rating  0.011201989
    ## SOP                0.011475809
    ## LOR                0.017145826
    ## CGPA               0.059550849
    ## Research           0.026054024

``` r
#default is using lambda according to the 1se rule
#Equivalently, coef(cv.out,s="lambda.1se")

# CV Plot
plot(cv.out)
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/.8Ildzyh58TunQ0yHeeuW4HI1WYaeioOJS06UilO6ds!/b/dFQBAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
# Refit the model using the best lambda according to CV results
bestlam <- cv.out$lambda.min #min lambda 
ridgefit <- glmnet(x,y,alpha=0,lambda=bestlam) 
coef(ridgefit) #this result should be very close to coef(cv.out,s="lambda.min")
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s0
    ## (Intercept)       -1.228428264
    ## GRE.Score          0.002180334
    ## TOEFL.Score        0.003371427
    ## University.Rating  0.008171788
    ## SOP                0.006010054
    ## LOR                0.017567549
    ## CGPA               0.090695147
    ## Research           0.024985443

Generally, the R square value of ridge regression equation is slightly lower than that of ordinary regression analysis, but the significance of regression coefficient is often significantly higher than that of ordinary regression.

Essentially, it is an improved least squares estimation method. By abandoning the unbiasedness of the least squares method, the regression coefficient can be obtained at the cost of losing part of the information and reducing the accuracy, which is more realistic and reliable.

Lasso
-----

As with ridge regression, the lasso shrinks the coefficient estimates towards zero.

``` r
lassofit <- glmnet(x,y,alpha=1) #fit a range of lambda

# Coefficient Plot
plot(lassofit,xvar="lambda")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/E1Rp46PjHePI91.WrXhtaPTjcGj3qYcuwhB.m3XpOCM!/b/dLsAAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
# Cross-validation
cv.out <- cv.glmnet(x,y,alpha=1)
coef(cv.out,s="lambda.min")
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                              1
    ## (Intercept)       -1.269295665
    ## GRE.Score          0.001852143
    ## TOEFL.Score        0.002740569
    ## University.Rating  0.005791558
    ## SOP                0.001405501
    ## LOR                0.016518422
    ## CGPA               0.118656405
    ## Research           0.023464983

``` r
coef(cv.out) #Equivalently, coef(cv.out,s="lambda.1se")
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                              1
    ## (Intercept)       -1.152721500
    ## GRE.Score          0.001747676
    ## TOEFL.Score        0.002121739
    ## University.Rating  0.002924462
    ## SOP                .          
    ## LOR                0.010528584
    ## CGPA               0.121606413
    ## Research           0.009284121

``` r
# CV Plot
plot(cv.out)
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/w1Tu2tfjROaOQsJll.A0oz.F53ktrDg60npTxE9ENIE!/b/dL8AAAAAAAAA&bo=oALgAQAAAAADB2E!&rf=viewer_4)

``` r
# Refit the model using the best lambda according to CV results
bestlam <- cv.out$lambda.min #min lambda 
lassofit <- glmnet(x,y,alpha=1,lambda=bestlam) 
coef(lassofit) #this result should be very close to coef(cv.out,s="lambda.min")
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s0
    ## (Intercept)       -1.268106057
    ## GRE.Score          0.001847229
    ## TOEFL.Score        0.002742433
    ## University.Rating  0.005807981
    ## SOP                0.001416626
    ## LOR                0.016493680
    ## CGPA               0.118673214
    ## Research           0.023498975

``` r
# Alternatively, use the 1se rule:
bestlam <- cv.out$lambda.1se #lambda according to the 1se rule
lassofit <- glmnet(x,y,alpha=1,lambda=bestlam) 
coef(lassofit) #this result should be very close to coef(cv.out,s="lambda.1se")
```

    ## 8 x 1 sparse Matrix of class "dgCMatrix"
    ##                             s0
    ## (Intercept)       -1.155029298
    ## GRE.Score          0.001761582
    ## TOEFL.Score        0.002113298
    ## University.Rating  0.002927255
    ## SOP                .          
    ## LOR                0.010522493
    ## CGPA               0.121472363
    ## Research           0.009237553
