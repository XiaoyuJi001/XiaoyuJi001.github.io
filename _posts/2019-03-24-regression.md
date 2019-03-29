---
layout: post
title: "Regression"
date: 2019-03-24
title: "Regression"
output: github_document
---

Regression
================

### Introduction

Demographic data from the U.S. Census Bureau. The document (insurance. csv) contains 1338 cases in US, which are beneficiaries of insurance schemes currently registered and insurance scheme and the characteristics representing patient characteristics and the total medical costs included in the previous year's plan. These characteristics are:

*age*: This is an integer indicating the age of the main beneficiaries (excluding those over 64 years of age, who are generally paid by the government).

*sex*: This is the gender of the policyholder, either male or female.

*bmi*: This is the Body Mass Index (BMI), which provides a way to determine whether a person's weight is overweight or underweight relative to height. BMI is equal to weight (kg) divided by the square of height (m). An ideal BMI index ranges from 18.5 to 24.9.

*children*: This is an integer representing the number of children/dependants included in the insurance plan.

*smoker*: Judge yes or no according to whether the insured smokes or not.

*region*: According to the beneficiary's residence in the United States, it is divided into four geographical regions: northeast, southeast, southwest and northwest.

It is very important to link these variables with the settled medical costs. We will make some effort below

### model 1

``` r
insurance <- read.csv("insurance.csv",stringsAsFactors = TRUE)
str(insurance)
```

    ## 'data.frame':    1338 obs. of  7 variables:
    ##  $ age     : int  19 18 28 33 32 31 46 37 37 60 ...
    ##  $ sex     : Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
    ##  $ bmi     : num  27.9 33.8 33 22.7 28.9 ...
    ##  $ children: int  0 1 3 0 0 0 1 3 2 0 ...
    ##  $ smoker  : Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
    ##  $ region  : Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
    ##  $ charges : num  16885 1726 4449 21984 3867 ...

``` r
# check the distribution of the charges
summary(insurance$charges)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1122    4740    9382   13270   16640   63770

Because the average is much larger than the median, it shows that the distribution of insurance premiums is right-sided, which can be confirmed by histogram.

![](regression_files/figure-markdown_github/pressure-1.png)

Before using regression models to fit data, it is necessary to determine how independent variables are related to dependent variables and independent variables. The correlation matrix provides a quick overview of these relationships. Given a set of variables, it can provide a correlation coefficient for the relationship between each pair of variables.

``` r
cor(insurance[c("age","bmi","children","charges")])
```

    ##                age       bmi   children    charges
    ## age      1.0000000 0.1092719 0.04246900 0.29900819
    ## bmi      0.1092719 1.0000000 0.01275890 0.19834097
    ## children 0.0424690 0.0127589 1.00000000 0.06799823
    ## charges  0.2990082 0.1983410 0.06799823 1.00000000

Perhaps it is more helpful to visualize the relationship between features by using scatter plots. Although we can create a scatter plot for every possible relationship, it can become cumbersome for a large number of features.

Another method is to create a scatter plot matrix, which simply arranges a set of scatter plots in the grid, which contains a variety of factors closely related to each other. It shows the relationship between each factor.

``` r
pairs(insurance[c("age","bmi","children","charges")])
```

![](regression_files/figure-markdown_github/unnamed-chunk-3-1.png) Like the correlation coefficient matrix, the scatter plot of the intersection points of each row and column shows the correlation between the two variables of the row and column. Since the X and Y axes above and below the diagonal are commutative, the diagonal graphs above and below are symmetric.

Despite some seemingly random dots, some seem to be showing a certain trend. The relationship between age and charges presents several relative lines, while scatter plots of BMI and charges constitute two different groups.

A linear regression model called ins\_model is fitted by R, which links six virables to the total medical cost.

``` r
ins_model <- lm(charges~age+children+bmi+sex+smoker+region,data=insurance)
ins_model 
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + children + bmi + sex + smoker + 
    ##     region, data = insurance)
    ## 
    ## Coefficients:
    ##     (Intercept)              age         children              bmi  
    ##        -11938.5            256.9            475.5            339.2  
    ##         sexmale        smokeryes  regionnorthwest  regionsoutheast  
    ##          -131.3          23848.5           -353.0          -1035.0  
    ## regionsouthwest  
    ##          -960.1

We specify only six variables, but we output eight coefficients in addition to intercept terms. This happens because the LM () function automatically applies a technique called dummy coding to variables of each factor type contained in the model. When a dummy coded variable is added to the regression model, a category is always excluded as a reference category. Then, the estimated coefficients are interpreted relative to the reference category. In our model, R automatically retains sexfemale, smokerno and regionnortheast variables, making female non-smokers in Northeast China a reference group. As a result, compared with women, men spend less than 131.30 dollar a year on health care and smokers spend an average of $23848.50 more than non-smokers. In addition, the coefficients of the other three areas in the model are negative, which means that the Northeast tends to have the highest average medical costs.

The results of the linear regression model are logical. Older age, smoking and obesity are often associated with other health problems, and additional family members or dependants may lead to increased visits and increased costs of preventive health care (such as vaccination, annual physical examination).

By typing ins\_model, we can get estimates of parameters that tell us about how independent variables relate to dependent variables. But they don't tell us how good it is to use the model to fit the data at all. To evaluate the performance of the model, the summary () command can be used to analyze the stored regression model.

``` r
summary(ins_model)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + children + bmi + sex + smoker + 
    ##     region, data = insurance)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11304.9  -2848.1   -982.1   1393.9  29992.8 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -11938.5      987.8 -12.086  < 2e-16 ***
    ## age                256.9       11.9  21.587  < 2e-16 ***
    ## children           475.5      137.8   3.451 0.000577 ***
    ## bmi                339.2       28.6  11.860  < 2e-16 ***
    ## sexmale           -131.3      332.9  -0.394 0.693348    
    ## smokeryes        23848.5      413.1  57.723  < 2e-16 ***
    ## regionnorthwest   -353.0      476.3  -0.741 0.458769    
    ## regionsoutheast  -1035.0      478.7  -2.162 0.030782 *  
    ## regionsouthwest   -960.0      477.9  -2.009 0.044765 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6062 on 1329 degrees of freedom
    ## Multiple R-squared:  0.7509, Adjusted R-squared:  0.7494 
    ## F-statistic: 500.8 on 8 and 1329 DF,  p-value: < 2.2e-16

For the regression model of real world data, it is not uncommon that the R-square value is quite low, so the R-square value of 0.75 is actually quite good.

### model 2

In linear regression, the relationship between independent variables and dependent variables is assumed to be linear, but this is not necessarily correct. For example, for all age groups, the impact of age on medical costs may not be constant; for the oldest population, treatment may be too expensive.

Suppose we have a presentiment that the effect of a feature is not cumulative, but only when the value of the feature reaches a given threshold. For example, for individuals within the normal weight range, the impact of BMI on medical costs may be zero, but for obese people (i.e., BMI is no less than 30), it may be closely related to higher costs. We can establish this relationship by creating a binary indicator variable, that is, if the BMI is greater than or equal to 30, then set it to 1, otherwise set it to 0.

So far, we have only considered the individual effects (contributions) of each feature on the results. What if some characteristics have a comprehensive effect on dependent variables? Smoking and obesity may have harmful effects, respectively, but it is reasonable to assume that their combined effects may be worse than each of their individual effects.

When two characteristics have a common influence, this is called interaction. If the interaction between two variables is suspected, the hypothesis can be tested by adding their interaction to the model, and the influence of interaction can be specified by using the formula grammar in R. In order to reflect the interaction between obesity index (bmi30) and smoker, we can write a formula in this form: charge ~ bmi30 \* smoker.

*Our improvements*:

Adding a polynomial Age Term

Create an Indicator for Obesity

Interaction between obesity and smoking

``` r
bmi30 <-ifelse(insurance$bmi>=30,1,0)
ins_model2 <-lm(charges~age+age^2+children+bmi+sex+bmi30*smoker+region,data=insurance)
summary(ins_model2)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + age^2 + children + bmi + sex + bmi30 * 
    ##     smoker + region, data = insurance)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -18234.3  -1826.1  -1251.6   -447.5  24803.9 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -4745.546    959.685  -4.945 8.59e-07 ***
    ## age               263.242      8.805  29.897  < 2e-16 ***
    ## children          520.402    101.958   5.104 3.81e-07 ***
    ## bmi               115.035     34.560   3.329 0.000897 ***
    ## sexmale          -491.179    246.563  -1.992 0.046565 *  
    ## bmi30            -865.057    425.775  -2.032 0.042381 *  
    ## smokeryes       13402.363    443.910  30.192  < 2e-16 ***
    ## regionnorthwest  -266.836    352.410  -0.757 0.449079    
    ## regionsoutheast  -825.000    354.800  -2.325 0.020209 *  
    ## regionsouthwest -1224.315    353.684  -3.462 0.000554 ***
    ## bmi30:smokeryes 19794.852    610.092  32.446  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4485 on 1327 degrees of freedom
    ## Multiple R-squared:  0.8639, Adjusted R-squared:  0.8628 
    ## F-statistic: 842.1 on 10 and 1327 DF,  p-value: < 2.2e-16

Analyzing the fitting statistics of the model helps to determine whether our changes improve the performance of the regression model. Compared with our first model, the R-square value has increased from 0.75 to about 0.87, and our model can now explain 87% of the changes in medical costs. In addition, our theory about the form of model function seems to be validated. The higher order term age2 is statistically significant, and the obesity index BMI 30 is also significant. The interaction between obesity and smoking shows a huge impact. In addition to the increased cost of smoking alone of more than $13404, obese smokers spend an additional $19810 a year, which may indicate that smoking exacerbates obesity-related diseases.

### piecewise

From above analysis, we guess there be strong link between age and charges. In this part, I will analysis the relationship between age and the costs by piecewise.

``` r
charges=insurance$charges
age=insurance$age
fit = lm( charges~ 0 + cut(age,6)) 
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ 0 + cut(age, 6))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -9685  -6876  -5612   5013  48303 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## cut(age, 6)(18,25.7]     9087.0      661.8   13.73   <2e-16 ***
    ## cut(age, 6)(25.7,33.3]  10267.6      785.9   13.06   <2e-16 ***
    ## cut(age, 6)(33.3,41]    11784.2      808.6   14.57   <2e-16 ***
    ## cut(age, 6)(41,48.7]    15651.7      824.9   18.98   <2e-16 ***
    ## cut(age, 6)(48.7,56.3]  16048.0      775.3   20.70   <2e-16 ***
    ## cut(age, 6)(56.3,64]    19312.0      839.9   22.99   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11580 on 1332 degrees of freedom
    ## Multiple R-squared:  0.5864, Adjusted R-squared:  0.5846 
    ## F-statistic: 314.8 on 6 and 1332 DF,  p-value: < 2.2e-16

``` r
#plotting
agelims = range(age) 
age.grid = seq(from=agelims[1], to=agelims[2]) 
preds = predict(fit, newdata=list(age=age.grid), se=TRUE) 
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(age, charges, xlim=agelims, cex=.5, col="darkgrey") 
lines(age.grid, preds$fit, lwd=2, col="darkblue") 
matlines(age.grid, se.bands, lwd=1, col="darkblue", lty=3)
title("constant piecewise")
```

![](regression_files/figure-markdown_github/unnamed-chunk-7-1.png) \#\#\# Cubic Spline

``` r
library(splines)
fit = lm(charges ~ bs(age,knots=c(18,26,34,42,48,56,64),degree=3)) # knots at age 25,40,60
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ bs(age, knots = c(18, 26, 34, 42, 48, 
    ##     56, 64), degree = 3))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -8473  -6708  -5845   5228  47770 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                                              Estimate
    ## (Intercept)                                                     22296
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)1    -14638
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)2    -11852
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)3    -13363
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)4    -10298
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)5     -9629
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)6     -5133
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)7     -7759
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)8     -1852
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)9        NA
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)10       NA
    ##                                                              Std. Error
    ## (Intercept)                                                        2096
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)1        2428
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)2        3127
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)3        3162
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)4        3017
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)5        2699
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)6        3054
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)7        2948
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)8        4086
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)9          NA
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)10         NA
    ##                                                              t value
    ## (Intercept)                                                   10.635
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)1   -6.030
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)2   -3.790
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)3   -4.226
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)4   -3.413
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)5   -3.568
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)6   -1.681
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)7   -2.632
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)8   -0.453
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)9       NA
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)10      NA
    ##                                                              Pr(>|t|)    
    ## (Intercept)                                                   < 2e-16 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)1  2.12e-09 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)2  0.000157 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)3  2.54e-05 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)4  0.000662 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)5  0.000373 ***
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)6  0.093011 .  
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)7  0.008574 ** 
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)8  0.650472    
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)9        NA    
    ## bs(age, knots = c(18, 26, 34, 42, 48, 56, 64), degree = 3)10       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11570 on 1329 degrees of freedom
    ## Multiple R-squared:  0.09298,    Adjusted R-squared:  0.08752 
    ## F-statistic: 17.03 on 8 and 1329 DF,  p-value: < 2.2e-16

``` r
## plotting
preds = predict(fit, newdata=list(age=age.grid), se=TRUE) 
```

    ## Warning in predict.lm(fit, newdata = list(age = age.grid), se = TRUE):
    ## prediction from a rank-deficient fit may be misleading

``` r
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(age, charges, xlim=agelims, cex=.5, col="darkgrey") 
lines(age.grid, preds$fit, lwd=2, col="darkblue") 
matlines(age.grid, se.bands, lwd=1, col="darkblue", lty=3)
abline(v=c(18,26,34,42,48,56,64),lty=3) 
legend("topright", col=c("darkblue"), lwd=2, legend=c("Cubic Spline"), bty="n")
title("Cubic Spline")
```

![](regression_files/figure-markdown_github/unnamed-chunk-8-1.png)
