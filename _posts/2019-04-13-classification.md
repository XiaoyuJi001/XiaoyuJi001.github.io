Classification
================

Introduction
------------

In this page, I choose a dataset about heart diseases collected from hopitials in Switzerland. This a relatively large dataset which contains 14 attributes such as age, sex, chest pain tye and so on. The "goal" field of dataset refers to the presence of heart disease in the patient. But today I only focus on "thalach" which means the max heart rate and whether the patient has heart disease.

### data processing

the "target" is 0 means the patient does not have heart disease, is 1 otherwise.

The range of the max heart rate is from 71 to 202. To make our model more intuitive, we divide the each rate over 202.

### feature the data

I draw a box graph and bar graphs to show Median, Tail Length, Abnormal Value and Distribution Interval of max rate in each target side.

``` r
library("gmodels")
```
``` r
library("MASS")
rm(list = ls()) 
heart <- read.csv("heart.csv")
attach(heart)
boxplot(thalach~target,ylab="maxrate",xlab="target",cex.lab=1.5)  
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/hC.cmijMASdFVGC*Q*2qL6FEdG**9RnLxTp7WZaj31A!/b/dL8AAAAAAAAA&bo=SAJlAQAAAAADBww!&rf=viewer_4)

``` r
thalach_0 <- thalach[target==0]/202
thalach_1 <- thalach[target==1]/202
hist(thalach_0,prob=TRUE,col="cornflowerblue",
     ylim=c(-0.5,5), xlim=c(0,1), xlab="maxrate of target=0", main="") 
lines(density(thalach_0),col="brown",lwd=2, lty=2) 
curve(dnorm(x, mean=mean(thalach_0), sd=sd(thalach_0)), col="burlywood", lwd=2, add=TRUE)
legend("topleft", legend=c("normal fit","nonparametric fit"),
       lty=1:2,col=c("burlywood","brown"),lwd=2,bty="n") 
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/FQo*6YKAzVLjArq8PRkkCB*GWh6JKFzksgczag5uYeY!/b/dDEBAAAAAAAA&bo=TAJcAQAAAAADBzE!&rf=viewer_4)

``` r
hist(thalach_1,prob=TRUE,col="cornflowerblue",
     ylim=c(-0.5,5), xlim=c(0,1), xlab="maxrate of target=1", main="") 
lines(density(thalach_1),col="brown",lwd=2, lty=2) 
curve(dnorm(x, mean=mean(thalach_1), sd=sd(thalach_1)), col="burlywood", lwd=2, add=TRUE)
legend("topleft", legend=c("normal fit","nonparametric fit"),
       lty=1:2,col=c("burlywood","brown"),lwd=2,bty="n")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/o.vUp7SzcKmNsRDqmZ9NyoTpn*Y0pZU1N9akHKPkMiE!/b/dDABAAAAAAAA&bo=SQJcAQAAAAADFyQ!&rf=viewer_4)

We also draw the linear regression box gragh and show linear regression curve in bar graphs. Bebause as shown above, the distribution of maxrate and target may do not fit the dataset well, I also try non-parametric fit. Wider non-linear variations can be obtained without presetting the specific form and error distribution of the model. Non-parametric fitting can be widely applied to different types of collections because it does not need to preset the specific form and error distribution of the model, so it can obtain a wide range of non-linear variations. At the same time, when extracting samples to estimate the collections, it does not need to depend on the distribution form of the collections to which the samples belong, so it can be widely applied to different types of collections. This can reduce the deviation, improve the prediction accuracy and understand the samples. The dynamic structure of sequences is extremely useful.

``` r
thalachdata=thalach/202
## Linear Regression #
lsfit <- lm(target~thalachdata)
summary(lsfit)
```

    ## 
    ## Call:
    ## lm(formula = target ~ thalachdata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9611 -0.4560  0.1675  0.3604  0.9482 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.8299     0.1723  -4.817 2.32e-06 ***
    ## thalachdata   1.8553     0.2299   8.070 1.70e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4531 on 301 degrees of freedom
    ## Multiple R-squared:  0.1779, Adjusted R-squared:  0.1751 
    ## F-statistic: 65.12 on 1 and 301 DF,  p-value: 1.697e-14

``` r
# prediction on training data
ls.p <- lsfit$fit
lstarget <- as.numeric(ls.p > .5)
table(lstarget,target,dnn=c("LS predicted target","true target"))
```

    ##                    true target
    ## LS predicted target   0   1
    ##                   0  80  34
    ##                   1  58 131

``` r
# for a prettier table:
CrossTable(lstarget,target,digits=2,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,
           dnn=c("LS predicted vote","true vote"),format="SPSS")
```

    ## 
    ##    Cell Contents
    ## |-------------------------|
    ## |                   Count |
    ## |          Column Percent |
    ## |-------------------------|
    ## 
    ## Total Observations in Table:  303 
    ## 
    ##                   | true vote 
    ## LS predicted vote |        0  |        1  | Row Total | 
    ## ------------------|-----------|-----------|-----------|
    ##                 0 |       80  |       34  |      114  | 
    ##                   |    57.97% |    20.61% |           | 
    ## ------------------|-----------|-----------|-----------|
    ##                 1 |       58  |      131  |      189  | 
    ##                   |    42.03% |    79.39% |           | 
    ## ------------------|-----------|-----------|-----------|
    ##      Column Total |      138  |      165  |      303  | 
    ##                   |    45.54% |    54.46% |           | 
    ## ------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
plot(ls.p ~ as.factor(target),
     xlab="Vote", ylab=c("predicted probability"), 
     col=c("cornflowerblue","brown"),main="Linear Regression")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/*CT*bawKflZ6ZyTJCUmZ.I.4XqFWmC8aFxUchB33v1w!/b/dFMBAAAAAAAA&bo=RgJjAQAAAAADBwQ!&rf=viewer_4)

And then, we find that when max rate &gt; 0.72, the probability of having heart disease is 0.5.

``` r
lspred_prob <- predict(lsfit,data.frame(thalachdata = seq(0,1,.01)))
lspred_vote <- as.numeric(lspred_prob > .5)
xx <- seq(0,1,.01)
lsplane <- xx[min(which(lspred_prob > .5))]
plot(thalachdata,target,ylim=c(-.5,1.5),col="cornflowerblue")
lines(xx,lspred_prob,col="brown",lwd=2)
abline(v=lsplane,lty=2,col="goldenrod")
text(lsplane+.01,-.3,paste("maxrate=",lsplane),pos=3,col="goldenrod")
par(new=TRUE)
plot(xx,lspred_vote,col="yellow",xlab="",ylab="",ylim=c(-.5,1.5), axes=FALSE)
legend("bottomright", legend=c("data","linear prediction","linear fit"),
       lty=c(0,0,1),pch=c(1,1,NA),col=c("cornflowerblue","yellow","brown"),bty="n") 
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/TopbwEqGaMedaRH7aQrgmsvo1NNG*SYEQ70fhj0FH7o!/b/dFIBAAAAAAAA&bo=QgJYAQAAAAADBzs!&rf=viewer_4)

``` r
## Logistic Regression #
logitfit <- glm(target~thalachdata,family=binomial)
summary(logitfit)
```

    ## 
    ## Call:
    ## glm(formula = target ~ thalachdata, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1383  -1.0780   0.6043   0.9200   2.1354  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -6.3915     0.9871  -6.475 9.50e-11 ***
    ## thalachdata   8.8782     1.3193   6.729 1.71e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 359.26  on 301  degrees of freedom
    ## AIC: 363.26
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# prediction on training data
logit.p <- logitfit$fit
logittarget <- as.numeric(logit.p > .5)
table(logittarget,target,dnn=c("Logistic predicted target","true vote"))
```

    ##                          true vote
    ## Logistic predicted target   0   1
    ##                         0  83  35
    ##                         1  55 130

``` r
CrossTable(logittarget,target,digits=2,prop.r=FALSE,prop.t=FALSE,prop.chisq=FALSE,dnn=c("Logistic predicted target","true target"),format="SPSS")
```

    ## 
    ##    Cell Contents
    ## |-------------------------|
    ## |                   Count |
    ## |          Column Percent |
    ## |-------------------------|
    ## 
    ## Total Observations in Table:  303 
    ## 
    ##                           | true target 
    ## Logistic predicted target |        0  |        1  | Row Total | 
    ## --------------------------|-----------|-----------|-----------|
    ##                         0 |       83  |       35  |      118  | 
    ##                           |    60.14% |    21.21% |           | 
    ## --------------------------|-----------|-----------|-----------|
    ##                         1 |       55  |      130  |      185  | 
    ##                           |    39.86% |    78.79% |           | 
    ## --------------------------|-----------|-----------|-----------|
    ##              Column Total |      138  |      165  |      303  | 
    ##                           |    45.54% |    54.46% |           | 
    ## --------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
plot(logit.p ~ as.factor(target),
     xlab="target", ylab=c("predicted probability"), 
     col=c("cornflowerblue","brown"),main="Logistic Regression")
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/OSIfZfJ*CDrqYjkwuNAIQ84RHgMYahKyyBK89SJKgxA!/b/dDIBAAAAAAAA&bo=QwJWAQAAAAADBzQ!&rf=viewer_4)

``` r
# prediction on income = seq(0,1,.01)
logitpred_prob <- predict(logitfit,data.frame(thalachdata=seq(0,1,.01)),type="response")
logitpred_target <- as.numeric(logitpred_prob > .5)

# plot
xx <- seq(0,1,.01)
logitplane <- xx[min(which(logitpred_prob > .5))]
plot(thalachdata,target,col="cornflowerblue")
lines(xx,logitpred_prob,col="brown",lwd=2)
abline(v=logitplane,lty=2,col="goldenrod")
text(logitplane+.01,.25,paste("maxrate=",logitplane),pos=3,col="goldenrod")
par(new=TRUE)
plot(xx,logitpred_target,col="yellow",xlab="",ylab="", axes=FALSE)
legend("bottomright", legend=c("data","logistic prediction","logistic fit"),
       lty=c(0,0,1),pch=c(1,1,NA),col=c("cornflowerblue","yellow","brown"),bty="n") 
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/DhNbdFmM1KtEZFLJ.lojeYJV.xR8xHIMW6OQnQjiH6w!/b/dLwAAAAAAAAA&bo=RwJNAQAAAAADFzs!&rf=viewer_4)

``` r
## Comparison
plot(thalachdata,target,col="cornflowerblue",ylim=c(-.5,1.5))
lines(xx,lspred_prob,col="burlywood",lwd=2)
lines(xx,logitpred_prob,col="brown",lwd=2)
legend("bottomright", legend=c("linear fit","logistic fit"),
       lty=1,lwd=2,col=c("burlywood","brown"),bty="n") 
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/6b.w1rmi.i*q4WmDZsdWsb9bX4LPQGQXc4ZW6WXRZ9c!/b/dDQBAAAAAAAA&bo=QwJMAQAAAAADFz4!&rf=viewer_4)

Finally, I do the logistic regression and prohit fit. As shown in the gragh below, the two curves are similar.

``` r
## Probit #
probitfit <- glm(target~thalachdata,family = binomial(link = "probit"))
summary(probitfit)
```

    ## 
    ## Call:
    ## glm(formula = target ~ thalachdata, family = binomial(link = "probit"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1774  -1.0856   0.5999   0.9239   2.1640  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -3.8475     0.5644  -6.817 9.27e-12 ***
    ## thalachdata   5.3529     0.7536   7.103 1.22e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 359.36  on 301  degrees of freedom
    ## AIC: 363.36
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
# prediction on income = seq(0,1,.01)
probitpred_prob <- predict(probitfit,data.frame(thalachdata=seq(0,1,.01)),type="response")

# plot
xx <- seq(0,1,.01)
plot(thalachdata,target,col="cornflowerblue",ylim=c(-.5,1.5))
lines(xx,logitpred_prob,col="brown",lwd=2)
lines(xx,probitpred_prob,col="burlywood",lwd=2)
legend("bottomright", legend=c("logistic fit","probit fit"),
       lty=1,lwd=2,col=c("brown","burlywood"),bty="n") 
```

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/Z6hJw2GYitNlhzZhEfwIYs.N.UhHMNUv.JKsNO3a4EA!/b/dL4AAAAAAAAA&bo=PgJJAQAAAAADF0Y!&rf=viewer_4)

``` r
detach(heart)
```
