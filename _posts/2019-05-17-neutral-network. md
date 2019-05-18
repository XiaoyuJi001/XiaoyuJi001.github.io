Neural Network
================

introduction
------------

We have a data which classified if patients have heart disease or not according to features in it. We will try to use this data to create a model which tries predict if a patient has this disease or not. We will use logistic regression (classification) algorithm and knowledge in neutral network.

### dataset information

-   Age (age in years)
-   Sex (1 = male; 0 = female)
-   CP (chest pain type)
-   TRESTBPS (resting blood pressure (in mm Hg on admission to the hospital))
-   CHOL (serum cholestoral in mg/dl)
-   FPS (fasting blood sugar &gt; 120 mg/dl) (1 = true; 0 = false)
-   RESTECH (resting electrocardiographic results)
-   THALACH (maximum heart rate achieved)
-   EXANG (exercise induced angina (1 = yes; 0 = no))
-   OLDPEAK (ST depression induced by exercise relative to rest)
-   SLOPE (the slope of the peak exercise ST segment)
-   CA (number of major vessels (0-3) colored by flourosopy)
-   THAL (3 = normal; 6 = fixed defect; 7 = reversable defect)
-   TARGET (1 or 0)

data processing
---------------

I use corrplot package to draw a graph showing the correlation coefficients between each two variables. As shown in the graph, cp,thalach, and slope have relative high positive coefficients with target and oldpeak ca and exang have relative high negative coefficients.


``` r
rm(list=ls())
data = read.csv("heart.csv") 
summary(data)
```

    ##      age           sex               cp           trestbps    
    ##  Min.   :29.00   Min.   :0.0000   Min.   :0.000   Min.   : 94.0  
    ##  1st Qu.:47.50   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:120.0  
    ##  Median :55.00   Median :1.0000   Median :1.000   Median :130.0  
    ##  Mean   :54.37   Mean   :0.6832   Mean   :0.967   Mean   :131.6  
    ##  3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:140.0  
    ##  Max.   :77.00   Max.   :1.0000   Max.   :3.000   Max.   :200.0  
    ##       chol            fbs            restecg          thalach     
    ##  Min.   :126.0   Min.   :0.0000   Min.   :0.0000   Min.   : 71.0  
    ##  1st Qu.:211.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:133.5  
    ##  Median :240.0   Median :0.0000   Median :1.0000   Median :153.0  
    ##  Mean   :246.3   Mean   :0.1485   Mean   :0.5281   Mean   :149.6  
    ##  3rd Qu.:274.5   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:166.0  
    ##  Max.   :564.0   Max.   :1.0000   Max.   :2.0000   Max.   :202.0  
    ##      exang           oldpeak         slope             ca        
    ##  Min.   :0.0000   Min.   :0.00   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:1.000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.80   Median :1.000   Median :0.0000  
    ##  Mean   :0.3267   Mean   :1.04   Mean   :1.399   Mean   :0.7294  
    ##  3rd Qu.:1.0000   3rd Qu.:1.60   3rd Qu.:2.000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :6.20   Max.   :2.000   Max.   :4.0000  
    ##       thal           target      
    ##  Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :2.000   Median :1.0000  
    ##  Mean   :2.314   Mean   :0.5446  
    ##  3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :3.000   Max.   :1.0000

``` r
corrplot(cor(data))
```

![](neutral_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# process
data[,1:13] = scale(data[,1:13]) # scale the data
data$target = as.factor(data$target)
nvar = ncol(data) - 1

# create training and test sets
set.seed(123)
train =  createDataPartition(data$target,p=0.5,list=F)
data_train = data[train,]
data_test = data[-train,]
ytrue = data_test$target
```

### logistic

Using logistic regression, we found that "cp" is most related.

``` r
fit <- glm(target~.,data_train,family='binomial')
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = target ~ ., family = "binomial", data = data_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3574  -0.4565   0.1656   0.6395   2.3756  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.09559    0.26321  -0.363 0.716483    
    ## age       0.16370    0.28991   0.565 0.572305    
    ## sex         -0.84420    0.28170  -2.997 0.002729 ** 
    ## cp           1.02794    0.29998   3.427 0.000611 ***
    ## trestbps    -0.19631    0.28889  -0.680 0.496809    
    ## chol        -0.77684    0.34603  -2.245 0.024766 *  
    ## fbs         -0.19117    0.25826  -0.740 0.459177    
    ## restecg      0.17427    0.25637   0.680 0.496650    
    ## thalach      0.51077    0.34412   1.484 0.137735    
    ## exang       -0.17934    0.29048  -0.617 0.536982    
    ## oldpeak     -0.87112    0.38076  -2.288 0.022145 *  
    ## slope        0.43741    0.28316   1.545 0.122411    
    ## ca          -0.61227    0.27816  -2.201 0.027727 *  
    ## thal        -0.23111    0.25074  -0.922 0.356677    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 209.43  on 151  degrees of freedom
    ## Residual deviance: 112.00  on 138  degrees of freedom
    ## AIC: 140
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# test err
phat = predict(fit,data_test,type="response")
yhat = as.numeric(phat > 0.5)
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 56 13
    ##     1 14 68

``` r
1-mean(yhat==ytrue) #misclassification error rate
```

    ## [1] 0.1788079

### Tree

Get the classification tree using random forest and boosting. We could see that if cp &gt;= 0.45, slope &lt;= 0.16 and oldpeak &lt; 0.46 there is 92% to say the patient has the heart disease.

``` r
set.seed(100)
fit = rpart(target ~.,data_train)
rpart.plot(fit,box.palette=list("Grays", "Reds"))
```

![](neutral_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# test err
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 53 16
    ##     1 15 67

``` r
1-mean(yhat==ytrue) #misclassification error rate
```

    ## [1] 0.205298

``` r
set.seed(100)
fit = randomForest(target ~.,data=data_train,mtry=6)

# test err
yhat = predict(fit,data_test) 
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 56 13
    ##     1 16 66

``` r
1-mean(yhat==ytrue) #misclassification error rate
```

    ## [1] 0.192053

``` r
set.seed(100)
ntree = 2000
data_boost = transform(data_train,target=as.numeric(target)-1)
fit = gbm(target~.,data_boost,distribution="adaboost",
          n.trees=ntree,
          interaction.depth = 10,
          shrinkage = 0.01)

# test error
phat = predict(fit,data_test,n.trees=ntree,type="response")
yhat = as.numeric(phat>0.5) 
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 60  9
    ##     1 25 57

``` r
1-mean(yhat==ytrue)
```

    ## [1] 0.2251656

### SVM

Support Vector Machine (SVM) was first proposed by Cortes and Vapnik in 1995. It has many unique advantages in solving small samples, non-linearity and high-dimensional pattern recognition, and can be extended to other machine learning problems such as function fitting. Support Vector Machine (SVM) is based on the VC dimension theory of statistical learning theory and the principle of structural risk minimization. According to the limited sample information, the best compromise between the complexity of the model ( Accuracy) and the learning ability the ability to identify any sample without error) is sought in order to obtain the best generalization ability.

``` r
set.seed (100)
fit= svm(target~.,data_train,kernel="radial",gamma=0.01,cost=30,scale=F)
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 58 11
    ##     1 15 67

``` r
1-mean(yhat==ytrue) #misclassification error rate
```

    ## [1] 0.1721854

### neutral network

``` r
set.seed(100)
fit = nnet(target ~.,data=data_train,
           size=10,maxit=10000,MaxNWts=10000,decay=0.1)
```

    ## # weights:  151
    ## initial  value 114.987448 
    ## iter  10 value 52.407214
    ## iter  20 value 38.233645
    ## iter  30 value 34.650124
    ## iter  40 value 33.285492
    ## iter  50 value 32.663211
    ## iter  60 value 32.305366
    ## iter  70 value 32.198282
    ## iter  80 value 32.063950
    ## iter  90 value 32.051493
    ## iter 100 value 32.050325
    ## iter 110 value 32.050156
    ## final  value 32.050155 
    ## converged

``` r
# test err 
yhat = predict(fit,data_test,type="class") 
table(ytrue,yhat)
```

    ##      yhat
    ## ytrue  0  1
    ##     0 60  9
    ##     1 19 63

``` r
1-mean(yhat==ytrue) #misclassification error rate
```

    ## [1] 0.1854305

``` r
# visualize results
# vidualize network
plotnet(fit,alpha_val=.2,
        circle_col="hotpink",
        pos_col="burlywood",
        neg_col="darkgray")
```

![](neutral_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# importance plots based on weights 
h = olden(fit)
h + coord_flip() + theme(axis.text=element_text(size=14),axis.title=element_text(size=14))
```

![](neutral_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# partial dependence plots of selected vars (other vars fixed at median)
h1 = lekprofile(fit,xsel=c("fbs"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h2 = lekprofile(fit,xsel=c("thalach"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h3 = lekprofile(fit,xsel=c("oldpeak"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
h4 = lekprofile(fit,xsel=c("thal"),group_vals=0.5) + 
  theme(legend.position="none",axis.title=element_blank())
grid.arrange(h1,h2,h3,h4,ncol=2)
```

![](neutral_files/figure-markdown_github/unnamed-chunk-5-3.png)
