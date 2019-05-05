
desicion tree
================

introduction
------------

In this passage, We will discuss a interesting topic that we coould use features of passengers on Titanic to determine whether one could survive. The data has been split into two groups: training set (train.csv) and test set(test.csv)

### Data Dictionary

-   survival Survival 0 = No, 1 = Yes
-   pclass Ticket class 1 = 1st, 2 = 2nd, 3 = 3rd
-   sex Sex
-   Age Age in years
-   sibsp \# of siblings / spouses aboard the Titanic
-   parch \# of parents / children aboard the Titanic
-   ticket Ticket number
-   fare Passenger fare
-   cabin Cabin number
-   embarked Port of Embarkation C = Cherbourg, Q = Queenstown, S = Southampton
-   pclass: A proxy for socio-economic status (SES) 1st = Upper 2nd = Middle 3rd = Lower
-   age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5

Note:

sibsp: The dataset defines family relations in this way:

Sibling = brother, sister, stepbrother, stepsister; Spouse = husband, wife (mistresses and fiances were ignored) ;

parch: The dataset defines family relations in this way;

Parent = mother, father Child = daughter, son, stepdaughter, stepson. Some children travelled only with a nanny, therefore parch=0 for them.;

preparation
-----------

### function setting

``` r
#1.data quality form
data_quality<- function(x){
  mode_data<- c()
  diff_data<- c()
  na_data<- c()
  na_datar<- c()
  fna_data<- c()
  fna_datar<- c()
  for (i in 1:ncol(x)){
    mode_data<-c(mode_data,mode(x[,i]))
    diff_data<- c(diff_data,length(unique(x[[i]])))
    na_data<- c(na_data,sum(is.na(x[,i])))
    nr<- paste(round(na_data[i]/nrow(x),4)*100,"%",sep = "")
    na_datar<- c(na_datar,nr)
    fna_data<- c(fna_data,sum(!is.na(x[,i])))
    fnr<- paste(round(fna_data[i]/nrow(x),4)*100,"%",sep = "")
    fna_datar<- c(fna_datar,fnr)
  }
  result<- rbind(mode_data,diff_data,na_data,na_datar,fna_data,fna_datar)
  colnames(result)<- colnames(x)
  rownames(result)<-c("Data Type","Number of Different Values","Number of Null Values","Null Value Ratio","Number of Valued Values","Valued Ratio")
  result<- as.data.frame(result)
  # print(ls(envir = parent.frame(n=1)))
  return(result)
} 
#2.data transformation
data_transform<- function(x){
  for (i in 1:ncol(x))
    if(length(unique(x[[i]])) < 5){
      x[[i]]<-as.factor(x[[i]])
    }
  return(x)
}
#3.NUMERICAL DATA QUALITY TABLE
quality_numeric<- function(x){
  m1<-c()
  m2<-c()
  m3<-c()
  stdev<-c()
  m3_r<-c()
  m3_l<-c()
  options(digits=2)
  for (i in 1:ncol(x)){
    m1<- c(m1,min(x[[i]],na.rm = T))
    m2<- c(m2,max(x[[i]],na.rm = T))
    m3<- c(m3,mean(x[[i]],na.rm = T))
    stdev<- c(stdev,sqrt(sd(x[[i]],na.rm = T)))
    m3_r<-c(m3_r,m3[i]-3*stdev[i])
    m3_l<-c(m3_l,m3[i]+3*stdev[i])
  }
  result<- cbind(m1,m2,m3,stdev,m3_r,m3_l)
  rownames(result)<- names(x)
  colnames(result)<- c("Min","Max","Mean","StDev","M-3","M+3")
  result<- as.data.frame(result)
  return(result)
}
quality_factor<- function(x){
  Level<- c()
  Count<- c()
  for (i in 1:ncol(x)){
    r<- table(x[[i]])
    le<- c()
    co<- c()
    for (k in 1:length(r)){
      le<- paste(le,names(r)[k],sep = ":")
      co<- paste(co,r[k],sep = ":")}
    Level<- rbind(Level,le)
    Count<- rbind(Count,co)}
  result<- cbind(Level,Count)
  rownames(result)<-names(x)
  colnames(result)<- c("Level","Count")
  result<- as.data.frame(result)
  return(result)
}
```

``` r
#pakages
library(stringr)
echo = F
library(ggplot2)
```

data processing
---------------

``` r
train<- read.csv("train.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = c("NA",""))
test<- read.csv("test.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = c("NA",""))
train_data_quality<- data_quality(train)
train_data_quality 
```

    ##                            PassengerId Survived  Pclass      Name
    ## Data Type                      numeric  numeric numeric character
    ## Number of Different Values         891        2       3       891
    ## Number of Null Values                0        0       0         0
    ## Null Value Ratio                    0%       0%      0%        0%
    ## Number of Valued Values            891      891     891       891
    ## Valued Ratio                      100%     100%    100%      100%
    ##                                  Sex     Age   SibSp   Parch    Ticket
    ## Data Type                  character numeric numeric numeric character
    ## Number of Different Values         2      89       7       7       681
    ## Number of Null Values              0     177       0       0         0
    ## Null Value Ratio                  0%  19.87%      0%      0%        0%
    ## Number of Valued Values          891     714     891     891       891
    ## Valued Ratio                    100%  80.13%    100%    100%      100%
    ##                               Fare     Cabin  Embarked
    ## Data Type                  numeric character character
    ## Number of Different Values     248       148         4
    ## Number of Null Values            0       687         2
    ## Null Value Ratio                0%     77.1%     0.22%
    ## Number of Valued Values        891       204       889
    ## Valued Ratio                  100%     22.9%    99.78%

There are 15 zero values in Fare, which may be abnormal values. The fare of a ticket is related to the passenger class. Therefore, in the data processing part, the average value can be filled according to the Pclass information grouping.

``` r
#numerical data table
numeric_train<- train[,c("Age","Fare","SibSp","Parch")]
quality_numeric_train<-quality_numeric(numeric_train)
quality_numeric_train
```

    ##        Min Max  Mean StDev  M-3  M+3
    ## Age   0.42  80 29.70   3.8 18.3 41.1
    ## Fare  0.00 512 32.20   7.0 11.1 53.4
    ## SibSp 0.00   8  0.52   1.1 -2.6  3.7
    ## Parch 0.00   6  0.38   0.9 -2.3  3.1

``` r
length(train$Fare[which(train$Fare==0)])
```

    ## [1] 15

Embarked has two empty characters, which have been replaced by NA when imported, so the data processing part can be filled with mode numbers.

``` r
factor_train<- train[,c("Survived","Pclass","Sex","Embarked")]
quality_factor_train<- quality_factor(factor_train)
quality_factor_train
```

    ##                 Level        Count
    ## Survived         :0:1     :549:342
    ## Pclass         :1:2:3 :216:184:491
    ## Sex      :female:male     :314:577
    ## Embarked       :C:Q:S  :168:77:644

``` r
table(train$Embarked,useNA = "always")
```

    ## 
    ##    C    Q    S <NA> 
    ##  168   77  644    2

``` r
# Conversion of data type 
train<- data_transform(train)
str(train)
```

    ## 'data.frame':    891 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : Factor w/ 2 levels "0","1": 1 2 2 2 1 1 1 1 2 2 ...
    ##  $ Pclass     : Factor w/ 3 levels "1","2","3": 3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  NA "C85" NA "C123" ...
    ##  $ Embarked   : Factor w/ 3 levels "C","Q","S": 3 1 3 3 3 2 3 3 3 1 ...

data analysis
-------------

``` r
#1.1 total survival
options(digits = 2)
ggplot(train,aes(x=Survived,fill=Survived))+geom_bar()+labs(title="total survival",x="survival or not",y="number")+scale_fill_manual(values=c("#999999", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5),legend.position = "none") 
```

![](decision_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
prop.table(table(train$Survived))
```

    ## 
    ##    0    1 
    ## 0.62 0.38

``` r
#1.2Overall age/gender distribution
ggplot(train,aes(x=Age,fill=Pclass))+geom_density(alpha=.3)+labs(title="Age distribution")+theme(plot.title = element_text(hjust = 0.5)) 
```

    ## Warning: Removed 177 rows containing non-finite values (stat_density).

![](decision_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
ggplot(train,aes(x=Sex,fill=Sex))+geom_bar()+labs(title="Gender distribution")+scale_fill_manual(values=c("#56B4E9", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5),legend.position = "none") 
```

![](decision_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
train_age<- train[!is.na(train$Age),]
tapply(train_age$Age,train_age$Pclass,mean)
```

    ##  1  2  3 
    ## 38 30 25

``` r
prop.table(table(train$Sex))
```

    ## 
    ## female   male 
    ##   0.35   0.65

Overall Survival: 38% (549) passengers were killed and 62% (342) passengers were rescued.

After deleting the missing values, the analysis results show that the average age of first class and second class cabins is larger than that of third class cabins. The average age of each class cabins is as follows: first class cabins are 38 years old, second class cabins are 30 years old, third class cabins are 25 years old, and male passengers account for 65% of the total passengers. Therefore, the high rate of male casualties is also due to the high proportion of samples.

``` r
# 2 Age survival
ggplot(train,aes(x=Age))+geom_density()+labs(title="Age distribution")+theme(plot.title = element_text(hjust = 0.5))
```

    ## Warning: Removed 177 rows containing non-finite values (stat_density).

![](decision_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# Children aged 0-14 years are generally taken as subjects of pediatric research in the medical field. Therefore, children aged 14 years and younger are designated here to analyze their survival.
train_age_14<- train_age[which(train_age$Age <= 14),]
train_age_14$pclass14<- ""
train_age_14$pclass14[train_age_14$Pclass==1 | train_age_14$Pclass==2]<- "high class"
train_age_14$pclass14[train_age_14$Pclass==3]<- "third class"

table(train_age_14$Survived)
```

    ## 
    ##  0  1 
    ## 32 45

``` r
table(train_age_14$pclass14,train_age_14$Survived)
```

    ##              
    ##                0  1
    ##   high class   1 23
    ##   third class 31 22

``` r
prop.table(table(train_age_14$Survived))
```

    ## 
    ##    0    1 
    ## 0.42 0.58

``` r
prop.table(table(train_age_14$pclass14,train_age_14$Survived),margin = 1)
```

    ##              
    ##                   0     1
    ##   high class  0.042 0.958
    ##   third class 0.585 0.415

``` r
ggplot(train_age_14,aes(x=Survived,fill=Survived))+geom_bar()+labs(title="children survival",x="survival or not",y="number")+scale_fill_manual(values=c("#999999", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
```

![](decision_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
ggplot(train_age_14,aes(x=pclass14,fill=Survived))+geom_bar()+labs(title="Survival of children in different cabins",x="cabin level",y="number")+scale_fill_manual(values=c("#999999", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5))
```

![](decision_files/figure-markdown_github/unnamed-chunk-8-3.png) The survival rate of children was 58%, and that of first-class and second-class children was 96% (24 children were rescued and only one child was killed).

The survival rate of third-class children was 42% (22 children were rescued and 31 children were killed). It can be seen that the most important factor affecting the survival of passengers is the class of cabin.

``` r
# genger analysis
train_female<- train[which(train$Sex=="female"),]
train_female$pclass_female<- ""
train_female$pclass_female[train_female$Pclass==1 | train_female$Pclass==2]<- "high levels"
train_female$pclass_female[train_female$Pclass==3]<- "thied level"

table(train_female$Survived)
```

    ## 
    ##   0   1 
    ##  81 233

``` r
table(train_female$pclass_female,train_female$Survived)
```

    ##              
    ##                 0   1
    ##   high levels   9 161
    ##   thied level  72  72

``` r
prop.table(table(train$Sex))
```

    ## 
    ## female   male 
    ##   0.35   0.65

``` r
prop.table(table(train_female$Survived,train_female$pclass_female),margin = 2)
```

    ##    
    ##     high levels thied level
    ##   0       0.053       0.500
    ##   1       0.947       0.500

``` r
ggplot(train,aes(x=Sex,fill=Survived))+geom_bar()+labs(title="survival of gender",x="gender",y="number")+scale_fill_manual(values=c("#56B4E9", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
```

![](decision_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(train_female,aes(x=pclass_female,fill=Survived))+geom_bar()+labs(title="survival of gender in different cabins?",x="cabin level",y="number")+scale_fill_manual(values=c("#56B4E9", "#E69F00"))+theme(plot.title = element_text(hjust = 0.5))
```

![](decision_files/figure-markdown_github/unnamed-chunk-9-2.png)

The survival rate of women was 65%. The survival rate of first-class and second-class women was 95%(161 women were rescued and 9 women were killed).

The survival rate of third-class women was 50% (72 women were rescued and 72 women were killed).

``` r
#----Empty String Processing Embarked
table(train$Embarked,useNA = "always")
```

    ## 
    ##    C    Q    S <NA> 
    ##  168   77  644    2

``` r
train$Embarked[which(is.na(train$Embarked))] <- 'S'
table(train$Embarked,useNA = "always")
```

    ## 
    ##    C    Q    S <NA> 
    ##  168   77  646    0

``` r
#----outliner processing fare
#The value of Fare 0 is filled according to the median of the warehouse grade.
a1<-tapply(train$Fare,train$Pclass,median)
train[which(train$Fare==0&train$Pclass==1),"Fare"]<- a1[[1]]
train[which(train$Fare==0&train$Pclass==2),"Fare"]<- a1[[2]]
train[which(train$Fare==0&train$Pclass==3),"Fare"]<- a1[[3]]

#----Age interpolates according to the median of address
# library(stringr)
table_words <- table(unlist(strsplit(train$Name,"\\s+")))  
sort(table_words [grep('\\.',names(table_words))],decreasing = TRUE) 
```

    ## 
    ##       Mr.     Miss.      Mrs.   Master.       Dr.      Rev.      Col. 
    ##       517       182       125        40         7         6         2 
    ##    Major.     Mlle.     Capt. Countess.      Don. Jonkheer.        L. 
    ##         2         2         1         1         1         1         1 
    ##     Lady.      Mme.       Ms.      Sir. 
    ##         1         1         1         1

``` r
tb <- cbind(train$Age,str_match(train$Name,"[a-zA-Z]+\\.")) 
table(tb[is.na(tb[,1]),2])
```

    ## 
    ##     Dr. Master.   Miss.     Mr.    Mrs. 
    ##       1       4      36     119      17

``` r
median.mr <- median(train$Age[grepl("Mr\\.",train$Name) & !is.na(train$Age)]) 
median.mrs <- median(train$Age[grepl("Mrs\\.",train$Name)],na.rm = T) 
median.dr <- median(train$Age[grepl("Dr\\.",train$Name) & !is.na(train$Age)])
median.miss <- median(train$Age[grepl("Miss\\.",train$Name) & !is.na(train$Age)])
median.master <- median(train$Age[grepl("Master\\.",train$Name) & !is.na(train$Age)])
cbind(median.mr,median.mrs,median.dr,median.miss,median.master)
```

    ##      median.mr median.mrs median.dr median.miss median.master
    ## [1,]        30         35        46          21           3.5

``` r
train$Age[grepl("Mr\\.",train$Name) & is.na(train$Age)] <- median.mr
train$Age[grepl("Mrs\\.",train$Name) & is.na(train$Age)] <- median.mrs
train$Age[grepl("Dr\\.",train$Name) & is.na(train$Age)] <- median.dr
train$Age[grepl("Miss\\.",train$Name) & is.na(train$Age)] <- median.miss
train$Age[grepl("Master\\.",train$Name) & is.na(train$Age)] <- median.master
ggplot(train,aes(x=Age,fill=Pclass))+geom_density(alpha=.3)
```

![](decision_files/figure-markdown_github/unnamed-chunk-10-1.png)

decision tree
-------------

Illustration 1: Build trees with all variables and prune them according to the complexity parameter CP Illustration 2: fit. tree $cptable is the complexity parameter and error of ten fold cross validation, from which the tree with the least prediction error is selected. Illustration 3: When validating, add type= "class" to output the classification result, otherwise output the probability value

``` r
library(rpart)
```

    ## Warning: package 'rpart' was built under R version 3.5.3

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 3.5.3

``` r
fit.tree<- rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train,method = "class",
                 parms = list(split="information"),control = rpart.control(xval = 10))
plotcp(fit.tree)
```

![](decision_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
fit.tree$cptable  
```

    ##      CP nsplit rel error xerror  xstd
    ## 1 0.444      0      1.00   1.00 0.042
    ## 2 0.031      1      0.56   0.56 0.036
    ## 3 0.015      5      0.43   0.49 0.034
    ## 4 0.010      6      0.42   0.51 0.035

``` r
prune.tree<- prune(fit.tree,cp=0.015) 

cols <- ifelse(prune.tree$frame$yval == 1, "darkred", "green4")
prp(prune.tree, main="Decision Tree",
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    branch=.5,           # change angle of branch lines
    faclen=0,            # faclen=0 to print full factor names
    trace=1,             # print the automatically calculated cex
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad
```

    ## cex 1   xlim c(0, 1)   ylim c(0, 1)

![](decision_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
rpart.plot(prune.tree,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="decision tree")
```

![](decision_files/figure-markdown_github/unnamed-chunk-11-3.png)

green if survived

Random Forest
-------------

Illustration 1: Random forest generates 500 trees by default, and sqrt (M) variables are extracted at each node. Illustration 2: importance (fit. ranf, type = 2) to see the importance of variables Illustration 3: The na. action = na. roughfix parameter replaces missing values in numerical variables with corresponding column medians, and category variables with modulus. Illustration 3: Random Forest generates traditional decision tree, while cforest () in part package generates random forest based on conditional inference tree

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.5.3

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
fit.ranf<- randomForest(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked ,data = train,na.action = na.roughfix,importance=T)
fit.ranf
```

    ## 
    ## Call:
    ##  randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp +      Parch + Fare + Embarked, data = train, importance = T, na.action = na.roughfix) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 18%
    ## Confusion matrix:
    ##     0   1 class.error
    ## 0 499  50       0.091
    ## 1 106 236       0.310

``` r
importance(fit.ranf,type=2)
```

    ##          MeanDecreaseGini
    ## Pclass                 34
    ## Sex                   101
    ## Age                    55
    ## SibSp                  17
    ## Parch                  13
    ## Fare                   64
    ## Embarked               12

The decision tree is constructed by using combination method, adding randomness, and based on different attributes and sample selection. Five hundred base classifiers are set up to vote in combination. Random forests reduce the correlation between decision trees by random and combination, and improve the flourishing error of combinatorial classifiers. The importance of the attributes extracted from the random forest ranges from high to low in order of sex, fare, age, number of siblings accompanied by passengers and number of parents/brothers accompanied by passengers.

Writing in Rmarkdown published on Github:
