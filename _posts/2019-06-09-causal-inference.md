causal inference
================

introduction
------------

This analysis is based on an interesting data set to explore the factors that influence employee turnover in enterprises.

Employee turnover is one of the key problems that perplex enterprises. In this analysis, I will endeavor to carry out the following work: - Fast visualization and exploratory analysis of some important variables, especially those related to basic information, income, promotion, satisfaction, performance and work-life balance. - Analysis of the factors leading to employee turnover and explore the impact of various variables. - Establish a model through an effective algorithm to predict whether an employee will resign or not. \#\#\# virables

``` r
library(ggplot2)
library(grid)
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.5.3

``` r
library(plyr)
library(rpart)
```

    ## Warning: package 'rpart' was built under R version 3.5.3

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 3.5.3

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.5.3

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:gridExtra':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.5.3

    ## Loading required package: lattice

``` r
library(gbm)
```

    ## Warning: package 'gbm' was built under R version 3.5.3

    ## Loaded gbm 2.1.5

``` r
library(survival)
```

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

``` r
#library(pROC)
#library(DMwR)
library(scales)
library(openxlsx)
```

    ## Warning: package 'openxlsx' was built under R version 3.5.3

``` r
data<- read.xlsx("turnover.xlsx")
summary(data)
```

    ##       Age         Attrition         BusinessTravel       DailyRate     
    ##  Min.   :18.00   Length:1470        Length:1470        Min.   : 102.0  
    ##  1st Qu.:30.00   Class :character   Class :character   1st Qu.: 465.0  
    ##  Median :36.00   Mode  :character   Mode  :character   Median : 802.0  
    ##  Mean   :36.92                                         Mean   : 802.5  
    ##  3rd Qu.:43.00                                         3rd Qu.:1157.0  
    ##  Max.   :60.00                                         Max.   :1499.0  
    ##   Department        DistanceFromHome   Education     EducationField    
    ##  Length:1470        Min.   : 1.000   Min.   :1.000   Length:1470       
    ##  Class :character   1st Qu.: 2.000   1st Qu.:2.000   Class :character  
    ##  Mode  :character   Median : 7.000   Median :3.000   Mode  :character  
    ##                     Mean   : 9.193   Mean   :2.913                     
    ##                     3rd Qu.:14.000   3rd Qu.:4.000                     
    ##                     Max.   :29.000   Max.   :5.000                     
    ##  EmployeeCount EmployeeNumber   EnvironmentSatisfaction    Gender         
    ##  Min.   :1     Min.   :   1.0   Min.   :1.000           Length:1470       
    ##  1st Qu.:1     1st Qu.: 491.2   1st Qu.:2.000           Class :character  
    ##  Median :1     Median :1020.5   Median :3.000           Mode  :character  
    ##  Mean   :1     Mean   :1024.9   Mean   :2.722                             
    ##  3rd Qu.:1     3rd Qu.:1555.8   3rd Qu.:4.000                             
    ##  Max.   :1     Max.   :2068.0   Max.   :4.000                             
    ##    HourlyRate     JobInvolvement    JobLevel       JobRole         
    ##  Min.   : 30.00   Min.   :1.00   Min.   :1.000   Length:1470       
    ##  1st Qu.: 48.00   1st Qu.:2.00   1st Qu.:1.000   Class :character  
    ##  Median : 66.00   Median :3.00   Median :2.000   Mode  :character  
    ##  Mean   : 65.89   Mean   :2.73   Mean   :2.064                     
    ##  3rd Qu.: 83.75   3rd Qu.:3.00   3rd Qu.:3.000                     
    ##  Max.   :100.00   Max.   :4.00   Max.   :5.000                     
    ##  JobSatisfaction MaritalStatus      MonthlyIncome    MonthlyRate   
    ##  Min.   :1.000   Length:1470        Min.   : 1009   Min.   : 2094  
    ##  1st Qu.:2.000   Class :character   1st Qu.: 2911   1st Qu.: 8047  
    ##  Median :3.000   Mode  :character   Median : 4919   Median :14236  
    ##  Mean   :2.729                      Mean   : 6503   Mean   :14313  
    ##  3rd Qu.:4.000                      3rd Qu.: 8379   3rd Qu.:20462  
    ##  Max.   :4.000                      Max.   :19999   Max.   :26999  
    ##  NumCompaniesWorked    Over18            OverTime        
    ##  Min.   :0.000      Length:1470        Length:1470       
    ##  1st Qu.:1.000      Class :character   Class :character  
    ##  Median :2.000      Mode  :character   Mode  :character  
    ##  Mean   :2.693                                           
    ##  3rd Qu.:4.000                                           
    ##  Max.   :9.000                                           
    ##  PercentSalaryHike PerformanceRating RelationshipSatisfaction
    ##  Min.   :11.00     Min.   :3.000     Min.   :1.000           
    ##  1st Qu.:12.00     1st Qu.:3.000     1st Qu.:2.000           
    ##  Median :14.00     Median :3.000     Median :3.000           
    ##  Mean   :15.21     Mean   :3.154     Mean   :2.712           
    ##  3rd Qu.:18.00     3rd Qu.:3.000     3rd Qu.:4.000           
    ##  Max.   :25.00     Max.   :4.000     Max.   :4.000           
    ##  StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear
    ##  Min.   :80    Min.   :0.0000   Min.   : 0.00     Min.   :0.000        
    ##  1st Qu.:80    1st Qu.:0.0000   1st Qu.: 6.00     1st Qu.:2.000        
    ##  Median :80    Median :1.0000   Median :10.00     Median :3.000        
    ##  Mean   :80    Mean   :0.7939   Mean   :11.28     Mean   :2.799        
    ##  3rd Qu.:80    3rd Qu.:1.0000   3rd Qu.:15.00     3rd Qu.:3.000        
    ##  Max.   :80    Max.   :3.0000   Max.   :40.00     Max.   :6.000        
    ##  WorkLifeBalance YearsAtCompany   YearsInCurrentRole
    ##  Min.   :1.000   Min.   : 0.000   Min.   : 0.000    
    ##  1st Qu.:2.000   1st Qu.: 3.000   1st Qu.: 2.000    
    ##  Median :3.000   Median : 5.000   Median : 3.000    
    ##  Mean   :2.761   Mean   : 7.008   Mean   : 4.229    
    ##  3rd Qu.:3.000   3rd Qu.: 9.000   3rd Qu.: 7.000    
    ##  Max.   :4.000   Max.   :40.000   Max.   :18.000    
    ##  YearsSinceLastPromotion YearsWithCurrManager
    ##  Min.   : 0.000          Min.   : 0.000      
    ##  1st Qu.: 0.000          1st Qu.: 2.000      
    ##  Median : 1.000          Median : 3.000      
    ##  Mean   : 2.188          Mean   : 4.123      
    ##  3rd Qu.: 3.000          3rd Qu.: 7.000      
    ##  Max.   :15.000          Max.   :17.000

From descriptive statistics: The proportion of employees not losing and losing is about 5:1. The average age of employees in an enterprise is about 36 years old. The average income of employees is about $6500, with a median of 4919, which better reflects the salary level of enterprises. \#\# basic information analysis

``` r
g1 <- ggplot(data, aes(x = Age, fill = Attrition)) +geom_density(alpha = 0.7)

g2 <- ggplot(data, aes(x = NumCompaniesWorked, fill = Attrition)) +geom_density(alpha = 0.7)

g3 <- ggplot(data, aes(x = YearsAtCompany, fill = Attrition)) +geom_density(alpha = 0.7)

g4 <- ggplot(data, aes(x = TotalWorkingYears, fill = Attrition)) +geom_density(alpha = 0.7)

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)
```

![](causual_files/figure-markdown_github/unnamed-chunk-2-1.png)

The characteristics of staff who turnover: Lower age attrition rate is higher, mainly concentrated in employees younger than 30 years old. The turnover rate is high among the groups of employees who have served in more than five companies. The turnover rate of employees with short working hours in the company is high, and the number of years less than 4 years is relatively concentrated. The turnover rate of employees with low working age is high, and it is concentrated in those with less than 7 years'working age. \#\#\# Explore the relationship between income, input and employee turnover

``` r
g5 <- ggplot(data, aes(x= Gender,fill = Attrition)) +geom_bar(position = "fill") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g6 <-ggplot(data, aes(x= JobLevel,fill = Attrition)) +geom_bar(position = "fill") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g7 <- ggplot(data, aes(x= Education,fill = Attrition)) +geom_bar(position = "fill") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g8 <- ggplot(data, aes(x= Department,fill = Attrition)) +geom_bar(position = "fill") +labs(y="Percentage") + scale_y_continuous(labels=percent)
grid.arrange(g5, g6, g7, g8, ncol = 2, nrow = 2)
```

![](causual_files/figure-markdown_github/unnamed-chunk-3-1.png) Employees in low-ranking positions have a high turnover rate, which is mainly concentrated in positions with rank 1. There seems to be no difference between education and gender. The turnover rate of employees in sales department is higher than that in other departments. \#\#\# Explore the relationship between income, input and employee turnover

``` r
g9 <- ggplot(data, aes(x = MonthlyIncome, fill = Attrition)) +geom_density(alpha = 0.7)
g10 <- ggplot(data, aes(x= JobInvolvement, group=Attrition)) +geom_bar(aes(y = ..prop.., fill = Attrition),stat="count", alpha = 0.7,position = "identity",color="black") + labs(y="Percentage") + scale_y_continuous(labels=percent)
grid.arrange(g9, g10, ncol = 2)
```

![](causual_files/figure-markdown_github/unnamed-chunk-4-1.png)

The turnover rate of low-income employees is high, and the turnover rate of employees whose income is about $10,000 is not low. High turnover rate of employees engaged in work

Income will have a small peak around $10,000, indicating that employees at this level also have a high turnover rate, which may be due to the fact that these employees are the elite talents of the enterprise, and may have higher pursuit or factors leading to leaving, which can be the focus of attention.

Payment and reward are always employees'inertial thinking, which is worth exploring. So before correctly understanding the impact of income on turnover, first look at the relationship between pay and reward.

``` r
ggplot(data, aes(x= JobInvolvement, y=MonthlyIncome, group = JobInvolvement)) +geom_boxplot(aes(fill = factor(..x..)),alpha=0.7) +theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +facet_grid(~Attrition) +ggtitle("Attrition")
```

![](causual_files/figure-markdown_github/unnamed-chunk-5-1.png) This is a very interesting result. For high or low income, it can not accurately explain that low income is the cause of employee turnover. But here we can find that the greater the difference between input and return, the more likely it is to be lost. Therefore, enterprises need to pay more attention to those who invest more but pay less. Such employees may not be hard, but do not grasp the correct way of work. Greater help should be given, such as training, job guidance, etc. Salaries are often one of the rewards.

### Exploring the relationship between variables related to work-life balance and employee turnover

``` r
g18<-  ggplot(data, aes(x= OverTime, group=Attrition)) +geom_bar(aes(y = ..prop.., fill = Attrition),stat="count", alpha = 0.7,position = "identity",color="black") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g19 <- ggplot(data, aes(x= WorkLifeBalance, group=Attrition)) +geom_bar(aes(y = ..prop.., fill = Attrition),stat="count", alpha = 0.7,position = "identity",color="black") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g20 <- ggplot(data, aes(x= BusinessTravel, group=Attrition)) +geom_bar(aes(y = ..prop.., fill = Attrition), stat="count", alpha = 0.7,position = "identity",color="black") +labs(y="Percentage") + scale_y_continuous(labels=percent)
g21 <- ggplot(data, aes(x = DistanceFromHome, fill = Attrition)) +geom_density(alpha = 0.7)
```
