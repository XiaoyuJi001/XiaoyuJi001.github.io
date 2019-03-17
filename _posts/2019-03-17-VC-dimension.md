---
layout: post
title: "VC dimension"
date: 2019-03-17
---

##  Abstract   
To understand the VC dimension explicitly, I would firstly introduce some basic concepts: dichotomy, growth function, shattering and break point. And then I would talk about the upper bound of growth fuction, the VC ineaulity, and its function in application.   

### Dichotomy:  
a division into two especially mutually exclusive or contradictory groups or entities.

### Shatter:  
Assuming that all dichotomy of data set D can be achieved by some operation H, it is said that data set D can be shatter by this operation H.

### Growth Function: 
The growth function for a hypothesis set H, denoted MH(N), is the maximum possible number of dichotomies H can generate on a data.

**Positive rays:**

 ![](http://m.qpic.cn/psb?/V12764hq3b2u4E/5YCEXVklHTBCizKh7RmKhzmtVXWJJZEJbotznqRWVXY!/b/dLkAAAAAAAAA&bo=TwFJAAAAAAADByU!&rf=viewer_4)
 
 In the N + 1 dashed line, take two and turn all the purple dots into red hearts, while the other purple dots into green balls, which has a total of 0.5N (N + 1) species; in addition, add a result of all the green balls, so MH(N) = 0.5n2+0.5n+1

**1D Perceptron**
For each dotted line, all the purple dots on the right (or left) are turned into red centers, and all the purple dots on the left (or right) into green balls. In addition, two kinds of dotted lines are all the results of red centers or green balls. 
MH(N) = 2(N-1+1)=2N

**2D Perceptron**

The situation of 2D perceptron is somewhat complicated, and it should be discussed according to N.  
N=1   MH(N) =2 

 ![](http://m.qpic.cn/psb?/V12764hq3b2u4E/j3mcDlx.AqaZ5F3M0lOIlXMJt88ssWCdo9b0le9*d7Y!/b/dDMBAAAAAAAA&bo=bAA8AAAAAAADB3I!&rf=viewer_4)

N=2   MH(N) =4

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/FqiT4*NKBDrilTP9nUB0fDIZBDqlIHcqZJ4MhWTfrVQ!/b/dFQBAAAAAAAA&bo=zAA3AAAAAAADB9k!&rf=viewer_4)

N=3    dH(3) = 8 = 2^3

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/9gqUm84R7p*1pqP*tqLuNRyvN**CCkaAQ1GVK4TkAH4!/b/dL4AAAAAAAAA&bo=uABTAAAAAAADB8k!&rf=viewer_4)
![](http://m.qpic.cn/psb?/V12764hq3b2u4E/UonDTrpbhFfmoKYGai3wXl372IgM0EDd.K2ewyTQSws!/b/dLYAAAAAAAAA&bo=owBMAAAAAAADB80!&rf=viewer_4)

N=4     MH(N) =14 < 2^N 

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/EtKQ5xfmoh1*AjQLcTmNjJL2WhkQ9w*.6jN0H.LCi*o!/b/dFIBAAAAAAAA&bo=ZwFXAAAAAAADBxM!&rf=viewer_4)

N>=5        MH(N) < 2^N

**Convex Set**

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/1wVeJEd.URwGsd0bJBk0eWRQInq7V5ZBp8hk8KOn*TA!/b/dMAAAAAAAAAA&bo=owBTAAAAAAADB9I!&rf=viewer_4)  The left one is convex sets, bur the right one is not

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/7X6YvoICbdlNpnd72KW8DdwH5mx*YtzQhKi009HinXs!/b/dD4BAAAAAAAA&bo=bwBaAAAAAAADBxc!&rf=viewer_4)  MH(N) = 2^N

**Break points**
The minimum number of N whose hypothesis set cannot be shattered by any distribution type. So for positive rays MH(2)=3 Break point is 2; For positive intervals, MH(3)=7 break point is 3; For convex set, no break point; For 2D perceptron, MH(4)=14 break point is 4. 

The Vapnik-hervonenkis (VC) dimension is a measure of the capacity (complexity, expressive power, richness, or flexibility) of a space of functions that can be learned by a statistical classification algorithm. It is defined as the cardinality of the largest set of points that the algorithm can shatter. It was originally defined by Vladimir Vapnik and Alexey Chervonenkis.
Briefly, So, we could understand the meaning of VC dimension. dvc (H) is the largest value of N for which MH(N) = 2^N    dvc = k-1

## Infinite to finite

According to the definition, break point k restrict the range of MH(N). Then, given N and k, it can be proved that the upper bound of the maximum value of MH(N) is polynomial. According to the Hoeffding inequality, it is feasible to use mH(N) instead of M to obtain machine learning.

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/YqG1oSvATzAAJVJNCC1P9HROTOy28yP9h.7OApphmfc!/b/dL4AAAAAAAAA&bo=LAJeAAAAAAADB1I!&rf=viewer_4)

But when N Towards infinity, the last value above will be more than 1.
What is the problem?
That is the upper bound 2n of growth function is too large, In fact, the growth function does have a smaller upper bound, which is the polynomial.
According to the definition of B (N, k), the maximum number of dichotomies with N points is k, which is also  ![](http://m.qpic.cn/psb?/V12764hq3b2u4E/ubz*NJjz0QEbS5j71XKanZL7LmfdL2q7SN8U8.ha5.E!/b/dFMBAAAAAAAA&bo=QgAmAAAAAAADB0Y!&rf=viewer_4)  the upper bound of B (N, k) growth function.
So, MH(N) = N (N,k) <= B(N,k).

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/2wGWh7Dylq4v1QIGCMWkPSVlMX0GgauXxD2Z7nqXxNE!/b/dLYAAAAAAAAA&bo=sAF4ALABeAADCSw!&rf=viewer_4)

The above is the primary level of VC inequality. I paint the difference in Final Version and primary version, To get the VC inequality, need more knowledge.
Due to the limit of length of article, you could read follow link.

[link](https://www.csie.ntu.edu.tw/~htlin/course/ml08fall/doc/vc_proof.pdf)

Bring VC dimension into VC inequality (replacing k-1 with dvc)

![](http://m.qpic.cn/psb?/V12764hq3b2u4E/cnfSQOu6QN78lw7k*fHb2cawbDh*C8IPxfwbzBCdSaM!/b/dLgAAAAAAAAA&bo=7QFXAO0BVwADCSw!&rf=viewer_4)

As long as the dvc is limited, then when n is large, the right side of inequality is a very small number, then the Eout(h) approximates the training Ein(h), then the hypothesis h has good generalization ability. More importantly, it is concluded that we only need training set D and hypothesis set H to find the optimal hypothesis function.


### Reference
 *Vapnik, V. N.; Chervonenkis, A. Ya. (1971). "On the Uniform Convergence of Relative Frequencies of Events to Their Probabilities". Theory of Probability & Its Applications. 16 (2): 264. doi:10.1137/1116025. This is an English translation, by B. Seckler, of the Russian paper: "On the Uniform Convergence of Relative Frequencies of Events to Their Probabilities". Dokl. Akad. Nauk. 181 (4): 781. 1968. The translation was reproduced as: Vapnik, V. N.; Chervonenkis, A. Ya. (2015). "On the Uniform Convergence of Relative Frequencies of Events to Their Probabilities". Measures of Complexity. p. 11. doi:10.1007/978-3-319-21852-6_3. ISBN 978-3-319-21851-9.*




 


