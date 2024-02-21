# Application of EM algorithm in parameter estimation for normal mixture models
Sutthida Wongchai, Kuntalee Chaisee Pd.D and Kamonrat Suphawan Ph.D, Department of Statistics, Faculty of Science, Chiang Mai University



### Introduction

This study is concerned with applying an Expectation – Maximization algorithm (EM algorithm) for estimating parameters in a normal mixture model. In general, the EM algorithm is used to deal with missing data. It can also be used in a normal mixture model for parameter estimation. We analyze the data of monthly rainfall in Chiang Mai in 14 years collected by Upper Northern Region Irrigation Hydrology Center, Thailand. We found that the averages of rainfall in July to October have normal mixture distribution with two components. The first component and the second component are from smaller and larger amount of rainfall, respectively. We show how to apply the EM algorithm for estimating parameters in the normal mixture model with two components by using R programming. 

### Result
The data consists of 56 values of monthly average hights of rainfall July to October, 2001-2014 collected at Huikeaw station (HK), Huimoh station (HM), Panghai station (PH). 

Table 1: Estimated parameters of the normal distribution for the rainfall datasets obtained from using MLE method.
<p align="center">
<img width="273" alt="Screen Shot 2024-02-21 at 17 49 31" src="https://github.com/jsutthida/Projects/assets/160230541/f1f47d58-4a07-4ffe-833c-00704038de59">

Table 2: Estimated parameters of the normal mixture distribution with two components for the rainfall datasets obtained from using the EM algorithm.
<p align="center">
<img width="273" alt="Screen Shot 2024-02-21 at 17 50 28" src="https://github.com/jsutthida/Projects/assets/160230541/6ea48b9c-6a29-48ff-b140-7cd86880bee6">

Histograms and density curves with estimated values from Table 1 and Table 2
![page1image1941424](https://github.com/jjustjeep/Projects/assets/160230541/4fe727fd-65aa-4da4-a0b0-59305e3834b5)
![page1image1941424](https://github.com/jjustjeep/Projects/assets/160230541/dfd11775-2a5c-429e-9c79-3e6af1911242)
![page1image1941632](https://github.com/jjustjeep/Projects/assets/160230541/2314b996-963c-4091-ac6b-7f3636f269f7)

### Conclusion
We found that 3 stations has about 95% , so most observations comes from the smaller group of normal mixture. The means of normal mixture distribution with the smaller amount of rainfall of Huikeaw, Huimoh and Panghai stations are about 6.06, 7.44 and 9.15 respectively. And, the means of normal mixture distribution with the larger amount of rainfall are about 17.66, 15.85 and 21.66 respectively. 
In conclusion, the normal mixture model seems more suitable for the rainfall data than the normal model because the density curves with the estimated values fit the data better.

### Coding
```@author: Sutthida Wongchai

setwd("D:/Project/R/Data") 
data=read.csv("huikeaw.csv",head=T) 

data1=cbind(data[,1],data[,2],data[,3]) data.rain=data1[,1][data1[,3]==1] 
data.rain 
n=length(data.rain) 

summary(data.rain) 

s=c(0.5,5,19,16,4,0) 
em=function(W,s) 
{ 
  Ep = s[1]*dnorm(W,s[2],sqrt(s[4]))/(s[1]*dnorm(W,s[2],sqrt(s[4]))+(1- s[1])*dnorm(W,s[3],sqrt(s[5]))) 
      s[1]=mean(Ep) s[2]=sum(Ep*W)/sum(Ep) s[3]=sum((1-Ep)*W)/sum(1-Ep) s[4]=sum(Ep*(W-s[2])^2)/sum(Ep) s[5]=sum((1-Ep)*(W-s[3])^2)/sum(1-Ep) s[6]=(sum(Ep*log(s[1])))+(sum((1-Ep)*log(1-s[1])))-((sum(Ep*log(2*pi*s[4])))/2)-((sum(Ep*((W-s[2])^2)))/(2*s[4]))-((sum((1-Ep)*log(2*pi*s[5])))/2)-((sum((1-Ep)*((W-s[3])^2)))/(2*s[5])) 
  print(s) 
} 
iter=function(W,s) 
{ 
  s1=em(W,s) 
  for (i in 1:5) 
    { 
      if (abs(s[i]-s1[i]) > 0.0001) 
        { 
        s=s1 
        iter(W,s) 
        } 
    else s1 
    } 
  s1 
} 
W=data.rain 
iter(W,s) 
p=iter(W,s) 
p 
weight.1=p[1];weight.2=1-p[1];mean.1=p[2];mean.2=p[3];var.1=p[4];var.2=p[5] 
weight.1 
weight.2 
mean.1 
mean.2 
var.1 
var.2 
#graph hist(data.rain,prob=T,breaks=10,xlim=c(0,25),ylim=c(0,0.15),xlab="rainfall(mm.)",main="Histogram of rainfall at Huikeaw station") 
curve(dnorm(x,mean(data.rain),sd(data.rain)),from=0,to=30,add=T,col=4,lwd=2,lty=2) 
curve((weight.1*dnorm(x,mean.1,sqrt(var.1)))+(weight.2*dnorm(x,mean.2,sqrt(var.2))),from=0,to=30, add=T,col=2,lwd=2) 
legend("topright",c("Normal","Normal mixture"),lty=c(2,1),col=c(4,2),lwd=c(2,2)) 
```


### Preferences
1. Arthur P. Dempster, Nan Laird, and Donald B. Rubin (1977). Maximum likelihood from incomplete data via the EM algorithm. Journal of the Royal Statistical Society B, 39, 1-38. 
2. Christopher M. Bishop (2006). Pattern recognition and machine learning. Singapore: Springer. 
3. Shu-Ching Chang and Hyung Jin Kim (2007). EM Algorithm, 10 ธันวาคม 2559.
