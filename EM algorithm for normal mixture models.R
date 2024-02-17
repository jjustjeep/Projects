@author: Sutthida Wongchai

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
