#Sumber Data: https://www.bps.go.id/linkTableDinamis/view/id/1115

#input data
Gita_Sandy=read.delim("clipboard")
Gita_Sandy
(Cramer-von Mises normality test, Pearson chi-square normality test)

#Uji Normalitas(Cramer-von Mises normality test, Pearson chi-square normality test)
cvm.test(Gita_Sandy$X2015)
pearson.test(Gita_Sandy$X2015)

#TRANSFORMASI BOX-COX
library (car)
trans=powerTransform (Gita_Sandy$X2015)
trans

#Uji Normalitas (Jarque Bera Test)
library(tseries)
jarque.bera.test (Gita_Sandy$X2015^-0.5976566)
jarque.bera.test(((Gita_Sandy$X2015^-0.5976566)-1)/-0.5976566)


#Membuat plot
#Uji Normalitas dg qqnorm, qqline, qqplot
plot(Gita_Sandy$X2015^-0.5976566)
windows()
qqnorm(Gita_Sandy$X2015^-0.5976566)	
qqline(Gita_Sandy$X2015^-0.5976566)


####MEMBANGKITKAN DATA DISTRIBUSI WEBULL####
x.wei<-rweibull(2.5,1.5)
mean.weibull<-mean(x.wei) 
var.weibull<-var(x.wei) 
mean.weibull
var.weibull
curve(dweibull(x,scale=2.5,shape=1.5),from=0,to=15, main="DistribusiWeibull")
library(fBasics) 
skewness(x.wei)
kurtosis(x.wei)
library(MASS) 
fitdistr(x.wei,densfun=dweibull,start=list(scale=1,shape=2))
