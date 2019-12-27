setwd("~/Desktop/Hunter_College_Docs/FALL2019/STAT706_707_General_Linear_Models/General_Linear_ModelsI/Project/CODE")

#########  HUNTER COLLEGE: DEPARTMENT OF MATHEMATICS & STATISTICS
#########  STAT 739     
#########  FALL 2019
#########  PROJECT: POINTS PREDICTION-A BAYESIAN PERSPECTIVE 
#########  CESAR R. PABON BERNAL
########   
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(zoo) 
library(chron) 
library(plyr)
library(xts)
library(lattice)
library(rugarch)
library(readxl)
library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(data.table)
library(lmtest)
library(alr3)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(leaps)
library(GGally)
library(corpcor)
library(mctest)
library(ppcor)
library(LearnBayes)
library(lattice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plyr)
library(readxl)
library(tidyverse)
library(read.csv)
library(ggrepel)
library(data.table)
library(baytrends)
library(lmtest)
library(qualityTools)
library(alr3)
library(gmodels)
library(stats)
library(lmtest)

################### LOAD DATA
df= read_excel("shots_bball_2018-19.xlsx")
print("dimensions of df:");dim(df)

######### A. INTRODUCTION
#1. summary 
summary(df)
str(df)

#3 Address missing values 
table(is.na(df))
sapply(df, function(x) sum(is.na(x)))

df= df %>% drop_na()
dim(df)
sapply(df, function(x) sum(is.na(x)))
df2= df %>% drop_na()
dim(df2)


######### B. METHODS
#######   I. OLS MLS
par(mfrow=c(1,4))
df2= df2[,c("PTS","TOV", "PF")]
df3= df2[which(df2$TOV < 2),]
hist(df3$TOV,xlab= "Turnovers", main = "Histrogram of Turnovers", col= "lightblue")
hist(df3$PF, xlab= "Personal Fouls", main = "Histrogram of Personal Fouls", col= "orange")
plot(df3$PTS~df3$TOV, xlab="Turnovers", main="Scatterplot of Avg. Points Vs. Turnovers", pch=20, col="lightblue")
abline(lm(df3$PTS~df3$TOV), col= "red")
plot(df3$PTS~df3$PF, xlab="Personal Fouls", main="Scatterplot of Avg. Points Vs. Personal Fouls", col="orange", pch=20)
abline(lm(df3$PTS~df3$PF), col= "red")

######  I.a. MLR on the 2 variables w/ respect to points
mlr_model = lm(df3$PTS~df3$PF+df3$TOV,x=TRUE,y=TRUE) 
summ_mlr_model= summary(mlr_model); summ_mlr_model
#cat("summ_mlr_model", file = "summ_mlr_model.txt", append = TRUE)
#capture.output(summ_mlr_model, file = "summ_mlr_model.txt", append = TRUE)



###### I.b. MLR residuals, qqplots + anova analysis 
par(mfrow=c(1,2))
plot(mlr_model$residuals ~ mlr_model$fitted.values, main="Residuals vs. Fitted", xlab="Fitted Values", ylab="Residuals",pch= 20, col= "#CCFF33")
abline(h=mean(mlr_model$residuals), col= "red")
plot(mlr_model,pch=20, which=c(2), col= "red")

anova(mlr_model)
vcov(mlr_model)


#######   II. Bayesian MLR
theta_sample=blinreg(mlr_model$y,mlr_model$x,200)
head(theta_sample)
tail(theta_sample)


#######    a) function for theta_sample 
S=readline(prompt="Type  <Return>   to continue : ")

if (.Platform$OS.type == "unix") x11() else windows()
par(mfrow=c(1,4))
hist(theta_sample$beta[,1],main="Intercept",xlab=expression(beta[0]), col= "goldenrod1")
hist(theta_sample$beta[,2],main="Turnovers",xlab=expression(beta[1]), col= "aquamarine2")
hist(theta_sample$beta[,3],main="Personal Fouls",xlab=expression(beta[2]), col= "cornflowerblue")
hist(theta_sample$sigma,main="ERROR SD",xlab=expression(sigma), col= "firebrick1")

apply(theta_sample$beta,2,quantile,c(.05,.5,.95))
quantile(theta_sample$sigma,c(.05,.5,.95))
S=readline(prompt="Type  <Return>   to continue : ")


S=sum(mlr_model$residual^2)
shape=mlr_model$df.residual/2; rate=S/2
sigma2=rigamma(1,shape,rate)
print("sigma^2: "); sigma2

MSE = sum(mlr_model$residuals^2)/mlr_model$df.residual
print("MSE:"); MSE
vbeta=vcov(mlr_model)/MSE
beta=rmnorm(1,mean=mlr_model$coef,varcov=vbeta*sigma2)
print("beta:");beta



#######    b) Estimation 
cov1=c(1,5,2) 
cov2=c(1,5,3) 
cov3=c(1,5,5)
X1=rbind(cov1,cov2,cov3)
mean.draws=blinregexpected(X1,theta_sample)
c.labels=c("A","B","C")

cols = brewer.pal(3, "Set1")
pal = colorRampPalette(cols)
par(mfrow=c(1,3))
for (j in 1:3)
        hist(mean.draws[,j],
             main=paste("Covariate set",c.labels[j]),xlab="Points", col = pal(20), border="darkgrey")

#######    b) Prediction 
X1=rbind(cov1,cov2,cov3)
pred.draws=blinregpred(X1,theta_sample)
c.labels=c("A","B","C","D")

cols = brewer.pal(5, "Blues")
pal = colorRampPalette(cols)
par(mfrow=c(1,3))
for (j in 1:3)
        hist(pred.draws[,j],
             main=paste("Covariate set",c.labels[j]),xlab="Points",  col = pal(20), border="darkgrey")



#######    e) Diagnsotics 
######          i.
pred.draws=blinregpred(mlr_model$x,theta_sample)
pred.sum=apply(pred.draws,2,quantile,c(.05,.95))
par(mfrow=c(1,1))
ind=1:length(df3$PTS)
matplot(rbind(ind,ind),pred.sum,type="l",lty=1,col=1,
        xlab="INDEX",ylab="Points")

points(ind,df3$PTS,pch=19, col= "red")
out=(df3$PTS>pred.sum[2,])
text(ind[out], df3$PTS[out], label=df3$PF[out], pos = 4, col="blue")

######          ii.
prob.out=bayesresiduals(mlr_model,theta_sample,2)
par(mfrow=c(1,1))
plot(df3$PF,prob.out, pch= 20, col= "red", xlab= "Covariates Personal Fouls & Turnovers" )
out = (prob.out > 0.35)
text(df3$PF[out], prob.out[out], label=df3$PF[out], pos = 4, col="purple")





