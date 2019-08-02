library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(ggpubr)
library(zoo) 
library(chron) 
library(plyr)
library(tseries)
library(xts)
library(lattice)
library(rugarch)
library(tseries)

#########  TIME SERIES ANALYSIS OF CITIBIKE USAGE IN NYC: FORECASTING
#########  
#########         
######### 

######### (1) load + info about data + rename columns 
#########
df = read.csv("df.csv", header = TRUE)
#df_train= read.csv("train_df.csv", header = TRUE)
#df_test= read.csv("test_df.csv", header = TRUE)

head(df)
dim(df)
names(df_train)
summary(df)
str(df)

#df_train1 = df_train %>% rename(c("Trips_per_day"= "Trips_per_24hrs",   "Miles_Per_Day"="Miles_per_24hrs")) 
#df_test1 = df_test %>% rename(c("Trips_per_day"= "Trips_per_24hrs",   "Miles_Per_Day"="Miles_per_24hrs")) 
df1 = df %>% rename(c("Trips_over_the_past_24.hours_.midnight_to_11.59pm."= "Trips_per_24hrs",   "Miles_traveled_today_.midnight_to_11.59_pm."="Miles_per_24hrs")) 


####### (2) CREATING SUBSETS FOR TRANSFORMATION: AVERAGE TRIPS PER MONTH FOR ALL USERS 
#######
#       a) Select Trips Per day from main df and make copy in case we cant to resuse df
df_trips2 = select(df1, Date, Trips_per_24hrs)
#df_train2 = select(df_train1, Date, Trips_per_24hrs)
#df_test2 = select(df_test1, Date, Trips_per_24hrs)


# change to numeric 
df_trips3 = lapply(df_trips2, as.numeric)
#df_train3 = lapply(df_train2, as.numeric)
#df_test3 = lapply(df_test2, as.numeric)


#zoo() and chron() are used to select numeric inputs to be used for mean values per month  
z1 = zoo(df_trips3$Trips_per_24hrs, chron(df_trips3$Date, origin = c(month=5, day=27, year=2013))) 
#z2 = zoo(df_train3$Trips_per_24hrs, chron(df_train3$Date, origin = c(month=5, day=27, year=2013)))
#z3 = zoo(df_test3$Trips_per_24hrs, chron(df_test3$Date, origin = c(month=1, day=1, year=2019)))

df.mo = aggregate(z1, as.yearmon, mean) 
#train.mo = aggregate(z2, as.yearmon, mean) 
#test.mo = aggregate(z3, as.yearmon, mean) 

test =  window(df.mo, start=c(2018,50))
train = window(df.mo, end=c(2018,49))

last_40 = window(arima_sim,start=41)
last_40
first_40 = window(arima_sim,end=40)
first_40

train_log = diff(log(train),  differences = 1)
test_log =  diff(log(test),  differences = 1)

model.ar1.fit = arima(train_log, order=c(2,0,1), method = 'ML')
model.ar1.fit


fcast = forecast(model.ar1.fit, newdata=test_log, h=6)
fcast

Arima.fit <- auto.arima(Train, xreg = SampleData$TimeTT)
forecast(Arima.fit, h = 508, xreg = NewData$TimeTT)



#Method 2: Using predicr function 
model.ar1.predict = predict(model.ar1.fit,n.ahead=8)
round(model.ar1.predict$pred,3)
round(model.ar1.predict$se,3)
Lo_95 = model.ar1.predict$pred-qnorm(0.975,0,1)*model.ar1.predict$se
Hi_95 = model.ar1.predict$pred+qnorm(0.975,0,1)*model.ar1.predict$s
comb =  ts.union(Lo_95, Hi_95, test_log)
comb

##################### (c) plot

last5= tail(test_log, 3)

plot(fcast, main = "Forecasts for Log-Diff(1) AR(2,0,1)", xlab= "Simulations", ylab="Forecasts")
points(last5,lty = 1, col = "red", lwd = 1, pch = 23)
points(fcast$mean,lty = 1, col = "blue", lwd = 1, pch = 1)
points(Hi_95, lty = 1, col = "darkgreen", lwd = 1.5, pch = 2)
points(Lo_95, lty = 1, col = "black", lwd = 1.5, pch = 4)
abline(h=coef(model.ar1.fit )[names(coef(model.ar1.fit))=='intercept'])
legend(4, 103.5, legend = c("Actual", "Forecasted","Hi_95", "Lo_95"), lwd = 2, 
       col = c("red","blue", "darkgreen", "black"), lty = c(0,0), merge = TRUE, 
       bty = "n", pch = c(23, 1, 2, 4), cex = .7, text.width= .3)





