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

#########  TIME SERIES ANALYSIS OF CITIBIKE USAGE IN NYC: MODELING SELECTION PART 2
#########  
#########         
######### 

######### (1) load + info about data + rename columns 
#########
df= read.csv("df.csv", header = TRUE)
head(df)
dim(df)
names(df)
summary(df)
str(df)

df1 = df %>% rename(c("Trips_over_the_past_24.hours_.midnight_to_11.59pm."= "Trips_per_24hrs",   "Miles_traveled_today_.midnight_to_11.59_pm."="Miles_per_24hrs")) 
names(df1)


####### (2) CREATING SUBSETS FOR TRANSFORMATION: AVERAGE TRIPS PER MONTH FOR ALL USERS 
#######
#       a) Select Trips Per day from main df and make copy in case we cant to resuse df
df_trips = select(df1, Date, Trips_per_24hrs); head(df_trips)
df_trips2 = select(df1, Date, Trips_per_24hrs); head(df_trips) 

# change to numeric 
df_trips <- lapply(df_trips, as.numeric)

#zoo() and chron() are used to select numeric inputs to be used for mean values per month  
z = zoo(df_trips$Trips_per_24hrs, chron(df_trips$Date, origin = c(month=5, day=27, year=2013))) 
df_trips.mo = aggregate(z, as.yearmon, mean) 

#       b) plot the aggregated Series
df_trips.moplot = plot(df_trips.mo, type = "l", col = "darkred", lwd = 1.5, main = "Average Trips Per Month", xlab = "Year", ylab = "Trips Per Month") 
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
acf(z, main = "ACF ")
pacf(z , main = "PACF ")
mtext("ACF ", side = 3, line = -1, outer = TRUE, col = "red")
mtext("PACF ", side = 3, line = -20.5,  outer = TRUE, col = "red")


#       c) Augmented Dickey-Fuller Test

adf.test(z, alternative="stationary")
# A formal ADF test does not reject the null hypothesis of non-stationarity, confirming our visual inspection. where p> 0.05

####### (3) Model Specification Identifying an appropriate model by exploring the series as stationary  
#######
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(3,1))

#       a) Remove unequal variances by using the log of the series + dickey test
df_trips.mo.log = log(df_trips.mo)
df_trips.logplot = plot(df_trips.mo.log, type = "l", col = "darkgreen", lwd = 1.5, main = "Average Trips Per Month-Log", xlab = "Year", ylab = "Trips Per Month (log)") 
adf.test(df_trips.mo.log, alternative = "stationary")

#       b) Address the trend component  by taking the difference of the series + dickey test 
df_trips.mo.diff = diff((df_trips.mo),  differences = 1)
df_trips.diffplot1 = plot(df_trips.mo.diff1, type = "l", col = "darkblue", lwd = 1.5, main = "Average Trips Per Month-Diff", xlab = "Year", ylab = "Trips Per Month (Diff(log)") 
adf.test(df_trips.mo.diff, alternative = "stationary")


#       c) Address the trend component by taking the difference of the log series + dickey test
df_trips.mo.diff_log = diff(log(df_trips.mo),  differences = 1)
df_trips.diffplot = plot(df_trips.mo.diff_log, type = "l", col = "darkred", lwd = 1.5, main = "Average Trips Per Month-Diff(log)", xlab = "Year", ylab = "Trips Per Month (Diff(log)") 
adf.test(df_trips.mo.diff_log, alternative = "stationary")



#       d) plots of ACF, PACF
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(3,2))
acf(df_trips.mo.log, main = "ACF Log")
pacf(df_trips.mo.log, main = "PACF Log")
mtext("ACF Log", side = 3, line = -1, adj = 0, outer = TRUE, col = "red")
mtext("PACF Log", side = 3, line = -1, adj = 1, outer = TRUE, col = "red")

acf(df_trips.mo.diff, main = "ACF Diff(1)")
pacf(df_trips.mo.diff, main = "PACF Diff(1)")
mtext("ACF Diff(1)", side = 3, line = -21.5, adj = 0, outer = TRUE, col = "red")
mtext("PACF Diff(1)", side = 3, line = -21.5, adj = 1, outer = TRUE,  col = "red")

acf(df_trips.mo.diff_log, main = "ACF Diff(log)")
pacf(df_trips.mo.diff_log, main = "ACF Diff(log)")
mtext("ACF Diff(log)", side = 3, line = -42.5, adj = 0, outer = TRUE,  col = "red")
mtext("PACF Diff(log)", side = 3, line = -42.5, adj = 1, outer = TRUE, col = "red")


####### (3) Model Fitting Estimating the values of the parameters of the model: ARIMA
#######

#       a) Log Auto.arima + dickey test w/ constant mean
par("mar")
par(mar=c(1,1,1,1))
log.ar = auto.arima(df_trips.mo.log, seasonal=FALSE);log.ar
tsdisplay(residuals(log.ar), col= "darkgreen", lag.max=45, main='(2,1,1) Log Model Residuals') # AIC=-15.88
checkresiduals(log.ar$residuals)
summary(log.ar)

#       b) Diff(1) Auto.arima + dickey test w/ constant mean
diff.ar = auto.arima(df_trips.mo.diff, seasonal=FALSE); diff.ar
tsdisplay(residuals(diff.ar), col= "darkblue", lag.max=45, main='(3,0,1) Diff Model Residuals') # AIC=1394.54
checkresiduals(diff.ar$residuals)
summary(diff.ar)

#fitx = arima(df_trips.mo.diff, order = c(4,0,), include.mean = FALSE); fitx
#summary(fitx)
#checkresiduals(fitx)
#tsdisplay(residuals(fitx), col= "darkblue", lag.max=45, main='(3,0,1) Diff Model Residuals') # AIC=1394.54

#       c) Diff(log) Auto.arima + dickey test w/ constant mean
diff_log.ar = auto.arima(df_trips.mo.diff_log, seasonal=FALSE); diff_log.ar 
tsdisplay(residuals(diff_log.ar), col= "darkred", lag.max=45, main='(2,0,1) Diff(log) Model Residuals') # AIC=-15.88
checkresiduals(diff_log.ar)
summary(diff_log.ar)

##### According to our findings, I have decided that based on the ACF/PACF diff_log is the best model at AR(2,0,1)
##### Our residuals should look like white noise, and they do, which means normal distributed

#       d) Diff(log) qqplot 
par("mar")
par(mar=c(1,1,1,1))
qqnorm(log.ar$residuals, asp = 1, main= "QQplot log Model Residuals")
qqline(log.ar$residuals, asp=1, col= "red", lwd= 1)
par("mar")
par(mar=c(1,1,1,1))
qqnorm(diff_log.ar$residuals, asp = 1, main= "QQplot Diff(log) Model Residuals")
qqline(diff_log.ar$residuals, asp=1, col= "red", lwd= 1)







