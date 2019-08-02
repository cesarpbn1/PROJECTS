library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(ggpubr)

#########  TIME SERIES ANALYSIS OF CITIBIKE USAGE IN NYC: EXPLORATORY TS NO MODELING PART1
#########  PART 1 EXPLORATORY ANALYSIS DATA (CLEANING, FORMATTING)
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


####### (2) CREATING SUBSETS FOR TRANSFORMATION
#######
#       a) Trips Per day 
df_trips = select(df1, Date, Trips_per_24hrs); head(df_trips)
df_trips2 = select(df1, Date, Trips_per_24hrs); head(df_trips)  #copy df_trips for later exploration 
#df_trips$log_Trips_per_24hrs = log(df1$Trips_per_24hrs)
#df_trips$cos_Trips_per_24hrs = cos(df1$Trips_per_24hrs)
#df_trips$sin_Trips_per_24hrs = sin(df1$Trips_per_24hrs)
#head(df_trips)

#       b) Miles Per day 
#df_miles = select(df1, Date, Miles_per_24hrs); head(df_miles)
#df_miles$log_Miles_per_24hrs = log(df1$Miles_per_24hrs)
#head(df_miles)
#df_miles$cos_Miles_per_24hrs = cos(df1$Miles_per_24hrs)
#df_miles$sin_Miles_per_24hrs = sin(df1$Miles_per_24hrs)
#head(df_miles)

####### (3) PLOT: EPXLORATION OF SEASONALITY 
#######
#       a) Converted date into a "date class" so that R can read it as a date for Trips_per_day--Year
df_trips$Date = as.Date(df_trips$Date, format = "%Y-%m-%d"); df_trips

trips_year_plot1 = ggplot(df_trips, aes(x = Date, y = `Trips_per_24hrs`)) +
        geom_line(color='darkred', size=.2) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
        xlab("Year") +
        ylab("Trips Per day for All Users") +
        ggtitle("Trips Per Day-Citibike NYC") +
        theme_classic()
trips_year_plot2 = ggplot(df_trips, aes(x = Date, y = `Trips_per_24hrs`)) +
        geom_point(shape=23, fill="darkgreen", color="darkred", size=0.5) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
        xlab("Year") +
        ylab("Trips Per day for All Users") +
        ggtitle("Trips Per Day-Citibike NYC") +
        theme_classic()

ggarrange(trips_year_plot1, trips_year_plot2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

#       B) Converted date into a "date class" so that R can read it as a date for Trips_per_day--Month
#               i.Using df_trips2, create an aggregated month for dot plot
df_trips2 = df_trips2 %>%
        mutate(month = month(Date))

df_trips2_month = df_trips2 %>%
        group_by(month) %>%
        summarise(trips_per = sum(Trips_per_24hrs))

trips_month_plot1 =
        df_trips2_month %>%
        mutate(month2 = as.Date(paste0("2013-", month,"-01"),"%Y-%m-%d")) %>%
        ggplot(aes(x = month2, y = trips_per)) +
        geom_point(color = "darkorchid4") +
        labs(title = "Trips Per Day-Citibike NYC",
             subtitle = "Data plotted by year",
             y = "Trips Per day for All Users",
             x = "Month") + theme_bw(base_size = 15) +
        scale_x_date(date_labels = "%b")

#               i. Using df_trips, create an aggregated month for boxplot
grouped_months= factor(months(df_trips$Date), levels = month.name)
trips_month_boxplot1 = ggplot(df_trips, aes(x=grouped_months, y=Trips_per_24hrs)) +
        geom_boxplot(outlier.colour = "red", outlier.shape = 1,  notch= FALSE, fill = "white", colour = "darkred")+
        xlab("Month") +
        ylab("Average Trips Per Month") +
        ggtitle("Average Trips Per Month-Citibike NYC") 

ggarrange(trips_month_plot1,trips_month_boxplot1,
          labels = c("C", "D"),
          ncol = 2, nrow = 1)



df_trips3 <- df_trips2 %>%
        mutate(year = year(Date))
df_trips3 <- df_trips3 %>%
        mutate(month = month(Date))
df_trips3_m_y <- df_trips3 %>%
        group_by(month, year) %>%
        summarise(trips_per = sum(Trips_per_24hrs))
                  
trips_month_plot2 =
df_trips3_m_y %>%
        mutate(month2 = as.Date(paste0("2013-", month,"-01"),"%Y-%m-%d")) %>%
        ggplot(aes(x = month2, y = trips_per)) +
        geom_bar(stat = "identity", fill = "darkred") +
        facet_wrap(~ year, ncol = 3) +
        labs(title = "Average Trips Per Month- Citibike NYC",
             subtitle = "Data plotted by month",
             y = "Average Trips Per Month ",
             x = "Month") + theme_bw(base_size = 15) +
        scale_x_date(date_labels = "%b")


ggarrange(trips_month_plot2,trips_month_boxplot1,
          labels = c("C", "D"),
          ncol = 2, nrow = 1)




#                                       *******SUMMARY FOR SEASONALITY******* 
## Plots show a general seasonal upward or positive trend  Now lets explore if its stationary process  

####### (4) PLOT:EXPLORATION OF STATIONARY PROCESS
#######
#        a) acf and pacf to explore stationarity





trips_avgmonth = ggplot(df_trips_mean, aes(x = Date, y = `mean_Trips_per_24hrs`)) +
        geom_line(color='darkred', size=.2) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
        xlab("Year") +
        ylab("Trips Per day for All Users") +
        ggtitle("Trips Per Day-Citibike NYC") +
        theme_classic()
trips_avgmonth



par(mfrow=c(2,1))
par("mar")
par(mar=c(1,1,1,1))
acf(df_trips$Trips_per_24hrs, main = "ACF")
pacf(df_trips$Trips_per_24hrs, main = "PACF")


library(zoo)
library(xts)
xts(df_trips, order.by=as.Date(rownames(df_trips),"%Y-%m-%d"))
dat_zoo <- read.zoo(df_trips, index.column=0, sep=",", format="%Y-%m-%d")
dat_zoo <- read.zoo(tmp,sep=",",FUN=as.yearmon)
dat_xts <- as.xts(dat_zoo)



#                                       *******SUMMARY FOR STATIONARITY*******
# Both ACF & PCAF plots show lag values outside the 95% boundary.  Therefore, process is not stationary and we must tranform
# Data.  I will first explore taking the log followed by taking the difference of the first moment 


####### (5) Elimination of Trend and Seasonal Components by Differencing and transformation
#######
#        a) Converted date into a "date class" so that R can read it as a date for Trips_per_day--Year



#####
diff(residuals())
month(df_trips$Date)
acf(df_trips$Trips_per_24hrs, main = "ACF")

lag1 = diff(residuals(df_trips, differences = 1))
auto.arima(lag, approximation = FALSE, trace = FALSE)

year(df_trips$Date)
acf(df_trips$Trips_per_24hrs, main = "ACF")
pacf(df_trips$Trips_per_24hrs, main = "PACF")

acf(df_trips$log_Trips_per_24hrs, main = "ACF")
pacf(df_trips$log_Trips_per_24hrs, main = "PACF")

lag1 = diff(df_trips$Trips_per_24hrs, differences = 1)
plot(lag1, type = "l")
auto.arima(lag1, approximation = FALSE, trace = FALSE)
auto.arima(df_trips$Trips_per_24hrs, approximation = FALSE, trace = FALSE)


time_trip_plot_log = ggplot(df_trips, aes(x = Date, y = `log_Trips_per_24hrs`)) +
        geom_line(color='darkgreen', size=.2) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
        xlab("Years") +
        ylab("Trips Per day for All Users") +
        ggtitle("Years vs. Trips Per day for All Users") +
        theme_classic()
time_trip_plot_log

time_trip_plot_log_points = ggplot(df_trips, aes(x = Date, y = `log_Trips_per_24hrs`)) +
                geom_point(shape=23, fill="darkgreen", color="darkgreen", size=0.5) +
                scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
                xlab("Years") +
                ylab("Trips Per day for All Users") +
                ggtitle("Years vs. Trips Per day for All Users") +
                theme_classic()
time_trip_plot_log_points







# MILES
df_miles$Date = as.Date(df_miles$Date, format = "%Y-%m-%d")
head(df_trips$miles)
time_miles_plot = ggplot(df_miles, aes(x = Date, y = `log_Miles_per_24hrs`)) +
                geom_point(shape=23, fill="red", color="purple", size=0.5) +
                scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
                xlab("Years") +
                ylab("Trips Per day for All Users") +
                ggtitle("Years vs. Trips Per day for All Users") +
                theme_classic()
time_miles_plot


#wo.lm= lm(`Miles_traveled_today_.midnight_to_11.59_pm.`~ `Trips_over_the_past_24.hours_.midnight_to_11.59pm.`, data = df)
#summary(wo.lm)


trips = df$Trips_over_the_past_24.hours_.midnight_to_11.59pm.
plot(trips, type = "l")

(time_plot3 <- ggplot(trips, aes(x = Date, y = trips)) +
                geom_point(shape=23, fill="blue", color="green", size=0.5) +
                scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
                xlab("Years") +
                ylab("Trips Per day for All Users") +
                ggtitle("Years vs. Trips Per day for All Users") +
                theme_classic())


months(df$Date)
test= log(df$Miles_traveled_today_.midnight_to_11.59_pm.)
head(test)
model1 = lm(df$Miles_traveled_today_.midnight_to_11.59_pm.~months(df$Date))
lines(x= months(df$Date), y=df$Miles_traveled_today_.midnight_to_11.59_pm., col = "red")
length(df$Date)
length(model1~months(df$Date))
summary(model1)

acf(df$Miles_traveled_today_.midnight_to_11.59_pm., main = "ACF")

plot(rstudent(model1))
acf(rstudent(model1))
pacf(rstudent(model1))




