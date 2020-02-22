# Clean environment
rm(list = setdiff(ls(), c("HHPC", "granularity", "granularity_P")))

start_time <- Sys.time() # take 17 minutes to run

# Load packages
pacman::p_load(forecastHybrid, forecast)

#### SPLIT DATA ####

# Which granularity ?
per <- "month" # Take 2 minutes to run
#per <- "week" # Take 22 minutes to run
#per <- "day"

if (per == "month") {
  # Monthly
  TS_M <- ts(granularity$MonthYear$Global_active_power_kwh, frequency = 12, start=c(2007, 1))
  Training_Set <- window(TS_M, start=c(2007, 1), end=c(2009, 12))
  Testing_Set <- window(TS_M, start=c(2010, 1))
  h <- length(Testing_Set)  
} else if  (per == "week") {
  # Weekly
  TS_M <- ts(granularity$WeekYear$Global_active_power_kwh, frequency=52, start=c(2007, 1))
  Training_Set <- window(TS_M, start=c(2007, 1), end=c(2009, 52))
  Testing_Set <- window(TS_M, start=c(2010, 1))
  h <- length(Testing_Set)  
} else if (per == "day") {
  # Daily
  TS_M <- ts(granularity$Date$Global_active_power_kwh, frequency=frequency(granularity$Date$Global_active_power_kwh), start=c(2007, 1))
  Training_Set <- window(TS_M, start=c(2007, 1), end=c(2009, 364))
  Testing_Set <- window(TS_M, start=c(2010, 1))
  h <- length(Testing_Set)  
}

sd(granularity$Date$Global_active_power_kwh)
sd(granularity$Date$kitchen_kwh)
plot(granularity$Date$Global_active_power_kwh)
plot(granularity$Date$Global_active_power_kwh)

autoplot(TS_M)
autoplot(Training_Set)
autoplot(Testing_Set)

x <- decompose(TS_M)
(var(x$seasonal) / var(x$x)) * 100

#### FORECAST ####

# TSLM
TSLM <- forecast:::forecast.lm(tslm(Training_Set ~ trend + season), h=h)
TSLM_acc <- forecast::accuracy(TSLM, Testing_Set)
autoplot(TSLM)

forecastfunction <- function(x, h) {
  forecast:::forecast.lm(tslm(x ~ trend + season), h=h)
}
e <- tsCV(Training_Set, forecastfunction, h = 1)
TSLM_rmse <- sqrt(mean(e^2, na.rm=TRUE))

# HoltWinter
HoltWinter <- forecast:::forecast.HoltWinters(HoltWinters(Training_Set, beta=FALSE, gamma=TRUE), h=h)
HoltWinter_acc <- forecast::accuracy(HoltWinter, Testing_Set)
autoplot(HoltWinter)

forecastfunction <- function(x, h) {
  forecast:::forecast.HoltWinters(HoltWinters(x, beta=FALSE, gamma=TRUE), h=h)
}
e <- tsCV(Training_Set, forecastfunction, h = 1)
HoltWinter_rmse <- sqrt(mean(e^2, na.rm=TRUE))

# AUTO-ARIMA
ARIMA <- forecast:::forecast(auto.arima(Training_Set, trace = T), h=h, level=c(80,95))
ARIMA_acc <- forecast::accuracy(ARIMA, Testing_Set)
autoplot(ARIMA)
  
forecastfunction <- function(x, h) {
  forecast:::forecast.Arima(auto.arima(x, stepwise=FALSE, approximation=FALSE), h=h,  level=c(20,50))
}
e <- tsCV(Training_Set, forecastfunction, h = 1)
ARIMA_rmse <- sqrt(mean(e^2, na.rm=TRUE))

# ETS
if(per == "month"){
  ETS <- forecast::forecast(ets(Training_Set), h=h)
  ETS_acc <- forecast::accuracy(ETS, Testing_Set)
  autoplot(ETS)
  
  forecastfunction <- function(x, h) {
    forecast::forecast(ets(x), h=h)
  }
  e <- tsCV(Training_Set, forecastfunction, h = 1)
  ETS_rmse <- sqrt(mean(e^2, na.rm=TRUE))
}

# Hybrid
HModel <- forecast(hybridModel(Training_Set, weights="insample.errors"), h=h)
Hybrid_acc <- forecast::accuracy(HModel, Testing_Set)
autoplot(HModel)

forecastfunction <- function(x, h) {
  forecast(hybridModel(x, weights="insample.errors"), h=h)
}
e <- tsCV(Training_Set, forecastfunction, h = 1)
Hybrid_rmse <- sqrt(mean(e^2, na.rm=TRUE))

# Naive
NaiveModel <- snaive(Training_Set, h=h)
NaiveModel_acc <- forecast::accuracy(NaiveModel, Testing_Set)
autoplot(NaiveModel)

forecastfunction <- function(x, h) {
  NaiveModel <- snaive(Training_Set, h=h)
}
e <- tsCV(Training_Set, forecastfunction, h = 1)
NaiveModel_rmse <- sqrt(mean(e^2, na.rm=TRUE))

#### METRICS ####

# Store metrics
if(per == "month") {
  print(results <- data.frame("name" = c("TSLM", "HoltWinter", "ARIMA", "ETS", "Hybrid", "Naive"),
                              "mape" = c(TSLM_acc[2,5], HoltWinter_acc[2,5], ARIMA_acc[2,5], ETS_acc[2,5], Hybrid_acc[2,5], NaiveModel_acc[2,5]),
                              "rmse_cv" = c(TSLM_rmse, HoltWinter_rmse, ARIMA_rmse, ETS_rmse, Hybrid_rmse, NaiveModel_rmse),
                              "mase" = c(TSLM_acc[2,6], HoltWinter_acc[2,6], ARIMA_acc[2,6], ETS_acc[2,6], Hybrid_acc[2,6], NaiveModel_acc[2,6]), 
                              "ACF" = c(TSLM_acc[2,7], HoltWinter_acc[2,7], ARIMA_acc[2,7], ETS_acc[2,7], Hybrid_acc[2,7], NaiveModel_acc[2,7])))  
} else if (per == "week") {
  print(results <- data.frame("name" = c("TSLM", "HoltWinter", "ARIMA","Hybrid", "Naive"),
                              "mape" = c(TSLM_acc[2,5], HoltWinter_acc[2,5], ARIMA_acc[2,5], Hybrid_acc[2,5], NaiveModel_acc[2,5]),
                              "rmse_cv" = c(TSLM_rmse, HoltWinter_rmse, ARIMA_rmse, Hybrid_rmse, NaiveModel_rmse),
                              "mase" = c(TSLM_acc[2,6], HoltWinter_acc[2,6], ARIMA_acc[2,6], Hybrid_acc[2,6], NaiveModel_acc[2,6]), 
                              "ACF" = c(TSLM_acc[2,7], HoltWinter_acc[2,7], ARIMA_acc[2,7], Hybrid_acc[2,7], NaiveModel_acc[2,7])))  
}

#### Plot ####
if (per == "month") {
  df <- cbind(TS_M, TSLM$mean, HoltWinter$mean, ARIMA$mean, ETS$mean, HModel$mean, NaiveModel$mean)
  colnames(df) <- c("Data","TSLM", "HoltWinter", "ARIMA", "ETS", "Hybrid", "SNaive")
  autoplot(df) +
    xlab("Year") + ylab(expression("Euros by month"))  
} else if (per == "week"){
  df <- cbind(TS_M, TSLM$mean, HoltWinter$mean, ARIMA$mean, HModel$mean, NaiveModel$mean)
  colnames(df) <- c("Data","TSLM", "HoltWinter", "ARIMA","Hybrid", "SNaive")
  autoplot(df) +
    xlab("Year") + ylab(expression("Euros by month"))  
}

end_time <- Sys.time()

paste("The script run in", as.numeric(round(difftime(end_time,start_time,units="mins"), 2)), "minutes")

#### TEST

