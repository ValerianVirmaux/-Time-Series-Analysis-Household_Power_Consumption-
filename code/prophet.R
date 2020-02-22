pacman::p_load(prophet, Metrics)

#### SPLIT ####

#per = 'month'
#per = 'week'
per = 'day'

if(per == "month") {
  train <- granularity$MonthYear %>% filter(granularity$MonthYear$MonthYear < as.Date("2010-01-01"))
  test <- granularity$MonthYear %>% filter(granularity$MonthYear$MonthYear >= as.Date("2010-01-01"))
} else if (per == "week"){
  train <- granularity$WeekYear %>% filter(granularity$WeekYear$WeekYear < as.Date("2010-01-01"))
  test <- granularity$WeekYear %>% filter(granularity$WeekYear$WeekYear >= as.Date("2010-01-01"))
} else if (per == "day"){
  train <- granularity$Date %>% filter(granularity$Date$Date < as.Date("2010-01-01"))
  test <- granularity$Date %>% filter(granularity$Date$Date >= as.Date("2010-01-01"))
}

#### FORECAST ####
train <- data.frame(train[,c(1,3)])
colnames(train) <- c("ds", "y")

m <- prophet(train, seasonality.mode = "additive", 
             yearly.seasonality = T,
             daily.seasonality = T, 
             weekly.seasonality = T)

future <- make_future_dataframe(m, periods = nrow(test), freq = per)
forecast <- predict(m, future)

plot(m, forecast)
prophet_plot_components(m, forecast)

#### METRICS ####
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])

mape(test$Global_active_power_kwh, tail(forecast$yhat, n = nrow(test))) * 100
mase(test$Global_active_power_kwh, tail(forecast$yhat, n = nrow(test)), step_size = 1)
