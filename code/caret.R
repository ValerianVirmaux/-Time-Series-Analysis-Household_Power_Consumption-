library(Metrics)
library(caret)
library(e1071)
library(randomForest)
library(forecast)

# FIRST METHODE

df <- data.frame(value = granularity$MonthYear$Global_active_power_kwh)
str(x)
x <- gglagplot(df, lags = 12)
acf(df)

df_2 <- df %>% mutate(
  prior_1 = lag(value, 6),
  prior_2 = lag(value, 12),
  prior_3 = lag(value, 18),
  prior_4 = lag(value, 24),
  prior_5 = lag(value, 30),
  prior_6 = lag(value, 36)
)

train <- df_2[1:c(nrow(df_2) - 1),]
test <- df_2[nrow(df_2),]

mod_rf <- randomForest::randomForest(value ~ ., data = train, ntree = 1000, na.action = na.omit)
pred_rf_lag <- predict(mod_rf, test)
mape(pred_rf_lag, test$value) * 100 #10

mod_svm <- svm(value ~ ., data = train, kernel = "radial", cost = 1000)
pred_svm <- predict(mod_svm, test)
mape(pred_svm, test$value) * 100 # 10 %


# Second method

df_all <- granularity$MonthYear

df_all$year <- year(df_all$MonthYear)
df_all$month <- month(df_all$MonthYear)
df_all$quarter <- quarter(df_all$MonthYear)

column_name <- c("Global_active_power_kwh", "month", "year", "quarter")
df_all<- df_all[column_name]

test <- df_all %>% filter(year == 2010)
train <- df_all %>% filter(year != 2010)

RF <- train(Global_active_power_kwh ~ ., data = train, method = "rf")
pred_rf <- predict(RF, test)
mape(pred_rf, test$Global_active_power_kwh)
mase(pred_rf, test$Global_active_power_kwh) # 0.73

SVM <- svm(Global_active_power_kwh ~ ., data = train, kernel = "radial", cost=600,scale=F)
pred_svm <- predict(SVM, test)
mape(pred_svm, test$Global_active_power_kwh)
mase(pred_svm, test$Global_active_power_kwh) # 0.92

GBT <- train(Global_active_power_kwh ~ ., data = train, method = "adaboost")
pred_gbt <- predict(GBT, test)
mape(pred_gbt, test$Global_active_power_kwh)
mase(pred_gbt, test$Global_active_power_kwh) # 0.73

# Plot
to_plot <- ts(data.frame(test = test$Global_active_power_kwh, 
                         rf = pred_rf,
                         svm = pred_svm))
autoplot(to_plot)



