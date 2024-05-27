library(ggplot2)
library(Metrics)
library(tseries)
library(forcats)
library(forecast)



sea_lvl = read.csv('Merged_Sea_Temp_Dataset.csv')

## EDA


sea_lvl_gauge_full = subset(sea_lvl , select = c(Observed.GMSL..mean.)) ## full gauge data
sea_lvl_gauge_1993_b = subset(sea_lvl , Year < 1993, select = c(Observed.GMSL..mean.)) ## gauge data before 1993
sea_lvl_gauge_1993_a = subset(sea_lvl , Year >= 1993, select = c(Observed.GMSL..mean.)) ## gauge data after 1993

sea_lvl_sat = subset(sea_lvl , Year >= 1993 , select = c(Altimetry..mean.)) ## satellite data after 1993

sea_lvl_gauge_sat = c(sea_lvl_gauge_1993_b$Observed.GMSL..mean. ,sea_lvl_sat$Altimetry..mean.)

sea_lvl_gauge_sat = data.frame(mean_values = sea_lvl_gauge_sat) ## gauge(till 1993) + satellite data(after 1993)



summary(sea_lvl_gauge_full)
summary(sea_lvl_gauge_1993_b)
summary(sea_lvl_gauge_1993_a)
summary(sea_lvl_gauge_sat)
summary(sea_lvl_sat)

boxplot(sea_lvl_sat$Altimetry..mean., sea_lvl_gauge_1993_a$Observed.GMSL..mean.,
        main = "Boxplots for comparing Gauge Data and Satellite Data",
        at = c(1,2),
        names = c("S data" , "G data"),
        las = 1,
        col = c("orange","green"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

### RMSE between satellite data and gauge data from 1993 

rmse1 = rmse(sea_lvl_gauge_1993_a$Observed.GMSL..mean. , sea_lvl_sat$Altimetry..mean.)
rmse1 ## 4.801 very good value

### graphs 

### sea level rise plot with confidence interval 
ggplot(sea_lvl, aes(x = Year)) +
  geom_line(aes(y = Observed.GMSL..mean., color = "Mean"), size = 1) +
  geom_line(aes(y = Observed.GMSL..lower., color = "Lower Bound"), size = 1) +
  geom_line(aes(y = Observed.GMSL..upper., color = "Upper Bound"), size = 1) +
  labs(x = "Year", y = "Mean Sea Level Rise (mm)", title = "Plot of Sea Level rise versus Year with 95% Confidence Intervals for Gauge") +
  scale_color_manual(values = c("Mean" = "blue", "Lower Bound" = "red", "Upper Bound" = "green")) +
  theme_minimal()



### sea level rise plot with human gauge data and satellite data 
ggplot(sea_lvl, aes(x = Year)) +
  geom_line(aes(y = Observed.GMSL..mean., color = "Gauge Data"), size = 1) +
  geom_line(aes(y = ifelse(Year >= 1993,Altimetry..mean. , NA), color = "Satellite Data"), size = 1) +
  labs(x = "Year", y = "Mean Sea Level Rise (mm)", title = "Historical Time Series Plot of Sea Level rise with both gauge and satellite data ") +
  scale_color_manual(values = c("Gauge Data" = "green", "Satellite Data" = "red" )) +
  theme_minimal()

## data 1993
data1993 = subset(sea_lvl , Year >=1993)

### sea level rise of only gauge data vs satellite data from 1993
ggplot(data1993, aes(x = Year)) +
  geom_line(aes(y = Observed.GMSL..mean., color = "Gauge Data"), size = 1) +
  geom_line(aes(y = Altimetry..mean., color = "Satellite Data"), size = 1) +
  labs(x = "Year", y = "Mean Sea Level Rise (mm)", title = "Time Series Plot of Sea Level rise versus Year Showing Gauge and Satellite Data from 1993") +
  scale_color_manual(values = c("Gauge Data" = "blue", "Satellite Data" = "red")) +
  theme_minimal()

# Regression 

library(tidyverse)
library(caret)
library(mgcv)

sea_lvl_comb = read.csv('Final_data_sealevel_temp.csv')

combined_avg = (sea_lvl_comb$Avg.Land.Temp.Anomaly + sea_lvl_comb$Avg.Ocean.Temp.Anomaly) / 2
sea_lvl_comb$combined_avg = combined_avg


mPoly1 = lm(sealevel ~ poly(Avg.Ocean.Temp.Anomaly , 2) , data = sea_lvl_comb)
mPoly2 = lm(sealevel ~ poly(Avg.Land.Temp.Anomaly , 2) , data = sea_lvl_comb)
mPoly3 = lm(sealevel ~ poly(Avg.Land.Temp.Anomaly , 17) + poly(Avg.Ocean.Temp.Anomaly , 17), data = sea_lvl_comb)
mPoly4 = lm(sealevel ~ poly(combined_avg, 16) , data = sea_lvl_comb)
mPoly5 = lm(sealevel ~ poly(Avg.Land.Temp.Anomaly , 2) + poly(Avg.Ocean.Temp.Anomaly , 2) , data = sea_lvl_comb) # 15.66475


mGam1 = gam(sealevel ~ s(Avg.Ocean.Temp.Anomaly ,k= 5) , data = sea_lvl_comb)
mGam2 = lm(sealevel ~ poly(Avg.Land.Temp.Anomaly , 2) , data = sea_lvl_comb)
mGam3 = gam(sealevel ~ s(Avg.Land.Temp.Anomaly , k=20) + s(Avg.Ocean.Temp.Anomaly , k = 20), data = sea_lvl_comb)
mGam4 = gam(sealevel ~ s(combined_avg, k=20) , data = sea_lvl_comb)
mGam5 = gam(sealevel ~ s(Avg.Land.Temp.Anomaly , k=4) + s(Avg.Ocean.Temp.Anomaly , k=4) + s(combined_avg, k=4) , data = sea_lvl_comb) # 17.8

mLin1 = lm(sealevel ~ Avg.Ocean.Temp.Anomaly + Avg.Land.Temp.Anomaly + combined_avg, data = sea_lvl_comb)


predictions <- predict(mPoly5, sea_lvl_comb)
predictions1 = predict(mGam5, sea_lvl_comb)
predictions2 = predict(mLin1, sea_lvl_comb)

# Model performance
modelPerfomance <- data.frame(
  RMSE = caret :: RMSE(predictions, sea_lvl_comb$sealevel),
  R2 = caret :: R2(predictions, sea_lvl_comb$sealevel)
)

mPoly5
mLin1
modelPerfomance
summary(mGam5)

# Model performance

plot_data <- data.frame(
  Observed = sea_lvl_comb$sealevel,
  Predicted = predictions
)

# Create a scatterplot
ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Observed sea",
    y = "Predicted Sea Level",
    title = "Observed vs. Predicted Sea Level",
    caption = "Dashed red line represents perfect predictions"
  )

plot(mPoly5)

ggplot(sea_lvl_comb, aes(y = sealevel, x = combined_avg)) +
  geom_point(aes(color = "Data Points")) +
  geom_line(aes(y = predictions, color = "Polynomial Regression Line")) +
  labs(title = "Scatter Plot of Sea Level Rise versus Average Global Temperature Anomaly with Regression Line",
       x = "Mean Global Temperature Anomaly",
       y = "Mean Global Sea Level Rise")+
  scale_color_manual(values = c("Data Points" = "red" ,"Polynomial Regression Line" = "blue")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12))


errors = residuals(mPoly5)
arima_data = data.frame(sea_lvl_comb$Year , errors)

plot(arima_data$sea_lvl_comb.Year , arima_data$errors , type = 'l')

adf.test(arima_data$errors)# 0.015 , d = 0
kpss.test(arima_data$errors)

errors_2 <- diff(arima_data$errors)
errors_2

adf.test(errors_2)# 0.01 , d = 1
kpss.test(errors_2)

errors_2_data = data.frame(errors_2)
errors_2_data


plot(errors_2_data$errors_2 , type = 'l')

acf(errors_2_data$errors_2) # q = 1 or 2
pacf(errors_2_data$errors_2) # p = 1,2


ts_data_ac <- ts(data = sea_lvl_comb$sealevel, start = c(1900), end = c(2018), frequency = 1)
ts_data_pred <- ts(data = mPoly5$fitted.values, start = c(1900), end = c(2018), frequency = 1)
xregss = data.frame(sea_lvl_comb$Avg.Land.Temp.Anomaly , sea_lvl_comb$Avg.Ocean.Temp.Anomaly)
xregss = as.matrix(xregss)
xregss


ar_model = Arima(y = ts_data_ac , xreg = xregss, order = c(2,1,2))
checkresiduals(ar_model)

summary(ar_model)

ar_forecast <- forecast(ar_model, xreg = mPoly5$fitted.values)














