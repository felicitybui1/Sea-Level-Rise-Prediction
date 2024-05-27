library(ggplot2)
library(Metrics)
library(tseries)
library(forecast)
library(caret)



# Read the data from the CSV file '2022_FINAL_DATA.csv' into the variable sea_lvl_comb
sea_lvl_comb = read.csv('2022_FINAL_DATA.csv')
sea_lvl_comb
sea_lvl_comb$Avg_Global_Temp_Anomaly

# Assign the values of 'Avg_Global_SL_Anomaly' column to a new column named 'sealevel' in sea_lvl_comb
# Compute 'sealevel_1993reg' by subtracting the value at index 94 from all the values in 'sealevel' column
sea_lvl_comb$sealevel = sea_lvl_comb$Avg_Global_SL_Anomaly

sea_lvl_comb$sealevel_1993reg = sea_lvl_comb$sealevel - sea_lvl_comb$sealevel[94]

# Create a line plot using ggplot, displaying 'Year' on the x-axis and 'Avg_Global_Temp_Anomaly' on the y-axis
# Color the line based on "Temperature Anomaly" and set plot aesthetics and theme
ggplot(sea_lvl_comb, aes(x = Year)) +
  geom_line(aes(y = Avg_Global_Temp_Anomaly, color = "Temperature Anomaly"), size = 1) +
  labs(x = "Year", y = "Global Temperature Anomaly Relative to 1901-2000 mean (°C)", title = "Plot of the External Regressor - Global Temperature Anomaly") +
  scale_color_manual(values = c("Temperature Anomaly" = "red")) +
  theme_minimal()

# Display summary statistics for the 'Avg_Global_Temp_Anomaly' column in sea_lvl_comb
summary(sea_lvl_comb$Avg_Global_Temp_Anomaly)

#####################################################################


# Fit a polynomial regression model (degree 3) using lm function:
# Predict 'sealevel_1993reg' based on a polynomial transformation of 'Avg_Global_Temp_Anomaly' with degree 3
mPoly5 = lm(sealevel_1993reg ~ poly(Avg_Global_Temp_Anomaly, 3)  , data = sea_lvl_comb) 

# Fit a linear regression model:
# Predict 'sealevel_1993reg' based on 'Avg_Global_Temp_Anomaly'
mLin1 = lm(sealevel_1993reg ~ Avg_Global_Temp_Anomaly, data = sea_lvl_comb)

# Fit a Gam Model
# Predict using a Gam Model 
mGam5 = gam(sealevel ~ s(Avg_Global_Temp_Anomaly , k = 2) , data = sea_lvl_comb)

# Generate predictions using the mLin1 linear regression model
predictions <- predict(mLin1, sea_lvl_comb)

# Compute model performance metrics (RMSE and R-squared)
modelPerfomance <- data.frame(
  RMSE = caret :: RMSE(predictions, sea_lvl_comb$sealevel_1993reg),
  R2 = caret :: R2(predictions, sea_lvl_comb$sealevel_1993reg)
)
# Calculate residuals from mLin1 linear regression model
errors = residuals(mLin1)
# Create a data frame combining 'Year' and errors
arima_data = data.frame(sea_lvl_comb$Year , errors)
# Create a time series of errors with start and end years and frequency specified
errors_of_regression = ts(data = errors, start = c(1900), end = c(2022), frequency = 1)

# Plot the errors_of_regression time series data
plot(errors_of_regression, type = 'l')

# Calculate the first differences of the errors_of_regression1 time series
errors_of_regression1 <- diff(arima_data$errors)
errors_of_regression1 = ts(data = errors_of_regression1, start = c(1900), end = c(2022), frequency = 1)

plot(diff(errors_of_regression1, type = 'l'))

# Conduct Augmented Dickey-Fuller test (adf.test) on errors_of_regression1 time series
# Conduct Kwiatkowski-Phillips-Schmidt-Shin test (kpss.test) on differenced errors_of_regression1 time series
adf.test(errors_of_regression1) # p = 0.0.01
kpss.test(diff(errors_of_regression1)) #p = 0.1

# Display model performance metrics
modelPerfomance

# Create a scatter plot using ggplot to visualize the relationship between 'sealevel_1993reg' and 'Avg_Global_Temp_Anomaly'
# Plot points using 'sealevel_1993reg' against 'Avg_Global_Temp_Anomaly' (x-axis)
# Add a regression line based on 'predictions'
ggplot(sea_lvl_comb, aes(y = sealevel_1993reg, x = Avg_Global_Temp_Anomaly )) +
  geom_point(aes(colour = "Original Data")) +
  geom_line(aes(y = predictions, color = "Regression Line")) +
  labs(title = "Scatter Plot of Sea Level Rise versus Average Global Temperature Anomaly",
       x = "Mean Global Temperature Anomaly (°C)",
       y = "Mean Global Sea Level Rise Relative to 1993 (mm)")+
  scale_color_manual(values = c("Original Data" = "black" ,"Regression Line" = "red")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12))

mLin1
#####################################################################

# Create a time series 'ts_data_ac' from 'sea_lvl_comb$sealevel_1993reg' with start and end years specified
ts_data_ac <- ts(data = sea_lvl_comb$sealevel_1993reg, start = c(1900), end = c(2022), frequency = 1)

# Divide the time series data into training and test sets
ts_data_train <- window(ts_data_ac,start=c(1900),end=c(2000))
ts_data_test <- window(ts_data_ac,start=c(2001),end=c(2022))

# Create time series 'xregss_time' and compute its differences 'xreggs_diff'
xregss_time = ts(data = sea_lvl_comb$Avg_Global_Temp_Anomaly, start = c(1900), end = c(2022), frequency = 1)
xreggs_diff = diff(xregss_test)

# Create data frames and matrices for ARIMAX modeling
xregss = data.frame(sea_lvl_comb$Avg_Global_Temp_Anomaly)
xregss = as.matrix(xregss)
xregss


xregss_train = data.frame(sea_lvl_comb$Avg_Global_Temp_Anomaly[1:101])
xregss_train = as.matrix(xregss_train)

xregss_test = data.frame(sea_lvl_comb$Avg_Global_Temp_Anomaly[101:123])
xregss_test = as.matrix(xregss_test)

# Fit ARIMAX (4,2,2) model with exogenous variable
ar_model = Arima(y = ts_data_train , xreg = xregss_train, order = c(4,2,2)) # ARIMAX

# Generate forecasts using the ARIMAX model
ar_forecast <- forecast(ar_model, xreg = xregss_test)
test_forecast = ar_forecast$mean

# Plot the forecasted values
plot(ar_forecast)

# Create a data frame 'test_plot' to combine historical, forecasted, and actual values for plotting
t_years = 2001 : 2022
test_plot = data.frame(year = sea_lvl_comb$Year, 
                       historical_data = ifelse(sea_lvl_comb$Year> 2000, NA, sea_lvl_comb$sealevel_1993reg) , 
                       fore = NA,
                       actual = NA
                       )
test_plot$fore[which(test_plot$year %in% t_years)] = test_forecast
test_plot$actual[which(test_plot$year %in% t_years)] = sea_lvl_comb$sealevel_1993reg[102:123]

test_plot

# Plot the historical data, forecasted values, and actual values using ggplot
ggplot(test_plot, aes(x = year)) +
  geom_line(aes(y = historical_data, color = "Historical Data (Train)"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, fore, NA), color = "Predictions"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, actual, NA), color = "Actual Sea Level Rise (Test)"), size = 1) +
  labs(x = "Year", y = "Mean Global Sea Level Rise Relative to 1993 Level (mm)", title = "ARIMAX (2,2,0) Plot for Actual data and Predicted values in time range 1900-2022 ") +
  scale_color_manual(values = c("Historical Data (Train)" = "black", "Predictions" = "red"  ,"Actual Sea Level Rise (Test)" = "green" )) +
  theme_minimal()

# Calculate accuracy metrics for the ARIMAX model
accuracy(ar_forecast, x = ts_data_ac)

###################################################################################################

# Read the NYC data from 'FINAL_NYC_DATA.csv' and subset data for years from 1900 onwards
nyc_data = read.csv('FINAL_NYC_DATA.csv')
nyc_data = subset(nyc_data, Year >= 1900)

# Calculate 'sea_lvl_1993' based on 'Avg_Sea_Level_Rise' relative to the value at index 94 and multiplied by 1000
nyc_data$sea_lvl_1993 = (nyc_data$Avg_Sea_Level_Rise - nyc_data$Avg_Sea_Level_Rise[94]) * 1000

# Create a time series 'ts_nyc' from 'nyc_data$sea_lvl_1993' with start and end years specified
ts_nyc = ts(nyc_data$sea_lvl_1993 , start = 1900 , end = 2022)

# Divide the time series data into training and test sets
ts_train_ny <- window(ts_nyc,start=c(1900),end=c(2000))
ts_test_ny <- window(ts_nyc,start=c(2001),end=c(2022))

# Fit ARIMAX (0,2,2) model with exogenous variable for NYC sea level rise data
arima_nyc = Arima(y = ts_train_ny , xreg = xregss_train , order = c(0,2,2)) 

# Generate forecasts using the ARIMAX model for NYC sea level rise data
ar_forecast_ny <- forecast(arima_nyc, xreg = xregss_test)
test_forecast_ny = ar_forecast_ny$mean
plot(ar_forecast_ny)

# Create a data frame 'test_plot_ny' to combine historical, forecasted, and actual values for plotting for NYC data
test_plot_ny = data.frame(year = nyc_data$Year, 
                       historical_data = ifelse(nyc_data$Year> 2000, NA, nyc_data$sea_lvl_1993) , 
                       fore = NA,
                       actual = NA
)
test_plot_ny$fore[which(test_plot_ny$year %in% t_years)] = test_forecast_ny
test_plot_ny$actual[which(test_plot_ny$year %in% t_years)] = nyc_data$sea_lvl_1993[102:123]


# Plot the historical data, forecasted values, and actual values using ggplot for NYC data
ggplot(test_plot_ny, aes(x = year)) +
  geom_line(aes(y = historical_data, color = "Historical Data (Train)"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, fore, NA), color = "Predictions"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, actual, NA), color = "Actual Sea Level Rise (Test)"), size = 1) +
  labs(x = "Year", y = "NYC Sea Level Rise Relative to 1993 Level (mm)", title = "ARIMAX (3,2,3) Plot for Actual data and Predicted values in time range 1900-2022 ") +
  scale_color_manual(values = c("Historical Data (Train)" = "black", "Predictions" = "red"  ,"Actual Sea Level Rise (Test)" = "green" )) +
  theme_minimal()

# Calculate accuracy metrics for the ARIMAX model for NYC data
accuracy(ar_forecast_ny, x = ts_nyc)

########################################################################################################

# Read the Long Island (LI) anomaly data from 'FINAL_LI_anomaly_data.csv' and calculate 'sea_lvl_1993'
li_data = read.csv('FINAL_LI_anomaly_data.csv')
li_data$sea_lvl_1993 = (li_data$Avg_Sea_Level_Rise - li_data$Avg_Sea_Level_Rise[47]) * 1000

# Create a time series 'ts_li' from 'li_data$sea_lvl_1993' with start and end years specified
ts_li = ts(li_data$sea_lvl_1993 , start = 1947 , end = 2022)

# Divide the time series data into training and test sets for LI data
ts_train_li <- window(ts_li,start=c(1947),end=c(2000))
ts_test_li <- window(ts_li,start=c(2001),end=c(2022))

# Fit ARIMAX (1,2,4) model with exogenous variable for LI sea level rise data
arima_li = Arima(y = ts_train_li , xreg = xregss_train[48:length(xregss_train)] , order = c(1,2,4)) 

# Generate forecasts using the ARIMAX model for LI sea level rise data
ar_forecast_li <- forecast(arima_li, xreg = xregss_test)
test_forecast_li = ar_forecast_li$mean
plot(ar_forecast_li)


# Create a data frame 'test_plot_li' to combine historical, forecasted, and actual values for plotting for LI data
test_plot_li = data.frame(year = li_data$Year, 
                          historical_data = ifelse(li_data$Year> 2000, NA, li_data$sea_lvl_1993) , 
                          fore = NA,
                          actual = NA
)
test_plot_li$fore[which(test_plot_li$year %in% t_years)] = test_forecast_li
test_plot_li$actual[which(test_plot_li$year %in% t_years)] = li_data$sea_lvl_1993[55:77]

test_plot_li

# Plot the historical data, forecasted values, and actual values using ggplot for LI data
ggplot(test_plot_li, aes(x = year)) +
  geom_line(aes(y = historical_data, color = "Historical Data (Train)"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, fore, NA), color = "Predictions"), size = 1) +
  geom_line(aes(y = ifelse(year >= 2000, actual, NA), color = "Actual Sea Level Rise (Test)"), size = 1) +
  labs(x = "Year", y = "LI Sea Level Rise Relative to 1993 Level (mm)", title = "ARIMAX (2,3,3) Plot for Actual data and Predicted values in time range 1947-2022 ") +
  scale_color_manual(values = c("Historical Data (Train)" = "black", "Predictions" = "red"  ,"Actual Sea Level Rise (Test)" = "green" )) +
  theme_minimal()

# Calculate accuracy metrics for the ARIMAX model for LI data
accuracy(ar_forecast_li, x = ts_li)

#########################################################################

# Original vector 'ssp1' and 'ssp5'
ssp1 = c(1.2507893, 1.5080144	,1.569444	,1.5580587	,1.508667	,1.4563461	,1.411167,	1.3726627,	1.3336911)

ssp1_reg = generate_incremented_values(ssp1)


ssp5 = c(1.2667643 ,1.6151066	,2.0419093, 2.527225	,3.020694,	3.5246368,	4.0536627,	4.5952874	,5.1201925
)
ssp5_reg = generate_incremented_values(ssp5)



# Define the function 'generate_incremented_values' to generate incremented values between elements in a vector
generate_incremented_values <- function(input_vector) {
  a <- vector()
  
  for (i in 1:(length(input_vector) - 1)) {
    start_value <- input_vector[i]
    end_value <- input_vector[i + 1]
    fixed_increment <- (end_value - start_value) / 10
    value_list <- seq(start_value, end_value, by = fixed_increment)
    a <- c(a, value_list)
    
  }
  a = a[!duplicated(a)]
  a = a[4:length(a)]
  return(a)
}
##########################################################

# Fit ARIMA models for different datasets
arima_f = Arima(y = ts_data_ac, xreg = xregss , order = c(4,2,2)) #4,2,2
summary(arima_li_f)
checkresiduals(arima_f)

# Fit ARIMA model for 'ts_nyc' with exogenous variable 'xregss' and order (0,2,2)
arima_ny_f = Arima(y = ts_nyc, xreg = xregss , order = c(0,2,2)) # 0,2,2

# Fit ARIMA model for 'ts_li' with subset of exogenous variable 'xregss' and order (1,2,4)
arima_li_f = Arima(y = ts_li, xreg = xregss[48:length(xregss)] , order = c(1,2,4)) # 1,2,4

##########################################################

# Generate forecasts using 'arima_f' model with exogenous variables 'ssp5_reg' and 'ssp1_reg'
ar_forecast_ssp5 <- forecast(arima_f, xreg = ssp5_reg)
ar_forecast_ssp1 <- forecast(arima_f, xreg = ssp1_reg)

# Plot the forecast for 'ar_forecast_ssp5'
plot(ar_forecast_ssp5)

# Create a data frame 'ssp_plot_data' with forecasted values for SSP1 and SSP5 scenarios
forecast_years = seq(2023, 2100, by = 1)
ssp_plot_data = data.frame(forecast_years, ar_forecast_ssp1$lower,ar_forecast_ssp5$upper)

ssp_plot_data

# Rename columns of 'ssp_plot_data' for clarity
ssp_plot_data <- ssp_plot_data %>%
  dplyr::rename(
    SSP1 = "X80.",
    SSP5 = "X80..1"
  )


# Create a dataframe 'ssp_total' combining historical and forecasted data for plotting
total_years = 1900 : 2100
available_data <- 2023:2100

ssp_total = data.frame(year = total_years , 
                       historical_data = ifelse(total_years > 2022, NA, sea_lvl_comb$sealevel_1993reg) , 
                       ssp1 = NA,
                       ssp5 = NA)
ssp_total$SSP1[which(ssp_total$year %in% available_data)] = ssp_plot_data$SSP1
ssp_total$SSP5[which(ssp_total$year %in% available_data)] = ssp_plot_data$SSP5

# Create a time series plot using ggplot for historical and forecasted sea level rise data
ggplot(ssp_total, aes(x = year)) +
  geom_line(aes(y = historical_data , color = "Actual Sea Level Rise"), linewidth = 0.8) +
  geom_line(aes(y = SSP1, color = "SSP Scenario 1.19"), linewidth = 0.8) +
  geom_line(aes(y = SSP5, color = "SSP Scenario 5.8.5"), linewidth = 0.8) +
  labs(x = "Year", y = "Mean Sea Level Rise Relative to 1993 (mm)", title = "Time Series Plot of Sea Level between 1900 and 2100 based on 2 different SSP Scenarios") +
  scale_color_manual(values = c("SSP Scenario 1.19" = "green", "SSP Scenario 5.8.5" = "red" , "Actual Sea Level Rise" = "black")) +
  theme_minimal()


########################################################################

# Generate forecasts for NYC data 'arima_ny_f' model with exogenous variables 'ssp5_reg' and 'ssp1_reg'
ar_forecast_ssp5_ny = forecast(arima_ny_f, xreg =  ssp5_reg)
ar_forecast_ssp1_ny = forecast(arima_ny_f, xreg = ssp1_reg)

# Plot the forecast for 'ar_forecast_ssp5_ny'
plot(ar_forecast_ssp5_ny)

# Create a data frame 'ssp_plot_data_ny' with forecasted values for SSP1 and SSP5 scenarios for NYC data
ssp_plot_data_ny = data.frame(forecast_years,ar_forecast_ssp1_ny$lower,ar_forecast_ssp5_ny$upper)

# Rename columns of 'ssp_plot_data_ny' for clarity
ssp_plot_data_ny <- ssp_plot_data_ny %>%
  dplyr::rename(
    SSP1 = "X80.",
    SSP5 = "X80..1"
  )

# Create a dataframe 'ssp_total_ny' combining historical and forecasted data for NYC data for plotting
ssp_total_ny = data.frame(year = total_years, 
                          historical_data = ifelse(total_years > 2022, NA, nyc_data$sea_lvl_1993 ) , 
                          ssp1 = NA,
                          ssp5 = NA)
ssp_total_ny$SSP1[which(ssp_total_ny$year %in% available_data)] = ssp_plot_data_ny$SSP1
ssp_total_ny$SSP5[which(ssp_total_ny$year %in% available_data)] = ssp_plot_data_ny$SSP5

# Create a time series plot using ggplot for historical and forecasted NYC sea level rise data
ggplot(ssp_total_ny, aes(x = year)) +
  geom_line(aes(y = historical_data , color = "Actual Sea Level Rise"), linewidth = 0.8) +
  geom_line(aes(y = SSP1, color = "SSP Scenario 1.19"), linewidth = 0.8) +
  geom_line(aes(y = SSP5, color = "SSP Scenario 5.8.5"), linewidth = 0.8) +
  labs(x = "Year", y = "NYC Mean Sea Level Rise Relative to 1993 (mm)", title = "Plot of NYC Sea Level between 1900 and 2100 based on 2 different SSP Scenarios") +
  scale_color_manual(values = c("SSP Scenario 1.19" = "green", "SSP Scenario 5.8.5" = "red" , "Actual Sea Level Rise" = "black")) +
  theme_minimal()

############################################################

# Generate forecasts for LI data 'arima_li_f' model with exogenous variables 'ssp5_reg' and 'ssp1_reg'
ar_forecast_ssp5_li = forecast(arima_li_f, xreg = ssp5_reg)
ar_forecast_ssp1_li = forecast(arima_li_f, xreg = ssp1_reg)

# Plot the forecast for 'ar_forecast_ssp5_li'
plot(ar_forecast_ssp5_li)

# Create a data frame 'ssp_plot_data_li' with forecasted values for SSP1 and SSP5 scenarios for LI data
ssp_plot_data_li = data.frame(forecast_years,ar_forecast_ssp1_li$lower,ar_forecast_ssp5_li$upper)

# Rename columns of 'ssp_plot_data_li' for clarity
ssp_plot_data_li <- ssp_plot_data_li %>%
  dplyr::rename(
    SSP1 = "X80.",
    SSP5 = "X80..1"
  )

# Create a dataframe 'ssp_total_li' combining historical and forecasted data for LI data for plotting
li_years = 1947 : 2100
ssp_total_li = data.frame(year = li_years, 
                          historical_data = ifelse(li_years > 2022, NA, li_data$sea_lvl_1993 ) , 
                          ssp1 = NA,
                          ssp5 = NA)
ssp_total_li$SSP1[which(ssp_total_li$year %in% available_data)] = ssp_plot_data_li$SSP1
ssp_total_li$SSP5[which(ssp_total_li$year %in% available_data)] = ssp_plot_data_li$SSP5

# Create a time series plot using ggplot for historical and forecasted LI sea level rise data
ggplot(ssp_total_li, aes(x = year)) +
  geom_line(aes(y = historical_data , color = "Actual Sea Level Rise"), linewidth = 0.8) +
  geom_line(aes(y = SSP1, color = "SSP Scenario 1.19"), linewidth = 0.8) +
  geom_line(aes(y = SSP5, color = "SSP Scenario 5.8.5"), linewidth = 0.8) +
  labs(x = "Year", y = "LI Mean Sea Level Rise Relative to 1993 (mm)", title = "Plot of LI Sea Level between 1947 and 2100 based on 2 different SSP Scenarios") +
  scale_color_manual(values = c("SSP Scenario 1.19" = "green", "SSP Scenario 5.8.5" = "red" , "Actual Sea Level Rise" = "black")) +
  theme_minimal()

###############################################################

# Combine Long Island (LI) sea level rise data with forecasted SSP5 and SSP1 data
li_ssp5 = c(li_data$sea_lvl_1993 , ssp_plot_data_li$SSP5)
li_ssp1 = c(li_data$sea_lvl_1993 , ssp_plot_data_li$SSP1)

li_csv = data.frame(li_years , li_ssp1 , li_ssp5)

# Combine New York City (NYC) sea level rise data with forecasted SSP5 and SSP1 data
ny_ssp5 = c(nyc_data$sea_lvl_1993 , ssp_plot_data_ny$SSP5)
ny_ssp1 = c(nyc_data$sea_lvl_1993 , ssp_plot_data_ny$SSP1)

ny_csv = data.frame(total_years , ny_ssp1 , ny_ssp5)

# Write the combined data frames to CSV files
write.csv(li_csv , 'li_ssp_data.csv')




