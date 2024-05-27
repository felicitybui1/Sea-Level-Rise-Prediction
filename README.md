# Sea-Level-Rise-Prediction

This project, "Rise and Forecast" aims to predict sea level rise based on temperature changes. Driven by ESG principles, Rise and Forecast seeks to answer the following questions:
1. What future business disruptions and consequences can be anticipated from various SSP scenarios in the context of global temperature and sea level changes?
2. How can measurements and forecasts of global sea level be applied to understand the implications of localized sea-level changes, such as the impact on New York City and Long Island coastlines?

<br>

## Model Construction

### Global Scope
This project seeks to combine regression and ARIMA models to build an ARIMAX model with strong predictive power. This process is broken down into three parts: building the regression model, building the ARIMA model, and constructing the ARIMAX model.

1. **EDA Visualization**:

![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/c41b53bd-8da0-4dc1-aed1-e79ee8a16a6c)
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/149dc613-95d8-4183-ad7b-400e918ef30f)

2.  **Regression Analysis**:
    - We performed regression analysis on mean global temperature against mean global sea-level rise. Temperature is the independent variable, and sea level rise is the dependent variable.
    - Several regression models were tested, including linear, polynomial, and GAM models. The aim is to avoid overfitting and seek the best-fit line that accurately describes the data points and regression errors.
    - Linear regression model was chosen, out of all regressions, to illustrate the relationship between temperature and sea-level rise efficiently while maintaining significant error values for ACF and PACF plots.

3. **ARIMA Model**:
    - We obtained auto-correlated residual errors from the regression model and analyzed the error patterns over time.
    - The Dickey-Fuller test (ADF) indicated that the errors were auto-correlated (p-value > 0.05), necessitating differencing to achieve stationarity. With d=1, the data became stationary, confirmed by the ADF test (p-value < 0.05) and KPSS test.
    - Optimal pairs of p and q values were determined from ACF and PACF analysis with d=1,2.

4. **ARIMAX Model**:
    - We incorporated the temperature regressor (exogenous variable) into the ARIMA model to build the ARIMAX model.
    - The model was trained on sea-level data from 1900 to 2000 and tested by forecasting sea levels from 2000 to 2022, using temperature data from the same period.
    - Fine-tuning minimized RMSE on the testing set, yielding an RMSE of 5.823 for the training set and 20.201 for the testing set. The model reduced test set RMSE by 39.14% from 33.193 to 20.201, indicating robust predictive capabilities for corresponding SSP scenarios.
  ![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/c673c9a2-63e4-4d94-99a9-6e285a365910)

4.1. **Model Comparison**:
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/7b651fe1-a065-4666-be21-cf48de3e84ad)



<br>


### Localized Scope: New York City and Long Island
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/f6901072-62c7-450c-a67d-8c621286de6e)
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/b5da7bd2-3937-4060-916c-681d2a8ae97e)

Following data cleaning and analysis, we constructed models for localized predictions using the same ARIMA methodology as the global model.

1. **Model for New York City**:
    - Parameter values (0,2,2) were derived using ACF plots, PACF plots, ADF test, and KPSS test.
    - The best ARIMAX model yielded an RMSE of 31.65.
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/298b9bac-7213-4808-bca2-ef15a2beb650)

2. **Model for Long Island**:
    - Parameter values (1,2,4) were used, derived through similar methods as NYC.
    - The best model yielded an RMSE of 34.54.
![image](https://github.com/felicitybui1/Sea-Level-Rise-Prediction/assets/168895497/7b4a4238-cb3c-4dc6-bf88-8d2edcfd4489)

