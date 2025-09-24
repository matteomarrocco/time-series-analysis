# Time Series Analysis

This repository contains the results of a **university project on time series econometrics**.  
The work is divided into two main parts, each implemented in **R**:

- `Time Series Economica.R` → 📊 **Economical Time Series Analysis: Natural Gas Price**  
- `Time Series Finanziaria.R` → 💹 **Financial Time Series Analysis: Texas Instruments Incorporated**

Both analyses focus on **modeling, diagnosing, and forecasting** time series data using econometric and statistical techniques.

## 📊 Economical Time Series Analysis: Natural Gas Price

**Objective**

The analyzed time series refers to the price of natural gas in the United States.
The goal of this project is to examine the dynamics of natural gas costs delivered to residential consumers, and to produce forecasts of future prices.

**Data Source**

The dataset was downloaded from the [U.S. Energy Information Administration (EIA)](https://www.eia.gov/totalenergy/data/monthly/).

It contains **452 monthly observations from January 1985 to August 2022**.
The unit of measurement is dollars per thousand cubic feet.

**Methodology**

- The data were analyzed using time series econometric techniques.

- Several transformations were attempted to stabilize the variance, but variability remained high.

- The selected forecasting model was an ARIMA(1,1,1) × (1,1,1)12, which incorporates both autoregressive and moving average components, as well as seasonal effects.

- The model also identified two anomalies in the dataset.

**Conclusion**

The analysis confirmed the challenges in stabilizing the variance of natural gas prices, as standard transformations did not significantly improve the results.

Despite this, the chosen seasonal ARIMA model provided forecasts for the next 12 months, offering insights into potential price trends for natural gas in the U.S. residential sector.


## 💹 Financial Time Series Anlaysis: Texas Instruments Incorporated

**Objective**

This project analyzes the stock price of Texas Instruments Incorporated, a global semiconductor company that designs, manufactures, tests, and sells analog and embedded processing chips.
The main goal of the analysis is to forecast future price movements, with a particular focus on volatility dynamics.

**Data Source**

The dataset was downloaded from [Yahoo Finance](https://finance.yahoo.com/).
It contains 12,736 daily observations from June 1, 1972 to November 30, 2022, expressed in USD.

**Preliminary Analysis**  

The analysis is not conducted on the entire dataset.  
Using the **Nyblom stability test**, significant parameter instability was detected in the full time series.  
As a result, the dataset was restricted to **1,238 daily observations**, covering the period from **January 1, 2018 to November 30, 2022**, to ensure more reliable modeling and inference.  

**Methodology**  

- The analysis focused on modeling the **log returns** of Texas Instruments’ stock price.  
- Several **GARCH-family models** were considered due to their ability to capture **conditional heteroskedasticity** in financial time series.  
- After model comparison, the most suitable candidates were identified as:  
  - **GJR-GARCH**  
  - **TGARCH**  
- Both models were used to perform volatility forecasting.  

**Conclusion**  
The analysis did not indicate a clear preference for a single model, as both the **GJR-GARCH** and **TGARCH** approaches provided meaningful insights.  
Forecasts of the **log returns** suggest relatively **stable expected values**, while forecasts of **volatility** show convergence toward the **unconditional variance** as the time horizon increases.  
