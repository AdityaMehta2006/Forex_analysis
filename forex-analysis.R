# Forex Exchange Rate Analysis & Prediction in R

# Install and Load Required Packages
library(quantmod)
library(ggplot2)
library(forecast)
library(dplyr)
library(zoo)
library(lmtest)
library(TTR)

# Fetch Forex Data from Yahoo Finance
forex_pairs <- c("USDINR=X", "EURINR=X", "GBPINR=X", "JPYINR=X")
forex_data <- list()

for (pair in forex_pairs) {
  tryCatch({
    getSymbols(pair, src = "yahoo", from = "2022-01-01", to = "2024-01-01", auto.assign = TRUE)
    df <- data.frame(Date = index(get(pair)), coredata(get(pair)))
    df <- df %>% select(Date, ends_with("Close"))
    colnames(df) <- c("Date", pair)
    forex_data[[pair]] <- df
  }, error = function(e) {
    message("Data for ", pair, " not found. Skipping...")
  })
}

# Merge all forex data
forex_combined <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), forex_data)

# Plot Forex Exchange Rate Trends
ggplot(forex_combined, aes(x = Date)) +
  geom_line(aes(y = `USDINR=X`, color = "USD/INR")) +
  geom_line(aes(y = `EURINR=X`, color = "EUR/INR")) +
  geom_line(aes(y = `GBPINR=X`, color = "GBP/INR")) +
  geom_line(aes(y = `JPYINR=X`, color = "JPY/INR")) +
  labs(title = "Forex Exchange Rates (INR)", x = "Date", y = "Exchange Rate") +
  theme_minimal()

# Calculate and plot Moving Averages
forex_combined$SMA_50 <- rollmean(forex_combined$`USDINR=X`, 50, fill = NA, align = "right")
forex_combined$EMA_50 <- EMA(forex_combined$`USDINR=X`, n = 50)

ggplot(forex_combined, aes(x = Date)) +
  geom_line(aes(y = `USDINR=X`), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_50), color = "red", size = 1, linetype = "dashed") +
  geom_line(aes(y = EMA_50), color = "green", size = 1) +
  labs(title = "USD/INR Exchange Rate with 50-Day Moving Averages", x = "Date", y = "Exchange Rate") +
  theme_minimal()

# Predict Future Exchange Rates Using ARIMA
forex_ts <- ts(forex_combined$`USDINR=X`, frequency = 252)
model <- auto.arima(forex_ts)
forecast_data <- forecast(model, h = 30)

autoplot(forecast_data) +
  labs(title = "USD/INR Exchange Rate Prediction for Next 30 Days", x = "Days", y = "Exchange Rate") +
  theme_minimal()

# Perform Regression Analysis
regression_model <- lm(`USDINR=X` ~ `EURINR=X`, data = forex_combined, na.action = na.omit)
summary(regression_model)

ggplot(forex_combined, aes(x = `EURINR=X`, y = `USDINR=X`)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Analysis: USD/INR vs. EUR/INR", x = "EUR/INR Exchange Rate", y = "USD/INR Exchange Rate") +
  theme_minimal()

# Monte Carlo Simulation for USD/INR Exchange Rate
set.seed(123) # For reproducibility
n_simulations <- 1000  # Number of simulations
n_days <- 30           # Forecast period
last_price <- tail(forex_combined$`USDINR=X`, 1)
returns <- diff(log(forex_combined$`USDINR=X`), na.rm = TRUE)
mu <- mean(returns)
sigma <- sd(returns)

simulated_paths <- matrix(NA, nrow = n_days, ncol = n_simulations)
for (i in 1:n_simulations) {
  simulated_returns <- rnorm(n_days, mean = mu, sd = sigma)
  simulated_paths[, i] <- last_price * exp(cumsum(simulated_returns))
}

simulated_df <- data.frame(Day = 1:n_days, simulated_paths)

# Plot Monte Carlo Simulation
simulated_df_long <- tidyr::pivot_longer(simulated_df, -Day, names_to = "Simulation", values_to = "Price")

ggplot(simulated_df_long, aes(x = Day, y = Price, group = Simulation)) +
  geom_line(alpha = 0.1, color = "blue") +
  labs(title = "Monte Carlo Simulation for USD/INR Exchange Rate", x = "Days", y = "Exchange Rate") +
  theme_minimal()

# Save Data to CSV
csv_filename <- "forex_data.csv"
write.csv(forex_combined, csv_filename, row.names = FALSE, na = "")
message("CSV file saved at: ", getwd(), "/", csv_filename)
