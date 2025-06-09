
### Needed Libraries ###

library(dplyr)
library(broom)

### Task 1: Merging the datasets ###

# We need
# ESG Indicator
# 1) ESG Rating

# Performance Indicator:
# Market is the S&P 500
# 1) Quarterly Abnormal Returns --> log return - capm beta * market log return
# 2) Daily Abnormal Return
# 3) Volatility
# 4) Idiosyncratic Volatility

# Corporate Finance Indicators:
# Statistics are winsorized at 1% level
# 1)Tobin's q
# 2)Size 
# 3)Cash
# 4)Leverage
# 5)ROE
# 6)Advertising
# 7)Dividend Yield

# To merge all three dataframes accordingly, we need to first caluclate the daily
# daily and quarterly abnormal log-return, volatility and idiosyncratic volatility

## 1. Handle the Date Column ##

# First check which rows are numeric serials
is_numeric_date <- grepl("^\\d{5}$", stock_data$date)
# Convert to Date using Excel origin
stock_data$date[is_numeric_date] <- as.character(
  as.Date(as.numeric(stock_data$date[is_numeric_date]), origin = "1899-12-30")
)
stock_data$date <- trimws(as.character(stock_data$date))

# Parse each format conditionally
# Use ISO-style parser if format is yyyy-mm-dd
iso_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", stock_data$date)
# Use d/m/y parser for dd/mm/yyyy
euro_format <- grepl("^\\d{2}/\\d{2}/\\d{4}$", stock_data$date)

# Initialize empty column
stock_data$parsed_date <- NA

# Parse ISO format
stock_data$parsed_date[iso_format] <- stock_data$date[iso_format]

# Parse European-style format
stock_data$parsed_date[euro_format] <- as.character(
  as.Date(stock_data$date[euro_format], format = "%d/%m/%Y")
)
# Convert all to Date
stock_data$parsed_date <- as.Date(stock_data$parsed_date)

# Clean the dataframe
stock_data <- stock_data %>%
  select(-date) %>%
  rename(date = parsed_date)

## 2. Calculate the daily Log Return for the company and for the market ##

stock_data <- stock_data %>%
  mutate(
    log_ret = log(1 + RET),
    mkt_log_ret = log(1 + sprtrn)
  )

## 3. Estimate Betas on the daily log returns ## 

# Regression for each Company

beta_df <- stock_data %>%
  group_by(TICKER) %>%
  filter(!is.na(log_ret) & !is.na(mkt_log_ret)) %>%
  do(tidy(lm(log_ret ~ mkt_log_ret, data = .))) %>%
  filter(term == "mkt_log_ret") %>%
  select(TICKER, beta = estimate)

# Add CAPM Beta to the dataframe

stock_data <- left_join(stock_data, beta_df, by = "TICKER")

#Calculate the abnormal daily log return

stock_data <- stock_data %>%
  mutate(
    abn_ret = log_ret - beta * mkt_log_ret
  )

## 4. Quarterly Abnormal Log Returns ## 

quarterly_abn_ret <- stock_data %>%
  mutate(
    year = format(date, "%Y"),
    quarter = paste0("Q", lubridate::quarter(date))
  ) %>%
  group_by(TICKER, year, quarter) %>%
  summarise(
    qtr_abn_log_return = sum(abn_ret, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Calculate the statistics on the daily returns 

# When need to take the average per company for the abnormal returns and caluclate
# the volatilities for the time series


stock_stats_df <- stock_data %>%
  group_by(TICKER) %>%
  summarise(
    n_obs_daily = n(),                                   
    hist_vol = sd(RET, na.rm = TRUE),              
    vol = sd(log_ret, na.rm = TRUE),           
    idio_vol = sd(abn_ret, na.rm = TRUE),          
    daily_abn_ret = mean(abn_ret, na.rm = TRUE),   
    .groups = "drop"
  )

stock_stats_q_df <- quarterly_abn_ret %>%
  group_by(TICKER) %>%
  summarise(
    n_obs_quarter = n(),                                   
    quarterly_abn_ret = mean(qtr_abn_log_return, na.rm = TRUE), 
    .groups = "drop"
  )

stock_stats_df <- left_join(stock_stats_df, stock_stats_q_df , by = "TICKER")

## 6. Join all three data frames now

# Rename ticker column
compustat_data <- compustat_data %>%
  rename(TICKER = `Ticker Symbol`)

esg_data <- esg_data %>%
  rename(TICKER = Ticker)

# Join to one data frame
Full_data_set <- left_join(compustat_data, esg_data, by = "TICKER" )
Full_data_set <- left_join(Full_data_set,stock_stats_df , by = "TICKER" )
