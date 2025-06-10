### Needed Libraries ###

options(scipen = 999) # Disable scientific notation

load("TM4_data.RData")

library(dplyr)
library(broom)
library(tidyr)

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
  filter(date <= as.Date("2020-01-01")) %>%
  group_by(TICKER) %>%
  filter(!is.na(log_ret) & !is.na(mkt_log_ret)) %>%
  do(tidy(lm(log_ret ~ mkt_log_ret, data = .))) %>%
  filter(term == "mkt_log_ret") %>%
  select(TICKER, beta = estimate)

# Add CAPM Beta to the dataframe

stock_data <- left_join(stock_data, beta_df, by = "TICKER")

## 3. Daily Abnormal Log Returns First Quarter 2020 ##

stock_data <- stock_data %>%
  mutate(
    abn_log_ret = log_ret - (beta * mkt_log_ret)
  )

daily_abn_ret<- stock_data %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-03-31")) %>%
  filter(!is.na(log_ret) & !is.na(mkt_log_ret) & !is.na(beta)) %>%
  mutate(
    abn_log_ret = log_ret - (beta * mkt_log_ret)
  )

## 4. Quarterly Abnormal Log Return First Quarter 2020 ## 

quarterly_abn_ret <- stock_data %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-03-31")) %>%
  mutate(
    year = format(date, "%Y"),
    quarter = paste0("Q", lubridate::quarter(date))
  ) %>%
  group_by(TICKER, year, quarter) %>%
  summarise(
    qtr_abn_log_return = sum(abn_log_ret, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Calculate the statistics on the daily returns 

# When need to take the average per company for the abnormal returns and caluclate
# the volatilities for the time series

Historic_volatility <- stock_data %>%
  group_by(TICKER) %>%
  summarise(
    hist_vol = sd(log_ret[ date >= as.Date("2019-01-01") & date <= as.Date("2020-01-01")], na.rm = TRUE) * sqrt(252)             
  )

daily_abn_ret_stats <- daily_abn_ret %>%
  group_by(TICKER) %>%
  summarise(
    n_obs_daily = n(),                                   
    vol = sd(log_ret, na.rm = TRUE) *sqrt(4),           
    idio_vol = sd(abn_log_ret, na.rm = TRUE)*sqrt(4),          
    daily_avg_abn_ret = mean(abn_log_ret, na.rm = TRUE) *100,   
    .groups = "drop"
  )

quarterly_abn_ret_stats <- quarterly_abn_ret %>%
  group_by(TICKER) %>%
  summarise(
    n_obs_quarter = n(),                                   
    quarterly_abn_ret = mean(qtr_abn_log_return, na.rm = TRUE) *100, 
    .groups = "drop"
  )

stock_stats_df <- left_join(daily_abn_ret_stats, Historic_volatility)
stock_stats_df <- left_join(stock_stats_df, quarterly_abn_ret_stats, by = "TICKER")

## 6. Join all three data frames now

# Rename ticker column
compustat_data <- compustat_data %>%
  rename(TICKER = `Ticker Symbol`)

esg_data <- esg_data %>%
  rename(TICKER = Ticker)

# Join to one data frame
Full_data_set <- left_join(compustat_data, esg_data, by = "TICKER" )
Full_data_set <- left_join(Full_data_set,stock_stats_df , by = "TICKER" )


## 7. Calculate Corporate Finance Indicators:

# Rename for easier handling
original_names <- names(Full_data_set)
cleaned_names <- gsub("\\r\\n", "", original_names)
cleaned_names <- gsub(" ", "_", cleaned_names)
cleaned_names <- gsub("\\(", "_", cleaned_names)  # Escaped (
cleaned_names <- gsub("\\)", "", cleaned_names)   # Escaped )
names(Full_data_set) <- cleaned_names


Full_data_set <- Full_data_set %>%
  mutate(
    Market_Equity = Company_Market_Cap_USD / 1000000,
    tobin_q = (`Assets_-_Total` - `Common/Ordinary_Equity_-_Total` + Market_Equity) / `Assets_-_Total`,
    Leverage = (`Debt_in_Current_Liabilities_-_Total` + `Long-Term_Debt_-_Total`) / `Assets_-_Total`,
    ROE = `Net_Income__Loss` / `Common/Ordinary_Equity_-_Total`,
    Dividend_yield = `Dividends_per_Share_-_Ex-Date_-_Fiscal` / `Price_Close_-_Annual_-_Calendar` * 100,
    Size = log(1+`Sales/Turnover__Net`),
    Cash = `Cash_and_Short-Term_Investments` / `Assets_-_Total`,
    Advertising = Advertising_Expense / `Assets_-_Total`,
    ESG = ESG_Score_FY2018 / 100
  )

## 8. Winsorize Function the at 1% level ##

winsorize <- function(x, p = 0.01) {
  quantiles <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, quantiles[2]), quantiles[1])
}

## 9. Redo the summary table ##

summary_vars <- Full_data_set %>%
  select(
    TICKER,
    quarterly_abn_ret, 
    ESG, 
    tobin_q, 
    Size, 
    Cash, 
    Leverage, 
    ROE, 
    Advertising, 
    hist_vol, 
    Dividend_yield, 
    vol, 
    idio_vol, 
    daily_avg_abn_ret,
    n_obs_daily,
    n_obs_quarter) %>%
  mutate(across(
    c(tobin_q, 
      Size, 
      Cash, 
      Leverage, 
      ROE, 
      Advertising),
    \(x) winsorize(x, p = 0.01)
  ))


summary_stats <- summary_vars %>%
  summarise(across(
    -c(TICKER, n_obs_daily, n_obs_quarter),
    list(
      Obs = ~ sum(!is.na(.)),
      Mean = ~ mean(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      `25%` = ~ quantile(., 0.25, na.rm = TRUE),
      Median = ~ quantile(., 0.5, na.rm = TRUE),
      `75%` = ~ quantile(., 0.75, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(Obs|Mean|SD|25%|Median|75%)$"
  )

summary_stats$Obs[nrow(summary_stats)] <- sum(summary_vars$n_obs_daily, na.rm= TRUE)



summary_stats <- summary_stats %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

knitr::kable(summary_stats, caption = "Table 1: Summary Statistics")

############# Question 2 #############################

############ Table 2 #################################
Full_data_set$TRBC_Industry_Name <- as.factor(Full_data_set$TRBC_Industry_Name)

# We create a data set with the quarterly abnormal returns, the ESG score, and the industry
quarterly_esg_data <- Full_data_set %>%
  select(TICKER, Company_Name.y, quarterly_abn_ret, ESG_Score_FY2018, TRBC_Industry_Name) %>%
  filter(!is.na(quarterly_abn_ret) & !is.na(ESG_Score_FY2018) & !is.na(TRBC_Industry_Name))

# Remove any rows that have duplicates in the TICKER and TRBC_Industry_Name columns
quarterly_esg_data <- quarterly_esg_data %>%
  distinct(TICKER, TRBC_Industry_Name, .keep_all = TRUE)

# Regress the quarterly abnormal returns on the ESG score
esg_regression <- lm(quarterly_abn_ret ~ ESG_Score_FY2018, data = quarterly_esg_data)
summary(esg_regression)

# Change industry_dummies to factors
quarterly_esg_data$TRBC_Industry_Name <- as.factor(quarterly_esg_data$TRBC_Industry_Name)
summary(quarterly_esg_data)
# Regress the quarterly abnormal returns on the ESG score and industry_dummies
industry_dummies <- model.matrix(~ TRBC_Industry_Name - 1, data = quarterly_esg_data)

esg_industry_regression <- lm(quarterly_abn_ret ~ ESG_Score_FY2018 + industry_dummies, data = quarterly_esg_data)
summary(esg_industry_regression)

# Regress the quarterly abnormal returns on the ESG score, firm controls, and industry dummies
firm_controls <- c("tobin_q", "Company_Market_Cap_USD", "Cash_and_Short-Term_Investments", 
                     "Leverage", "ROE", "Advertising_Expense", "hist_vol", "Dividend_yield")

quarterly_esg_controls_data <- Full_data_set %>%
  select(TICKER, quarterly_abn_ret, ESG_Score_FY2018, TRBC_Industry_Name, all_of(firm_controls)) %>%
  filter(!is.na(quarterly_abn_ret) & !is.na(ESG_Score_FY2018) & !is.na(TRBC_Industry_Name))

# Remove any rows that have duplicates in the TICKER and TRBC_Industry_Name columns
quarterly_esg_controls_data <- quarterly_esg_controls_data %>%
  distinct(TICKER, TRBC_Industry_Name, .keep_all = TRUE)

# remove rows with NAs
quarterly_esg_controls_data <- quarterly_esg_controls_data %>%
  filter(complete.cases(.))

# Change the column name "Cash_and_Short-Term_Investments" to "Cash_and_Short_Term_Investments"
quarterly_esg_controls_data <- quarterly_esg_controls_data %>%
  rename(Cash_and_Short_Term_Investments = `Cash_and_Short-Term_Investments`)

summary(quarterly_esg_controls_data)

# Regress the quarterly abnormal returns on the ESG, firm controls, and industry dummies
esg_firm_controls_regression <- lm(
  quarterly_abn_ret ~ ESG_Score_FY2018 + 
    tobin_q + Company_Market_Cap_USD + 
    Cash_and_Short_Term_Investments + 
    Leverage + ROE + Advertising_Expense + 
    hist_vol + Dividend_yield + 
    TRBC_Industry_Name, 
  data = quarterly_esg_controls_data
)
summary(esg_firm_controls_regression)
str(quarterly_esg_controls_data)
summary(quarterly_esg_controls_data)
summary(quarterly_esg_controls_data$TRBC_Industry_Name)

# For each of the three regression models, we will make the standard errors heteroskedasticity robust using white 
# standard errors.

# Load the necessary package for robust standard errors
if (!require("sandwich")) install.packages("sandwich")
library(sandwich)
# Load the necessary package for robust standard errors
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)
# Step 1: Calculate robust standard errors for each model
esg_regression_robust <- coeftest(esg_regression, vcov = vcovHC(esg_regression, type = "HC1"))
esg_industry_regression_robust <- coeftest(esg_industry_regression, vcov = vcovHC(esg_industry_regression, type = "HC1"))
esg_firm_controls_regression_robust <- coeftest(esg_firm_controls_regression, vcov = vcovHC(esg_firm_controls_regression, type = "HC1"))

# For each of the three regression models, we will create a table using the stargazer package.
if (!require("stargazer")) install.packages("stargazer")
library(stargazer)

# Place your models into a list for stargazer
model_list <- list(esg_regression_robust, esg_industry_regression_robust, esg_firm_controls_regression_robust)

# Create a vector of labels for the final table.
covariate_labels <- c("ES", "Tobin's q", "Size", "Cash", "Leverage", "ROE",
                      "Advertising", "Historical volatility", "Dividend")

# Generate the table
table_2 <- stargazer(
  model_list,
  type = "text", # Use "html" or "latex" for reports; "text" for console output
  title = "Cross-sectional regressions for quarterly abnormal returns",
  
  # --- Column & Row Labels ---
  column.labels = c("Abnormal Ret.", "Abnormal Ret.", "Abnormal Ret."),
  covariate.labels = covariate_labels,
  dep.var.labels.include = FALSE, # We will add the dependent var manually
  
  # --- Table Content & Layout ---
  omit = c("Constant", "industry_dummies", "TRBC_Industry_Name"), # Hide intercept and fixed effects coefficients
  add.lines = list(c("Industry FE", "No", "Yes", "Yes")),
  align = TRUE,
  
  # --- Model Statistics to Include ---
  keep.stat = c("n", "adj.rsq"), # Show number of observations and Adj. R-squared
  
  # --- Notes and Significance Stars ---
  star.cutoffs = c(0.1, 0.05, 0.01), # p-value thresholds for *, **, ***
  notes.align = "l",
  notes = "Standard errors are in parentheses.",
  notes.append = TRUE
)

# Assuming esg_industry_regression is your regression model object
# (e.g., from lm() or similar)

# 1. Manually list the problematic observation numbers from your warning message
problematic_obs_indices <- c(14, 47, 55, 61, 117, 120, 141, 157, 182, 253) # Add more if your warning listed them

# 2. Access the original data used to fit the model
# The original data frame used to fit the model can often be extracted using model.frame()
# or by directly referencing the data argument if you saved it.

# Method A: Using model.frame() (most robust if you didn't save the original data frame)
original_data <- model.frame(esg_industry_regression)

# 3. Examine the problematic observations
cat("--- Problematic Observations (Raw Data) ---\n")
print(original_data[problematic_obs_indices, ])

# 4. Calculate and inspect hat values (leverage scores)
# Hat values are a good way to quantify influence.
hat_values <- hatvalues(esg_industry_regression)

cat("\n--- Hat Values for Problematic Observations ---\n")
print(hat_values[problematic_obs_indices])

cat("\n--- Summary of Hat Values (Overall Distribution) ---\n")
summary(hat_values)

plot(hat_values, main = "Hat Values (Leverage Scores)",
     xlab = "Observation Index", ylab = "Hat Value")
points(problematic_obs_indices, hat_values[problematic_obs_indices], col = "red", pch = 16)
abline(h = 2 * (length(coef(esg_industry_regression))) / nrow(original_data),
       col = "blue", lty = 2) # Common cutoff for high leverage (2 * (p/n))
text(problematic_obs_indices, hat_values[problematic_obs_indices],
     labels = problematic_obs_indices, pos = 3, col = "red")

# 5. Compare problematic observations to the overall distribution

cat("\n--- Summary Statistics of All Variables in the Model ---\n")
summary(original_data)

############## Table 4 ######################################
Full_data_set$TRBC_Industry_Name <- as.factor(Full_data_set$TRBC_Industry_Name)

# We create a data set with the quarterly abnormal returns, the ESG score, and the industry
quarterly_esg_data_vol <- Full_data_set %>%
  select(TICKER, Company_Name.y, vol, idio_vol, ESG_Score_FY2018, TRBC_Industry_Name) %>%
  filter(!is.na(vol) & !is.na(idio_vol) & !is.na(ESG_Score_FY2018) & !is.na(TRBC_Industry_Name))

# Remove any rows that have duplicates in the TICKER and TRBC_Industry_Name columns
quarterly_esg_data_vol <- quarterly_esg_data_vol %>%
  distinct(TICKER, TRBC_Industry_Name, .keep_all = TRUE)

# Regress the vol on the ESG score
esg_regression_vol <- lm(vol ~ ESG_Score_FY2018, data = quarterly_esg_data_vol)
summary(esg_regression_vol)

# Regress the idio vol on the ESG score
esg_regression_idio_vol <- lm(idio_vol ~ ESG_Score_FY2018, data = quarterly_esg_data_vol)
summary(esg_regression_idio_vol)

# Change industry_dummies to factors
quarterly_esg_data_vol$TRBC_Industry_Name <- as.factor(quarterly_esg_data_vol$TRBC_Industry_Name)
summary(quarterly_esg_data_vol)
# Regress the quarterly abnormal returns on the ESG score and industry_dummies
industry_dummies <- model.matrix(~ TRBC_Industry_Name - 1, data = quarterly_esg_data_vol)

esg_industry_regression_vol <- lm(vol ~ ESG_Score_FY2018 + industry_dummies, data = quarterly_esg_data_vol)
summary(esg_industry_regression_vol)

esg_industry_regression_idio_vol <- lm(idio_vol ~ ESG_Score_FY2018 + industry_dummies, data = quarterly_esg_data_vol)
summary(esg_industry_regression_idio_vol)

# Regress the quarterly abnormal returns on the ESG score, firm controls, and industry dummies
firm_controls <- c("tobin_q", "Company_Market_Cap_USD", "Cash_and_Short-Term_Investments", 
                   "Leverage", "ROE", "Advertising_Expense", "hist_vol", "Dividend_yield")

quarterly_esg_controls_data_vol <- Full_data_set %>%
  select(TICKER, vol, idio_vol, ESG_Score_FY2018, TRBC_Industry_Name, all_of(firm_controls)) %>%
  filter(!is.na(vol) & !is.na(idio_vol) & !is.na(ESG_Score_FY2018) & !is.na(TRBC_Industry_Name))

# Remove any rows that have duplicates in the TICKER and TRBC_Industry_Name columns
quarterly_esg_controls_data_vol <- quarterly_esg_controls_data_vol %>%
  distinct(TICKER, TRBC_Industry_Name, .keep_all = TRUE)

# remove rows with NAs
quarterly_esg_controls_data_vol <- quarterly_esg_controls_data_vol %>%
  filter(complete.cases(.))

# Change the column name "Cash_and_Short-Term_Investments" to "Cash_and_Short_Term_Investments"
quarterly_esg_controls_data_vol <- quarterly_esg_controls_data_vol %>%
  rename(Cash_and_Short_Term_Investments = `Cash_and_Short-Term_Investments`)

# Regress the quarterly abnormal returns on the ESG, firm controls, and industry dummies
esg_firm_controls_regression_vol <- lm(
  vol ~ ESG_Score_FY2018 + 
    tobin_q + Company_Market_Cap_USD + 
    Cash_and_Short_Term_Investments + 
    Leverage + ROE + Advertising_Expense + 
    hist_vol + Dividend_yield + 
    TRBC_Industry_Name, 
  data = quarterly_esg_controls_data_vol
)

esg_firm_controls_regression_idio_vol <- lm(
  idio_vol ~ ESG_Score_FY2018 + 
    tobin_q + Company_Market_Cap_USD + 
    Cash_and_Short_Term_Investments + 
    Leverage + ROE + Advertising_Expense + 
    hist_vol + Dividend_yield + 
    TRBC_Industry_Name, 
  data = quarterly_esg_controls_data_vol
)
summary(esg_firm_controls_regression_vol)
summary(esg_firm_controls_regression_idio_vol)

# For each of the three regression models, we will make the standard errors heteroskedasticity robust using white 
# standard errors.

# Load the necessary package for robust standard errors
if (!require("sandwich")) install.packages("sandwich")
library(sandwich)
# Load the necessary package for robust standard errors
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)
# Step 1: Calculate robust standard errors for each model
esg_regression_robust_vol <- coeftest(esg_regression_idio_vol, vcov = vcovHC(esg_regression_idio_vol, type = "HC1"))
esg_regression_robust_idio_vol <- coeftest(esg_regression_idio_vol, vcov = vcovHC(esg_regression_idio_vol, type = "HC1"))

esg_industry_regression_robust_vol <- coeftest(esg_industry_regression_vol, vcov = vcovHC(esg_industry_regression_vol, type = "HC1"))
esg_industry_regression_robust_idio_vol <- coeftest(esg_industry_regression_idio_vol, vcov = vcovHC(esg_industry_regression_idio_vol, type = "HC1"))

esg_firm_controls_regression_robust_vol <- coeftest(esg_firm_controls_regression_vol, vcov = vcovHC(esg_firm_controls_regression_vol, type = "HC1"))
esg_firm_controls_regression_robust_idio_vol <- coeftest(esg_firm_controls_regression_idio_vol, vcov = vcovHC(esg_firm_controls_regression_idio_vol, type = "HC1"))

# Place your models into a list for stargazer
model_list_vol <- list(esg_regression_robust_vol, esg_industry_regression_robust_vol, 
                       esg_firm_controls_regression_robust_vol, esg_regression_robust_idio_vol, 
                       esg_industry_regression_robust_idio_vol, esg_firm_controls_regression_robust_idio_vol)

# Create a vector of labels for the final table.
covariate_labels_vol <- c("ES", "Tobin's q", "Size", "Cash", "Leverage", "ROE",
                      "Advertising", "Historical volatility", "Dividend")

# Generate the table
table_4 <- stargazer(
  model_list_vol,
  type = "text", # Use "html" or "latex" for reports; "text" for console output
  title = "Cross-sectional regressions for quarterly abnormal returns",
  
  # --- Column & Row Labels ---
  column.labels = c("Volatility", "Volatility", "Volatility", "Idio. Volatility", "Idio. Volatility", "Idio. Volatility"),
  covariate.labels = covariate_labels_vol,
  dep.var.labels.include = FALSE, # We will add the dependent var manually
  
  # --- Table Content & Layout ---
  omit = c("Constant", "industry_dummies", "TRBC_Industry_Name"), # Hide intercept and fixed effects coefficients
  add.lines = list(c("Industry FE", "No", "Yes", "Yes", "No", "Yes", "Yes")),
  align = TRUE,
  
  # --- Model Statistics to Include ---
  keep.stat = c("n", "adj.rsq"), # Show number of observations and Adj. R-squared
  
  # --- Notes and Significance Stars ---
  star.cutoffs = c(0.1, 0.05, 0.01), # p-value thresholds for *, **, ***
  notes.align = "l",
  notes = "Standard errors are in parentheses.",
  notes.append = TRUE
)
