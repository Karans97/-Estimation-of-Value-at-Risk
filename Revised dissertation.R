#packages
require(quantmod);
require(PerformanceAnalytics)
library(robustbase)
library(PortfolioAnalytics)
library(portfolio.optimization)
library(evd)
library(extRemes)
library(evir)
library(tseries)
library(DEoptim)
library(xts)
library(POT)
library(ruga?ch)

#Start And End Date of the Sample 
start.date <- as.Date(c("2013-01-01"))
end.date <- as.Date(c("2023-01-30"))

#Apple Stock Prices
getSymbols("AAPL", from = start.date, to = end.date)
ApplePrices <- xts(AAPL$AAPL.Close)
AppleReturns <- log(ApplePrice?/Lag(ApplePrices,1))
AppleReturns <- AppleReturns[-1]



#Amazon Stock Prices
getSymbols("AMZN", from = start.date, to = end.date)
AmazonPrices <- xts(AMZN$AMZN.Close)
AmazonReturns <- log(AmazonPrices/Lag(AmazonPrices,1))
AmazonReturns <- AmazonReturns[-1?

#Netflix Stock Prices
getSymbols("NFLX", from = start.date, to = end.date)
NetflixPrices <- xts(NFLX$NFLX.Close)
NetflixReturns <- log(NetflixPrices/Lag(NetflixPrices,1))
NetflixReturns <- NetflixReturns[-1]

#Google Stock Prices
getSymbols("GOOGL", from?= start.date, to = end.date)
GooglePrices <- xts(GOOGL$GOOGL.Close)
GoogleReturns <- log(GooglePrices/Lag(GooglePrices,1))
GoogleReturns <- GoogleReturns[-1]

# Remove NA values from AppleReturns
AppleReturns <- na.omit(AppleReturns)

# Remove NA values fr?m AmazonReturns
AmazonReturns <- na.omit(AmazonReturns)

# Remove NA values from NetflixReturns
NetflixReturns <- na.omit(NetflixReturns)

# Remove NA values from GoogleReturns
GoogleReturns <- na.omit(GoogleReturns)



# Historical Simulation for Apple
po?tfolio_aapl <- AppleReturns
VaR_aapl <- quantile(portfolio_aapl,probs= c(0.05,0.025,0.01))

# Historical Simulation for Amazon
portfolio_amzn <- AmazonReturns
VaR_amzn <- quantile(portfolio_amzn, probs= c(0.05,0.025,0.01))

# Historical Simulation for Tesl?
portfolio_nflx <- NetflixReturns
VaR_nflx <- quantile(portfolio_nflx,probs= c(0.05,0.025,0.01))

# Historical Simulation for Google
portfolio_googl <- GoogleReturns
VaR_googl <- quantile(portfolio_googl, probs= c(0.05,0.025,0.01))


# Display the VaR resu?ts
print("VaR for Apple:")
print(VaR_aapl)

print("VaR for Amazon:")
print(VaR_amzn)

print("VaR for Netflix:")
print(VaR_nflx)

print("VaR for Google:")
print(VaR_googl)

# Calculate volatility for Apple
volatility_aapl <- sd(AppleReturns) * sqrt(252)  # ?ssuming 252 trading days in a year

# Calculate volatility for Amazon
volatility_amzn <- sd(AmazonReturns) * sqrt(252)

# Calculate volatility for Google
volatility_googl <- sd(GoogleReturns) * sqrt(252)

# Calculate volatility for Netflix
volatility_nflx ?- sd(NetflixReturns) * sqrt(252)



# Display the volatilities
print("Volatility for Apple:")
print(volatility_aapl)

print("Volatility for Amazon:")
print(volatility_amzn)

print("Volatility for Google:")
print(volatility_googl)

print("Volatility for Net?lix:")
print(volatility_nflx)


# VarCov Approach(Delta Normal)

# Calculate mean return and standard deviation for each stock

Applemean <- mean(AppleReturns)
Applesd <- sd(AppleReturns)

Amazonmean <- mean(AmazonReturns)
Amazonsd <- sd(AmazonReturns)

Ne?flixmean <- mean(NetflixReturns)
Netflixsd <- sd(NetflixReturns)

Googlemean <- mean(GoogleReturns)
Googlesd <- sd(GoogleReturns)

# Set the desired confidence levels
confidence_levels <- c(0.95, 0.99, 0.975)

# Loop over each confidence level
for (confide?ce_level in confidence_levels) {
  # Calculate the z-score corresponding to the confidence level
  z_score <- qnorm(1 - confidence_level)
  
  # Calculate individual stock VaR
  apple_var <- Applesd * z_score
  amazon_var <- Amazonsd * z_score
  netflix_va? <- Netflixsd * z_score
  google_var <- Googlesd * z_score
  
  # Print the individual stock VaR
  cat("For Confidence Level:", confidence_level * 100, "%\n")
  cat("Apple VaR: ", apple_var, "\n")
  cat("Amazon VaR: ", amazon_var, "\n")
  cat("Netflix VaR:?", netflix_var, "\n")
  cat("Google VaR: ", google_var, "\n\n")
}

#Skewness and Kurtosis
# Apple stock
skewness_apple <- skewness(AppleReturns)
kurtosis_apple <- kurtosis(AppleReturns)
cat("Apple Skewness:", skewness_apple, "\n")
cat("Apple Kurtosis:", ku?tosis_apple, "\n")

# Amazon stock
skewness_amazon <- skewness(AmazonReturns)
kurtosis_amazon <- kurtosis(AmazonReturns)
cat("Amazon Skewness:", skewness_amazon, "\n")
cat("Amazon Kurtosis:", kurtosis_amazon, "\n")

# Netflix stock
skewness_netflix <- skew?ess(NetflixReturns)
kurtosis_netflix <- kurtosis(NetflixReturns)
cat("Netflix Skewness:", skewness_netflix, "\n")
cat("Netflix Kurtosis:", kurtosis_netflix, "\n")

# Google stock
skewness_google <- skewness(GoogleReturns)
kurtosis_google <- kurtosis(Google?eturns)
cat("Google Skewness:", skewness_google, "\n")
cat("Google Kurtosis:", kurtosis_google, "\n")

# Jarque-Bera test
library(tseries)

# Apple stock
jarque_bera_test_apple <- jarque.bera.test(AppleReturns)
cat("Apple Jarque-Bera test p-value:", jarque?bera_test_apple$p.value, "\n")

# Amazon stock
jarque_bera_test_amazon <- jarque.bera.test(AmazonReturns)
cat("Amazon Jarque-Bera test p-value:", jarque_bera_test_amazon$p.value, "\n")

# Netflix stock
jarque_bera_test_netflix <- jarque.bera.test(NetflixRe?urns)
cat("Netflix Jarque-Bera test p-value:", jarque_bera_test_netflix$p.value, "\n")

# Google stock
jarque_bera_test_google <- jarque.bera.test(GoogleReturns)
cat("Google Jarque-Bera test p-value:", jarque_bera_test_google$p.value, "\n")



# Additional?package for normal distribution curve
library(ggplot2)

# Function to create histogram with normal curve
createHistogram <- function(returns, company) {
  returns.df <- data.frame(returns)
  colnames(returns.df) <- c("returns")
  p <- ggplot(returns.df, ae?(x=returns)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) + 
    stat_function(fun = dnorm, args = list(mean = mean(returns.df$returns, na.rm = TRUE), sd = sd(returns.df$returns, na.rm = TRUE)), color = "red", size = 1? +
    theme_minimal() +
    ggtitle(paste0(company, " returns histogram"))
  print(p)
}

# Create histograms
createHistogram(AppleReturns, "Apple")
createHistogram(AmazonReturns, "Amazon")
createHistogram(NetflixReturns, "Netflix")
createHistogram(GoogleR?turns, "Google")

#Monte carlo simulation

# Define the number of simulation paths
paths <- 1000

# Set the desired confidence levels
confidence_levels <- c(0.05, 0.01, 0.025)

# Function to calculate VaR using Monte Carlo simulation
calculate_VaR <- funct?on(returns, confidence_level) {
  # Calculate the mean and standard deviation of the returns
  mean_returns <- mean(returns,na.rm=TRUE)
  sd_returns <- sd(returns,na.rm = TRUE)
  
  # Perform Monte Carlo simulation
  simulated_returns <- mean_returns + sd_?eturns * rnorm(paths)
  
  # Calculate VaR using the simulated returns
  VaR <- quantile(simulated_returns, probs = confidence_level)
  
  # Return the VaR result
  return(VaR)
}

# Calculate VaR for the portfolio
portfolio_returns <- 0.25 * AppleReturns +?0.25 * AmazonReturns + 0.25 * NetflixReturns + 0.25 * GoogleReturns

# Loop over each confidence level
for (confidence_level in confidence_levels) {
  # Calculate VaR for individual stocks
  VaR_apple <- calculate_VaR(AppleReturns, confidence_level)
  VaR_?mazon <- calculate_VaR(AmazonReturns, confidence_level)
  VaR_netflix <- calculate_VaR(NetflixReturns, confidence_level)
  VaR_google <- calculate_VaR(GoogleReturns, confidence_level)
  
  VaR_portfolio <- calculate_VaR(portfolio_returns, confidence_level)?  
  # Print the VaR results
  cat("VaR at", (1 - confidence_level) * 100, "% confidence level:\n")
  cat("Apple:", VaR_apple, "\n")
  cat("Amazon:", VaR_amazon, "\n")
  cat("Netflix:", VaR_netflix, "\n")
  cat("Google:", VaR_google, "\n")
  cat("Portfolio?", VaR_portfolio, "\n\n")
}


#Extreme value theory

# Install and load the package
if (!require(evir)) install.packages("evir"); library(evir)

# Function to perform EVT analysis and return VaR
evt_analysis <- function(returns, threshold_quantile, var_qua?tile) {
  # Select data over the threshold
  threshold <- quantile(returns, threshold_quantile)
  excesses <- returns[returns > threshold] - threshold
  
  # Keep only positive excesses
  excesses <- excesses[excesses > 0]
  
  # Check if there's enough ta?l data
  if (length(excesses) > 10) { # Change the number as necessary
    # Fit Generalized Pareto Distribution (GPD)
    gpd_fit <- tryCatch(gpd(excesses, threshold),
                        error = function(e) return(NA)) # Handle errors
    
    if(!an?(is.na(gpd_fit))){
      # Calculate VaR
      var <- evd::qgpd(var_quantile, shape = gpd_fit$par.ests[1], scale = gpd_fit$par.ests[2])
      return(list(gpd_fit = gpd_fit, VaR = var))
    }
  }
  return(NA)
}


# Perform EVT analysis on each returns serie? and calculate VaR for different threshold and VaR quantiles
evt_models <- list()

# Define the desired VaR quantiles
var_quantiles <- c(0.95, 0.975, 0.99)

# For 95% threshold quantile
evt_models$AppleReturns_EVT_95 <- evt_analysis(AppleReturns, 0.95, var?quantiles)
evt_models$AmazonReturns_EVT_95 <- evt_analysis(AmazonReturns, 0.95, var_quantiles)
evt_models$NetflixReturns_EVT_95 <- evt_analysis(NetflixReturns, 0.95, var_quantiles)
evt_models$GoogleReturns_EVT_95 <- evt_analysis(GoogleReturns, 0.95, var_qu?ntiles)

# For 97.5% threshold quantile
evt_models$AppleReturns_EVT_975 <- evt_analysis(AppleReturns, 0.975, var_quantiles)
evt_models$AmazonReturns_EVT_975 <- evt_analysis(AmazonReturns, 0.975, var_quantiles)
evt_models$NetflixReturns_EVT_975 <- evt_analy?is(NetflixReturns, 0.975, var_quantiles)
evt_models$GoogleReturns_EVT_975 <- evt_analysis(GoogleReturns, 0.975, var_quantiles)

# For 99% threshold quantile
evt_models$AppleReturns_EVT_99 <- evt_analysis(AppleReturns, 0.99, var_quantiles)
evt_models$Amazon?eturns_EVT_99 <- evt_analysis(AmazonReturns, 0.99, var_quantiles)
evt_models$NetflixReturns_EVT_99 <- evt_analysis(NetflixReturns, 0.99, var_quantiles)
evt_models$GoogleReturns_EVT_99 <- evt_analysis(GoogleReturns, 0.99, var_quantiles)

# Print the EVT mod?ls and VaR
evt_models


#Backtesting for Historical Simulation by kupiec backtesting
#Kupiec Test
# Perform Kupiec test for each individual stock
# Set the confidence levels
confidence_levels <- c(0.95, 0.975, 0.99)

# Perform Kupiec test for each individu?l stock
stocks <- list(
  "Apple" = portfolio_aapl,
  "Amazon" = portfolio_amzn,
  "Netflix" = portfolio_nflx,
  "Google" = portfolio_googl
)

for (stock_name in names(stocks)) {
  # Get the actual returns for the stock
  actual_returns <- stocks[[stock_na?e]]
  
  for (level in confidence_levels) {
    # Calculate VaR using historical simulation for the stock
    VaR_stock <- quantile(actual_returns, probs = level)
    
    # Compute the exceedance indicator
    exceedance_indicator <- actual_returns < VaR_?tock
    
    # Count the number of exceedances
    num_exceedances <- sum(exceedance_indicator)
    
    # Perform the Kupiec test
    num_observations <- length(actual_returns)
    num_failures <- num_exceedances
    alpha <- 1 - level
    log_likelihood?<- num_failures * log(alpha) + (num_observations - num_failures) * log(1 - alpha)
    kupiec_test_statistic <- -2 * log_likelihood
    critical_value <- qchisq(1 - level, df = 1)
    
    # Compare the test statistic with the critical value
    if (kupiec_?est_statistic > critical_value) {
      print(paste0("Kupiec test for ", stock_name, " at ", level * 100, "%: VaR model rejected"))
    } else {
      print(paste0("Kupiec test for ", stock_name, " at ", level * 100, "%: VaR model accepted"))
    }
  }
}

? Backtesting for Historical simulation by Kupiec for last 500 observations
# Set the confidence levels
confidence_levels <- c(0.95, 0.975, 0.99)

# Perform Kupiec test for each individual stock
stocks <- list("Apple" = AppleReturns, "Amazon" = AmazonReturn?, "Netflix" = NetflixReturns, "Google" = GoogleReturns)
n_obs <- 500

for (stock_name in names(stocks)) {
  for (level in confidence_levels) {
    # Get the actual returns for the stock
    actual_returns <- tail(stocks[[stock_name]], n_obs)
    
    # Cal?ulate VaR using historical simulation for the stock
    VaR_stock <- quantile(actual_returns, probs = level)
    
    # Compute the exceedance indicator
    exceedance_indicator <- actual_returns < VaR_stock
    
    # Count the number of exceedances
    n?m_exceedances <- sum(exceedance_indicator)
    
    # Perform the Kupiec test
    num_observations <- length(actual_returns)
    num_failures <- num_exceedances
    alpha <- 1 - level
    log_likelihood <- num_failures * log(alpha) + (num_observations - nu?_failures) * log(1 - alpha)
    kupiec_test_statistic <- -2 * log_likelihood
    critical_value <- qchisq(1 - level, df = 1)
    
    # Compare the test statistic with the critical value
    if (kupiec_test_statistic > critical_value) {
      print(paste0(?Kupiec test for ", stock_name, " at ", level * 100, "% confidence level: VaR model rejected"))
    } else {
      print(paste0("Kupiec test for ", stock_name, " at ", level * 100, "% confidence level: VaR model accepted"))
    }
  }
}

#Variance Covariance?approach backtesting
#Backtesting for Var Cov Approach by Kupiec test
#Back for Var cov approach

# Set the desired confidence levels
confidence_levels <- c(0.95, 0.99, 0.975)

# Define the actual returns for each stock (sample data)
actual_returns_apple <? AppleReturns
actual_returns_amazon <- AmazonReturns
actual_returns_netflix <- NetflixReturns
actual_returns_google <- GoogleReturns

# Loop over each confidence level
for (confidence_level in confidence_levels) {
  # Calculate the z-score corresponding to?the confidence level
  z_score <- qnorm(1 - confidence_level)
  
  # Calculate individual stock VaR
  apple_var <- Applesd * z_score
  amazon_var <- Amazonsd * z_score
  netflix_var <- Netflixsd * z_score
  google_var <- Googlesd * z_score
  
  # Compute t?e exceedance indicator for each stock
  exceedance_indicator_apple <- actual_returns_apple < apple_var
  exceedance_indicator_amazon <- actual_returns_amazon < amazon_var
  exceedance_indicator_netflix <- actual_returns_netflix < netflix_var
  exceedance_i?dicator_google <- actual_returns_google < google_var
  
  # Count the number of exceedances for each stock
  num_exceedances_apple <- sum(exceedance_indicator_apple)
  num_exceedances_amazon <- sum(exceedance_indicator_amazon)
  num_exceedances_netflix <- ?um(exceedance_indicator_netflix)
  num_exceedances_google <- sum(exceedance_indicator_google)
  
  # Perform the Kupiec test for each stock
  num_observations <- length(actual_returns_apple)  # Assuming all stocks have the same number of observations
  num?failures_apple <- num_exceedances_apple
  num_failures_amazon <- num_exceedances_amazon
  num_failures_netflix <- num_exceedances_netflix
  num_failures_google <- num_exceedances_google
  alpha <- 1 - confidence_level
  log_likelihood_apple <- num_failures?apple * log(alpha) + (num_observations - num_failures_apple) * log(1 - alpha)
  log_likelihood_amazon <- num_failures_amazon * log(alpha) + (num_observations - num_failures_amazon) * log(1 - alpha)
  log_likelihood_netflix <- num_failures_netflix * log(alp?a) + (num_observations - num_failures_netflix) * log(1 - alpha)
  log_likelihood_google <- num_failures_google * log(alpha) + (num_observations - num_failures_google) * log(1 - alpha)
  kupiec_test_statistic_apple <- -2 * log_likelihood_apple
  kupiec_test?statistic_amazon <- -2 * log_likelihood_amazon
  kupiec_test_statistic_netflix <- -2 * log_likelihood_netflix
  kupiec_test_statistic_google <- -2 * log_likelihood_google
  critical_value <- qchisq(1 - confidence_level, df = 1)
  
  # Compare the test stat?stic with the critical value for each stock
  if (kupiec_test_statistic_apple > critical_value) {
    print(paste0("Kupiec test for Apple at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Apple at ", confi?ence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_amazon > critical_value) {
    print(paste0("Kupiec test for Amazon at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Amazon a? ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_netflix > critical_value) {
    print(paste0("Kupiec test for Netflix at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test ?or Netflix at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_google > critical_value) {
    print(paste0("Kupiec test for Google at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("K?piec test for Google at ", confidence_level * 100, "%: VaR model accepted"))
  }
}


#Backtesting for Var Cov Approach by Kupiec for last 500 observations
#Kupiec backtesting

# Set the desired confidence levels
confidence_levels <- c(0.95, 0.99, 0.975)

#?Define the actual returns for each stock (sample data)
actual_returns_apple <- tail(AppleReturns, 500)
actual_returns_amazon <- tail(AmazonReturns, 500)
actual_returns_netflix <- tail(NetflixReturns, 500)
actual_returns_google <- tail(GoogleReturns, 500)

? Loop over each confidence level
for (confidence_level in confidence_levels) {
  # Calculate the z-score corresponding to the confidence level
  z_score <- qnorm(1 - confidence_level)
  
  # Calculate individual stock VaR
  apple_var <- Applesd * z_score
 ?amazon_var <- Amazonsd * z_score
  netflix_var <- Netflixsd * z_score
  google_var <- Googlesd * z_score
  
  # Compute the exceedance indicator for each stock
  exceedance_indicator_apple <- actual_returns_apple < apple_var
  exceedance_indicator_amazon <? actual_returns_amazon < amazon_var
  exceedance_indicator_netflix <- actual_returns_netflix < netflix_var
  exceedance_indicator_google <- actual_returns_google < google_var
  
  # Count the number of exceedances for each stock
  num_exceedances_apple <- ?um(exceedance_indicator_apple)
  num_exceedances_amazon <- sum(exceedance_indicator_amazon)
  num_exceedances_netflix <- sum(exceedance_indicator_netflix)
  num_exceedances_google <- sum(exceedance_indicator_google)
  
  # Perform the Kupiec test for each ?tock
  num_observations <- length(actual_returns_apple)  # Assuming all stocks have the same number of observations
  num_failures_apple <- num_exceedances_apple
  num_failures_amazon <- num_exceedances_amazon
  num_failures_netflix <- num_exceedances_netf?ix
  num_failures_google <- num_exceedances_google
  alpha <- 1 - confidence_level
  log_likelihood_apple <- num_failures_apple * log(alpha) + (num_observations - num_failures_apple) * log(1 - alpha)
  log_likelihood_amazon <- num_failures_amazon * log(alp?a) + (num_observations - num_failures_amazon) * log(1 - alpha)
  log_likelihood_netflix <- num_failures_netflix * log(alpha) + (num_observations - num_failures_netflix) * log(1 - alpha)
  log_likelihood_google <- num_failures_google * log(alpha) + (num_obs?rvations - num_failures_google) * log(1 - alpha)
  kupiec_test_statistic_apple <- -2 * log_likelihood_apple
  kupiec_test_statistic_amazon <- -2 * log_likelihood_amazon
  kupiec_test_statistic_netflix <- -2 * log_likelihood_netflix
  kupiec_test_statistic_?oogle <- -2 * log_likelihood_google
  critical_value <- qchisq(1 - confidence_level, df = 1)
  
  # Compare the test statistic with the critical value for each stock
  if (kupiec_test_statistic_apple > critical_value) {
    print(paste0("Kupiec test for Ap?le at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Apple at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_amazon > critical_value) {
    print(paste0("Kupiec tes? for Amazon at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Amazon at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_netflix > critical_value) {
    print(paste0(?Kupiec test for Netflix at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Netflix at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_google > critical_value) {
    p?int(paste0("Kupiec test for Google at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Google at ", confidence_level * 100, "%: VaR model accepted"))
  }
}


#Monte Carlo simulation backtesting
#Kupiec backt?sting

# Set the desired confidence levels
confidence_levels <- c(0.05, 0.01, 0.025)

# Define the number of simulation paths
paths <- 1000

# Function to calculate VaR using Monte Carlo simulation
calculate_VaR <- function(returns, confidence_level) {
  #?Calculate the mean and standard deviation of the returns
  mean_returns <- mean(returns, na.rm = TRUE)
  sd_returns <- sd(returns, na.rm = TRUE)
  
  # Perform Monte Carlo simulation
  simulated_returns <- mean_returns + sd_returns * rnorm(paths)
  
  # Ca?culate VaR using the simulated returns
  VaR <- quantile(simulated_returns, probs = confidence_level)
  
  # Return the VaR result
  return(VaR)
}

# Calculate VaR for the portfolio
portfolio_returns <- 0.25 * AppleReturns + 0.25 * AmazonReturns + 0.25 * N?tflixReturns + 0.25 * GoogleReturns

# Loop over each confidence level
for (confidence_level in confidence_levels) {
  # Calculate VaR for individual stocks
  VaR_apple <- calculate_VaR(AppleReturns, confidence_level)
  VaR_amazon <- calculate_VaR(AmazonRe?urns, confidence_level)
  VaR_netflix <- calculate_VaR(NetflixReturns, confidence_level)
  VaR_google <- calculate_VaR(GoogleReturns, confidence_level)
  
  # Compute the exceedance indicator for each stock
  exceedance_indicator_apple <- AppleReturns < Va?_apple
  exceedance_indicator_amazon <- AmazonReturns < VaR_amazon
  exceedance_indicator_netflix <- NetflixReturns < VaR_netflix
  exceedance_indicator_google <- GoogleReturns < VaR_google
  
  # Count the number of exceedances for each stock
  num_exceed?nces_apple <- sum(exceedance_indicator_apple)
  num_exceedances_amazon <- sum(exceedance_indicator_amazon)
  num_exceedances_netflix <- sum(exceedance_indicator_netflix)
  num_exceedances_google <- sum(exceedance_indicator_google)
  
  # Perform the Kupiec?test for each stock
  num_observations <- length(AppleReturns)  # Assuming all stocks have the same number of observations
  num_failures_apple <- num_exceedances_apple
  num_failures_amazon <- num_exceedances_amazon
  num_failures_netflix <- num_exceedanc?s_netflix
  num_failures_google <- num_exceedances_google
  alpha <- 1 - confidence_level
  log_likelihood_apple <- num_failures_apple * log(alpha) + (num_observations - num_failures_apple) * log(1 - alpha)
  log_likelihood_amazon <- num_failures_amazon * ?og(alpha) + (num_observations - num_failures_amazon) * log(1 - alpha)
  log_likelihood_netflix <- num_failures_netflix * log(alpha) + (num_observations - num_failures_netflix) * log(1 - alpha)
  log_likelihood_google <- num_failures_google * log(alpha) + (?um_observations - num_failures_google) * log(1 - alpha)
  kupiec_test_statistic_apple <- -2 * log_likelihood_apple
  kupiec_test_statistic_amazon <- -2 * log_likelihood_amazon
  kupiec_test_statistic_netflix <- -2 * log_likelihood_netflix
  kupiec_test_sta?istic_google <- -2 * log_likelihood_google
  critical_value <- qchisq(1 - confidence_level, df = 1)
  
  # Compare the test statistic with the critical value for each stock
  if (kupiec_test_statistic_apple > critical_value) {
    print(paste0("Kupiec test?for Apple at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Apple at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_amazon > critical_value) {
    print(paste0("Kup?ec test for Amazon at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Amazon at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_netflix > critical_value) {
    print(?aste0("Kupiec test for Netflix at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Netflix at ", confidence_level * 100, "%: VaR model accepted"))
  }
  
  if (kupiec_test_statistic_google > critical_value) ?
    print(paste0("Kupiec test for Google at ", confidence_level * 100, "%: VaR model rejected"))
  } else {
    print(paste0("Kupiec test for Google at ", confidence_level * 100, "%: VaR model accepted"))
  }
}



#Backtesting for Monte Carlo Simulation b? Kupiec for last 500 observations
# Kupiec test
kupiec_test <- function(returns, VaR, confidence_level) {
  # Compute the exceedance indicator
  exceedance_indicator <- returns < VaR
  
  # Count the number of exceedances
  num_exceedances <- sum(exceedanc?_indicator)
  
  # Perform the Kupiec test
  num_observations <- length(returns)
  num_failures <- num_exceedances
  alpha <- 1 - confidence_level
  log_likelihood <- num_failures * log(alpha) + (num_observations - num_failures) * log(1 - alpha)
  kupiec_t?st_statistic <- -2 * log_likelihood
  critical_value <- qchisq(1 - confidence_level, df = 1)
  
  # Compare the test statistic with the critical value
  if (kupiec_test_statistic > critical_value) {
    return(paste("Kupiec test at", (1 - confidence_level)?* 100, "% confidence level: VaR model rejected"))
  } else {
    return(paste("Kupiec test at", (1 - confidence_level) * 100, "% confidence level: VaR model accepted"))
  }
}

# Loop over each confidence level
for (confidence_level in confidence_levels) {
? # Calculate VaR for individual stocks
  VaR_apple <- calculate_VaR(AppleReturns, confidence_level)
  VaR_amazon <- calculate_VaR(AmazonReturns, confidence_level)
  VaR_netflix <- calculate_VaR(NetflixReturns, confidence_level)
  VaR_google <- calculate_Va?(GoogleReturns, confidence_level)
  
  # Perform Kupiec test for the last 500 observations
  actual_returns_apple <- tail(AppleReturns, 500)
  actual_returns_amazon <- tail(AmazonReturns, 500)
  actual_returns_netflix <- tail(NetflixReturns, 500)
  actual_?eturns_google <- tail(GoogleReturns, 500)
  
  test_result_apple <- kupiec_test(actual_returns_apple, VaR_apple, confidence_level)
  test_result_amazon <- kupiec_test(actual_returns_amazon, VaR_amazon, confidence_level)
  test_result_netflix <- kupiec_test?actual_returns_netflix, VaR_netflix, confidence_level)
  test_result_google <- kupiec_test(actual_returns_google, VaR_google, confidence_level)
  
  # Print the Kupiec test results
  cat("Apple:", test_result_apple, "\n")
  cat("Amazon:", test_result_amazo?, "\n")
  cat("Netflix:", test_result_netflix, "\n")
  cat("Google:", test_result_google, "\n")
}


# Extreme Value theory backtesting
#Kupiec backtesting
# Function to perform Kupiec test
kupiec_test <- function(returns, VaR, alpha){
  # Number of excepti?ns
  exceptions <- sum(returns < -VaR)
  # Total number of observations
  N <- length(returns)
  # Expected number of exceptions
  expected_exceptions <- N * (1 - alpha)
  # Kupiec test statistic
  test_stat <- -2 * ((exceptions * log(exceptions / N) + 
  ?                     (N - exceptions) * log((N - exceptions) / N)) - 
                       (exceptions * log((1 - alpha)) + 
                          (N - exceptions) * log(alpha)))
  # P-value
  p_value <- 1 - pchisq(test_stat, df = 1)
  return(list(te?t_stat = test_stat, p_value = p_value))
}
# Define the list of model names and their corresponding threshold quantiles
model_names <- c("AppleReturns_EVT_95", "AmazonReturns_EVT_95", "NetflixReturns_EVT_95", "GoogleReturns_EVT_95",
                 "AppleR?turns_EVT_975", "AmazonReturns_EVT_975", "NetflixReturns_EVT_975", "GoogleReturns_EVT_975",
                 "AppleReturns_EVT_99", "AmazonReturns_EVT_99", "NetflixReturns_EVT_99", "GoogleReturns_EVT_99")

threshold_quantiles <- rep(0.95, length(model_name?))

# Perform Kupiec backtesting for each EVT model
backtest_results <- list()

for (i in 1:length(model_names)) {
  model_name <- model_names[i]
  threshold_quantile <- threshold_quantiles[i]
  
  if (!is.null(evt_models[[model_name]]) && "VaR" %in% names?evt_models[[model_name]])) {
    var <- evt_models[[model_name]]$VaR
    
    if (!is.na(var) && is.vector(var)) {
      returns_name <- gsub("_EVT_.*", "", model_name)
      returns <- get(returns_name)  # use get() to turn a string into a variable
      ?      backtest_results[[model_name]] <- kupiec_backtest(returns, var, threshold_quantile)
    } else {
      backtest_results[[model_name]] <- NA
    }
  } else {
    backtest_results[[model_name]] <- NA
  }
}

# Print the backtest results
backtest_results?

# Kupiec backtesting on Extreme Value Theory for last 500 observations
#Kupiec backtesting
# Function to perform Kupiec test
kupiec_test <- function(returns, VaR, alpha){
  # Number of exceptions
  exceptions <- sum(returns < -VaR)
  # Total number of ob?ervations
  N <- length(returns)
  # Expected number of exceptions
  expected_exceptions <- N * (1 - alpha)
  # Kupiec test statistic
  test_stat <- -2 * ((exceptions * log(exceptions / N) + 
                        (N - exceptions) * log((N - exceptions) ? N)) - 
                       (exceptions * log((1 - alpha)) + 
                          (N - exceptions) * log(alpha)))
  # P-value
  p_value <- 1 - pchisq(test_stat, df = 1)
  return(list(test_stat = test_stat, p_value = p_value))
}

# Define the list ?f model names and their corresponding threshold quantiles
model_names <- c("AppleReturns_EVT_95", "AmazonReturns_EVT_95", "NetflixReturns_EVT_95", "GoogleReturns_EVT_95",
                 "AppleReturns_EVT_975", "AmazonReturns_EVT_975", "NetflixReturns_EVT?975", "GoogleReturns_EVT_975",
                 "AppleReturns_EVT_99", "AmazonReturns_EVT_99", "NetflixReturns_EVT_99", "GoogleReturns_EVT_99")

threshold_quantiles <- c(0.95, 0.95, 0.95, 0.95, 0.975, 0.975, 0.975, 0.975, 0.99, 0.99, 0.99, 0.99)

# Perform?Kupiec backtesting for each EVT model
backtest_results <- list()

for (i in 1:length(model_names)) {
  model_name <- model_names[i]
  threshold_quantile <- threshold_quantiles[i]
  
  if (!is.null(evt_models[[model_name]]) && "VaR" %in% names(evt_models[[m?del_name]])) {
    var <- evt_models[[model_name]]$VaR
    
    if (!is.na(var) && is.vector(var)) {
      returns_name <- gsub("_EVT_.*", "", model_name)
      returns <- get(returns_name)  # use get() to turn a string into a variable
      
      # Selec? only the last 500 observations
      var <- tail(var, 500)
      returns <- tail(returns, 500)
      
      backtest_results[[model_name]] <- kupiec_backtest(returns, var, threshold_quantile)
    } else {
      backtest_results[[model_name]] <- NA
    }
 ?} else {
    backtest_results[[model_name]] <- NA
  }
}

# Print the backtest results
backtest_results




#TUFF backtesting

#TUFF test on Historical Simulation
# List of portfolios
portfolios <- list(
  "Apple" = portfolio_aapl,
  "Amazon" = portfolio_am?n,
  "Netflix" = portfolio_nflx,
  "Google" = portfolio_googl
)

# List of VaR estimates
var_estimates <- list(
  "Apple" = VaR_aapl,
  "Amazon" = VaR_amzn,
  "Netflix" = VaR_nflx,
  "Google" = VaR_googl
)

# Function for TUFF backtest
tuff_backtest <- fun?tion(actual_returns, var_estimates) {
  
  # Calculate hits
  hits <- actual_returns < -var_estimates
  
  # Traffic Light Test
  tlt <- sum(hits) / length(actual_returns)
  
  # Unconditional Coverage Test
  uct <- ifelse(sum(actual_returns < -var_estimat?s) != 0,
                sum(hits) / sum(actual_returns < -var_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hits == TRUE]) / length(hits)
  
  # Failure Rate
  fr <- ifelse(sum(hits) != 0, 
               length(hits[hits == TR?E]) / sum(hits),
               NA)
  
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Apply TUFF backtest on each portfolio
for (stock_name in names(portfolios)) {
  print(paste("TUFF Backtest for", stock_name, ":"))
  print(tuff_backtest(port?olios[[stock_name]], var_estimates[[stock_name]]))
}

#TUFF backtesting on Historical Simulation for last 500 observations
#TUFF test
# List of portfolios
portfolios <- list(
  "Apple" = portfolio_aapl,
  "Amazon" = portfolio_amzn,
  "Netflix" = portfolio_?flx,
  "Google" = portfolio_googl
)

# List of VaR estimates
var_estimates <- list(
  "Apple" = VaR_aapl,
  "Amazon" = VaR_amzn,
  "Netflix" = VaR_nflx,
  "Google" = VaR_googl
)

# Function for TUFF backtest
tuff_backtest <- function(actual_returns, var_es?imates) {
  
  # Slice the last 500 observations
  actual_returns <- tail(actual_returns, 500)
  var_estimates <- tail(var_estimates, 500)
  
  # Calculate hits
  hits <- actual_returns < -var_estimates
  
  # Traffic Light Test
  tlt <- sum(hits) / length?actual_returns)
  
  # Unconditional Coverage Test
  uct <- ifelse(sum(actual_returns < -var_estimates) != 0,
                sum(hits) / sum(actual_returns < -var_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hits == TRUE]) / l?ngth(hits)
  
  # Failure Rate
  fr <- ifelse(sum(hits) != 0, 
               length(hits[hits == TRUE]) / sum(hits),
               NA)
  
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Apply TUFF backtest on each portfolio
for (stock_name in?names(portfolios)) {
  print(paste("TUFF Backtest for", stock_name, ":"))
  print(tuff_backtest(portfolios[[stock_name]], var_estimates[[stock_name]]))
}


#TUFF backtesting on Variance Covariance approach

#TUFF backtesting
# Function to perform TUFF back?esting
tuff_backtest <- function(actual_returns, VaR_estimates) {
  # Calculate hits
  hits <- actual_returns < -VaR_estimates
  
  # Traffic Light Test
  tlt <- sum(hits) / length(actual_returns)
  
  # Unconditional Coverage Test
  uct <- ifelse(sum(actu?l_returns < -VaR_estimates) != 0,
                sum(hits) / sum(actual_returns < -VaR_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hits == TRUE]) / length(hits)
  
  # Failure Rate
  fr <- ifelse(sum(hits) != 0, 
            ?  length(hits[hits == TRUE]) / sum(hits),
               NA)
  
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Perform TUFF backtesting for each stock
tuff_results <- list()

# For Apple
tuff_results$Apple_TUFF <- tuff_backtest(AppleReturns, a?ple_var)

# For Amazon
tuff_results$Amazon_TUFF <- tuff_backtest(AmazonReturns, amazon_var)

# For Netflix
tuff_results$Netflix_TUFF <- tuff_backtest(NetflixReturns, netflix_var)

# For Google
tuff_results$Google_TUFF <- tuff_backtest(GoogleReturns, google?var)

# Print the TUFF backtesting results
for (stock in names(tuff_results)) {
  cat("TUFF Backtest for", stock, ":\n")
  cat("TLT:", tuff_results[[stock]]$TLT, "\n")
  cat("UCT:", tuff_results[[stock]]$UCT, "\n")
  cat("FT:", tuff_results[[stock]]$FT, "\?")
  cat("FR:", tuff_results[[stock]]$FR, "\n\n")
}


#TUFF backtesting for Variance Covariance approach for last 500 observations
#TUFF backtesting
# Function to perform TUFF backtesting
# Slice the last 500 observations
actual_returns <- tail(actual_retu?ns, 500)
var_estimates <- tail(var_estimates, 500)
tuff_backtest <- function(actual_returns, VaR_estimates) {
  # Calculate hits
  hits <- actual_returns < -VaR_estimates
  
  # Traffic Light Test
  tlt <- sum(hits) / length(actual_returns)
  
  # Uncondit?onal Coverage Test
  uct <- ifelse(sum(actual_returns < -VaR_estimates) != 0,
                sum(hits) / sum(actual_returns < -VaR_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hits == TRUE]) / length(hits)
  
  # Failure Rate
? fr <- ifelse(sum(hits) != 0, 
               length(hits[hits == TRUE]) / sum(hits),
               NA)
  
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Perform TUFF backtesting for each stock
tuff_results <- list()

# For Apple
tuff_results?Apple_TUFF <- tuff_backtest(AppleReturns, apple_var)

# For Amazon
tuff_results$Amazon_TUFF <- tuff_backtest(AmazonReturns, amazon_var)

# For Netflix
tuff_results$Netflix_TUFF <- tuff_backtest(NetflixReturns, netflix_var)

# For Google
tuff_results$Google?TUFF <- tuff_backtest(GoogleReturns, google_var)

# Print the TUFF backtesting results
for (stock in names(tuff_results)) {
  cat("TUFF Backtest for", stock, ":\n")
  cat("TLT:", tuff_results[[stock]]$TLT, "\n")
  cat("UCT:", tuff_results[[stock]]$UCT, "\n?)
  cat("FT:", tuff_results[[stock]]$FT, "\n")
  cat("FR:", tuff_results[[stock]]$FR, "\n\n")
}


#TUFF backtesting for Monte Carlo Simulation

#TUFF backtesting
# Function to perform TUFF backtesting
tuff_backtest <- function(actual_returns, VaR_estimates? {
  # Calculate hits
  hits <- actual_returns < -VaR_estimates
  
  # Traffic Light Test
  tlt <- sum(hits) / length(actual_returns)
  
  # Unconditional Coverage Test
  uct <- ifelse(sum(actual_returns < -VaR_estimates) != 0,
                sum(hits) / ?um(actual_returns < -VaR_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hits == TRUE]) / length(hits)
  
  # Failure Rate
  fr <- ifelse(sum(hits) != 0, 
               length(hits[hits == TRUE]) / sum(hits),
               NA)
 ?
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Perform TUFF backtesting for each stock and the portfolio
tuff_results <- list()

# For Apple
tuff_results$Apple_TUFF <- tuff_backtest(AppleReturns, VaR_apple)

# For Amazon
tuff_results$Amazon_T?FF <- tuff_backtest(AmazonReturns, VaR_amazon)

# For Netflix
tuff_results$Netflix_TUFF <- tuff_backtest(NetflixReturns, VaR_netflix)

# For Google
tuff_results$Google_TUFF <- tuff_backtest(GoogleReturns, VaR_google)

# For the Portfolio
tuff_results$Portf?lio_TUFF <- tuff_backtest(portfolio_returns, VaR_portfolio)

# Print the TUFF backtesting results
for (stock in names(tuff_results)) {
  cat("TUFF Backtest for", stock, ":\n")
  cat("TLT:", tuff_results[[stock]]$TLT, "\n")
  cat("UCT:", tuff_results[[stock?]$UCT, "\n")
  cat("FT:", tuff_results[[stock]]$FT, "\n")
  cat("FR:", tuff_results[[stock]]$FR, "\n\n")
}

#TUFF backtesting for Monte Carlo Simulation for last 500 observations
#TUFF backtesting
# Function to perform TUFF backtesting
# Slice the last 500?observations
actual_returns <- tail(actual_returns, 500)
var_estimates <- tail(var_estimates, 500)
tuff_backtest <- function(actual_returns, VaR_estimates) {
  # Calculate hits
  hits <- actual_returns < -VaR_estimates
  
  # Traffic Light Test
  tlt <- su?(hits) / length(actual_returns)
  
  # Unconditional Coverage Test
  uct <- ifelse(sum(actual_returns < -VaR_estimates) != 0,
                sum(hits) / sum(actual_returns < -VaR_estimates),
                NA)
  
  # Frequency Test
  ft <- length(hits[hi?s == TRUE]) / length(hits)
  
  # Failure Rate
  fr <- ifelse(sum(hits) != 0, 
               length(hits[hits == TRUE]) / sum(hits),
               NA)
  
  return(list(TLT = tlt, UCT = uct, FT = ft, FR = fr))
}

# Perform TUFF backtesting for each stock ?nd the portfolio
tuff_results <- list()

# For Apple
tuff_results$Apple_TUFF <- tuff_backtest(AppleReturns, VaR_apple)

# For Amazon
tuff_results$Amazon_TUFF <- tuff_backtest(AmazonReturns, VaR_amazon)

# For Netflix
tuff_results$Netflix_TUFF <- tuff_backt?st(NetflixReturns, VaR_netflix)

# For Google
tuff_results$Google_TUFF <- tuff_backtest(GoogleReturns, VaR_google)

# For the Portfolio
tuff_results$Portfolio_TUFF <- tuff_backtest(portfolio_returns, VaR_portfolio)

# Print the TUFF backtesting results
for?(stock in names(tuff_results)) {
  cat("TUFF Backtest for", stock, ":\n")
  cat("TLT:", tuff_results[[stock]]$TLT, "\n")
  cat("UCT:", tuff_results[[stock]]$UCT, "\n")
  cat("FT:", tuff_results[[stock]]$FT, "\n")
  cat("FR:", tuff_results[[stock]]$FR, "\n\?")
}


#Extreme Value Theory Backtesting by TUFF
#TUFF backtesting

# Define the TUFF backtesting function
tuff_backtest <- function(actual_losses, predicted_losses, confidence_level) {
  # Calculate the number of observations
  n <- length(actual_losses)
? 
  # Calculate the number of exceedances
  exceedances <- sum(actual_losses > predicted_losses)
  
  # Calculate the number of non-exceedances
  non_exceedances <- n - exceedances
  
  # Calculate the test statistic
  tuff_statistic <- exceedances - (1 - ?onfidence_level) * n
  
  # Calculate the p-value
  p_value <- 1 - pnorm(tuff_statistic)
  
  # Return the p-value
  return(p_value)
}

# Perform TUFF backtesting for each EVT model
tuff_backtest_results <- list()

# For 95% threshold quantile
if (!is.na(e?t_models$AppleReturns_EVT_95) && all(!is.na(evt_models$AppleReturns_EVT_95$VaR)) &&
    is.vector(evt_models$AppleReturns_EVT_95$VaR)) {
  tuff_backtest_results$AppleReturns_EVT_95 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_95$VaR, 0.95)
} ?lse {
  tuff_backtest_results$AppleReturns_EVT_95 <- NA
}

if (!is.na(evt_models$AmazonReturns_EVT_95) && all(!is.na(evt_models$AmazonReturns_EVT_95$VaR)) &&
    is.vector(evt_models$AmazonReturns_EVT_95$VaR)) {
  tuff_backtest_results$AmazonReturns_EVT_95?<- tuff_backtest(AmazonReturns, evt_models$AmazonReturns_EVT_95$VaR, 0.95)
} else {
  tuff_backtest_results$AmazonReturns_EVT_95 <- NA
}

if (!is.na(evt_models$NetflixReturns_EVT_95) && all(!is.na(evt_models$NetflixReturns_EVT_95$VaR)) &&
    is.vector(evt?models$NetflixReturns_EVT_95$VaR)) {
  tuff_backtest_results$NetflixReturns_EVT_95 <- tuff_backtest(NetflixReturns, evt_models$NetflixReturns_EVT_95$VaR, 0.95)
} else {
  tuff_backtest_results$NetflixReturns_EVT_95 <- NA
}

if (!is.na(evt_models$GoogleRetu?ns_EVT_95) && all(!is.na(evt_models$GoogleReturns_EVT_95$VaR)) &&
    is.vector(evt_models$GoogleReturns_EVT_95$VaR)) {
  tuff_backtest_results$GoogleReturns_EVT_95 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_95$VaR, 0.95)
} else {
  tuff_?acktest_results$GoogleReturns_EVT_95 <- NA
}

# For 97.5% threshold quantile
if (!is.na(evt_models$AppleReturns_EVT_975) && all(!is.na(evt_models$AppleReturns_EVT_975$VaR)) &&
    is.vector(evt_models$AppleReturns_EVT_975$VaR)) {
  tuff_backtest_results$Ap?leReturns_EVT_975 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_975$VaR, 0.95)
} else {
  tuff_backtest_results$AppleReturns_EVT_975 <- NA
}

if (!is.na(evt_models$AmazonReturns_EVT_975) && all(!is.na(evt_models$AmazonReturns_EVT_975$VaR)) &&
?   is.vector(evt_models$AmazonReturns_EVT_975$VaR)) {
  tuff_backtest_results$AmazonReturns_EVT_975 <- tuff_backtest(AmazonReturns, evt_models$AmazonReturns_EVT_975$VaR, 0.95)
} else {
  tuff_backtest_results$AmazonReturns_EVT_975 <- NA
}

if (!is.na(evt_m?dels$NetflixReturns_EVT_975) && all(!is.na(evt_models$NetflixReturns_EVT_975$VaR)) &&
    is.vector(evt_models$NetflixReturns_EVT_975$VaR)) {
  tuff_backtest_results$NetflixReturns_EVT_975 <- tuff_backtest(NetflixReturns, evt_models$NetflixReturns_EVT_975$?aR, 0.95)
} else {
  tuff_backtest_results$NetflixReturns_EVT_975 <- NA
}

if (!is.na(evt_models$GoogleReturns_EVT_975) && all(!is.na(evt_models$GoogleReturns_EVT_975$VaR)) &&
    is.vector(evt_models$GoogleReturns_EVT_975$VaR)) {
  tuff_backtest_results$G?ogleReturns_EVT_975 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_975$VaR, 0.95)
} else {
  tuff_backtest_results$GoogleReturns_EVT_975 <- NA
}

# For 99% threshold quantile
if (!is.na(evt_models$AppleReturns_EVT_99) && all(!is.na(evt_models?AppleReturns_EVT_99$VaR)) &&
    is.vector(evt_models$AppleReturns_EVT_99$VaR)) {
  tuff_backtest_results$AppleReturns_EVT_99 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_99$VaR, 0.95)
} else {
  tuff_backtest_results$AppleReturns_EVT_99 <- N?
}

if (!is.na(evt_models$AmazonReturns_EVT_99) && all(!is.na(evt_models$AmazonReturns_EVT_99$VaR)) &&
    is.vector(evt_models$AmazonReturns_EVT_99$VaR)) {
  tuff_backtest_results$AmazonReturns_EVT_99 <- tuff_backtest(AmazonReturns, evt_models$AmazonRetur?s_EVT_99$VaR, 0.95)
} else {
  tuff_backtest_results$AmazonReturns_EVT_99 <- NA
}

if (!is.na(evt_models$NetflixReturns_EVT_99) && all(!is.na(evt_models$NetflixReturns_EVT_99$VaR)) &&
    is.vector(evt_models$NetflixReturns_EVT_99$VaR)) {
  tuff_backtest_r?sults$NetflixReturns_EVT_99 <- tuff_backtest(NetflixReturns, evt_models$NetflixReturns_EVT_99$VaR, 0.95)
} else {
  tuff_backtest_results$NetflixReturns_EVT_99 <- NA
}

if (!is.na(evt_models$GoogleReturns_EVT_99) && all(!is.na(evt_models$GoogleReturns_EVT_?9$VaR)) &&
    is.vector(evt_models$GoogleReturns_EVT_99$VaR)) {
  tuff_backtest_results$GoogleReturns_EVT_99 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_99$VaR, 0.95)
} else {
  tuff_backtest_results$GoogleReturns_EVT_99 <- NA
}

# Print ?he TUFF backtest results
tuff_backtest_results


#TUFF backtesting for last 500 observations
# Define the TUFF backtesting function
# Ensure the last 500 observations are considered

tuff_backtest <- function(actual_losses, predicted_losses, confidence_lev?l) {
  actual_losses <- tail(actual_losses, 500)
  predicted_losses <- tail(predicted_losses, 500)
  # Calculate the number of observations
  n <- length(actual_losses)
  
  # Calculate the number of exceedances
  exceedances <- sum(actual_losses > predict?d_losses)
  
  # Calculate the number of non-exceedances
  non_exceedances <- n - exceedances
  
  # Calculate the test statistic
  tuff_statistic <- exceedances - (1 - confidence_level) * n
  
  # Calculate the p-value
  p_value <- 1 - pnorm(tuff_statisti?)
  
  # Return the p-value
  return(p_value)
}

# Perform TUFF backtesting for each EVT model
tuff_backtest_results <- list()

# For 95% threshold quantile
if (!is.na(evt_models$AppleReturns_EVT_95) && all(!is.na(evt_models$AppleReturns_EVT_95$VaR)) &&
  ? is.vector(evt_models$AppleReturns_EVT_95$VaR)) {
  tuff_backtest_results$AppleReturns_EVT_95 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_95$VaR, 0.95)
} else {
  tuff_backtest_results$AppleReturns_EVT_95 <- NA
}

if (!is.na(evt_models$Amazo?Returns_EVT_95) && all(!is.na(evt_models$AmazonReturns_EVT_95$VaR)) &&
    is.vector(evt_models$AmazonReturns_EVT_95$VaR)) {
  tuff_backtest_results$AmazonReturns_EVT_95 <- tuff_backtest(AmazonReturns, evt_models$AmazonReturns_EVT_95$VaR, 0.95)
} else {
  ?uff_backtest_results$AmazonReturns_EVT_95 <- NA
}

if (!is.na(evt_models$NetflixReturns_EVT_95) && all(!is.na(evt_models$NetflixReturns_EVT_95$VaR)) &&
    is.vector(evt_models$NetflixReturns_EVT_95$VaR)) {
  tuff_backtest_results$NetflixReturns_EVT_95 <- ?uff_backtest(NetflixReturns, evt_models$NetflixReturns_EVT_95$VaR, 0.95)
} else {
  tuff_backtest_results$NetflixReturns_EVT_95 <- NA
}

if (!is.na(evt_models$GoogleReturns_EVT_95) && all(!is.na(evt_models$GoogleReturns_EVT_95$VaR)) &&
    is.vector(evt_mo?els$GoogleReturns_EVT_95$VaR)) {
  tuff_backtest_results$GoogleReturns_EVT_95 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_95$VaR, 0.95)
} else {
  tuff_backtest_results$GoogleReturns_EVT_95 <- NA
}

# For 97.5% threshold quantile
if (!is.n?(evt_models$AppleReturns_EVT_975) && all(!is.na(evt_models$AppleReturns_EVT_975$VaR)) &&
    is.vector(evt_models$AppleReturns_EVT_975$VaR)) {
  tuff_backtest_results$AppleReturns_EVT_975 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_975$VaR, ?.975)
} else {
  tuff_backtest_results$AppleReturns_EVT_975 <- NA
}

if (!is.na(evt_models$AmazonReturns_EVT_975) && all(!is.na(evt_models$AmazonReturns_EVT_975$VaR)) &&
    is.vector(evt_models$AmazonReturns_EVT_975$VaR)) {
  tuff_backtest_results$AmazonR?turns_EVT_975 <- tuff_backtest(AmazonReturns, evt_models$AmazonReturns_EVT_975$VaR, 0.975)
} else {
  tuff_backtest_results$AmazonReturns_EVT_975 <- NA
}

if (!is.na(evt_models$NetflixReturns_EVT_975) && all(!is.na(evt_models$NetflixReturns_EVT_975$VaR)) &?
    is.vector(evt_models$NetflixReturns_EVT_975$VaR)) {
  tuff_backtest_results$NetflixReturns_EVT_975 <- tuff_backtest(NetflixReturns, evt_models$NetflixReturns_EVT_975$VaR, 0.975)
} else {
  tuff_backtest_results$NetflixReturns_EVT_975 <- NA
}

if (!is.?a(evt_models$GoogleReturns_EVT_975) && all(!is.na(evt_models$GoogleReturns_EVT_975$VaR)) &&
    is.vector(evt_models$GoogleReturns_EVT_975$VaR)) {
  tuff_backtest_results$GoogleReturns_EVT_975 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_97?$VaR, 0.975)
} else {
  tuff_backtest_results$GoogleReturns_EVT_975 <- NA
}

# For 99% threshold quantile
if (!is.na(evt_models$AppleReturns_EVT_99) && all(!is.na(evt_models$AppleReturns_EVT_99$VaR)) &&
    is.vector(evt_models$AppleReturns_EVT_99$VaR)) {
? tuff_backtest_results$AppleReturns_EVT_99 <- tuff_backtest(AppleReturns, evt_models$AppleReturns_EVT_99$VaR, 0.99)
} else {
  tuff_backtest_results$AppleReturns_EVT_99 <- NA
}

if (!is.na(evt_models$AmazonReturns_EVT_99) && all(!is.na(evt_models$AmazonRet?rns_EVT_99$VaR)) &&
    is.vector(evt_models$AmazonReturns_EVT_99$VaR)) {
  tuff_backtest_results$AmazonReturns_EVT_99 <- tuff_backtest(AmazonReturns, evt_models$AmazonReturns_EVT_99$VaR, 0.99)
} else {
  tuff_backtest_results$AmazonReturns_EVT_99 <- NA
}
?if (!is.na(evt_models$NetflixReturns_EVT_99) && all(!is.na(evt_models$NetflixReturns_EVT_99$VaR)) &&
    is.vector(evt_models$NetflixReturns_EVT_99$VaR)) {
  tuff_backtest_results$NetflixReturns_EVT_99 <- tuff_backtest(NetflixReturns, evt_models$NetflixRet?rns_EVT_99$VaR, 0.99)
} else {
  tuff_backtest_results$NetflixReturns_EVT_99 <- NA
}

if (!is.na(evt_models$GoogleReturns_EVT_99) && all(!is.na(evt_models$GoogleReturns_EVT_99$VaR)) &&
    is.vector(evt_models$GoogleReturns_EVT_99$VaR)) {
  tuff_backtest_r?sults$GoogleReturns_EVT_99 <- tuff_backtest(GoogleReturns, evt_models$GoogleReturns_EVT_99$VaR, 0.99)
} else {
  tuff_backtest_results$GoogleReturns_EVT_99 <- NA
}

# Print the TUFF backtest results
tuff_backtest_results
