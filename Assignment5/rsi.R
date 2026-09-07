rsi <- function(data, period) {
  
  # Check that data is numeric
  if (!is.numeric(data)) {
    stop("Data must be numeric")
  }
  
  # Check that period is valid
  if (!is.numeric(period) || length(period) != 1 || period <= 0) {
    stop("Period must be a positive number")
  }
  
  # Check that enough observations are available
  if (length(data) <= period) {
    stop("Data length must be greater than period")
  }
  
  # Calculate differences between consecutive values
  diff_values <- diff(data)
  
  # Initialize vectors for gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Separate gains and losses
  for (i in 1:length(diff_values)) {
    
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate initial average gain and loss
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize RSI values with NA
  rsi_values <- rep(NA_real_, length(data))
  
  # Calculate RSI using Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    
    avg_gain <- (
      avg_gain * (period - 1) + gains[i - 1]
    ) / period
    
    avg_loss <- (
      avg_loss * (period - 1) + losses[i - 1]
    ) / period
    
    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      
      rs <- avg_gain / avg_loss
      
      rsi_values[i] <- 100 - (
        100 / (1 + rs)
      )
    }
  }
  
  return(rsi_values)
}