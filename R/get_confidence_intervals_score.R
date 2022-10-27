


get_confidence_intervals_score <- function(data, alpha, remove_missing = TRUE) {

  na.rm <- remove_missing

  mean <- mean(data, na.rm = na.rm)
  sd <- sd(data, na.rm = na.rm)
  n <- length(data)

  df <- n-1

  standard_error <- sd/sqrt(n)
  t <- abs(qt(p = alpha/2, df = df))
  margin_of_error <- t*standard_error

  lower_ci <- mean - margin_of_error
  upper_ci <- mean + margin_of_error

  list(mean = mean,
       sd = sd,
       n = n,
       df = df,
       alpha = alpha,
       margin_of_error = margin_of_error,
       t = t,
       lower_ci = lower_ci,
       upper_ci = upper_ci,
       text_output = paste("We can be", paste0(((1-alpha)*100), "%"), "confident that the true score is between", round(lower_ci, 2), "and", round(upper_ci, 2)))

}


get_confidence_intervals_score(c(1, 2, 3, NA, 5, 6), 0.05)
