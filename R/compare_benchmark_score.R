

# data <- 73 + 19 * scale(rnorm(20)) # 73 = mean, 19 = sd
# compare_benchmark_score(data, benchmark = 67, alpha = 0.5, tail = "one")

compare_benchmark_score <- function(data, benchmark, alpha, tail = "one") {

    mean <- mean(data)
    sd <- sd(data)
    n <- length(data)
    df <- n - 1

    t <- (mean - benchmark) / (sd / sqrt(n))

    if (tail == "one") {
      probability <- t_dist_one_tailed(t, df)
      } else if (tail == "two") {
        probability <- t_dist_two_tailed(t, df)
        } else {
          stop("arguments for tail must be 'one' or 'two'")
          }

    confidence <- 1 - probability

    se <- sd/sqrt(n)
    margin_of_error <- t*se

    lower_ci <- mean - margin_of_error
    upper_ci <- mean + margin_of_error

    list(mean = mean,
         sd = sd,
         se = se,
         n = n,
         df = df,
         alpha = alpha,
         probability = probability,
         tail = tail,
         confidence = confidence,
         margin_of_error = margin_of_error,
         t = t,
         lower_ci = lower_ci,
         upper_ci = upper_ci,
         text_output = paste("We can be",
                             paste0(round(confidence, 2)*100, "%"),
                                   "confident that the true score is between",
                                   round(lower_ci, 2), "and",
                                   round(upper_ci, 2)))
  }






