

#' @importFrom stats median

mean_ci_2 <- function(x, z) {

  n <- length(x)
  p <- 0.5 # percentile (0.5) for the median

  se <- sqrt(n*(p*(1-p)))

  me <- z*se

  lower_ci <- n*p - me
  upper_ci <- n*p + me


  sorted <- sort(x)

  lower_ci <- dplyr::nth(sorted, ceiling(lower_ci)) # use ceiling instead of rounding
  upper_ci <- dplyr::nth(sorted, ceiling(upper_ci))
  list(lower_ci = lower_ci,
       median = median(x),
       upper_ci = upper_ci)

}

# x <- c(82, 96, 100, 104, 105, 110, 111, 117, 118, 118, 118, 127, 132, 133, 134, 134, 139, 141, 141, 150, 161, 178, 201, 201, 211, 223, 256)
# z <- 1.96

# mean_ci_2(x, 1.96)
