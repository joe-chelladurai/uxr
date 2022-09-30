


#' Mean Confidence Intervals (Large Samples)
#'
#' @param x values
#' @param z z value
#'
#' @return lower_ci, upper_ci
#' @export


#' @importFrom stats median
#' @examples
#' mean_ci_2(c(1, 2, 3, 4, 5, 6, 7), 1.96)
#' mean_ci_2(c(2, 4, 6, 8), 1.96)


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

