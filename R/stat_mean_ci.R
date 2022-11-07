

#' Mean Confidence Intervals
#'
#' @param x values
#' @param alpha alpha
#'
#' @return lower_ci, upper_ci
#' @export

#' @importFrom stats median qt
#' @examples
#' mean_ci(c(1, 2, 3, 4, 5, 6, 7), 1.96)
#' mean_ci(c(2, 4, 6, 8), 1.96)


stat_mean_ci <- function(x, alpha) {
  log <- log(x)

  mean <- mean(log)

  sd <- sd(log)

  n <- length(x)

  df <- n-1

  se <- sd(log)/sqrt(n)

  t<- qt(p=alpha/2, df,lower.tail=F)


  lower_ci <- ceiling(exp(mean - se*t))
  upper_ci <- ceiling(exp(mean + se*t))

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)
}

