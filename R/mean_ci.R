
#' @importFrom stats median qt


mean_ci <- function(x, alpha) {
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

# times <- c(94, 95, 96, 113, 121, 132, 190, 193, 255, 298)
# mean_ci(times, 0.05)
