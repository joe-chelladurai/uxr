
#' Task Time Benchmark
#'
#' @param x a vector
#' @param benchmark benchmark
#' @param alpha alpha
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom stats pt sd
#' @examples
#' task_time_benchmark(c(1, 2, 3), 2, 0.05)


task_time_benchmark <- function(x, benchmark, alpha) {

  n <- length(x)
  t <- (log(benchmark) - mean(log(x)))/(sd(log(x))/sqrt(n))
  df <- n-1


  probability <- pt(q = t, df = df, lower.tail = FALSE)

  se <- sd(log(x))/sqrt(n)

  t1<- qt(p=alpha/2, df,lower.tail=F)


  lower_ci <- exp(mean(log(x)) - se*t1)
  upper_ci <- exp(mean(log(x)) + se*t1)

  list(lower_ci = lower_ci,
       upper_ci = upper_ci,
       t = t,
       probability = probability)
}
