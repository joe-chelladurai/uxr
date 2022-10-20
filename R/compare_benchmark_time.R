
#' Compare Time with a Benchmark
#'
#' @param time a column or vector of time values
#' @param benchmark benchmark
#' @param alpha alpha
#' @param remove_missing TRUE/FALSE remove missing values?
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom stats pt sd
#' @examples
#' compare_benchmark_time(time = c(1, 2, 3),
#'                        benchmark = 2,
#'                        alpha = 0.05)


compare_benchmark_time <- function(benchmark, time, alpha, remove_missing = FALSE) {

  n <- length(time)
  t <- (log(benchmark) - mean(log(time)))/(sd(log(time))/sqrt(n))
  df <- n-1


  probability <- pt(q = t, df = df, lower.tail = FALSE)

  se <- sd(log(time))/sqrt(n)

  t1<- qt(p=alpha/2, df,lower.tail=F)


  lower_ci <- exp(mean(log(time), na.rm = remove_missing) - se*t1)
  upper_ci <- exp(mean(log(time), na.rm = remove_missing) + se*t1)

  list(lower_ci = lower_ci,
       upper_ci = upper_ci,
       t = t,
       probability = probability)
}
