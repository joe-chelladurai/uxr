

#' Benchmark Comparison
#'
#' @param success success
#' @param total total
#' @param benchmark benchmark
#'
#' @return probability
#' @export
#'
#' @importFrom stats dbinom
#' @importFrom scales percent
#' @examples
#' benchmark_comparison(10, 12, 0.7)
#' benchmark_comparison(60, 100, 0.7)



benchmark_comparison <- function(success, total, benchmark) {
  result <- 1 - sum(dbinom(success:total, benchmark, size = total))

  list(success = success,
       total = total,
       benchmark = benchmark,
       probability = round(result, 3),
       text_result = paste0("Based on the success rate of ", scales::percent(success/total, 2), ", the probability that the completion rate exceeds a benchmark of ", scales::percent(benchmark), " is ", scales::percent(result))
  )
}

