
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

  result <- data.frame(lower_ci = lower_ci,
       upper_ci = upper_ci,
       t = t,
       probability = probability)


  result2 <- result |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    huxtable::as_hux()

  huxtable::position(result2) <- "left"


  cli::cli_h1("Compare Time with a Benchmark")


  result3 <- result2 |> dplyr::filter(!stringr::str_detect(term, "text_result"))
  result3 <- huxtable::map_align(result3, huxtable::by_cols("left", "right"))

  huxtable::print_screen(result3, colnames = FALSE)

  result4 <- data.frame(result2)

  return(invisible(result4))

}

