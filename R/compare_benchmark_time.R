
#' Compare Time with a Benchmark
#'
#' @param time a column or vector of time values
#' @param benchmark benchmark
#' @param alpha alpha
#' @param remove_missing TRUE/FALSE remove missing values?
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom stats pt sd
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @examples
#' compare_benchmark_time(time = c(60, 53, 70, 42, 62, 43, 81),
#'                        benchmark = 60,
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


  result_table <- result |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    as_hux()

  huxtable::position(result_table) <- "left"


  cli::cli_h1("Compare Time with a Benchmark")


  result_print <- result_table |> dplyr::filter(!stringr::str_detect(term, "text_result"))
  result_print <- map_align(result_print, by_cols("left", "right"))

  print_screen(result_print, colnames = FALSE)

  result_output <- data.frame(result_table)

  return(invisible(result_output))

}

