

#' Compare Time with a Benchmark
#' @param data dataframe
#' @param column a column or vector of time values
#' @param benchmark benchmark
#' @param alpha alpha
#' @param remove_missing TRUE/FALSE (Default is TRUE)
#' @param input Default: "long" - long form of data, "values" to pass values directly. If using this option, must specify count and total.
#' @param output Default: "console" - prints output in console and returns tibble invisibly.
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom stats pt sd
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @examples
#' data <- data.frame(time = c(60, 53, 70, 42, 62, 43, 81))
#' benchmark_time(data, column = time, benchmark = 60, alpha = 0.05)


benchmark_time <- function(data, column, benchmark, alpha, remove_missing = FALSE, input = "long", output = "console") {

  column <- deparse(substitute(column))

  if(input == "long") {
    if (remove_missing == TRUE) {
      n <- length(na.omit(data[[column]]))
    } else {
      n <- length(data[[column]])
    }

    t <- (log(benchmark) - mean(log(data[[column]]), na.rm = remove_missing))/(sd(log(data[[column]]), na.rm = remove_missing)/sqrt(n))
    df <- n-1
  }

  probability <- pt(q = t, df = df, lower.tail = FALSE)

  se <- sd(log(data[[column]]))/sqrt(n)

  t1<- qt(p=alpha/2, df,lower.tail=F)


  lower_ci <- exp(mean(log(data[[column]]), na.rm = remove_missing) - se*t1)
  upper_ci <- exp(mean(log(data[[column]]), na.rm = remove_missing) + se*t1)

  result_table <- as_tibble(data.frame(lower_ci = lower_ci,
       upper_ci = upper_ci,
       t = t,
       probability = probability))


  result_print <- result_table |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    as_hux()

  if(output == "console") {
    huxtable::position(result_print) <- "left"
    cli::cli_h1("Compare Time with a Benchmark")
    result_print <- result_print |> dplyr::filter(!stringr::str_detect(term, "output_text"))
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)
    return(invisible(result_table))
  } else if (output == "tibble") {
    return(result_table)
  }


}

