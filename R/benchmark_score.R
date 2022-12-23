

#' Compare Score with a Benchmark
#' @param data dataframe
#' @param column a column of scores from the dataframe
#' @param benchmark benchmark
#' @param mean if input = "values", enter mean value
#' @param sd if input = "values", enter standard deviation value
#' @param n if input = "values", enter total number of scores
#' @param tail one-tailed or two-tailed test
#' @param remove_missing TRUE/FALSE (Default is TRUE)
#' @param input Default: "long" - long form of data, "values" to pass values directly. If using this option, must specify mean, sd, and n.
#' @param output Default: "console" - prints output in console and returns tibble invisibly.
#' @return dataframe of results when saved to an object. show console output by default
#' @export
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom stats pt sd na.omit
#' @examples
#' scores <- 80 + 23 * scale(rnorm(172)) # 80 = mean, 23 = sd
#' data <- data.frame(scores = scores)
#' benchmark_score(data, scores, 67)
#' data |> benchmark_score(scores, 67)
#' benchmark_score(mean = 80, sd = 23, n = 172, benchmark = 67, input = "values")



benchmark_score <- function(data, column, benchmark, mean, sd, n, tail = "one", remove_missing = TRUE, input = "long", output = "console") {

  column <- deparse(substitute(column))

  if(missing(mean)) { mean <- NULL }
  if(missing(sd)) {sd <- NULL}
  if(missing(n)) {n <- NULL}

  if (input == "long") {
    mean <- mean(data[[column]], na.rm = remove_missing)
    sd <- sd(data[[column]], na.rm = remove_missing)
    if (remove_missing == TRUE) {
      n <- length(na.omit(data[[column]]))
    } else {
      n <- length(data[[column]])
    }
  } else if (input == "values") {
    mean <- mean
    sd <- sd
    n <- n
  }

  df <- n - 1

  t <- (mean - benchmark) / (sd / sqrt(n))

  if (tail == "one") {
    probability <- dist_t(t, df, tail = "one")
  } else if (tail == "two") {
    probability <- dist_t(t, df, tail = "two")
  } else {
    stop("arguments for tail must be 'one' or 'two'")
  }

  confidence <- 1 - probability

  se <- sd / sqrt(n)
  margin_of_error <- t * se

  lower_ci <- mean - margin_of_error
  upper_ci <- mean + margin_of_error

  output_text <- paste(
    "We can be",
    paste0(round(confidence, 2) * 100, "%"),
    "confident that the true score is between",
    round(lower_ci, 2), "and",
    round(upper_ci, 2)
  )

  result_table <- as_tibble(data.frame(
    mean = mean,
    sd = sd,
    se = se,
    n = n,
    df = df,
    probability = probability,
    tail = tail,
    confidence = confidence,
    margin_of_error = margin_of_error,
    t = t,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    output_text = output_text
  ))


  result_table_print <- result_table |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    dplyr::rename(result = 2) |>
    as_hux()

  if (output == "console") {
    cli::cli_h1("Compare Score with a Benchmark")
    cli::cli_text(result_table$output_text)
    huxtable::position(result_table_print) <- "left"
    result_print <- result_table_print |> dplyr::filter(!stringr::str_detect(term, "output_text"))
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)
    return(invisible(result_table))
  } else if (output == "tibble") {
    return(result_table)
  }
}
