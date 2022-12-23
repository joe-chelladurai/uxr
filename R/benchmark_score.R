

#' Compare Score with a Benchmark
#'
#' @param benchmark benchmark
#' @param scores a column or vector of scores
#' @param tail one-tailed or two-tailed test
#' @param remove_missing TRUE/FALSE remove missing values? (default is TRUE)
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom stats pt sd na.omit
#' @examples
#' scores <- 80 + 23 * scale(rnorm(172)) # 80 = mean, 23 = sd
#' benchmark_score(benchmark = 67, scores = scores)

benchmark_score <- function(benchmark, scores, tail = "one", remove_missing = TRUE) {

    mean <- mean(scores, na.rm = remove_missing)
    sd <- sd(scores, na.rm = remove_missing)

    if (remove_missing == TRUE) {
      n <- length(na.omit(scores))
    } else {
      n <- length(scores)
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

    se <- sd/sqrt(n)
    margin_of_error <- t*se

    lower_ci <- mean - margin_of_error
    upper_ci <- mean + margin_of_error

    text_output <-  paste("We can be",
                        paste0(round(confidence, 2)*100, "%"),
                        "confident that the true score is between",
                        round(lower_ci, 2), "and",
                        round(upper_ci, 2))

    result <- data.frame(mean = mean,
         sd = sd,
         se = se,
         n = n,
         df = df,
         probability = round(probability, 3),
         tail = tail,
         confidence = round(confidence, 3),
         margin_of_error = margin_of_error,
         t = round(t, 3),
         lower_ci = lower_ci,
         upper_ci = upper_ci,
         text_output = text_output)


    cli::cli_h1("Compare Score with a Benchmark")

    cli::cli_text(result$text_output)

    result_table <- result |>
      t() |>
      data.frame() |>
      tibble::rownames_to_column("term") |>
      data.frame() |>
      dplyr::rename(result = t.result.) |>
      as_hux()

    huxtable::position(result_table) <- "left"

    result_print <- result_table |> dplyr::filter(!stringr::str_detect(term, "text_output"))
    result_print <- map_align(result_print, by_cols("left", "right"))

    print_screen(result_print, colnames = FALSE)

    result_output <- data.frame(result_table)

    return(invisible(result_output))
  }



