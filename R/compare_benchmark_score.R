

#' Compare Score with a Benchmark
#'
#' @param data a column or vector of scores
#' @param benchmark benchmark
#' @param alpha alpha
#' @param tail one-tailed or two-tailed test
#' @param remove_missing TRUE/FALSE remove missing values? (default is TRUE)
#' @return lower_ci, upper_ci, t, probability
#' @export
#' @importFrom stats pt sd na.omit
#' @examples
#' data <- 68 + 17 * scale(rnorm(20)) # 68 = mean, 17 = sd
#' compare_benchmark_score(data, benchmark = 60, alpha = 0.5)


compare_benchmark_score <- function(data, benchmark, alpha, tail = "one", remove_missing = TRUE) {

    mean <- mean(data, na.rm = remove_missing)
    sd <- sd(data, na.rm = remove_missing)

    if (remove_missing == TRUE) {
      n <- length(na.omit(data))
    } else {
      n <- length(data)
    }

    df <- n - 1

    t <- (mean - benchmark) / (sd / sqrt(n))

    if (tail == "one") {
      probability <- t_dist_one_tailed(t, df)
      } else if (tail == "two") {
        probability <- t_dist_two_tailed(t, df)
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
         alpha = alpha,
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

    result2 <- result |>
      t() |>
      data.frame() |>
      tibble::rownames_to_column("term") |>
      data.frame() |>
      dplyr::rename(result = t.result.) |>
      huxtable::as_hux()

    huxtable::position(result2) <- "left"

    result3 <- result2 |> dplyr::filter(!stringr::str_detect(term, "text_output"))
    result3 <- huxtable::map_align(result3, huxtable::by_cols("left", "right"))

    huxtable::print_screen(result3, colnames = FALSE)

    result4 <- data.frame(result2)

    return(invisible(result4))
  }




# length_na <- function(data, remove_missing) {
#
#   if (remove_missing == TRUE) {
#     n <- length(na.omit(data))
#   } else {
#     n <- length(data)
#   }
#   return(n)
# }


