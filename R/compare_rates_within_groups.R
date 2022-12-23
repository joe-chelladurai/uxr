

#' Compare Rates Within Groups
#'
#' @param data data
#' @param x var 1
#' @param y var 2
#' @param conf_level Confidence level
#' @param input input type currently only accepts "wide"
#' @param output Default is "console", also accepts "tibble"
#' @importFrom dplyr bind_cols rename
#' @importFrom tibble as_tibble
#' @return results
#' @export
#' @examples
#' A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
#' B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
#' data <- data.frame(A, B)
#' compare_rates_within_groups(data, A, B, input = "wide")

compare_rates_within_groups <- function(data, x, y, conf_level = 0.95, input, output = "console") {

  if(missing(input)) { stop("please specify input = 'wide'")}

  conf_int <- get_confidence_intervals_within_groups(data, x, y, conf_level)

  test <- test_mcnemar(data, x, y) |> as_tibble() |> rename(p_value = value)

  result <- bind_cols(test, conf_int)


  if(output == "console") {
    cli::cli_h1("Compare Rates Within Groups")
    cli::cli_alert("McNemar's Test")
    result_print <- result |> as_hux()
    huxtable::position(result_print) <- "left"
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)
    return(invisible(result))
  } else if (output == "tibble") {
    return(result)
  }

}





