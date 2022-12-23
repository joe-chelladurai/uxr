

#' Compare Rates Within Groups
#'
#' @param data data
#' @param x var 1
#' @param y var 2
#' @param conf_level Confidence level
#' @importFrom dplyr bind_cols rename
#' @importFrom tibble as_tibble
#' @return results
#' @export
#' @examples
#' A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
#' B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
#' data <- data.frame(A, B)


compare_rates_within_groups <- function(data, x, y, conf_level = 0.95) {

  conf_int <- get_confidence_intervals_within_groups(data, x, y, conf_level)

  test <- test_mcnemar(data, x, y) |> as_tibble() |> rename(p_value = value)

  result <- bind_cols(test, conf_int)

  return(result)

}
