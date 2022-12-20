

#' McNemar Test
#'
#' @param data data
#' @param id id or group
#' @param x var 1
#' @param y var 2
#' @return results
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom dplyr filter rename mutate
#' @importFrom stats as.formula
#' @export
#' @examples
#' A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
#' B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
#' data <- data.frame(A, B)
#' test_mcnemar(data, A, B)


test_mcnemar <- function(data, x, y) {

  discordant_pairs <- get_concordant_discordant_pairs(data, x, y) |>
    filter(pairs %in% c("b", "c")) |>
    select(pairs, n) |>
    deframe() |>
    as.list()

  x <- discordant_pairs |>
    unlist() |>
    sum()

  y <- discordant_pairs |>
    unlist() |>
    min()

  y <- c(0:y)

  factorial_fun <- function(x, y) {
    a <- factorial(x) / (factorial(y) * factorial(x - y))
    b <- (0.5 ^ y) * (1 - 0.5) ^ (x - y)
    a * b
  }

  r <- lapply(x, factorial_fun, y = y)
  sum(r[[1]])*2

}



