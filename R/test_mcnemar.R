

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

  discordant_pairs <- get_concordant_discordant_pairs(data, {{x}}, {{y}}) |>
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



data_tidy <- data |> rowid_to_column("id") |>
  pivot_longer(cols = -id, names_to = "group", values_to = "status")


test_mcnemar_tidy <- function(data, group, var) {


  data <- data |>
    mutate(situation = as.numeric(factor({{var}}))-1)

  data
}

data_tidy2 <- data_tidy |>
  mutate(status2 = case_when(status == 1 ~ "pass",
                             status == 0 ~ "fail"))


data_tidy2 |>
  mutate(status3 = as.numeric(factor(status2))-1) |>
  pivot_wider(cols = c("id", "status3"), names_from = id, values_from = status3)


data_tidy2 |>
  mutate(status3 = as.numeric(factor(status2))-1) |>
  select(id, group, status3) |>
  pivot_wider(names_from = "group", values_from = "status3")



data_tidy2

get_concordant_discordant_pairs()

test_mcnemar_tidy(data_tidy2, group, status)


