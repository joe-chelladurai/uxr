


#' Get Confidence Intervals Within Groups
#'
#' @param data data
#' @param x var 1
#' @param y var 2
#' @param conf_level Confidence level
#' @importFrom dplyr bind_cols rename summarise pull
#' @importFrom tibble as_tibble
#' @return results
#' @export
#' @examples
#' A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
#' B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
#' data <- data.frame(A, B)
#' get_confidence_intervals_within_groups(data, A, B)


get_confidence_intervals_within_groups <- function(data, x, y, conf_level = 0.95) {

  z <- abs(qnorm((1 - conf_level) / 2))


  adj_pairs <- get_concordant_discordant_pairs(data, x, y) |>
    mutate(adj = (z^2) / 8) |>
    mutate(adj_n = n + adj)

  N_adj <- adj_pairs |>
    summarise(sum = sum(adj_n)) |>
    pull()

  p1_adj <- adj_pairs |>
    filter(pairs %in% c("a", "b")) |>
    pull(adj_n) |>
    sum() |>
    divide_by(N_adj)

  p2_adj <- adj_pairs |>
    filter(pairs %in% c("a", "c")) |>
    pull(adj_n) |>
    sum() |>
    divide_by(N_adj)

  p12_adj <- adj_pairs |>
    filter(pairs == "b") |>
    pull(adj_n) |>
    divide_by(N_adj)

  p21_adj <- adj_pairs |>
    filter(pairs == "c") |>
    pull(adj_n) |>
    divide_by(N_adj)

  ci <- ((p12_adj + p21_adj) - raise_to_power((p21_adj - p12_adj), 2)) |>
    divide_by(N_adj) |>
    sqrt() |>
    multiply_by(z)



  if (p1_adj > p2_adj) {
    lower_ci <- (p1_adj - p2_adj) - ci
    upper_ci <- (p1_adj - p2_adj) + ci
  } else {
    lower_ci <- (p2_adj - p1_adj) - ci
    upper_ci <- (p2_adj - p1_adj) + ci
  }

  return(data.frame(
    lower_ci = lower_ci,
    upper_ci = upper_ci
  ))
}


