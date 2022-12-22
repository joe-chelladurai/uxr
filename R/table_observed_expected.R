

#' Observed Expected Table
#'
#' @param data data
#' @param x x
#' @param y y
#' @importFrom stats chisq.test
#' @importFrom dplyr relocate left_join
#' @importFrom rlang :=
#' @return results
#' @export


table_observed_expected <- function(data, x, y) {

  table <- suppressWarnings(chisq.test(
    summarise(data, tab = list(table({{x}}, {{y}})))$tab[[1]]))

  observed <- table$observed |>
    as_tibble(.name_repair = ~ c("x", "y", "observed"))

  expected <- table$expected |>
    as_tibble(rownames = "x") |>
    pivot_longer(-1, names_to="y", values_to = "expected")

  observed |> left_join(expected, by = c("x", "y")) |>
    mutate(diff = observed-expected) |>
    mutate(diff2 = diff^2) |>
    mutate(component = diff2/expected) |>
    mutate({{x}} := x) |>
    mutate({{y}} := y) |>
    select(-x, -y) |> relocate({{x}}, {{y}})
}
