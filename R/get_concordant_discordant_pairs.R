

#' Get concordant and discordant pairs for two variables
#'
#' @param data = data
#' @param x variable 1
#' @param y variable 2
#' @return a data frame
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom dplyr mutate case_when count select relocate left_join
#' @importFrom stringr str_replace
#' @importFrom tibble deframe rowid_to_column add_row
#' @importFrom utils globalVariables
#' @importFrom stats chisq.test
#' @export
#' @examples
#' mtcars$id <- seq.int(nrow(mtcars))
#' get_concordant_discordant_pairs(mtcars, x = vs, y = am)



get_concordant_discordant_pairs <- function(data, x, y) {

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))


  table <- data |>
    select({{x}}, {{y}}) |>
    rowid_to_column("id") |> pivot_longer(cols = -id) |>
    mutate(value = as.numeric(factor(value))-1) |>
    pivot_wider() |>
    mutate(pairs = case_when(
      {{x}} > {{y}} ~ "b",
      {{x}} < {{y}} ~ "c",
      {{x}} + {{y}} == 2 ~ "a",
      {{x}} + {{y}} == 0 ~ "d")) |>
    count(pairs)


  if ("a" %in% table$pairs == FALSE) { table <- table |> add_row(pairs = "a") }
  if ("b" %in% table$pairs == FALSE) { table <- table |> add_row(pairs = "b") }
  if ("c" %in% table$pairs == FALSE) { table <- table |> add_row(pairs = "c") }
  if ("d" %in% table$pairs == FALSE) { table <- table |> add_row(pairs = "d") }

  table <- table |>
    mutate(type = case_when(pairs == "a" ~ "concordant: x = 1 & y = 1",
                            pairs == "d" ~ "concordant: x = 0 & y = 0",
                            pairs == "b" ~ "discordant: x = 1 & y = 0",
                            pairs == "c" ~ "discordant: x = 0 & y = 1")) |>
    mutate(type = str_replace(type, 'x', x_name)) |>
    mutate(type = str_replace(type, 'y', y_name)) |>
    mutate(n = replace_na(n, 0))

  return(table)

}
