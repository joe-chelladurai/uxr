

## Get pairs from long data



A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)

data <- data.frame(A, B)

data <- data |>
  rowid_to_column("id") |>
  pivot_longer(cols = -id, names_to = "group", values_to = "num_value")



widen <- function(data, id, group, var) {
  data |>
    select({{id}}, {{group}}, {{var}}) |>
    pivot_wider(names_from = {{group}}, values_from = {{var}}) |>
    select(-{{id}})
}




v <- levels(factor(data$group))
xa1 <- v[[1]]
xa2 <- v[[2]]

widen(data_long, id, group, num_value) |>
  dichotomize(get(xa1), get(xa2)) |>
  add_pairs_type(get(xa1), get(xa2)) |>
  count(pairs, type) |> format_pairs_type()

# TO DO
# get factor levels and
# pass it to dichotomize and add_pairs_type
