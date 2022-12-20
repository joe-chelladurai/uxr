




# data <- tibble::tribble(~fruit, ~count,
#                         "Apple"          , 29,
#                         "Banana"         , 24,
#                         "Cucumber"       , 22,
#                         "Dragon Fruit"   , 19
#                         )
#
# data <- data |>
#   uncount(weights = count) |>
#   rowid_to_column("id")
#
# test_chisq_one(data, fruit)

test_chisq_one <- function(data, x) {

  case_total <- nrow(data)
  distinct_groups <- nrow(distinct(data, {{x}}))

  table <- data |>
    count({{x}}, name = "observed") |>
    mutate(expected = case_total/distinct_groups) |>
    mutate(residual = observed - expected) |>
    mutate(residual_sq = residual^2) |>
    mutate(component = residual_sq/expected)


  chi_sq <- table |>
    summarise(chi_sq = sum(component))

  chi_sq_result <- chi_sq

  chi_sq_result

}






