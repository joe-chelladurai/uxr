
#' @export

#A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
#B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
#data <- data.frame(A, B)


compare_rates_within_groups <- function(data, x, y, z = 1.96) {

  conf_int <- get_confidence_intervals_within_groups(data, x, y, z)

  test <- test_mcnemar(data, x, y) |> as_tibble() |> rename(p_value = value)

  result <- bind_cols(test, conf_int)

  return(result)

}
