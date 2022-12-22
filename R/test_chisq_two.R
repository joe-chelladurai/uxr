

#' Chi-squared Two Sample
#'
#' @param data data
#' @param x x
#' @param y y
#' @importFrom stats pchisq
#' @return results
#' @export
#' @examples
#' data <- tibble::tribble(~fruit, ~count,
#'                         "Apple"          , 29,
#'                         "Banana"         , 24,
#'                         "Cucumber"       , 22,
#'                         "Dragon Fruit"   , 19
#'                         )
#'
#' data <- data |>
#'   tidyr::uncount(weights = count) |>
#'   tibble::rowid_to_column("id")
#'
#' test_chisq_one(data, fruit)


test_chisq_two <- function(data, x, y) {

  table <- table_observed_expected(data, {{x}}, {{y}})

  chi_sq <- sum(table$component)

  p_value <- pchisq(chisq, 2, lower.tail = FALSE)

  cat("chisq   :",chi_sq, "\n")
  cat("df      :", 2, "\n")
  cat("p_value :", p_value)

}
