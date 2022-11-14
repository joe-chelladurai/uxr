


test_chisq_two <- function(data, x, y) {

  table <- table_observed_expected(data, {{x}}, {{y}})

  chi_sq <- sum(table$component)

  p_value <- pchisq(chisq, 2, lower.tail = FALSE)

  cat("chisq   :",chi_sq, "\n")
  cat("df      :", 2, "\n")
  cat("p_value :", p_value)

}
