


design = c("A","B")
complete = c(11, 5)
incomplete = c(1, 5)
data <- data.frame(design, complete, incomplete)
data <- data |> pivot_longer(!design, names_to = "rate", values_to = "n") |>
  tidyr::uncount(n)



test_fisher <- function(data, x, y) {

  table <- summarise(data, tab = list(table({{x}}, {{y}})))$tab[[1]]

  result <- fisher.test(table)

  data.frame(p_value = result$p.value,
             odds_ratio = unname(result$estimate)
             )
}

test_fisher(data, design, rate)
