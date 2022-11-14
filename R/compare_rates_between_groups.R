




compare_rates_between_groups <- function(data, x, y, test) {

  if(missing(test)) {test = ""}

  table <- table_observed_expected(data, {{x}}, {{y}})

  expected <- min(table$expected)


  if(test == "fisher") {
      cli::cli_alert("Fisher's Test")
      result <-   test_fisher(data, {{x}}, {{y}}) |> data.frame() |> t()
  } else if (test == "n_1_prop") {
    cli::cli_alert("N-1 Two Proportions test")
    result <- test_n_1_prop(data, {{x}}, {{y}}) |> data.frame() |> t()
  } else if (test == "") {
    if(expected < 1) {
      cli::cli_alert("Fisher's Test")
      result <-   test_fisher(data, {{x}}, {{y}}) |> data.frame() |> t()
    } else if (expected >= 1) {
      cli::cli_alert("N-1 Two Proportions test")
      result <- test_n_1_prop(data, {{x}}, {{y}}) |> data.frame() |> t()
    }
  }


 list(result, test)

}


design1 = c("A","B")
complete1 = c(11, 1)
incomplete1 = c(1, 0)
data1 <- data.frame(design, complete1, incomplete1)
data1 <- data1 |> pivot_longer(!design, names_to = "rate", values_to = "n") |>
  tidyr::uncount(n)

design2 = c("A","B")
complete2 = c(11, 5)
incomplete2 = c(1, 5)
data2 <- data.frame(design, complete2, incomplete2)
data2 <- data2 |> pivot_longer(!design, names_to = "rate", values_to = "n") |>
  tidyr::uncount(n)





compare_rates_between_groups(data1, design, rate)


compare_rates_between_groups(data2, design, rate)



