

#' Compare Rates Between Groups
#'
#' @param data data
#' @param x var 1
#' @param y var 2
#' @param test Type of test (fisher, n-1 two prop)
#' @return results
#' @export


compare_rates_between_groups <- function(data, x, y, test) {

  if(missing(test)) {test = ""}

  table <- table_observed_expected(data, x, y)

  expected <- min(table$expected)


  if(test == "fisher") {
      cli::cli_alert("Fisher's Test")
      result <-   test_fisher(data, x, y) |> data.frame() |> t()
  } else if (test == "n_1_prop") {
    cli::cli_alert("N-1 Two Proportions test")
    result <- test_n_1_prop(data, x, y) |> data.frame() |> t()
  } else if (test == "") {
    if(expected < 1) {
      cli::cli_alert("Fisher's Test")
      result <-   test_fisher(data, x, y) |> data.frame() |> t()
    } else if (expected >= 1) {
      cli::cli_alert("N-1 Two Proportions test")
      result <- test_n_1_prop(data, x, y) |> data.frame() |> t()
    }
  }


 list(result, test)

}

