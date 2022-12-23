

#' Compare Rates Between Groups
#'
#' @param data data
#' @param group column in dataframe : group
#' @param event column in dataframe : event
#' @param test Type of test (fisher, n-1 two prop)
#' @param input Defaults to "long"
#' @param output "console" prints output to console; "tibble" returns tibble
#' @return results
#' @export
#' @examples
#' design = c("A","B")
#' complete = c(34, 24)
#' incomplete = c(317, 301)
#' data <- data.frame(design, complete, incomplete)
#' data <- data |> tidyr::pivot_longer(!design, names_to = "rate", values_to = "n") |>
#'   tidyr::uncount(n)
#' compare_rates_between_groups(data, group = design, event = rate)

compare_rates_between_groups <- function(data, group, event, test, input = "long", output = "console") {

  if(missing(test)) {test = ""}

  if(input == "long") {
    table <- table_observed_expected(data, {{group}}, {{event}})

    expected <- min(table$expected)


    if(test == "fisher") {
      cli::cli_h1("Compare Rates Between Groups")
      cli::cli_alert("Fisher's Test")
      result <-   test_fisher(data, {{group}}, {{event}})
    } else if (test == "n_1_prop") {
      cli::cli_h1("Compare Rates Between Groups")
      cli::cli_alert("N-1 Two Proportions test")
      result <- test_n_1_prop(data, {{group}}, {{event}})
    } else if (test == "") {
      if(expected < 1) {
        cli::cli_h1("Compare Rates Between Groups")
       cli::cli_alert("Fisher's Test")
        result <-   test_fisher(data, {{group}}, {{event}})
      } else if (expected >= 1) {
        cli::cli_h1("Compare Rates Between Groups")
        cli::cli_alert("N-1 Two Proportions test")
        result <- test_n_1_prop(data, {{group}}, {{event}})
      }
    }
  }

  result_table <- result |>
    data.frame() |> t() |> data.frame() |>
    rownames_to_column("term") |>
    rename(value = 2)


  if(output == "console") {

    result_print <- result_table |> as_hux()
    huxtable::position(result_print) <- "left"
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)
    return(invisible(result_table))
  } else if (output == "tibble") {
    return(result)
  }

}



