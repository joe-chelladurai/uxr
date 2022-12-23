

#' Compare Means Within Groups
#'
#' @param data dataframe
#' @param var1 variable 1
#' @param var2 variable 2
#' @param input Default: "long" - long form of data, "values" to pass values directly. If using this option, must specify mean, sd, and n.
#' @param output Default: "console" - prints output in console and returns tibble invisibly.
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @return results
#' @export
#' @examples
#' data <- data.frame(id = c(1:7),
#'   task1 = c(4, 1, 2, 3, 8, 4, 4),
#'   task2 = c(7, 13, 9, 7, 18, 8, 10))
#' compare_means_within_groups(data, task1, task2)


compare_means_within_groups <- function(data, var1, var2, input = "wide", output = "console") {
  lower_ci <- upper_ci <- X1 <- X2 <- NULL

  var1 <- deparse(substitute(var1))
  var2 <- deparse(substitute(var2))

  if(input == "wide") {
    result <- stats::t.test(data[[var1]], data[[var2]], paired = TRUE)
  }

  result_table <- result$estimate |>
    data.frame() |>
    t() |>
    data.frame()


  ci <- result$conf.int |>
    t() |>
    data.frame() |>
    dplyr::rename(
      lower_ci = X1,
      upper_ci = X2
    ) |>
    dplyr::mutate(
      lower_ci = round(lower_ci, 2),
      upper_ci = round(upper_ci, 2)
    )

  ci_level <- attributes(result$conf.int) |> unlist()


  result_table <- result_table |>
    dplyr::mutate(
      t = round(result$statistic, 2),
      p = scales::pvalue(result$p.value),
      df = round(result$parameter),
      ci_level = ci_level
    ) |>
    dplyr::bind_cols(tibble::tibble(ci)) |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column(" ") |>
    data.frame()


  if(output == "console") {
    cli::cli_h1("Compare Means Within Groups")
    result_print <- result_table |> as_hux()
    huxtable::position(result_print) <- "left"
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)
    return(invisible(result_table))
  } else if(output == "tibble") {
    return(result_table)
  }


}






