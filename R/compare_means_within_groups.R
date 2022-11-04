


#' Compare Means Within Groups
#'
#' @param x x
#' @param y y
#' @param ... other arguments passed to paired t-test
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @return results
#' @export

compare_means_within_groups <- function(x, y, ...) {
  lower_ci <- upper_ci <- X1 <- X2 <- NULL

  result <- stats::t.test(x, y, paired = TRUE, ...)
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

  cli::cli_h1("Compare Means Within Groups")

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


  result_table <- result_table |> as_hux()
  huxtable::position(result_table) <- "left"
  result_table <- map_align(result_table, by_cols("left", "right"))
  print_screen(result_table, colnames = FALSE)
  result_output <- data.frame(result_table)
  return(invisible(result_output))
}






