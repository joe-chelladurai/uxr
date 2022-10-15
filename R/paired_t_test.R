

#' Paired t-test
#'
#' @param x x
#' @param y y
#' @param ... other arguments passed to paired t-test
#' @return results
#' @export
#'
#'
#'
paired_t_test <- function(x, y, ...) {
  lower_ci <- upper_ci <- X1 <- X2 <- NULL

  result <- stats::t.test(x, y, paired = TRUE, ...)
  result2 <- result$estimate |>
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

  cli::cli_h1(result$method)

  result2 <- result2 |>
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


  result2 <- result2 |> huxtable::as_hux()
  huxtable::position(result2) <- "left"
  result2 <- huxtable::map_align(result2, huxtable::by_cols("left", "right"))
  huxtable::print_screen(result2, colnames = FALSE)
  result3 <- data.frame(result2)
  return(invisible(result3))
}

