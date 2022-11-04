

#' Compare Means Between Groups
#'
#' @param data data
#' @param variable variable
#' @param grouping_variable Group
#' @param groups Specify groups from grouping variable
#' @param equal_variances Specify whether variances are equal (TRUE/FALSE)
#' @return results
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom dplyr filter rename mutate
#' @importFrom stats as.formula
#' @export
#' @examples
#' compare_means_between_groups(mtcars, mpg, cyl, c(4, 6))


compare_means_between_groups <- function(data, variable, grouping_variable, groups, equal_variances = FALSE) {

  lower_ci <- upper_ci <- X1 <- X2 <- NULL


  lhs <- deparse(substitute(variable))
  rhs <- deparse(substitute(grouping_variable))

  formula <- as.formula(paste(lhs, "~", rhs))


  data <- data %>% # Note: needs %>% pipe
    filter({{grouping_variable}} %in% groups)

  if(equal_variances == TRUE) {
    result <- stats::t.test(formula, data = data, var.equal = TRUE)
  } else if(equal_variances == FALSE) {
    result <- stats::t.test(formula, data = data, var.equal = FALSE)
  }




  result_table <- result$estimate |> data.frame()  |> t() |> data.frame()

  #x_name <- deparse(substitute(x))
  #x_name <- sub(".*\\$", "", x_name)

  # y_name <- deparse(substitute(y))
  # y_name <- sub(".*\\$", "", y_name)

  #names(result_table)[1] <- x_name
  #names(result_table)[2] <- y_name

  ci <-result$conf.int  |> t() |> data.frame() |> dplyr::rename(lower_ci = X1,
                                                                upper_ci = X2) |> dplyr::mutate(lower_ci = round(lower_ci, 2),
                                                                                                upper_ci = round(upper_ci, 2))
  ci_level <- attributes(result$conf.int) |> unlist()

  cli::cli_h1(result$method)

  result_table <- result_table |>
    dplyr::mutate(t = round(result$statistic, 2),
                  p = scales::pvalue(result$p.value),
                  df = round(result$parameter),
                  ci_level = ci_level) |>
    dplyr::bind_cols(tibble::tibble(ci)) |>
    t() |> data.frame() |> tibble::rownames_to_column(" ") |> data.frame()


  result_table <- result_table |> as_hux()
  huxtable::position(result_table) <- "left"
  result_table <- map_align(result_table, by_cols("left", "right"))
  print_screen(result_table, colnames =FALSE)
  result_output <- data.frame(result_table)
  return(invisible(result_output))
}









