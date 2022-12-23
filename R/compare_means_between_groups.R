

#' Compare Means Between Groups
#'
#' @param data data
#' @param var1 variable 1
#' @param var2 variable 2
#' @param variable variable
#' @param grouping_variable Group
#' @param groups Specify groups from grouping variable
#' @param test Default: "Welch", choose between "student" and "Welch"
#' @param input Default: "wide", choose between "long" and "wide". "wide" requires data var1 var2. "long" requires data, variable, grouping_variable groups
#' @param output Default: "console" - prints output in console and returns tibble invisibly.
#' @return results
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom dplyr filter rename mutate
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_remove
#' @importFrom stats as.formula t.test
#' @export
#' @examples
#'
#' # Wide data - default
#' data_wide <- data.frame(A = c(4, 2, 5, 3, 6, 2, 5),
#'                         B = c(5, 2, 1, 2, 1, 3, 2))
#'
#' compare_means_between_groups(data_wide, var1 = A, var2 = B)
#'
#' # Long data
#' data_long <- data_wide |> tibble::rowid_to_column("id") |>
#'   tidyr::pivot_longer(cols = -id, names_to = "group", values_to = "variable")
#'
#' compare_means_between_groups(data_long, variable = variable,
#'                              grouping_variable = group, groups = c("A", "B"), input = "long")

compare_means_between_groups <- function(data, var1, var2, variable, grouping_variable, groups, test = "Welch", input = "wide", output = "console") {
  if (input == "wide") {
    var1 <- deparse(substitute(var1))
    var2 <- deparse(substitute(var2))


    if (test == "student") {
      result <- t.test(data[[var1]], data[[var2]], var.equal = TRUE)
    } else if (test == "Welch") {
      result <- t.test(data[[var1]], data[[var2]], var.equal = FALSE)
    }

    result <- t.test(data[[var1]], data[[var2]])

    x_name <- deparse(substitute(var1))
    x_name <- sub(".*\\$", "", x_name)

    y_name <- deparse(substitute(var2))
    y_name <- sub(".*\\$", "", y_name)

    means <- result$estimate

    names(means)[1] <- x_name
    names(means)[2] <- y_name

    means <- data.frame(means) |>
      rownames_to_column("variables") |>
      as_tibble()

    means$variables <- gsub('[\"]', "", means$variables)
    means <- means |> mutate(variables = paste0("mean_of_", variables))
    means <- means |> pivot_wider(names_from = variables, values_from = means)
  }

  if (input == "long") {
    lhs <- deparse(substitute(variable))
    rhs <- deparse(substitute(grouping_variable))

    formula <- as.formula(paste(lhs, "~", rhs))


    data <- data %>% # Note: needs %>% pipe
      filter({{ grouping_variable }} %in% groups)

    if (test == "student") {
      result <- stats::t.test(formula, data = data, var.equal = TRUE)
    } else if (test == "Welch") {
      result <- stats::t.test(formula, data = data, var.equal = FALSE)
    }


    means <- result$estimate |>
      data.frame() |>
      rownames_to_column("variables") |>
      rename(means = 2) |>
      mutate(variables = str_remove(variables, "mean in group ")) |>
      mutate(variables = paste0("mean_of_", variables)) |>
      pivot_wider(names_from = variables, values_from = means)
  }

  ## for both wide and long

  ci <- result$conf.int |>
    t() |>
    data.frame() |>
    dplyr::rename(
      lower_ci = X1,
      upper_ci = X2
    )
  ci_level <- attributes(result$conf.int) |> unlist()

  p_value <- result$p.value

  t <- result$statistic

  df <- result$parameter

  result_table <- means |>
    bind_cols(as_tibble(data.frame(t = t, df = df, p_value = p_value, ci_level = ci_level))) |>
    bind_cols(ci)

  if (output == "console") {
    cli::cli_h1(result$method)

    result_print <- result_table |>
      t() |>
      data.frame() |>
      rownames_to_column("term") |>
      rename(value = 2)

    result_print <- result_print |> as_hux()
    huxtable::position(result_print) <- "left"
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames = FALSE)

    return(invisible(result_table))
  } else if (output == "tibble") {
    return(result_table)
  }
}
