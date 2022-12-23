

## compare_means_between_groups_long


data_long <- data.frame(A = c(4, 2, 5, 3, 6, 2, 5),
                        B = c(5, 2, 1, 2, 1, 3, 2))

data_long <- data_long |> rowid_to_column("id") |>
  pivot_longer(cols = -id, names_to = "group", values_to = "variable")



compare_means_between_groups_long <- function(data, variable, grouping_variable, groups, test = "Welch", input = "long", output = "console") {

  lhs <- deparse(substitute(variable))
  rhs <- deparse(substitute(grouping_variable))

  formula <- as.formula(paste(lhs, "~", rhs))


  data <- data %>% # Note: needs %>% pipe
    filter({{grouping_variable}} %in% groups)

  if(test == "student") {
    result <- stats::t.test(formula, data = data, var.equal = TRUE)
  } else if(test == "Welch") {
    result <- stats::t.test(formula, data = data, var.equal = FALSE)
  }

  means <- result$estimate |> data.frame() |>
    rownames_to_column("variables") |>
    rename(means = 2) |>
    mutate(variables = stringr::str_remove(variables, "mean in group ")) |>
    mutate(variables = paste0("mean_of_", variables)) |>
    pivot_wider(names_from = variables, values_from = means)

  ci <-result$conf.int  |> t() |> data.frame() |> dplyr::rename(lower_ci = X1,
                                                                upper_ci = X2)
  ci_level <- attributes(result$conf.int) |> unlist()

  p_value <- result$p.value

  t <- result$statistic

  df <- result$parameter

  result_table <- means |> bind_cols(as_tibble(data.frame(t = t, df = df, p_value = p_value, ci_level = ci_level))) |> bind_cols(ci)


  if(output == "console") {
    cli::cli_h1(result$method)

    result_print <- result_table |>
      t() |> data.frame() |> tibble::rownames_to_column("term") |>
      rename(value = 2)

    result_print <- result_print |> as_hux()
    huxtable::position(result_print) <- "left"
    result_print <- map_align(result_print, by_cols("left", "right"))
    print_screen(result_print, colnames =FALSE)

    return(invisible(result_table))
  } else if(output == "tibble") {
    return(result_table)
  }
}


compare_means_between_groups_long(data_long, variable, group, groups = c("A", "B"), output = "tibble")
