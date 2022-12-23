data_wide <- data.frame(A = c(4, 2, 5, 3, 6, 2, 5),
                        B = c(5, 2, 1, 2, 1, 3, 2))

compare_means_between_groups_wide <- function(data, var1, var2, test = "Welch", input = "wide", output = "console") {

  var1 <- deparse(substitute(var1))
  var2 <- deparse(substitute(var2))

  if(test == "student") {
    result <- t.test(data[[var1]], data[[var2]], var.equal = TRUE)
  } else if(test == "Welch") {
    result <- t.test(data[[var1]], data[[var2]], var.equal = FALSE)
  }


  x_name <- deparse(substitute(var1))
  x_name <- sub(".*\\$", "", x_name)

  y_name <- deparse(substitute(var2))
  y_name <- sub(".*\\$", "", y_name)

  means <- result$estimate

  names(means)[1] <- x_name
  names(means)[2] <- y_name

  means <- data.frame(means) |> rownames_to_column("variables") |> as_tibble()

  means$variables <- gsub('[\"]', '', means$variables)
  means <- means |> mutate(variables = paste0("mean_of_", variables))
  means <- means |> pivot_wider(names_from = variables, values_from = means)

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

compare_means_between_groups_wide(data, A, B, output = "tibble")
