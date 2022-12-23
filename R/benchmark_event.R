

#' Compare Probability of an Event with Benchmark
#'
#' @param data dataset
#' @param column name of column
#' @param benchmark benchmark
#' @param event specify event as given in column (example: 0, "pass", "success")
#' @param count number of times event has occurred. Use only when using input = "values"
#' @param total total number of all events. Use only when using input = "values"
#' @param event_type Optional: a string describing the type of event. For example, success, failure, etc.
#' @param notes whether output should contain minimal or technical type of notes. Defaults to "minimal". Use "none" to turn off.
#' @param remove_missing TRUE/FALSE (Default is TRUE)
#' @param input Default: "long" - long form of data, "values" to pass values directly. If using this option, must specify count and total.
#' @param output Default: "console" - prints output in console and returns tibble invisibly.
#' @return dataframe of results when saved to object. show console output by default
#' @export
#' @import magrittr
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom stringr str_detect
#' @importFrom stats dbinom
#' @importFrom scales percent
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom cli cli_h1 cli_text
#' @examples
#' data <- data.frame(task_1 = c("y", "y", "y", "y", "n", "n", "n", NA, NA, NA, NA, NA, NA, NA),
#'                    task_2 = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1))
#' benchmark_event(data, column = task_1, benchmark = 0.8, event = "y")
#' benchmark_event(data, column = task_2, benchmark = 0.3, event = 1, event_type = "success")
#' benchmark_event(benchmark = 0.8, count = 4, total = 7, input = "values")




benchmark_event <- function(data,
                            column,
                            benchmark,
                            event,
                            count,
                            total,
                            event_type = "",
                            remove_missing = TRUE,
                            notes = "minimal",
                            input = "long",
                            output = "console") {
  if (input == "long") {
    column <- deparse(substitute(column))
    if (remove_missing == TRUE) {
      total <- length(data[[column]][!is.na(data[[column]])])
    } else {
      total <- length(data[[column]])
    }
    event_count <- table(data[[column]])[[event]]
  } else if (input == "values") {
    event_count <- count
    total <- total
  }

  result <- 1 - sum(dbinom(event_count:total, prob = benchmark, size = total))

  probability <- round(result, 3)

  rate <- event_count |>
    divide_by(total) |>
    percent(2)

  benchmark_text <- benchmark |> percent()

  result_percent <- result |> percent()

  if (event_type == "") {
    event_type <- "event"
  }

  # notes
  output_text <- match.arg(notes)
  none <- ""
  minimal <- paste(
    "Based on the", event_type, paste0("rate of ", rate, ","),
    "the probability that this rate exceeds a benchmark of",
    benchmark_text, "is",
    result_percent
  )

  technical <- paste(
    "Probability values were computed based on the binomial distribution",
    "With the", event_type, paste0("rate of ", rate, ","),
    "the probability that this rate exceeds a benchmark of",
    benchmark_text, "is",
    result
  )



  # result table
  result_table <- as_tibble(data.frame(
    count = event_count,
    total = total,
    benchmark = benchmark,
    result = result,
    probability = probability,
    output_text = switch(output_text,
                         none = none,
                         minimal = minimal,
                         technical = technical
    )
  ))

  if (notes == "none") {
    result_table <- result_table |> select(-output_text)
  }

  # print table
  result_table_print <- result_table |>
    t() |>
    data.frame() |>
    rownames_to_column("term") |>
    data.frame() |>
    rename(result = 2) |>
    as_hux()

  huxtable::position(result_table_print) <- "left"

  result_print <- result_table_print |>
    filter(!str_detect(term, "output_text"))

  result_print <- map_align(result_print, by_cols("left", "right"))



  # return

  if (output == "console") {
    cli_h1("Compare Event Rate with a Benchmark")
    cli_text(result_table$output_text)
    print_screen(result_print, colnames = FALSE)
    return(invisible(result_table))
  } else if (output == "tibble") {
    return(result_table)
  }




}



