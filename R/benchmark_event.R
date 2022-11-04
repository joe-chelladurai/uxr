

#' Compare Probability of an Event with Benchmark
#'
#' @param event event
#' @param total total
#' @param benchmark benchmark
#' @param event_type Optional: a string describing the type of event. For example, success, failure, etc.
#' @param notes whether output should contain minimal, technical, or executive type of notes.
#' @return list of event rate, probability, notes
#' @export
#' @import magrittr
#' @importFrom huxtable position map_align print_screen by_cols as_hux
#' @importFrom stringr str_detect
#' @importFrom stats dbinom
#' @importFrom scales percent
#' @examples
#' benchmark_event(benchmark = 0.7,
#'                      event = 10,
#'                      total = 12,
#'                      event_type = "success",
#'                      notes = "minimal")



benchmark_event <- function(benchmark, event, total, event_type = "", notes = c("minimal", "technical")) {

  result <- 1 - sum(dbinom(event:total, prob = benchmark, size = total))

  rate <- event |> divide_by(total) |> percent(2)

  if(event_type == "") {
    event_type <- "event"
  }

  benchmark <- benchmark |> percent()

  result_percent <- result |> percent()

  probability <- round(result, 3)

  minimal <-  paste("Based on the", event_type, paste0("rate of ", rate, ","),
                    "the probability that this rate exceeds a benchmark of",
                    benchmark, "is",
                    result_percent)

  technical <- paste("Probability values were computed based on the binomial distribution",
                     "With the", event_type, paste0("rate of ", rate, ","),
                     "the probability that this rate exceeds a benchmark of",
                     benchmark, "is",
                     result)


  text_result <- match.arg(notes)


  cli::cli_h1("Compare Event Rate with a Benchmark")



  result <- data.frame(event = event,
       total = total,
       benchmark = benchmark,
       probability = probability,
       text_result =  switch(text_result,
                             minimal = minimal,
                             technical = technical)
  )

  cli::cli_text(result$text_result)

  result_table <- result |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    dplyr::rename(result = t.result.) |>
    as_hux()

  huxtable::position(result_table) <- "left"

  result_print <- result_table |> dplyr::filter(!stringr::str_detect(term, "text_result"))
  result_print <- map_align(result_print, by_cols("left", "right"))

  print_screen(result_print, colnames = FALSE)

  result_output <- data.frame(result_table)

  return(invisible(result_output))


}




