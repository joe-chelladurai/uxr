

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
#' @importFrom stats dbinom
#' @importFrom scales percent
#' @examples
#' compare_benchmark_event(benchmark = 0.7,
#'                      event = 10,
#'                      total = 12,
#'                      event_type = "success",
#'                      notes = "minimal")



compare_benchmark_event <- function(benchmark, event, total, event_type = "", notes = c("minimal", "technical", "executive")) {

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

  executive <-  paste("Executive: Based on the", event_type, paste0("rate of ", rate, ","),
                         "the probability that this rate exceeds a benchmark of",
                         benchmark, "is",
                         result_percent)

  text_result <- match.arg(notes)


  cli::cli_h1("Compare Event Rate with a Benchmark")

  result <- data.frame(event = event,
       total = total,
       benchmark = benchmark,
       probability = probability,
       text_result =  switch(text_result,
                             minimal = minimal,
                             technical = technical,
                             executive = executive)
  )

  result2 <- result |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column("term") |>
    data.frame() |>
    dplyr::rename(result = t.result.) |>
    huxtable::as_hux()

  huxtable::position(result2) <- "left"

  result2 <- huxtable::map_align(result2, huxtable::by_cols("left", "right"))

  huxtable::print_screen(result2, colnames = FALSE)

  result3 <- data.frame(result2)

  return(invisible(result3))


}



compare_benchmark_event(benchmark = 0.7,
                     event = 10,
                     total = 12,
                     event_type = "success",
                     notes = "minimal")
