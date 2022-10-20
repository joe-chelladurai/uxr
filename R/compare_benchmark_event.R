

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

  minimal <-  paste("Minimal: Based on the", event_type, paste0("rate of ", rate, ","),
                    "the probability that this rate exceeds a benchmark of",
                    benchmark, "is",
                    result_percent)

  technical <- paste("Technical: ",
                     "Probability values were computed based on values in a binomial distribution",
                     "Based on the", event_type, paste0("rate of ", rate, ","),
                     "the probability that this rate exceeds a benchmark of",
                     benchmark, "is",
                     result)

  executive <-  paste("Executive: Based on the", event_type, paste0("rate of ", rate, ","),
                         "the probability that this rate exceeds a benchmark of",
                         benchmark, "is",
                         result_percent)

  text_result <- match.arg(notes)




  list(event = event,
       total = total,
       benchmark = benchmark,
       probability = probability,
       text_result =  switch(text_result,
                             minimal = minimal,
                             technical = technical,
                             executive = executive)
  )
}



