


#' Get Confidence Intervals for Event Rate
#'
#' @param event event
#' @param total total
#' @param confidence_level confidence level as z value
#' @return lower_ci, upper_ci
#' @export
#' @examples
#' get_confidence_intervals_event(event = 80,
#'                                total = 100,
#'                                confidence_level = 1.96)



get_confidence_intervals_event <- function(event, total, confidence_level) {


  adjusted_proportion <- function(event, total, confidence_level) {
    p_adj <- (event + confidence_level^2/2)/(total + confidence_level^2)
    p_adj
  }

  p_adj <- adjusted_proportion(event, total, confidence_level)


  p <- p_adj

  total <- total + confidence_level*2

  value <- confidence_level*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)
}
