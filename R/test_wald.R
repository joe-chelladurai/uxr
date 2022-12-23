
#' Wald Confidence Intervals
#'
#' @param success success
#' @param total total
#' @param conf_level conf_level (default: 0.95)
#'
#' @return lower_ci, upper_ci
#' @export
#' @examples
#' test_wald(10, 12, 0.95)
#' test_wald(5, 7, 0.95)

test_wald <- function(success, total, conf_level = 0.95) {

  z <- abs(qnorm((1 - conf_level) / 2))

  p <- success/total

  value <- z*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)

}

