
#' Wald Confidence Intervals
#'
#' @param success success
#' @param total total
#' @param ci_z z value
#'
#' @return lower_ci, upper_ci
#' @export
#' @examples
#' test_wald(10, 12, 1.96)
#' test_wald(5, 7, 1.96)

test_wald <- function(success, total, ci_z) {

  p <- success/total

  value <- ci_z*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)

}

