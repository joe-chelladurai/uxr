


#' Adjusted Wald Confidence Intervals
#'
#' @param success success
#' @param total total
#' @param ci_z z value
#'
#' @return lower_ci, upper_ci
#' @export
#' @examples
#' test_wald_adj(10, 12, 1.96)
#' test_wald_adj(5, 7, 1.96)



test_wald_adj <- function(success, total, ci_z) {


  adjusted_proportion <- function(success, total, ci_z) {
    p_adj <- (success + ci_z^2/2)/(total + ci_z^2)
    p_adj
  }

  p_adj <- adjusted_proportion(success, total, ci_z)


  p <- p_adj
  total <- total + ci_z*2

  value <- ci_z*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)
}

