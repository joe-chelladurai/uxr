


#' Adjusted Wald Confidence Intervals
#'
#' @param success success
#' @param total total
#' @param conf_level conf_level (default: 0.95)
#' @return lower_ci, upper_ci
#' @export
#' @examples
#' test_wald_adj(10, 12, 0.95)
#' test_wald_adj(5, 7, 0.95)



test_wald_adj <- function(success, total, conf_level = 0.95) {


  z <- abs(qnorm((1 - conf_level) / 2))

  adjusted_proportion <- function(success, total, conf_level) {

    z <- abs(qnorm((1 - conf_level) / 2))

    p_adj <- (success + z^2/2)/(total + z^2)
    p_adj
  }

  p_adj <- adjusted_proportion(success, total, conf_level)


  p <- p_adj

  total <- total + z*2

  value <- z*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)
}

