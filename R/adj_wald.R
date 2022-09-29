


adj_wald_ci <- function(success, total, ci_z) {


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

# adj_wald_ci(7, 10, 1.96)
