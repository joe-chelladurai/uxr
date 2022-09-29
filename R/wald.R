


wald_ci <- function(success, total, ci_z) {

  p <- success/total

  value <- ci_z*(sqrt((p*(1-p)/total)))

  lower_ci <- p - value
  upper_ci <- p + value

  list(lower_ci = lower_ci,
       upper_ci = upper_ci)

}

# wald_ci(10,12, 1.96)
