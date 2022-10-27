


t_dist_two_tailed <- function(t_score, degrees_of_freedom) {
  2 * pt(-abs(t_score), degrees_of_freedom)
}
