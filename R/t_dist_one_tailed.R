


t_dist_one_tailed <- function(t_score, degrees_of_freedom) {
  pt(-abs(t_score), degrees_of_freedom)
}
