
set.seed(1)

sample_xy <-
  tibble::tibble(
    x = runif(100),
    y = runif(100)
  )



usethis::use_data(sample_xy, overwrite = TRUE)



#' Sample XY - Example Dataset
#'
#' Example data of two variables with uniform distribution
#'
#' @format ## `sample_xy`
#' A data frame with 100 rows and 2 columns:
#' \describe{
#'   \item{x}{Column x}
#'   \item{y}{Column y}
#' }
"sample_xy"
