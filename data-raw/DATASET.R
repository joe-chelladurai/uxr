
set.seed(1)

sample_xy <-
  tibble::tibble(
    x = runif(100),
    y = runif(100)
  )



usethis::use_data(sample_xy, overwrite = TRUE)
