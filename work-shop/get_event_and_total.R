

data <- data.frame(var = c("y", "y", "y", "y", "n", "n", "n", NA, NA, NA, NA, NA, NA, NA),
                   var2 = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1))

get_event_and_total <- function(data, col, event, remove_missing = TRUE) {

  col <- deparse(substitute(col))

  if (remove_missing == TRUE) {
    total <- length(data[[col]][!is.na(data[[col]])])
  } else {
    total <- length(data[[col]])
  }

  event_count <- table(data[[col]])[[event]]

  return(tibble::as_tibble(data.frame(
    total = total,
    event_count = event_count
  )))

}

get_event_and_total(data, var, event = "y", remove_missing = FALSE)
