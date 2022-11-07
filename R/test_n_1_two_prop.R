

#' N-1 Two Proportions Test
#'
#' @param data data
#' @param x x
#' @param y y
#' @param value value
#' @param z z
#' @return results
#' @export

test_n_1_two_prop <- function(data, x, y, value, z) {

  table_count <- summarise(data, tab = list(table({{x}}, {{y}})))$tab[[1]]

  a <- abs(diff(prop.table(table_count, 1)))
  a <- a[1, value]
  n <- nrow(data)
  b <- sqrt((n - 1) / n)
  num <- a * b

  count <- table_count |>
    data.frame() %>%
    filter(.[2] == value)
  count <- sum(count$Freq)
  total <- table_count |> data.frame()
  total <- sum(total$Freq)
  P <- count / total
  Q <- 1 - P
  c <- P * Q
  d <- sum(1 / rowSums(table_count))

  den <- sqrt(c * d)


  z_value <- num/den

  pvalue <- 2*pnorm(q=z_value, lower.tail=FALSE)



  z_sq <- z^2
  z_q <- z_sq/4
  z_h <- z_sq/2
  tab <- table_count
  den_x <- rowSums(table_count)[[1]]
  den_y <- rowSums(table_count)[[2]]
  num_x <- table_count[1, value]
  num_y <- table_count[2, value]
  p_adj1 <- (num_x + z_q)/(den_x + z_h)
  p_adj2 <- (num_y + z_q)/(den_y + z_h)
  n_adj1 <- den_x + z_h
  n_adj2 <- den_y + z_h
  p_diff <- abs(p_adj1 - p_adj2)
  adj <- z*sqrt(((p_adj1*(1-p_adj1))/n_adj1) + ((p_adj2*(1-p_adj2))/n_adj2))
  lower_ci <- p_diff - adj
  upper_ci <- p_diff + adj



  result <- data.frame(
    z = z_value,
    p = pvalue,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  )

  result


}





