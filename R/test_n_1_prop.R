

#' N-1 Two Proportions Test
#'
#' @param data data
#' @param x x
#' @param y y
#' @param value value
#' @param z z
#' @return results
#' @export
#' @examples
#' design = c("A","B")
#' complete = c(37, 22)
#' incomplete = c(418, 416)
#' data <- data.frame(design, complete, incomplete)
#' data <- data |> pivot_longer(!design, names_to = "rate", values_to = "n") |>
#'   tidyr::uncount(n)
#' test_n_1_prop(data, design, rate, z = 1.645)


test_n_1_prop <- function(data, x, y, z = 1.96) {

  prop_tab <- data |> group_by({{x}}, {{y}}) |>
    count() |> ungroup() |>
    complete({{x}}, {{y}}, fill = list(n = 0)) |>
    group_by({{x}}) |>
    mutate(prop = n/sum(n)) |> ungroup()

  n <- prop_tab |>
    summarise(total_cases = sum(n)) |> purrr::pluck(1)

  a <- prop_tab |> group_by({{x}}) |> slice(1) |> ungroup() |>
    summarise(sum =abs(diff(prop))) |> purrr::pluck(1)


  b <- sqrt((n-1)/n)

  c <- prop_tab |> group_by({{y}}) |> summarise(sum = sum(n)) |>
    mutate(total = sum(sum)) |>
    mutate(fr = sum/total) |>
    summarise(prod = prod(fr)) |> purrr::pluck(1)

  d <- prop_tab |> group_by({{x}}) |>
    summarise(sum = sum(n)) |>
    mutate(num = 1) |> mutate(num/sum) |> ungroup() |>
    summarise(sum(num/sum)) |> purrr::pluck(1)

  ci <- prop_tab |> ungroup() |> rename(x = n) |> select(-prop) |>
    group_by(design) |>
    mutate(n = sum(x)) |> slice(1) |>
    mutate(num = x + (z^2)/4) |>
    mutate(den = n + (z^2)/2) |> mutate(p_adj = num/den) |>
    mutate(adj_n = (p_adj*(1-p_adj))/den) |>
    ungroup() |>
    mutate(lhs = diff(p_adj)) |>
    mutate(rhs = sum(adj_n)) |>
    mutate(rhs = z*sqrt(rhs)) |>
    mutate(upper_ci = abs(lhs) + rhs) |>
    mutate(lower_ci = abs(lhs) - rhs) |> slice(1)


  den <- sqrt(c*d)

  num <- a*b

  z <- num/den

  p_value <- 2*pnorm(q=z, lower.tail=FALSE)

  list(a   = a,
       b   = b,
       c   = c,
       d   = d,
       den = den,
       num = num,
       z   = z,
       p_value = p_value,
       n   = n,
       lower_ci = ci$lower_ci,
       upper_ci = ci$upper_ci)

}



