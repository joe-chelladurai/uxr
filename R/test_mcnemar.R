



data <- tribble(~id, ~A, ~B,
                1	, 1	, 0,
                2	, 1	, 1,
                3	, 1	, 1,
                4	, 1	, 0,
                5	, 1	, 0,
                6	, 1	, 1,
                7	, 1	, 1,
                8	, 0	, 1,
                9	, 1	, 0,
                10	, 1	, 1,
                11	, 0	, 0,
                12	, 1	, 1,
                13	, 1	, 0,
                14	, 1	, 1,
                15	, 1	, 0
)

test_mcnemar(data, id, A, B)


test_mcnemar <- function(data, id, x, y) {

  discordant_pairs <- get_concordant_discordant_pairs(data, {{id}}, {{x}}, {{y}}) |>
    filter(pairs %in% c("b", "c")) |>
    select(pairs, n) |>
    deframe() |>
    as.list()

  x <- discordant_pairs |>
    unlist() |>
    sum()

  y <- discordant_pairs |>
    unlist() |>
    min()

  y <- c(0:y)

  factorial_fun <- function(x, y) {
    a <- factorial(x) / (factorial(y) * factorial(x - y))
    b <- (0.5 ^ y) * (1 - 0.5) ^ (x - y)
    a * b
  }

  r <- lapply(x, factorial_fun, y = y)
  sum(r[[1]])*2

}





