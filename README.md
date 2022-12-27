
<!-- README.md is generated from README.Rmd. Please edit that file -->

# uxr

<!-- badges: start -->

\[![](https://cranlogs.r-pkg.org/badges/grand-total/uxr)\]

<!-- badges: end -->

The purpose of this package is to provide convenience functions for user
experience research. Particularly, this package focuses on quantitative
user experience testing and reporting.

## Installation

You can install the `uxr` package with:

``` r
install.packages("uxr")
```

## Examples

``` r
library(uxr)
```

### Compare Probability of an Event with Benchmark

``` r
data <- data.frame(task_1 = c("y", "y", "y", "y", "n", "n", "n", NA, NA, NA, NA, NA, NA, NA),
                    task_2 = c(0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1))

## with dataframe column

benchmark_event(data, 
                column = task_1, 
                benchmark = 0.8, 
                event = "y")
#> 
#> ── Compare Event Rate with a Benchmark ─────────────────────────────────────────
#> Based on the event rate of 58%, the probability that this rate exceeds a
#> benchmark of 80% is 3%
#>   term            result  
#>   count                4  
#>   total                7  
#>   benchmark          0.8  
#>   result        0.033344  
#>   probability      0.033
```

``` r
benchmark_event(data, 
                column = task_2, 
                benchmark = 0.3, 
                event = 1, 
                event_type = "success")
#> 
#> ── Compare Event Rate with a Benchmark ─────────────────────────────────────────
#> Based on the success rate of 42%, the probability that this rate exceeds a
#> benchmark of 30% is 78%
#>   term             result  
#>   count                 6  
#>   total                14  
#>   benchmark           0.3  
#>   result        0.7805158  
#>   probability       0.781
```

``` r
## pipeable
data |> 
  benchmark_event(column = task_2, 
                  benchmark = 0.3, 
                  event = 1, 
                  event_type = "success")
#> 
#> ── Compare Event Rate with a Benchmark ─────────────────────────────────────────
#> Based on the success rate of 42%, the probability that this rate exceeds a
#> benchmark of 30% is 78%
#>   term             result  
#>   count                 6  
#>   total                14  
#>   benchmark           0.3  
#>   result        0.7805158  
#>   probability       0.781
```

``` r
# specify `input = "values` to use with direct values
benchmark_event(benchmark = 0.8, 
                count = 9, 
                total = 11, 
                input = "values")
#> 
#> ── Compare Event Rate with a Benchmark ─────────────────────────────────────────
#> Based on the event rate of 82%, the probability that this rate exceeds a
#> benchmark of 80% is 38%
#>   term             result  
#>   count                 9  
#>   total                11  
#>   benchmark           0.8  
#>   result        0.3825985  
#>   probability       0.383
```

``` r
# get confidence intervals
# test_wald_adj(10, 12)
```

### Compare Score with a Benchmark

``` r
scores <- 80 + 23 * scale(rnorm(172)) # 80 = mean, 23 = sd
data <- data.frame(scores = scores)
```

``` r
# with dataframe column
benchmark_score(data, scores, 67)
#> 
#> ── Compare Score with a Benchmark ──────────────────────────────────────────────
#> We can be 100% confident that the true score is between 67 and 93
#>   term                    result  
#>   mean                        80  
#>   sd                          23  
#>   se                    1.753734  
#>   n                          172  
#>   df                         171  
#>   probability       2.732705e-12  
#>   tail                       one  
#>   confidence                   1  
#>   margin_of_error             13  
#>   t                     7.412757  
#>   lower_ci                    67  
#>   upper_ci                    93
```

``` r
# pipeable
data |> benchmark_score(scores, 67)
#> 
#> ── Compare Score with a Benchmark ──────────────────────────────────────────────
#> We can be 100% confident that the true score is between 67 and 93
#>   term                    result  
#>   mean                        80  
#>   sd                          23  
#>   se                    1.753734  
#>   n                          172  
#>   df                         171  
#>   probability       2.732705e-12  
#>   tail                       one  
#>   confidence                   1  
#>   margin_of_error             13  
#>   t                     7.412757  
#>   lower_ci                    67  
#>   upper_ci                    93
```

``` r
# specify `input = "values` to use with direct values
benchmark_score(mean = 80, 
                sd = 23, 
                n = 172, 
                benchmark = 67, 
                input = "values")
#> 
#> ── Compare Score with a Benchmark ──────────────────────────────────────────────
#> We can be 100% confident that the true score is between 67 and 93
#>   term                    result  
#>   mean                        80  
#>   sd                          23  
#>   se                    1.753734  
#>   n                          172  
#>   df                         171  
#>   probability       2.732705e-12  
#>   tail                       one  
#>   confidence                   1  
#>   margin_of_error             13  
#>   t                     7.412757  
#>   lower_ci                    67  
#>   upper_ci                    93
```

### Compare Time with a Benchmark

``` r
data <- data.frame(time = c(60, 53, 70, 42, 62, 43, 81))
benchmark_time(data, column = time, benchmark = 60, alpha = 0.05)
#> 
#> ── Compare Time with a Benchmark ───────────────────────────────────────────────
#>   term          t.result_table.  
#>   lower_ci                 45.8  
#>   upper_ci                 71.7  
#>   t                       0.509  
#>   probability             0.314
```

### Compare Means Between Groups

``` r
# Wide data - default

data_wide <- data.frame(A = c(4, 2, 5, 3, 6, 2, 5),
                        B = c(5, 2, 1, 2, 1, 3, 2))

compare_means_between_groups(data_wide, var1 = A, var2 = B)
#> 
#> ── Welch Two Sample t-test ─────────────────────────────────────────────────────
#>   term          value  
#>   mean_of_A      3.86  
#>   mean_of_B      2.29  
#>   t              1.99  
#>   df             11.8  
#>   p_value      0.0707  
#>   ci_level       0.95  
#>   lower_ci     -0.156  
#>   upper_ci        3.3
```

``` r
# Long data

data_long <- data_wide |> tibble::rowid_to_column("id") |>
  tidyr::pivot_longer(cols = -id, names_to = "group", values_to = "variable")

compare_means_between_groups(data_long, 
                             variable = variable,
                             grouping_variable = group, 
                             groups = c("A", "B"), 
                             input = "long")
#> 
#> ── Welch Two Sample t-test ─────────────────────────────────────────────────────
#>   term          value  
#>   mean_of_A      3.86  
#>   mean_of_B      2.29  
#>   t              1.99  
#>   df             11.8  
#>   p_value      0.0707  
#>   ci_level       0.95  
#>   lower_ci     -0.156  
#>   upper_ci        3.3
```

``` r
A <- 51.6 + 4.07 * scale(rnorm(11)) 
A <- c(A, NA)
B <- 49.6 + 4.63 * scale(rnorm(12))
data <- data.frame(A, B)

compare_means_between_groups(data, A, B)
#> 
#> ── Welch Two Sample t-test ─────────────────────────────────────────────────────
#>   term          value  
#>   mean_of_A      51.6  
#>   mean_of_B      49.6  
#>   t               1.1  
#>   df               21  
#>   p_value       0.283  
#>   ci_level       0.95  
#>   lower_ci      -1.77  
#>   upper_ci       5.77
```

### Compare Means Within Groups

``` r
data <- data.frame(id = c(1:7), task1 = c(4, 1, 2, 3, 8, 4, 4), task2 = c(7, 13, 9, 7, 18, 8, 10))
compare_means_within_groups(data, task1, task2)
#> 
#> ── Compare Means Within Groups ─────────────────────────────────────────────────
#>   X.                result.estimate  
#>   mean.difference         -6.571429  
#>   t                           -5.18  
#>   p                           0.002  
#>   df                              6  
#>   ci_level                     0.95  
#>   lower_ci                    -9.68  
#>   upper_ci                    -3.46
```

### Compare Rates Between Groups

``` r
design = c("A","B")
complete = c(10, 4)
incomplete = c(2, 9)
data <- data.frame(design, complete, incomplete)
data <- data |> tidyr::pivot_longer(!design, names_to = "rate", values_to = "n") |>
  tidyr::uncount(n)

compare_rates_between_groups(data, group = design, event = rate)
#> 
#> ── Compare Rates Between Groups ────────────────────────────────────────────────
#> → N-1 Two Proportions test
#>   term         value  
#>   a            0.526  
#>   b             0.98  
#>   c            0.246  
#>   d             0.16  
#>   den          0.199  
#>   num          0.515  
#>   z             2.59  
#>   p_value    0.00955  
#>   n               25  
#>   lower_ci     0.133  
#>   upper_ci     0.776
```

### Compare Rates Within Groups

``` r
A <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
B <- c(0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0)
data <- data.frame(A, B)

compare_rates_within_groups(data, A, B, input = "wide")
#> 
#> ── Compare Rates Within Groups ─────────────────────────────────────────────────
#> → McNemar's Test
#>   p_value   lower_ci   upper_ci  
#>   0.125     0.000569   0.59
```
