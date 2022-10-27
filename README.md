


# Quantitative User Experience (UX) Research in R

The purpose of this package is to provide convenience functions for user experience research.
Particularly, this package focuses on quantitative user experience testing and reporting.

## Installation

**Please note that this package is in early development and there will be breaking changes.**

You can install the package directly from CRAN using:
```
install.packages("uxr")
```
To install the development version, use:
```
remotes::install_github("joe-chelladurai/uxr")
```

## Available functions

**Published on CRAN**

 - `adjusted_wald_ci`
 - `wald test`
 - `benchmark_comparison`
 - `task_completion`
 - `mean_ci`
 - `mean_ci_2`


**Development Version**

 - `t_test`
 - `t_test_paired`
 - `compare_benchmark_event`
 - `compare_benchmark_time`
 - `get_concordant_discordant_pairs`
 - `get_confidence_intervals_event`


To do:
 - `compare_means_within_groups` - two-sample t-test
 - `compare_means_between_groups` - paired t-test
 - `compare_rates_within_groups` - mcnemar exact test
 - `compare_rates_between_groups` - n-1 two proportion test
 
 - `compare_benchmark_score` 
 
