


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

 - `benchmark_event`
 - `benchmark_time`
 - `benchmark_score`
 - `get_concordant_discordant_pairs`
 - `test_t`
 - `test_t_paired`
 - `test_mcnemar`
 - `test_fisher`
 - `test_n_1_prop`
 - `test_wald`
 - `test_wald_adj`
 - `compare_means_within_groups` - two-sample t-test (Welsh/Student)
 - `compare_means_between_groups` - paired t-test
 - `compare_rates_within_groups` - mcnemar exact test
 - `compare_rates_between_groups` - n-1 two proportion test
 
 
