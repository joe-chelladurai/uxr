% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_chisq_one.R
\name{test_chisq_one}
\alias{test_chisq_one}
\title{Chi-squared One Sample}
\usage{
test_chisq_one(data, x)
}
\arguments{
\item{data}{data}

\item{x}{x}
}
\value{
results
}
\description{
Chi-squared One Sample
}
\examples{
data <- tibble::tribble(~fruit, ~count,
                        "Apple"          , 29,
                        "Banana"         , 24,
                        "Cucumber"       , 22,
                        "Dragon Fruit"   , 19
                        )

data <- data |>
  tidyr::uncount(weights = count) |>
  tibble::rowid_to_column("id")

test_chisq_one(data, fruit)
}
