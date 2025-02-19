% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{base_stagg}
\alias{base_stagg}
\title{Sample data for staggered difference in difference}
\format{
\code{base_stagg} is a data frame with 950 observations and 7 variables:
\itemize{
\item id: panel identifier.
\item year: from 1 to 10.
\item year_treated: the period at which the individual is treated.
\item time_to_treatment: different between the year and the treatment year.
\item treated: indicator taking value 1 if the individual is treated, 0 otherwise.
\item treatment_effect_true: true effect of the treatment.
\item x1: explanatory variable, correlated with the period.
\item y: the dependent variable affected by the treatment.
}
}
\source{
This data has been generated from \pkg{R}.
}
\usage{
data(base_stagg, package = "fixest")
}
\description{
This data has been generated to illustrate the Sun and Abraham (Journal of Econometrics, 2021) method for staggered difference-in-difference. This is a balanced panel of 95 individuals and 10 periods. Half the individuals are treated. For those treated, the treatment date can vary from the second to the last period. The effect of the treatment depends on the time since the treatment: it is first negative and then increasing.
}
\keyword{datasets}
