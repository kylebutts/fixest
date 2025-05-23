% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixest_multi.R
\name{as.list.fixest_multi}
\alias{as.list.fixest_multi}
\title{Transforms a fixest_multi object into a list}
\usage{
\method{as.list}{fixest_multi}(x, ...)
}
\arguments{
\item{x}{A \code{fixest_multi} object, obtained from a \code{fixest} estimation leading to
multiple results.}

\item{...}{Not currently used.}
}
\value{
Returns a list containing all the results of the multiple estimations.
}
\description{
Extracts the results from a \code{fixest_multi} object and place them into a list.
}
\examples{

base = iris
names(base) = c("y", "x1", "x2", "x3", "species")

# Multiple estimation
res = feols(y ~ csw(x1, x2, x3), base, split = ~species)

# All the results at once
as.list(res)


}
\seealso{
The main fixest estimation functions: \code{\link{feols}}, \code{\link[=feglm]{fepois}},
\code{\link[=femlm]{fenegbin}}, \code{\link{feglm}}, \code{\link{feNmlm}}. Tools for mutliple fixest
estimations: \code{\link{summary.fixest_multi}}, \code{\link{print.fixest_multi}}, \code{\link{as.list.fixest_multi}},
\code{\link[fixest]{sub-sub-.fixest_multi}}, \code{\link[fixest]{sub-.fixest_multi}}.
}
