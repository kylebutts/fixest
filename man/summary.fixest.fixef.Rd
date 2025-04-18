% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary.fixest.fixef}
\alias{summary.fixest.fixef}
\title{Summary method for fixed-effects coefficients}
\usage{
\method{summary}{fixest.fixef}(object, n = 5, ...)
}
\arguments{
\item{object}{An object returned by the function \code{\link{fixef.fixest}}.}

\item{n}{Positive integer, defaults to 5. The \code{n} first fixed-effects for each
fixed-effect dimension are reported.}

\item{...}{Not currently used.}
}
\value{
It prints the number of fixed-effect coefficients per fixed-effect dimension, as well as
the number of fixed-effects used as references for each dimension, and the mean and variance
of the fixed-effect coefficients. Finally, it reports the first 5 (arg. \code{n}) elements of
each fixed-effect.
}
\description{
This function summarizes the main characteristics of the fixed-effects coefficients.
It shows the number of fixed-effects that have been set as references and the first
elements of the fixed-effects.
}
\examples{

data(trade)

# We estimate the effect of distance on trade
# => we account for 3 fixed-effects effects
est_pois = femlm(Euros ~ log(dist_km)|Origin+Destination+Product, trade)

# obtaining the fixed-effects coefficients
fe_trade = fixef(est_pois)

# printing some summary information on the fixed-effects coefficients:
summary(fe_trade)


}
\seealso{
\code{\link{femlm}}, \code{\link{fixef.fixest}}, \code{\link{plot.fixest.fixef}}.
}
\author{
Laurent Berge
}
