% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{AIC.fixest}
\alias{AIC.fixest}
\title{Aikake's an information criterion}
\usage{
\method{AIC}{fixest}(object, ..., k = 2)
}
\arguments{
\item{object}{A \code{fixest} object. Obtained using the functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.}

\item{...}{Optionally, more fitted objects.}

\item{k}{A numeric, the penalty per parameter to be used; the default k = 2 is the
classical AIC (i.e. \code{AIC=-2*LL+k*nparams}).}
}
\value{
It return a numeric vector, with length the same as the number of objects taken as arguments.
}
\description{
This function computes the AIC (Aikake's, an information criterion) from a \code{fixest} estimation.
}
\details{
The AIC is computed as:
\deqn{AIC = -2\times LogLikelihood + k\times nbParams}
with k the penalty parameter.

You can have more information on this criterion on \code{\link[stats:AIC]{AIC}}.
}
\examples{

# two fitted models with different expl. variables:
res1 = femlm(Sepal.Length ~ Sepal.Width + Petal.Length +
             Petal.Width | Species, iris)
res2 = femlm(Sepal.Length ~ Petal.Width | Species, iris)

AIC(res1, res2)
BIC(res1, res2)


}
\seealso{
See also the main estimation functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.
Other statictics methods: \code{\link{BIC.fixest}}, \code{\link{logLik.fixest}}, \code{\link{nobs.fixest}}.
}
\author{
Laurent Berge
}
