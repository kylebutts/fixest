% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{formula.fixest}
\alias{formula.fixest}
\title{Extract the formula of a \code{fixest} fit}
\usage{
\method{formula}{fixest}(x, type = c("full", "linear", "iv", "NL"), ...)
}
\arguments{
\item{x}{An object of class \code{fixest}. Typically the result of a \code{\link{femlm}}, \code{\link{feols}}
or \code{\link{feglm}} estimation.}

\item{type}{A character scalar. Default is \code{type = "full"} which gives back a formula
containing the linear part of the model along with the fixed-effects (if any) and the
IV part (if any). If \code{type = "linear"} then only the linear formula is returned.
If \code{type = "NL"} then only the non linear in parameters part is returned.}

\item{...}{Not currently used.}
}
\value{
It returns a formula.
}
\description{
This function extracts the formula from a \code{fixest} estimation (obtained with \code{\link{femlm}},
\code{\link{feols}} or \code{\link{feglm}}). If the estimation was done with fixed-effects, they are added
in the formula after a pipe (\dQuote{|}). If the estimation was done with a non
linear in parameters part, then this will be added in the formula in between \code{I()}.
}
\examples{

# simple estimation on iris data, using "Species" fixed-effects
res = femlm(Sepal.Length ~ Sepal.Width + Petal.Length +
            Petal.Width | Species, iris)

# formula with the fixed-effect variable
formula(res)

# linear part without the fixed-effects
formula(res, "linear")


}
\seealso{
See also the main estimation functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.
\code{\link{model.matrix.fixest}}, \code{\link{update.fixest}}, \code{\link{summary.fixest}}, \code{\link{vcov.fixest}}.
}
\author{
Laurent Berge
}
