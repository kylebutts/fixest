% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{confint.fixest}
\alias{confint.fixest}
\title{Confidence interval for parameters estimated with \code{fixest}}
\usage{
\method{confint}{fixest}(
  object,
  parm,
  level = 0.95,
  vcov,
  se,
  cluster,
  ssc = NULL,
  coef.col = FALSE,
  ...
)
}
\arguments{
\item{object}{A \code{fixest} object. Obtained using the functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.}

\item{parm}{The parameters for which to compute the confidence interval (either an
integer vector OR a character vector with the parameter name). If missing, all
parameters are used.}

\item{level}{The confidence level. Default is 0.95.}

\item{vcov}{Versatile argument to specify the VCOV. In general, it is either a character
scalar equal to a VCOV type, either a formula of the form: \code{vcov_type ~ variables}. The
VCOV types implemented are: "iid", "hetero" (or "HC1"), "cluster", "twoway",
"NW" (or "newey_west"), "DK" (or "driscoll_kraay"), and "conley". It also accepts
object from \code{\link{vcov_cluster}}, \code{\link[=vcov_hac]{vcov_NW}}, \code{\link[=vcov_hac]{NW}},
\code{\link[=vcov_hac]{vcov_DK}}, \code{\link[=vcov_hac]{DK}}, \code{\link{vcov_conley}} and
\code{\link[=vcov_conley]{conley}}. It also accepts covariance matrices computed externally.
Finally it accepts functions to compute the covariances. See the \code{vcov} documentation
in the \href{https://lrberge.github.io/fixest/articles/fixest_walkthrough.html#the-vcov-argument-1}{vignette}.}

\item{se}{Character scalar. Which kind of standard error should be computed:
\dQuote{standard}, \dQuote{hetero}, \dQuote{cluster}, \dQuote{twoway}, \dQuote{threeway}
or \dQuote{fourway}? By default if there are clusters in the estimation:
\code{se = "cluster"}, otherwise \code{se = "iid"}. Note that this argument is deprecated,
you should use \code{vcov} instead.}

\item{cluster}{Tells how to cluster the standard-errors (if clustering is requested).
Can be either a list of vectors, a character vector of variable names, a formula or
an integer vector. Assume we want to perform 2-way clustering over \code{var1} and \code{var2}
contained in the data.frame \code{base} used for the estimation. All the following
\code{cluster} arguments are valid and do the same thing:
\code{cluster = base[, c("var1", "var2")]}, \code{cluster = c("var1", "var2")}, \code{cluster = ~var1+var2}.
If the two variables were used as fixed-effects in the estimation, you can leave it
blank with \code{vcov = "twoway"} (assuming \code{var1} [resp. \code{var2}] was
the 1st [resp. 2nd] fixed-effect). You can interact two variables using \code{^} with
the following syntax: \code{cluster = ~var1^var2} or \code{cluster = "var1^var2"}.}

\item{ssc}{An object of class \code{ssc.type} obtained with the function \code{\link{ssc}}. Represents
how the degree of freedom correction should be done.You must use the function \code{\link{ssc}}
for this argument. The arguments and defaults of the function \code{\link{ssc}} are:
\code{adj = TRUE}, \code{fixef.K="nested"}, \code{cluster.adj = TRUE}, \code{cluster.df = "min"},
\code{t.df = "min"}, \verb{fixef.force_exact=FALSE)}. See the help of the function \code{\link{ssc}} for details.}

\item{coef.col}{Logical, default is \code{FALSE}. If \code{TRUE} the column \code{coefficient} is
inserted in the first position containing the coefficient names.}

\item{...}{Not currently used.}
}
\value{
Returns a data.frame with two columns giving respectively the lower and upper bound
of the confidence interval. There is as many rows as parameters.
}
\description{
This function computes the confidence interval of parameter estimates obtained from a
model estimated with \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.
}
\examples{

# Load trade data
data(trade)

# We estimate the effect of distance on trade (with 3 fixed-effects)
est_pois = femlm(Euros ~ log(dist_km) + log(Year) | Origin + Destination +
                 Product, trade)

# confidence interval with "normal" VCOV
confint(est_pois)

# confidence interval with "clustered" VCOV (w.r.t. the Origin factor)
confint(est_pois, se = "cluster")


}
\author{
Laurent Berge
}
