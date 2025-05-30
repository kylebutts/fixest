% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitstats.R
\name{wald}
\alias{wald}
\title{Wald test of nullity of coefficients}
\usage{
wald(x, keep = NULL, drop = NULL, print = TRUE, vcov, se, cluster, ...)
}
\arguments{
\item{x}{A \code{fixest} object. Obtained using the methods
\code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.}

\item{keep}{Character vector. This element is used to display only a subset of variables. This
should be a vector of regular expressions (see \code{\link[base:regex]{base::regex}} help for more info). Each
variable satisfying any of the regular expressions will be kept. This argument is applied post
aliasing (see argument \code{dict}). Example: you have the variable \code{x1} to \code{x55} and want to display
only \code{x1} to \code{x9}, then you could use \code{keep = "x[[:digit:]]$"}. If the first character is an
exclamation mark, the effect is reversed (e.g. keep = "!Intercept" means: every variable that
does not contain \dQuote{Intercept} is kept). See details.}

\item{drop}{Character vector. This element is used if some variables are not to be displayed.
This should be a vector of regular expressions (see \code{\link[base:regex]{base::regex}} help for more info). Each
variable satisfying any of the regular expressions will be discarded. This argument is applied
post aliasing (see argument \code{dict}). Example: you have the variable \code{x1} to \code{x55} and want to
display only \code{x1} to \code{x9}, then you could use \verb{drop = "x[[:digit:]]\{2\}}". If the first character
is an exclamation mark, the effect is reversed (e.g. drop = "!Intercept" means: every variable
that does not contain \dQuote{Intercept} is dropped). See details.}

\item{print}{Logical, default is \code{TRUE}. If \code{TRUE}, then a verbose description of the test
is prompted on the R console. Otherwise only a named vector containing the test statistics
is returned.}

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

\item{...}{Any other element to be passed to \code{\link{summary.fixest}}.}
}
\value{
A named vector containing the following elements is returned: \code{stat}, \code{p}, \code{df1},
and \code{df2}. They correspond to the test statistic, the p-value, the first and
second degrees of freedoms.

If no valid coefficient is found, the value \code{NA} is returned.
}
\description{
Wald test used to test the joint nullity of a set of coefficients.
}
\details{
The type of VCOV matrix plays a crucial role in this test. Use the arguments \code{se} and
\code{cluster} to change the type of VCOV for the test.
}
\examples{

data(airquality)

est = feols(Ozone ~ Solar.R + Wind + poly(Temp, 3), airquality)

# Testing the joint nullity of the Temp polynomial
wald(est, "poly")

# Same but with clustered SEs
wald(est, "poly", cluster = "Month")

# Now: all vars but the polynomial and the intercept
wald(est, drop = "Inte|poly")

#
# Toy example: testing pre-trends
#

data(base_did)

est_did = feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)

# The graph of the coefficients
coefplot(est_did)

# The pre-trend test
wald(est_did, "period::[1234]$")

# If "period::[1234]$" looks weird to you, check out
# regular expressions: e.g. see ?regex.
# Learn it, you won't regret it!


}
