% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel.R
\name{unpanel}
\alias{unpanel}
\title{Dissolves a \code{fixest} panel}
\usage{
unpanel(x)
}
\arguments{
\item{x}{A \code{fixest_panel} object (obtained from function \code{\link{panel}}).}
}
\value{
Returns a data set of the exact same dimension. Only the attribute 'panel_info' is erased.
}
\description{
Transforms a \code{fixest_panel} object into a regular data.frame.
}
\examples{

data(base_did)

# Setting a data set as a panel
pdat = panel(base_did, ~id+period)

# ... allows you to use leads and lags in estimations
feols(y~l(x1, 0:1), pdat)

# Now unpanel => returns the initial data set
class(pdat) ; dim(pdat)
new_base = unpanel(pdat)
class(new_base) ; dim(new_base)



}
\seealso{
Alternatively, the function \code{\link{panel}} changes a \code{data.frame} into a panel from which the
functions \code{l} and \code{f} (creating leads and lags) can be called. Otherwise you can set the panel
'live' during the estimation using the argument \code{panel.id} (see for example in the function
\code{\link{feols}}).
}
\author{
Laurent Berge
}
