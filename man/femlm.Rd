% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{femlm}
\alias{femlm}
\alias{fenegbin}
\title{Fixed-effects maximum likelihood models}
\usage{
femlm(
  fml,
  data,
  family = c("poisson", "negbin", "logit", "gaussian"),
  vcov,
  start = 0,
  fixef,
  fixef.rm = "perfect",
  offset,
  subset,
  split,
  fsplit,
  split.keep,
  split.drop,
  cluster,
  se,
  ssc,
  panel.id,
  fixef.tol = 1e-05,
  fixef.iter = 10000,
  nthreads = getFixest_nthreads(),
  lean = FALSE,
  verbose = 0,
  warn = TRUE,
  notes = getFixest_notes(),
  theta.init,
  combine.quick,
  mem.clean = FALSE,
  only.env = FALSE,
  only.coef = FALSE,
  data.save = FALSE,
  env,
  ...
)

fenegbin(
  fml,
  data,
  vcov,
  theta.init,
  start = 0,
  fixef,
  fixef.rm = "perfect",
  offset,
  subset,
  split,
  fsplit,
  split.keep,
  split.drop,
  cluster,
  se,
  ssc,
  panel.id,
  fixef.tol = 1e-05,
  fixef.iter = 10000,
  nthreads = getFixest_nthreads(),
  lean = FALSE,
  verbose = 0,
  warn = TRUE,
  notes = getFixest_notes(),
  combine.quick,
  mem.clean = FALSE,
  only.env = FALSE,
  only.coef = FALSE,
  data.save = FALSE,
  env,
  ...
)
}
\arguments{
\item{fml}{A formula representing the relation to be estimated. For example: \code{fml = z~x+y}.
To include fixed-effects, insert them in this formula using a pipe: e.g.
\code{fml = z~x+y|fixef_1+fixef_2}. Multiple estimations can be performed at once:
for multiple dep. vars, wrap them in \code{c()}: ex \code{c(y1, y2)}. For multiple indep.
vars, use the stepwise functions: ex \code{x1 + csw(x2, x3)}.
The formula \code{fml = c(y1, y2) ~ x1 + cw0(x2, x3)} leads to 6 estimation, see details.
Square brackets starting with a dot can be used to call global variables:
\code{y.[i] ~ x.[1:2]} will lead to \code{y3 ~ x1 + x2} if \code{i} is equal to 3 in
the current environment (see details in \code{\link{xpd}}).}

\item{data}{A data.frame containing the necessary variables to run the model.
The variables of the non-linear right hand side of the formula are identified
with this \code{data.frame} names. Can also be a matrix.}

\item{family}{Character scalar. It should provide the family. The possible values
are "poisson" (Poisson model with log-link, the default), "negbin" (Negative Binomial
model with log-link), "logit" (LOGIT model with log-link), "gaussian" (Gaussian model).}

\item{vcov}{Versatile argument to specify the VCOV. In general, it is either a character
scalar equal to a VCOV type, either a formula of the form: \code{vcov_type ~ variables}. The
VCOV types implemented are: "iid", "hetero" (or "HC1"), "cluster", "twoway",
"NW" (or "newey_west"), "DK" (or "driscoll_kraay"), and "conley". It also accepts
object from \code{\link{vcov_cluster}}, \code{\link[=vcov_hac]{vcov_NW}}, \code{\link[=vcov_hac]{NW}},
\code{\link[=vcov_hac]{vcov_DK}}, \code{\link[=vcov_hac]{DK}}, \code{\link{vcov_conley}} and
\code{\link[=vcov_conley]{conley}}. It also accepts covariance matrices computed externally.
Finally it accepts functions to compute the covariances. See the \code{vcov} documentation
in the \href{https://lrberge.github.io/fixest/articles/fixest_walkthrough.html#the-vcov-argument-1}{vignette}.}

\item{start}{Starting values for the coefficients. Can be: i) a numeric of length 1
(e.g. \code{start = 0}, the default), ii) a numeric vector of the exact same length as the
number of variables, or iii) a named vector of any length (the names will be
used to initialize the appropriate coefficients).}

\item{fixef}{Character vector. The names of variables to be used as fixed-effects.
These variables should contain the identifier of each observation (e.g., think of it
as a panel identifier). Note that the recommended way to include fixed-effects is to
insert them directly in the formula.}

\item{fixef.rm}{Can be equal to "perfect" (default), "singleton", "both" or "none".
Controls which observations are to be removed. If "perfect", then observations
having a fixed-effect with perfect fit (e.g. only 0 outcomes in Poisson estimations)
will be removed. If "singleton", all observations for which a fixed-effect appears
only once will be removed. Note, importantly, that singletons are removed in just one pass,
there is no recursivity implemented. The meaning of "both" and "none" is direct.}

\item{offset}{A formula or a numeric vector. An offset can be added to the estimation.
If equal to a formula, it should be of the form (for example) \code{~0.5*x**2}. This
offset is linearly added to the elements of the main formula 'fml'.}

\item{subset}{A vector (logical or numeric) or a one-sided formula. If provided,
then the estimation will be performed only on the observations defined by this argument.}

\item{split}{A one sided formula representing a variable (eg \code{split = ~var}) or a vector.
If provided, the sample is split according to the variable and one estimation is performed
for each value of that variable. If you also want to include the estimation for the
full sample, use the argument \code{fsplit} instead. You can use the special operators
\verb{\%keep\%} and \verb{\%drop\%} to select only a subset of values for which to split the
sample. E.g. \code{split = ~var \%keep\% c("v1", "v2")} will split the sample only according
to the values \code{v1} and \code{v2} of the variable \code{var}; it is equivalent to supplying the
argument \code{split.keep = c("v1", "v2")}. By default there is partial matching on each value,
you can trigger a regular expression evaluation by adding a \code{'@'} first,
as in: \code{~var \%drop\% "@^v[12]"} which will drop values starting with \code{"v1"} or
\code{"v2"} (of course you need to know regexes!).}

\item{fsplit}{A one sided formula representing a variable (eg \code{fsplit = ~var}) or a vector.
If provided, the sample is split according to the variable and one estimation is performed
for each value of that variable. This argument is the same as \code{split} but also includes the
full sample as the first estimation. You can use the special operators \verb{\%keep\%} and \verb{\%drop\%}
to select only a subset of values for which to split the sample.
E.g. \code{fsplit = ~var \%keep\% c("v1", "v2")} will split the sample only according to the
values \code{v1} and \code{v2} of the variable \code{var}; it is equivalent to supplying the
argument \code{split.keep = c("v1", "v2")}. By default there is partial matching on each value,
you can trigger a regular expression evaluation by adding an \code{'@'} first,
as in: \code{~var \%drop\% "@^v[12]"} which will drop values starting with \code{"v1"}
or \code{"v2"} (of course you need to know regexes!).}

\item{split.keep}{A character vector. Only used when \code{split}, or \code{fsplit}, is supplied.
If provided, then the sample will be split only on the values of \code{split.keep}.
The values in \code{split.keep} will be partially matched to the values of \code{split}.
To enable regular expressions, you need to add an \code{'@'} first.
For example \code{split.keep = c("v1", "@other|var")} will keep only the value
in \code{split} partially matched by \code{"v1"} or the values containing \code{"other"} or \code{"var"}.}

\item{split.drop}{A character vector. Only used when \code{split}, or \code{fsplit}, is supplied.
If provided, then the sample will be split only on the values that are not in \code{split.drop}.
The values in \code{split.drop} will be partially matched to the values of \code{split}.
To enable regular expressions, you need to add an \code{'@'} first. For example
\code{split.drop = c("v1", "@other|var")} will drop only the value in \code{split} partially
matched by \code{"v1"} or the values containing \code{"other"} or \code{"var"}.}

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

\item{se}{Character scalar. Which kind of standard error should be computed:
\dQuote{standard}, \dQuote{hetero}, \dQuote{cluster}, \dQuote{twoway}, \dQuote{threeway}
or \dQuote{fourway}? By default if there are clusters in the estimation:
\code{se = "cluster"}, otherwise \code{se = "iid"}. Note that this argument is deprecated,
you should use \code{vcov} instead.}

\item{ssc}{An object of class \code{ssc.type} obtained with the function \code{\link{ssc}}. Represents
how the degree of freedom correction should be done.You must use the function \code{\link{ssc}}
for this argument. The arguments and defaults of the function \code{\link{ssc}} are:
\code{adj = TRUE}, \code{fixef.K="nested"}, \code{cluster.adj = TRUE}, \code{cluster.df = "min"},
\code{t.df = "min"}, \verb{fixef.force_exact=FALSE)}. See the help of the function \code{\link{ssc}} for details.}

\item{panel.id}{The panel identifiers. Can either be: i) a one sided formula
(e.g. \code{panel.id = ~id+time}), ii) a character vector of length 2
(e.g. \code{panel.id=c('id', 'time')}, or iii) a character scalar of two variables
separated by a comma (e.g. \code{panel.id='id,time'}). Note that you can combine variables
with \code{^} only inside formulas (see the dedicated section in \code{\link{feols}}).}

\item{fixef.tol}{Precision used to obtain the fixed-effects. Defaults to \code{1e-5}.
It corresponds to the maximum absolute difference allowed between two coefficients
of successive iterations. Argument \code{fixef.tol} cannot be lower
than \code{10000*.Machine$double.eps}. Note that this parameter is dynamically
controlled by the algorithm.}

\item{fixef.iter}{Maximum number of iterations in fixed-effects algorithm
(only in use for 2+ fixed-effects). Default is 10000.}

\item{nthreads}{The number of threads. Can be: a) an integer lower than, or equal to,
the maximum number of threads; b) 0: meaning all available threads will be used;
c) a number strictly between 0 and 1 which represents the fraction of all threads to use.
The default is to use 50\% of all threads. You can set permanently the number
of threads used within this package using the function \code{\link{setFixest_nthreads}}.}

\item{lean}{Logical, default is \code{FALSE}. If \code{TRUE} then all large objects are removed
from the returned result: this will save memory but will block the possibility to
use many methods. It is recommended to use the arguments \code{se} or \code{cluster} to
obtain the appropriate standard-errors at estimation time, since obtaining different
SEs won't be possible afterwards.}

\item{verbose}{Integer, default is 0. It represents the level of information that
should be reported during the optimisation process. If \code{verbose=0}:
nothing is reported. If \code{verbose=1}: the value of the coefficients and the
likelihood are reported. If \code{verbose=2}: \code{1} + information on the computing time of
the null model, the fixed-effects coefficients and the hessian are reported.}

\item{warn}{Logical, default is \code{TRUE}. Whether warnings should be displayed
(concerns warnings relating to convergence state).}

\item{notes}{Logical. By default, two notes are displayed: when NAs are removed
(to show additional information) and when some observations are removed because
of only 0 (or 0/1) outcomes in a fixed-effect setup (in Poisson/Neg. Bin./Logit models).
To avoid displaying these messages, you can set \code{notes = FALSE}. You can
remove these messages permanently by using \code{setFixest_notes(FALSE)}.}

\item{theta.init}{Positive numeric scalar. The starting value of the dispersion
parameter if \code{family="negbin"}. By default, the algorithm uses as a starting value
the theta obtained from the model with only the intercept.}

\item{combine.quick}{Logical. When you combine different variables to transform them
into a single fixed-effects you can do e.g. \code{y ~ x | paste(var1, var2)}.
The algorithm provides a shorthand to do the same operation: \code{y ~ x | var1^var2}.
Because pasting variables is a costly operation, the internal algorithm may use a
numerical trick to hasten the process. The cost of doing so is that you lose the labels.
If you are interested in getting the value of the fixed-effects coefficients
after the estimation, you should use \code{combine.quick = FALSE}. By default it is
equal to \code{FALSE} if the number of observations is lower than 50,000, and to \code{TRUE}
otherwise.}

\item{mem.clean}{Logical, default is \code{FALSE}. Only to be used if the data set is
large compared to the available RAM. If \code{TRUE} then intermediary objects are removed as
much as possible and \code{\link{gc}} is run before each substantial C++ section in the internal
code to avoid memory issues.}

\item{only.env}{(Advanced users.) Logical, default is \code{FALSE}. If \code{TRUE}, then only
the environment used to make the estimation is returned.}

\item{only.coef}{Logical, default is \code{FALSE}. If \code{TRUE}, then only the estimated
coefficients are returned. Note that the length of the vector returned is always
the length of the number of coefficients to be estimated: this means that the
variables found to be collinear are returned with an NA value.}

\item{data.save}{Logical scalar, default is \code{FALSE}. If \code{TRUE}, the data used for
the estimation is saved within the returned object. Hence later calls to predict(),
vcov(), etc..., will be consistent even if the original data has been modified
in the meantime.
This is especially useful for estimations within loops, where the data changes
at each iteration, such that postprocessing can be done outside the loop without issue.}

\item{env}{(Advanced users.) A \code{fixest} environment created by a \code{fixest} estimation
with \code{only.env = TRUE}. Default is missing. If provided, the data from this environment
will be used to perform the estimation.}

\item{...}{Not currently used.}
}
\value{
A \code{fixest} object. Note that \code{fixest} objects contain many elements and most of
them are for internal use, they are presented here only for information.
To access them, it is safer to use the user-level methods
(e.g. \code{\link{vcov.fixest}}, \code{\link{resid.fixest}}, etc) or functions (like for instance
\code{\link{fitstat}} to access any fit statistic).
\item{nobs}{The number of observations.}
\item{fml}{The linear formula of the call.}
\item{call}{The call of the function.}
\item{method}{The method used to estimate the model.}
\item{family}{The family used to estimate the model.}
\item{data}{The original data set used when calling the function. Only available when
the estimation was called with \code{data.save = TRUE}}
\item{fml_all}{A list containing different parts of the formula. Always contain the
linear formula. Then, if relevant: \code{fixef}: the fixed-effects;
\code{NL}: the non linear part of the formula.}
\item{nparams}{The number of parameters of the model.}
\item{fixef_vars}{The names of each fixed-effect dimension.}
\item{fixef_id}{The list (of length the number of fixed-effects) of the
fixed-effects identifiers for each observation.}
\item{fixef_sizes}{The size of each fixed-effect (i.e. the number of unique
identifier for each fixed-effect dimension).}
\item{convStatus}{Logical, convergence status.}
\item{message}{The convergence message from the optimization procedures.}
\item{obs_selection}{(When relevant.) List containing vectors of integers. It represents
the sequential selection of observation vis a vis the original data set.}
\item{fixef_removed}{(When relevant.) In the case there were fixed-effects and some
observations were removed because of only 0/1 outcome within a fixed-effect, it gives the
list (for each fixed-effect dimension) of the fixed-effect identifiers that were removed.}
\item{coefficients}{The named vector of estimated coefficients.}
\item{coeftable}{The table of the coefficients with their standard errors, z-values
and p-values.}
\item{loglik}{The log-likelihood.}
\item{iterations}{Number of iterations of the algorithm.}
\item{ll_null}{Log-likelihood of the null model (i.e. with the intercept only).}
\item{ll_fe_only}{Log-likelihood of the model with only the fixed-effects.}
\item{ssr_null}{Sum of the squared residuals of the null model (containing only with
the intercept).}
\item{pseudo_r2}{The adjusted pseudo R2.}
\item{fitted.values}{The fitted values are the expected value of the dependent variable
for the fitted model: that is \eqn{E(Y|X)}.}
\item{residuals}{The residuals (y minus the fitted values).}
\item{sq.cor}{Squared correlation between the dependent variable and the
expected predictor (i.e. fitted.values) obtained by the estimation.}
\item{hessian}{The Hessian of the parameters.}
\item{cov.iid}{The variance-covariance matrix of the parameters.}
\item{se}{The standard-error of the parameters.}
\item{scores}{The matrix of the scores (first derivative for each observation).}
\item{residuals}{The difference between the dependent variable and the expected predictor.}
\item{sumFE}{The sum of the fixed-effects coefficients for each observation.}
\item{offset}{(When relevant.) The offset formula.}
}
\description{
This function estimates maximum likelihood models with any number of fixed-effects.
}
\details{
Note that the functions \code{\link{feglm}} and \code{\link{femlm}} provide the same results when using
the same families but differ in that the latter is a direct maximum likelihood
optimization (so the two can really have different convergence rates).
}
\section{Combining the fixed-effects}{

You can combine two variables to make it a new fixed-effect using \code{^}.
The syntax is as follows: \code{fe_1^fe_2}. Here you created a new variable which is the combination
of the two variables fe_1 and fe_2. This is identical to doing \code{paste0(fe_1, "_", fe_2)}
but more convenient.

Note that pasting is a costly operation, especially for large data sets.
Thus, the internal algorithm uses a numerical trick which is fast, but the drawback is
that the identity of each observation is lost (i.e. they are now equal to a meaningless
number instead of being equal to \code{paste0(fe_1, "_", fe_2)}). These \dQuote{identities}
are useful only if you're interested in the value of the fixed-effects (that you can
extract with \code{\link{fixef.fixest}}). If you're only interested in coefficients of the variables,
it doesn't matter. Anyway, you can use \code{combine.quick = FALSE} to tell the internal
algorithm to use \code{paste} instead of the numerical trick. By default, the numerical
trick is performed only for large data sets.
}

\section{Lagging variables}{


To use leads/lags of variables in the estimation, you can: i) either provide the argument
\code{panel.id}, ii) either set your data set as a panel with the function
\code{\link{panel}}, \code{\link[=l]{f}} and \code{\link[=l]{d}}.

You can provide several leads/lags/differences at once: e.g. if your formula is equal to
\code{f(y) ~ l(x, -1:1)}, it means that the dependent variable is equal to the lead of \code{y},
and you will have as explanatory variables the lead of \code{x1}, \code{x1} and the lag of \code{x1}.
See the examples in function \code{\link{l}} for more details.
}

\section{Interactions}{


You can interact a numeric variable with a "factor-like" variable by using
\code{i(factor_var, continuous_var, ref)}, where \code{continuous_var} will be interacted with
each value of \code{factor_var} and the argument \code{ref} is a value of \code{factor_var}
taken as a reference (optional).

Using this specific way to create interactions leads to a different display of the
interacted values in \code{\link{etable}}. See examples.

It is important to note that \emph{if you do not care about the standard-errors of
the interactions}, then you can add interactions in the fixed-effects part of the formula,
it will be incomparably faster (using the syntax \code{factor_var[continuous_var]}, as explained
in the section \dQuote{Varying slopes}).

The function \code{\link{i}} has in fact more arguments, please see details in its associated help page.
}

\section{On standard-errors}{


Standard-errors can be computed in different ways, you can use the arguments \code{se} and \code{ssc}
in \code{\link{summary.fixest}} to define how to compute them. By default, in the presence
of fixed-effects, standard-errors are automatically clustered.

The following vignette: \href{https://lrberge.github.io/fixest/articles/standard_errors.html}{On standard-errors} describes in details how the standard-errors are computed in
\code{fixest} and how you can replicate standard-errors from other software.

You can use the functions \code{\link{setFixest_vcov}} and \code{\link[=ssc]{setFixest_ssc}} to
permanently set the way the standard-errors are computed.
}

\section{Multiple estimations}{


Multiple estimations can be performed at once, they just have to be specified in the formula.
Multiple estimations yield a \code{fixest_multi} object which is \sQuote{kind of} a list of
all the results but includes specific methods to access the results in a handy way.
Please have a look at the dedicated vignette:
\href{https://lrberge.github.io/fixest/articles/multiple_estimations.html}{Multiple estimations}.

To include multiple dependent variables, wrap them in \code{c()} (\code{list()} also works).
For instance \code{fml = c(y1, y2) ~ x1} would estimate the model \code{fml = y1 ~ x1} and
then the model \code{fml = y2 ~ x1}.

To include multiple independent variables, you need to use the stepwise functions.
There are 4 stepwise functions: \code{sw}, \code{sw0}, \code{csw}, \code{csw0}, and \code{mvsw}. Of course \code{sw}
stands for stepwise, and \code{csw} for cumulative stepwise. Finally \code{mvsw} is a bit special,
it stands for multiverse stepwise. Let's explain that.
Assume you have the following formula: \code{fml = y ~ x1 + sw(x2, x3)}.
The stepwise function \code{sw} will estimate the following two models: \code{y ~ x1 + x2} and
\code{y ~ x1 + x3}. That is, each element in \code{sw()} is sequentially, and separately,
added to the formula. Would have you used \code{sw0} in lieu of \code{sw}, then the model
\code{y ~ x1} would also have been estimated. The \code{0} in the name means that the model
without any stepwise element also needs to be estimated.
The prefix \code{c} means cumulative: each stepwise element is added to the next. That is,
\code{fml = y ~ x1 + csw(x2, x3)} would lead to the following models \code{y ~ x1 + x2} and
\code{y ~ x1 + x2 + x3}. The \code{0} has the same meaning and would also lead to the model without
the stepwise elements to be estimated: in other words, \code{fml = y ~ x1 + csw0(x2, x3)}
leads to the following three models: \code{y ~ x1}, \code{y ~ x1 + x2} and \code{y ~ x1 + x2 + x3}.
Finally \code{mvsw} will add, in a stepwise fashion all possible combinations of the variables
in its arguments. For example \code{mvsw(x1, x2, x3)} is equivalent to
\code{sw0(x1, x2, x3, x1 + x2, x1 + x3, x2 + x3, x1 + x2 + x3)}. The number of models
to estimate grows at a factorial rate: so be cautious!

Multiple independent variables can be combined with multiple dependent variables, as in
\code{fml = c(y1, y2) ~ cw(x1, x2, x3)} which would lead to 6 estimations. Multiple
estimations can also be combined to split samples (with the arguments \code{split}, \code{fsplit}).

You can also add fixed-effects in a stepwise fashion. Note that you cannot perform
stepwise estimations on the IV part of the formula (\code{feols} only).

If NAs are present in the sample, to avoid too many messages, only NA removal
concerning the variables common to all estimations is reported.

A note on performance. The feature of multiple estimations has been highly optimized for
\code{feols}, in particular in the presence of fixed-effects. It is faster to estimate
multiple models using the formula rather than with a loop. For non-\code{feols} models using
the formula is roughly similar to using a loop performance-wise.
}

\section{Argument sliding}{


When the data set has been set up globally using
\code{\link{setFixest_estimation}}\code{(data = data_set)}, the argument \code{vcov} can be used implicitly.
This means that calls such as \code{feols(y ~ x, "HC1")}, or \code{feols(y ~ x, ~id)}, are valid:
i) the data is automatically deduced from the global settings, and ii) the \code{vcov}
is deduced to be the second argument.
}

\section{Piping}{


Although the argument 'data' is placed in second position, the data can be piped to the
estimation functions. For example, with R >= 4.1, \code{mtcars |> feols(mpg ~ cyl)} works as
\code{feols(mpg ~ cyl, mtcars)}.
}

\section{Tricks to estimate multiple LHS}{


To use multiple dependent variables in \code{fixest} estimations, you need to include them
in a vector: like in \code{c(y1, y2, y3)}.

First, if names are stored in a vector, they can readily be inserted in a formula to
perform multiple estimations using the dot square bracket operator. For instance if
\code{my_lhs = c("y1", "y2")}, calling \code{fixest} with, say \code{feols(.[my_lhs] ~ x1, etc)} is
equivalent to using \code{feols(c(y1, y2) ~ x1, etc)}. Beware that this is a special feature
unique to the \emph{left-hand-side} of \code{fixest} estimations (the default behavior of the DSB
operator is to aggregate with sums, see \code{\link{xpd}}).

Second, you can use a regular expression to grep the left-hand-sides on the fly. When the
\code{..("regex")} feature is used naked on the LHS, the variables grepped are inserted into
\code{c()}. For example \verb{..("Pe") ~ Sepal.Length, iris} is equivalent to
\verb{c(Petal.Length, Petal.Width) ~ Sepal.Length, iris}. Beware that this is a
special feature unique to the \emph{left-hand-side} of \code{fixest} estimations
(the default behavior of \code{..("regex")} is to aggregate with sums, see \code{\link{xpd}}).
}

\section{Dot square bracket operator in formulas}{


In a formula, the dot square bracket (DSB) operator can: i) create manifold variables at once,
or ii) capture values from the current environment and put them verbatim in the formula.

Say you want to include the variables \code{x1} to \code{x3} in your formula. You can use
\code{xpd(y ~ x.[1:3])} and you'll get \code{y ~ x1 + x2 + x3}.

To summon values from the environment, simply put the variable in square brackets. For example:
\code{for(i in 1:3) xpd(y.[i] ~ x)} will create the formulas \code{y1 ~ x} to \code{y3 ~ x} depending on the
value of \code{i}.

You can include a full variable from the environment in the same way:
\code{for(y in c("a", "b")) xpd(.[y] ~ x)} will create the two formulas \code{a ~ x} and \code{b ~ x}.

The DSB can even be used within variable names, but then the variable must be nested in
character form. For example \code{y ~ .["x.[1:2]_sq"]} will create \code{y ~ x1_sq + x2_sq}. Using the
character form is important to avoid a formula parsing error. Double quotes must be used. Note
that the character string that is nested will be parsed with the function \code{\link{dsb}}, and thus it
will return a vector.

By default, the DSB operator expands vectors into sums. You can add a comma, like in \code{.[, x]},
to expand with commas--the content can then be used within functions. For instance:
\code{c(x.[, 1:2])} will create \code{c(x1, x2)} (and \emph{not} \code{c(x1 + x2)}).

In all \code{fixest} estimations, this special parsing is enabled, so you don't need to use \code{xpd}.

One-sided formulas can be expanded with the DSB operator: let \code{x = ~sepal + petal}, then
\code{xpd(y ~ .[x])} leads to \code{color ~ sepal + petal}.

You can even use multiple square brackets within a single variable, but then the use of nesting
is required. For example, the following \code{xpd(y ~ .[".[letters[1:2]]_.[1:2]"])} will create
\code{y ~ a_1 + b_2}. Remember that the nested character string is parsed with \code{\link{dsb}},
which explains this behavior.

When the element to be expanded i) is equal to the empty string or, ii) is of length 0, it is
replaced with a neutral element, namely \code{1}. For example, \verb{x = "" ; xpd(y ~ .[x])} leads to
\code{y ~ 1}.
}

\examples{

# Load trade data
data(trade)

# We estimate the effect of distance on trade => we account for 3 fixed-effects
# 1) Poisson estimation
est_pois = femlm(Euros ~ log(dist_km) | Origin + Destination + Product, trade)

# 2) Log-Log Gaussian estimation (with same FEs)
est_gaus = update(est_pois, log(Euros+1) ~ ., family = "gaussian")

# Comparison of the results using the function etable
etable(est_pois, est_gaus)
# Now using two way clustered standard-errors
etable(est_pois, est_gaus, se = "twoway")

# Comparing different types of standard errors
sum_hetero   = summary(est_pois, se = "hetero")
sum_oneway   = summary(est_pois, se = "cluster")
sum_twoway   = summary(est_pois, se = "twoway")
sum_threeway = summary(est_pois, se = "threeway")

etable(sum_hetero, sum_oneway, sum_twoway, sum_threeway)


#
# Multiple estimations:
#

# 6 estimations
est_mult = femlm(c(Ozone, Solar.R) ~ Wind + Temp + csw0(Wind:Temp, Day), airquality)

# We can display the results for the first lhs:
etable(est_mult[lhs = 1])

# And now the second (access can be made by name)
etable(est_mult[lhs = "Solar.R"])

# Now we focus on the two last right hand sides
# (note that .N can be used to specify the last item)
etable(est_mult[rhs = 2:.N])

# Combining with split
est_split = fepois(c(Ozone, Solar.R) ~ sw(poly(Wind, 2), poly(Temp, 2)),
                  airquality, split = ~ Month)

# You can display everything at once with the print method
est_split

# Different way of displaying the results with "compact"
summary(est_split, "compact")

# You can still select which sample/LHS/RHS to display
est_split[sample = 1:2, lhs = 1, rhs = 1]




}
\references{
Berge, Laurent, 2018, "Efficient estimation of maximum likelihood models with
multiple fixed-effects: the R package FENmlm." CREA Discussion Papers,
13 (\href{https://github.com/lrberge/fixest/blob/master/_DOCS/FENmlm_paper.pdf}{}).

For models with multiple fixed-effects:

Gaure, Simen, 2013, "OLS with multiple high dimensional category variables",
Computational Statistics & Data Analysis 66 pp. 8--18

On the unconditionnal Negative Binomial model:

Allison, Paul D and Waterman, Richard P, 2002, "Fixed-Effects Negative
Binomial Regression Models", Sociological Methodology 32(1) pp. 247--265
}
\seealso{
See also \code{\link{summary.fixest}} to see the results with the appropriate standard-errors,
\code{\link{fixef.fixest}} to extract the fixed-effects coefficients, and the function
\code{\link{etable}} to visualize the results of multiple estimations.
And other estimation methods: \code{\link{feols}}, \code{\link{feglm}}, \code{\link{fepois}}, \code{\link{feNmlm}}.
}
\author{
Laurent Berge
}
