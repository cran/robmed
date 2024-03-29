% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_mediation.R
\name{fit_mediation}
\alias{fit_mediation}
\alias{print.fit_mediation}
\alias{summary.reg_fit_mediation}
\alias{summary.cov_fit_mediation}
\alias{fit_mediation.formula}
\alias{fit_mediation.default}
\title{(Robustly) fit a mediation model}
\usage{
fit_mediation(object, ...)

\method{fit_mediation}{formula}(formula, data, ...)

\method{fit_mediation}{default}(
  object,
  x,
  y,
  m,
  covariates = NULL,
  method = c("regression", "covariance"),
  robust = TRUE,
  family = "gaussian",
  model = c("parallel", "serial"),
  contrast = FALSE,
  fit_yx = TRUE,
  control = NULL,
  ...
)
}
\arguments{
\item{object}{the first argument will determine the method of the generic
function to be dispatched.  For the default method, this should be a data
frame containing the variables.}

\item{\dots}{additional arguments to be passed down.  For the default
method, this can be used to specify tuning parameters directly instead
of via \code{control}.}

\item{formula}{an object of class "formula" (or one that can be coerced to
that class): a symbolic description of the model to be fitted.  Hypothesized
mediator variables should be wrapped in a call to \code{\link{m}()} (see
examples), and any optional control variables should be wrapped in a call to
\code{\link{covariates}()}.}

\item{data}{for the \code{formula} method, a data frame containing the
variables.}

\item{x}{a character, integer or logical vector specifying the columns of
\code{object} containing the independent variables of interest.}

\item{y}{a character string, an integer or a logical vector specifying the
column of \code{object} containing the dependent variable.}

\item{m}{a character, integer or logical vector specifying the columns of
\code{object} containing the hypothesized mediator variables.}

\item{covariates}{optional; a character, integer or logical vector
specifying the columns of \code{object} containing additional covariates to
be used as control variables.}

\item{method}{a character string specifying the method of
estimation.  Possible values are \code{"regression"} (the default)
to estimate the effects via regressions, or \code{"covariance"} to
estimate the effects via the covariance matrix.  Note that the effects are
always estimated via regressions if more than one independent variable or
hypothesized mediator is specified, or if control variables are supplied.}

\item{robust}{a logical indicating whether to robustly estimate the effects
(defaults to \code{TRUE}).  For estimation via regressions
(\code{method = "regression"}), this can also be a character string, with
\code{"MM"} specifying the MM-estimator of regression, and \code{"median"}
specifying median regression.}

\item{family}{a character string specifying the error distribution to be
used in maximum likelihood estimation of regression models.  Possible values
are \code{"gaussian"} for a normal distribution (the default),
\code{skewnormal} for a skew-normal distribution, \code{"student"} for
Student's t distribution, \code{"skewt"} for a skew-t distribution, or
\code{"select"} to select among these four distributions via BIC (see
\sQuote{Details}).  This is only relevant if \code{method = "regression"}
and \code{robust = FALSE}.}

\item{model}{a character string specifying the type of model in case of
multiple mediators.  Possible values are \code{"parallel"} (the default) for
the parallel multiple mediator model, or \code{"serial"} for the serial
multiple mediator model.  This is only relevant for models with multiple
hypothesized mediators, which are currently only implemented for estimation
via regressions (\code{method = "regression"}).}

\item{contrast}{a logical indicating whether to compute pairwise contrasts
of the indirect effects (defaults to \code{FALSE}).  This can also be a
character string, with \code{"estimates"} for computing the pairwise
differences of the indirect effects, and \code{"absolute"} for computing the
pairwise differences of the absolute values of the indirect effects.  This
is only relevant for models with multiple indirect effects, which are
currently only implemented for estimation via regressions
(\code{method = "regression"}).  For models with multiple independent
variables of interest and multiple hypothesized mediators, contrasts are
only computed between indirect effects corresponding to the same independent
variable.}

\item{fit_yx}{a logical indicating whether to fit the regression model
\code{y ~ x + covariates} to estimate the total effect (the default is
\code{TRUE}).  This is only relevant if \code{method = "regression"} and
\code{robust = FALSE}.}

\item{control}{a list of tuning parameters for the corresponding robust
method.  For robust regression (\code{method = "regression"}, and
\code{robust = TRUE} or \code{robust = "MM"}), a list of tuning
parameters for \code{\link[robustbase]{lmrob}()} as generated by
\code{\link{reg_control}()}.  For winsorized covariance matrix estimation
(\code{method = "covariance"} and \code{robust = TRUE}), a list of tuning
parameters for \code{\link{cov_Huber}()} as generated by
\code{\link{cov_control}()}.  No tuning parameters are necessary for median
regression (\code{method = "regression"} and \code{robust = "median"}).}
}
\value{
An object inheriting from class \code{"fit_mediation"} (class
\code{"reg_fit_mediation"} if \code{method = "regression"} or
\code{"cov_fit_mediation"} if \code{method = "covariance"}) with
the following components:
\item{a}{a numeric vector containing the point estimates of the effects of
the independent variables on the proposed mediator variables.}
\item{b}{a numeric vector containing the point estimates of the direct
effects of the proposed mediator variables on the dependent variable.}
\item{d}{in case of a serial multiple mediator model, a numeric vector
containing the point estimates of the effects of proposed mediator variables
on other mediator variables occurring later in the sequence (only
\code{"reg_fit_mediation"} if applicable).}
\item{total}{a numeric vector containing the point estimates of the total
effects of the independent variables on the dependent variable.}
\item{direct}{a numeric vector containing the point estimates of the direct
effects of the independent variables on the dependent variable.}
\item{indirect}{a numeric vector containing the point estimates of the
indirect effects.}
\item{ab}{for back-compatibility with versions <0.10.0, the point estimates
of the indirect effects are also included here.  \bold{This component is
deprecated and may be removed as soon as the next version.}}
\item{fit_mx}{an object of class \code{"\link[robustbase]{lmrob}"},
\code{"\link[quantreg]{rq}"}, \code{"\link[stats]{lm}"} or \code{"lmse"}
containing the estimation results from the regression of the proposed
mediator variable on the independent variables, or a list of such objects
in case of more than one hypothesized mediator (only
\code{"reg_fit_mediation"}).}
\item{fit_ymx}{an object of class \code{"\link[robustbase]{lmrob}"},
\code{"\link[quantreg]{rq}"}, \code{"\link[stats]{lm}"} or \code{"lmse"}
containing the estimation results from the regression of the dependent
variable on the proposed mediator and independent variables (only
\code{"reg_fit_mediation"}).}
\item{fit_yx}{an object of class \code{"\link[stats]{lm}"} or \code{"lmse"}
containing the estimation results from the regression of the dependent
variable on the independent variables (only \code{"reg_fit_mediation"}
if arguments \code{robust = FALSE} and \code{fit_yx = TRUE} were used).}
\item{cov}{an object of class \code{"\link{cov_Huber}"} or
\code{"\link{cov_ML}"} containing the covariance matrix estimates
(only \code{"cov_fit_mediation"}).}
\item{x, y, m, covariates}{character vectors specifying the respective
variables used.}
\item{data}{a data frame containing the independent, dependent and
proposed mediator variables, as well as covariates.}
\item{robust}{either a logical indicating whether the effects were estimated
robustly, or one of the character strings \code{"MM"} and \code{"median"}
specifying the type of robust regressions.}
\item{model}{a character string specifying the type of mediation model
fitted: \code{"simple"} in case of one independent variable and one
hypothesized mediator, \code{"multiple"} in case of multiple independent
variables and one hypothesized mediator, \code{"parallel"} in case of
parallel multiple mediators, or \code{"serial"} in case of serial multiple
mediators (only \code{"reg_fit_mediation"}).}
\item{contrast}{either a logical indicating whether contrasts of the
indirect effects were computed, or one of the character strings
\code{"estimates"} and \code{"absolute"} specifying the type of contrasts
of the indirect effects (only \code{"reg_fit_mediation"}).}
\item{control}{a list of tuning parameters used (if applicable).}
}
\description{
(Robustly) estimate the effects in a mediation model.
}
\details{
With \code{method = "regression"}, and \code{robust = TRUE} or
\code{robust = "MM"}, the effects are computed via the robust MM-estimator
of regression from \code{\link[robustbase]{lmrob}()}.  This is the default
behavior.

With \code{method = "regression"} and \code{robust = "median"}, the effects
are estimated via median regressions with \code{\link[quantreg]{rq}()}.
Unlike the robust MM-regressions above, median regressions are not robust
against outliers in the explanatory variables.

With \code{method = "regression"}, \code{robust = FALSE} and
\code{family = "select"}, the error distribution to be used in maximum
likelihood estimation of the regression models is selected via BIC.  The
following error distributions are included in the selection procedure: a
normal distribution, a skew-normal distribution, Student's t distribution,
and a skew-t distribution.  Note that the parameters of those distributions
are estimated as well.  The skew-normal and skew-t distributions thereby
use a centered parametrization such that the residuals are (approximately)
centered around 0.  Moreover, the skew-t distribution is only evaluated in
the selection procedure if both the skew-normal and Student's t distribution
yield an improvement in BIC over the normal distribution.  Otherwise the
estimation with a skew-t error distribution can be unstable.  Furthermore,
this saves a considerable amount of computation time in a bootstrap test,
as estimation with those error distributions is orders of magnitude slower
than any other implemented estimation procedure.

With \code{method = "covariance"} and \code{robust = TRUE}, the effects are
estimated based on a Huber M-estimator of location and scatter.  Note that
this covariance-based approach is less robust than the approach based on
robust MM-regressions described above.
}
\note{
The default method takes a data frame its first argument so that it can
easily be used with the pipe operator (\R's built-in \code{|>} or
\pkg{magrittr}'s \code{\%>\%}).
}
\section{Mediation models}{

The following mediation models are implemented.  In the regression equations
below, the \eqn{i_j} are intercepts and the \eqn{e_j} are random error terms.

\itemize{

  \item{\emph{Simple mediation model}: The mediation model in its simplest
  form is given by the equations
  \deqn{M = i_1 + aX + e_1,}
  \deqn{Y = i_2 + bM + cX + e_2,}
  \deqn{Y = i_3 + c'X + e_3,}
  where \eqn{Y} denotes the dependent variable, \eqn{X} the independent
  variable, and \eqn{M} the hypothesized mediator.  The main parameter of
  interest is the product of coefficients \eqn{ab}, called the indirect
  effect.  The coefficients \eqn{c} and \eqn{c'} are called the direct and
  total effect, respectively.}

  \item{\emph{Parallel multiple mediator model}: The simple mediation model
  can be extended with multiple mediators \eqn{M_1, \dots, M_k} in the
  following way:
  \deqn{M_1 = i_1 + a_1 X + e_1,}
  \deqn{\vdots}{\dots}
  \deqn{M_k = i_k + a_k X + e_k,}
  \deqn{Y = i_{k+1} + b_1 M_1 + \dots + b_k M_k + c X + e_{k+1},}
  \deqn{Y = i_{k+2} + c' X + e_{k+2}.}
  The main parameters of interest are the individual indirect effects
  \eqn{a_1 b_1, \dots, a_k b_k}.}

  \item{\emph{Serial multiple mediator model}: It differs from the parallel
  multiple mediator model in that it allows the hypothesized mediators
  \eqn{M_1, \dots, M_k} to influence each other in a sequential manner.
  It is given by the equations
  \deqn{M_1 = i_1 + a_1 X + e_1,}
  \deqn{M_2 = i_1 + d_{21} M_1 + a_2 X + e_2,}
  \deqn{\vdots}{\dots}
  \deqn{M_k = i_k + d_{k1} M_1 + \dots +  d_{k,k-1} M_{k-1} + a_k X + e_k,}
  \deqn{Y = i_{k+1} + b_1 M_1 + \dots + b_k M_k + c X + e_{k+1},}
  \deqn{Y = i_{k+2} + c' X + e_{k+2}.}
  The serial multiple mediator model quickly grows in complexity with
  increasing number of mediators due to the combinatorial increase in
  indirect paths through the mediators.  It is therefore only implemented
  for two and three mediators to maintain a focus on easily interpretable
  models.  For two serial mediators, the three indirect effects
  \eqn{a_1 b_1}, \eqn{a_2 b_2}, and \eqn{a_1 d_{21} b_2} are the main
  parameters of interest.  For three serial mediators, there are already
  seven indirect effects: \eqn{a_1 b_1}, \eqn{a_2 b_2}, \eqn{a_3 b_3},
  \eqn{a_1 d_{21} b_2}, \eqn{a_1 d_{31} b_3}, \eqn{a_2 d_{32} b_3}, and
  \eqn{a_1 d_{21} d_{32} b_3}.}

  \item{\emph{Multiple independent variables to be mediated}: The simple
  mediation model can also be extended by allowing multiple independent
  variables \eqn{X_1, \dots, X_l} instead of multiple mediators.  It is
  defined by the equations
  \deqn{M = i_1 + a_1 X_1 + \dots + a_l X_l + e_1,}
  \deqn{Y = i_2 + b M + c_1 X_1 + \dots + c_l X_l + e_2,}
  \deqn{Y = i_3 + c_1' X_1 + \dots + c_l' X_l + e_3.}
  The indirect effects \eqn{a_1 b, \dots, a_l b} are the main parameters of
  interest.  Note that an important special case of this model occurs when a
  categorical independent variable is represented by a group of dummy
  variables.}

  \item{\emph{Control variables}: To isolate the effects of the independent
  variables of interest from other factors, control variables can be added
  to all regression equations of a mediation model.  Note that that there is
  no intrinsic difference between independent variables of interest and
  control variables in terms of the model or its estimation.  The difference
  is purely conceptual in nature: for the control variables, the estimates
  of the direct and indirect paths are not of particular interest to the
  researcher.  Control variables can therefore be specified separately from
  the independent variables of interest.  Only for the latter, results for
  the indirect effects are included in the output.}

  \item{\emph{More complex models}: Some of the models described above can
  be combined, for instance parallel and serial multiple mediator models
  support multiple independent variables of interest and control variables.}

}
}

\examples{
data("BSG2014")

## seed to be used for the random number generator
seed <- 20211117

## simple mediation
# set seed of the random number generator
set.seed(seed)
# The results in Alfons et al. (2022a) were obtained with an
# older version of the random number generator.  To reproduce
# those results, uncomment the two lines below.
# RNGversion("3.5.3")
# set.seed(20150601)
# perform mediation analysis
fit_simple <- fit_mediation(TeamCommitment ~
                              m(TaskConflict) +
                              ValueDiversity,
                            data = BSG2014)
boot_simple <- test_mediation(fit_simple)
summary(boot_simple)

\donttest{
## serial multiple mediators
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
fit_serial <- fit_mediation(TeamScore ~
                              serial_m(TaskConflict,
                                       TeamCommitment) +
                              ValueDiversity,
                            data = BSG2014)
boot_serial <- test_mediation(fit_serial)
summary(boot_serial)

## parallel multiple mediators and control variables
# set seed of the random number generator
set.seed(seed)
# perform mediation analysis
fit_parallel <- fit_mediation(TeamPerformance ~
                                parallel_m(ProceduralJustice,
                                           InteractionalJustice) +
                                SharedLeadership +
                                covariates(AgeDiversity,
                                           GenderDiversity),
                              data = BSG2014)
boot_parallel <- test_mediation(fit_parallel)
summary(boot_parallel)
}

}
\references{
Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022a) A Robust Bootstrap Test
for Mediation Analysis.  \emph{Organizational Research Methods},
\bold{25}(3), 591--617.  doi:10.1177/1094428121999096.

Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022b) Robust Mediation Analysis:
The \R Package \pkg{robmed}.  \emph{Journal of Statistical Software},
\bold{103}(13), 1--45.  doi:10.18637/jss.v103.i13.

Azzalini, A. and Arellano-Valle, R. B. (2013) Maximum Penalized Likelihood
Estimation for Skew-Normal and Skew-t Distributions.  \emph{Journal of
Statistical Planning and Inference}, \bold{143}(2), 419--433.
doi:10.1016/j.jspi.2012.06.022.

Yuan, Y. and MacKinnon, D.P. (2014) Robust Mediation Analysis Based on
Median Regression.  \emph{Psychological Methods}, \bold{19}(1), 1--20.
doi:10.1037/a0033820.

Zu, J. and Yuan, K.-H. (2010) Local Influence and Robust Procedures for
Mediation Analysis.  \emph{Multivariate Behavioral Research}, \bold{45}(1),
1--44.  doi:10.1080/00273170903504695.
}
\seealso{
\code{\link{test_mediation}()}

\code{\link[robustbase]{lmrob}()}, \code{\link[stats]{lm}()},
\code{\link{cov_Huber}()}, \code{\link{cov_ML}()}
}
\author{
Andreas Alfons
}
\keyword{multivariate}
