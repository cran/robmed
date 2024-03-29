% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.R
\name{coef.test_mediation}
\alias{coef.test_mediation}
\alias{coef.boot_test_mediation}
\alias{coef.fit_mediation}
\title{Coefficients in (robust) mediation analysis}
\usage{
\method{coef}{test_mediation}(object, parm = NULL, ...)

\method{coef}{boot_test_mediation}(object, parm = NULL, type = c("boot", "data"), ...)

\method{coef}{fit_mediation}(object, parm = NULL, ...)
}
\arguments{
\item{object}{an object inheriting from class \code{"\link{test_mediation}"}
containing results from (robust) mediation analysis, or an object inheriting
from class \code{"\link{fit_mediation}"} containing a (robust) mediation
model fit.}

\item{parm}{an integer, character or logical vector specifying the
paths for which to extract coefficients, or \code{NULL} to extract all
coefficients.  In case of a character vector, possible values are
\code{"a"}, \code{"b"}, \code{"d"} (only serial multiple mediator
models), \code{"total"}, \code{"direct"}, and \code{"indirect"}.}

\item{\dots}{additional arguments are currently ignored.}

\item{type}{a character string specifying whether to extract the means
of the bootstrap distribution (\code{"boot"}; the default), or the
coefficient estimates based on the original data set (\code{"data"}).}
}
\value{
A numeric vector containing the requested coefficients.
}
\description{
Extract coefficients from models computed in (robust) mediation analysis.
}
\examples{
data("BSG2014")

# fit robust mediation model and extract coefficients
fit <- fit_mediation(BSG2014,
                     x = "ValueDiversity",
                     y = "TeamCommitment",
                     m = "TaskConflict")
coef(fit)

# run fast-and-robust bootstrap test and extract coefficients
boot <- test_mediation(fit)
coef(boot, type = "data")  # from original sample
coef(boot, type = "boot")  # means of bootstrap replicates

}
\seealso{
\code{\link{test_mediation}()}, \code{\link{fit_mediation}()},
\code{\link[=confint.test_mediation]{confint}()}, \code{\link{p_value}()}
}
\author{
Andreas Alfons
}
\keyword{utilities}
