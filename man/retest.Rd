% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retest.R
\name{retest}
\alias{retest}
\alias{retest.boot_test_mediation}
\alias{retest.sobel_test_mediation}
\title{Retest for mediation}
\usage{
retest(object, ...)

\method{retest}{boot_test_mediation}(object, alternative, level, type, contrast, ...)

\method{retest}{sobel_test_mediation}(object, alternative, order, ...)
}
\arguments{
\item{object}{an object inheriting from class
\code{"\link{test_mediation}"} containing results from (robust) mediation
analysis.}

\item{\dots}{additional arguments to be passed down to methods.}

\item{alternative}{a character string specifying the alternative hypothesis
in the test for the indirect effect.  Possible values are \code{"twosided"},
\code{"less"} or \code{"greater"}.}

\item{level}{numeric; the confidence level of the confidence interval in
the bootstrap test.}

\item{type}{a character string specifying the type of confidence interval
to be computed in the bootstrap test.  Possible values are \code{"bca"} for
the bias-corrected and accelerated (BCa) bootstrap, or \code{"perc"} for the
percentile bootstrap.}

\item{contrast}{a logical indicating whether to compute pairwise contrasts
of the indirect effects.  This can also be a character string, with
\code{"estimates"} for computing the pairwise differences of the indirect
effects (such that it is tested whether two indirect effects are equal),
and \code{"absolute"} for computing the pairwise differences of the absolute
values of the indirect effects (such that it is tested whether two indirect
effects are equal in magnitude).  This is only relevant for models with
multiple indirect effects, which are currently only implemented for
bootstrap tests and estimation via regressions.}

\item{order}{a character string specifying the order of approximation of
the standard error in Sobel's test.  Possible values are \code{"first"}
for a first-order approximation, and \code{"second"} for a second-order
approximation.}
}
\value{
An object of the same class as \code{object} with updated test
results (see \code{\link{test_mediation}()}).
}
\description{
Re-perform a test for the indirect effect(s) based on results from (robust)
mediation analysis.  This function is purely available for computational
convenience if the analysis was accidentally run with the wrong parameter
settings, as it avoids having to re-run the bootstrap procedure.  It must
not be abused for \eqn{p}{p}-hacking.
}
\note{
From version 0.9.0 onwards, the behavior of this function changed.
For arguments that are not supplied, the corresponding values of
\code{object} are now used as defaults.
}
\examples{
data("BSG2014")

# run fast-and-robust bootstrap test
boot <- test_mediation(BSG2014,
                       x = "ValueDiversity",
                       y = "TeamCommitment",
                       m = "TaskConflict")
summary(boot)

# now compute 97.5\% confidence interval
retest(boot, level = 0.975)

}
\seealso{
\code{\link{test_mediation}()}
}
\author{
Andreas Alfons
}
\keyword{multivariate}
