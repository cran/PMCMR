% File summary.PMCMR.Rd
% Part of the R package PMCMR
% Copyright 2015 Thorsten Pohlert
% Distributed under GPL 3 or later

\name{summary.PMCMR}
\alias{summary.PMCMR}
\title{Summarizing PMCMR objects}
\usage{
\method{summary}{PMCMR}(object, \dots)
}
\arguments{
  \item{object}{an object of class \code{"PMCMR"}, usually, a result of a
    call to any of the posthoc-tests included in the package PMCMR.}
  \item{\dots}{further arguments, currently ignored.}
}
\description{
  \code{summary} method for class \code{"PMCMR"}.
}
%%\details{%
%%
%%}
\value{
  The function \code{summary.PMCMR} computes and returns a list of the
  pairwise comparisons including the H0, the corresponding statistic and
  the (adjusted) p-value.
}
\examples{
out <- posthoc.kruskal.conover.test(count ~ spray,
data = InsectSprays, p.adjust="bonf")
summary(out)
}
\keyword{methods}
\keyword{print}
