% File print.PMCMR.Rd
% Part of the R package PMCMR
% Copyright 2015 Thorsten Pohlert
% Distributed under GPL 3 or later

\name{print.PMCMR}
\alias{print.PMCMR}
\title{Prints PMCMR objects}
\usage{
\method{print}{PMCMR}(x, \dots)
}
\arguments{
  \item{x}{an object of class \code{"PMCMR"}, usually, a result of a
    call to any of the posthoc-tests included in the package PMCMR.}
   \item{\dots}{further arguments, currently ignored.}
}
\description{
  \code{print} method for class \code{"PMCMR"}.
}
%%\details{
%%
%%}
\value{
  The function \code{print.PMCMR} returns the lower triangle of 
  the (adjusted) p-values from any of the posthoc tests included in the
  package PMCMR.
}
\examples{
out <- posthoc.kruskal.conover.test(count ~ spray,
data = InsectSprays, p.adjust="bonf")
print(out)
}
\keyword{print}
