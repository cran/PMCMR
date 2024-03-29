% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.PMCMR.R
\name{get.pvalues}
\alias{get.pvalues}
\title{Get Pvalues from PMCMR Objects}
\usage{
get.pvalues(object, ...)
}
\arguments{
\item{object}{either an object of class \code{"PMCMR"}, usually, a result of a
call to any of the posthoc-tests included in the package PMCMR. Or
an object of class \code{"pairwise.htest"}, a result of a call to
\code{\link[stats]{pairwise.prop.test}}, \code{\link[stats]{pairwise.t.test}} or
\code{\link[stats]{pairwise.wilcox.test}}.}

\item{\dots}{further arguments, currently ignored.}
}
\value{
a named vector with p-values
}
\description{
Returns a vector of pvalues that includes the names of the pairwise
groups (i.e. the null hypothesis). The output can be used by
\code{\link[multcompView]{multcompLetters}} to find homogeneous groups.
}
\keyword{utilities}
