\name{vanWaerden.test}
\alias{vanWaerden.test}
\alias{vanWaerden.test.default}
\alias{vanWaerden.test.formula}
\title{van der Waerden's z-score test}
\description{
  Performs a van der Waerden's normal scores test.
}
\usage{
vanWaerden.test(x, \dots)

\method{vanWaerden.test}{default}(x, g, \dots)

\method{vanWaerden.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{formula}{a formula of the form \code{response ~ group} where
    \code{response} gives the data values and \code{group} a vector or
    factor of the corresponding groups.} 
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{vanWaerden.test} performs a van der Waerden test of the
  null that the location parameters of the distribution of \code{x}
  are the same in each group (sample).  The alternative is that they
  differ in at least one.

  See \code{vignette("PMCMR")} for details.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the van der Waerden chi-squared statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"Van der Waerden normal scores test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
\note{
  A tie correction is not applied in this function.
}
\references{
 W. J. Conover and R. L. Iman (1979), \emph{On multiple-comparisons
  procedures}, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.
}
\seealso{
\code{\link{kruskal.test}}
}
\examples{
##
require(stats) 
data(InsectSprays)
attach(InsectSprays)
vanWaerden.test(count, spray)
posthoc.vanWaerden.test(count, spray, "bonferroni")
detach(InsectSprays)
rm(InsectSprays)
}
\keyword{htest}
