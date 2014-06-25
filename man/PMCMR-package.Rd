\name{PCMCR-package}
\alias{PCMCR-package}
\alias{PCMCR}
\docType{package}
\title{
Calculate pairwise multiple comparisons between mean rank sums.
}
\description{
The Kruskal–Wallis one-way analysis of variance by ranks can be employed, if the data do not meet the assumptions for one-way ANOVA. Provided that significant differences were detected by the Kruskal-Wallis test, one may be interested in applying post-hoc tests according to Nemenyi for pairwise multiple comparisons of the ranked data. Similarly, one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the Friedman test (\code{friedman.test}). The consequent post-hoc pairwise multiple comparison test according to Nemenyi is also provided in this package.
}
\details{
\tabular{ll}{
Package: \tab PMCMR \cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-24\cr
License: \tab >= GPL-3\cr
}

See the vignette for details.

}
\author{
Thorsten Pohlert

Maintainer: <thorsten.pohlert@gmx.de>
}
\references{
Janez Demsar (2006), Statistical comparisons of classifiers over multiple data sets, \emph{Journal of Machine Learning Research}, 7, 1-30. 

Lothar Sachs (1997), \emph{Angewandte Statistik}. Berlin: Springer. Pages: 668-675. 
}
\keyword{ htest }
\seealso{
  \code{\link{posthoc.kruskal.nemenyi.test}},
  \code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{kruskal.test}},
\code{\link{friedman.test}},
\code{\link{Tukey}},
\code{\link{Chisquare}}
}
