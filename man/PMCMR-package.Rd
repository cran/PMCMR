\name{PCMCR-package}
\alias{PCMCR-package}
\alias{PCMCR}
\docType{package}
\title{
Calculate pairwise multiple comparisons between mean rank sums
}
\description{
The Kruskal and Wallis one-way analysis of variance by ranks can be
employed, if the data do not meet the assumptions for one-way
ANOVA. Provided that significant differences were detected by the
Kruskal-Wallis-Test, one may be interested in applying post-hoc tests
for pairwise multiple comparisons of the ranked data (Nemenyi's test,
Dunn's test, Conover's test, van der Waerden's test). Similarly, one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the Friedman-Test. The consequent post-hoc pairwise multiple comparison tests according to Nemenyi and Conover are also provided in this package.
}
\details{
\tabular{ll}{
Package: \tab PMCMR \cr
Type: \tab Package\cr
Version: \tab 3.0 \cr
Date: \tab 2015-11-19\cr
License: \tab >= GPL-3\cr
}

See the vignette for details.

}
\author{
Thorsten Pohlert

Maintainer: <thorsten.pohlert@gmx.de>
}
\references{
W. J. Conover and R. L. Iman (1979), \emph{On multiple-comparisons
  procedures}, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.

W. J. Conover (1999), \emph{Practical nonparametric Statistics}, 3rd. Edition, Wiley.

Janez Demsar (2006), Statistical comparisons of classifiers over multiple data sets, \emph{Journal of Machine Learning Research}, 7, 1-30.

O.J. Dunn (1964). Multiple comparisons using rank sums. \emph{Technometrics}, 6, 241-252. 

S. A. Glantz (2012), \emph{Primer of Biostatistics}, 7th edition. New York: McGraw Hill.

P. Nemenyi (1963) \emph{Distribution-free Multiple Comparisons}. Ph.D. thesis, Princeton University.

Lothar Sachs (1997), \emph{Angewandte Statistik}. Berlin: Springer. Pages: 668-675.
}
\keyword{ htest }
\seealso{
\code{\link{posthoc.kruskal.nemenyi.test}},
\code{\link{posthoc.kruskal.dunn.test}},
\code{\link{posthoc.kruskal.conover.test}},
\code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{posthoc.friedman.conover.test}},
\code{\link{dunn.test.control}},
\code{\link{kruskal.test}},
\code{\link{friedman.test}},
\code{\link{Tukey}},
\code{\link{TDist}},
\code{\link{Chisquare}},
\code{\link{Normal}},
\code{\link{p.adjust}}
}

