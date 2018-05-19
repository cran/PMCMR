\name{PCMCR-package}
\alias{PCMCR-package}
\alias{PCMCR}
\docType{package}
\title{
Calculate pairwise multiple comparisons between mean rank sums
}
\description{
   Note, that the 'PMCMR' package is superseded
   by the novel 'PMCMRplus' package. The 'PMCMRplus'
   package contains all functions from 
   'PMCMR' and many more parametric and non-parametric
   multiple comparison procedures, one-factorial trend
   tests, as well as improved method functions, 
   such as print, summary and plot. 
   The 'PMCMR' package is no longer maintained,
   but kept for compatibility of reverse depending
   packages for some time.
}
\details{
  \tabular{ll}{
    Package: \tab PMCMR \cr
    Type: \tab Package\cr
    Version: \tab 4.3 \cr
    Date: \tab 2018-05-19\cr
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

S. A. Glantz (2012), \emph{Primer of Biostatistics}, 7th edition. New
York: McGraw Hill.

N. A. Heckert and J. J. Filliben (2003). NIST Handbook 148: Dataplot Reference Manual, Volume 2: Let Subcommands and Library Functions. National Institute of Standards and Technology Handbook Series, June 2003.

A. R. Jonckheere (1954). A distribution-free k-sample test against
ordered alternatives. \emph{Biometrica}, 41, 133-145.

P. Nemenyi (1963) \emph{Distribution-free Multiple Comparisons}. Ph.D. thesis, Princeton University.

D. Quade (1979), Using weighted rankings in the analysis of complete blocks with additive block effects. \emph{Journal of the American Statistical Association}, 74, 680-683.

Lothar Sachs (1997), \emph{Angewandte Statistik}. Berlin:
Springer. Pages: 668-675.
}
\seealso{
\code{\link{posthoc.kruskal.nemenyi.test}},
\code{\link{posthoc.kruskal.dunn.test}},
\code{\link{posthoc.kruskal.conover.test}},
\code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{posthoc.friedman.conover.test}},
\code{\link{posthoc.quade.test}},
\code{\link{dunn.test.control}},
\code{\link{jonckheere.test}},
\code{\link{durbin.test}},
\code{\link{posthoc.durbin.test}},
\code{\link{kruskal.test}},
\code{\link{friedman.test}},
\code{\link{Tukey}},
\code{\link{TDist}},
\code{\link{Chisquare}},
\code{\link{Normal}},
\code{\link{p.adjust}},
\code{\link{print.PMCMR}},
\code{\link{summary.PMCMR}},
\code{\link{get.pvalues}},
\code{\link[multcompView]{multcompLetters}}
}
\keyword{htest}
\keyword{nonparametric}
\keyword{package}
