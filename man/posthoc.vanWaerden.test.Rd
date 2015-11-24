\name{posthoc.vanWaerden.test}
\alias{posthoc.vanWaerden.test}
\alias{posthoc.vanWaerden.test.default}
\alias{posthoc.vanWaerden.test.formula}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise Test for Multiple Comparisons of normal scores (van der Waerden test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise multiple comparisons between group levels according
to van der Waerden.
}
\usage{
posthoc.vanWaerden.test(x, \dots)

\method{posthoc.vanWaerden.test}{default}( x, g, p.adjust.method =
p.adjust.methods, \dots)

\method{posthoc.vanWaerden.test}{formula}(formula, data, subset,
na.action, p.adjust.method = p.adjust.methods, \dots)

}
%- maybe also 'usage' for other objects documented here.
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
  \item{p.adjust.method}{Method for adjusting p values (see \code{\link{p.adjust}}).}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For one-factorial designs with samples that do not meet the assumptions
for one-way-ANOVA and subsequent post-hoc tests, the van der Waerden
test  \code{\link{vanWaerden.test}} using normal scores can be
employed. Provided that significant differences were detected by this
global test, one may be interested in applying post-hoc tests according
to van der Waerden for pairwise multiple comparisons of the group levels.

First, the data are ranked according to Kruskal-Wallis. Second, the
ranks are transformed to normal scores. The group means of normal scores
and the total variance is used to calculate quantiles of the
student-t-distribution and consequent p-values.

See \code{vignette("PMCMR")} for details.
}

\value{
Object of class \code{"pairwise.htest"}
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The two-sided p-value of the student-t-distribution.}
  \item{statistic}{The estimated quantile of the student-t-distribution.}
  \item{p.adjust.method}{The applied method for p-value adjustment.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
W. J. Conover and R. L. Iman (1979), \emph{On multiple-comparisons
  procedures}, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
\note{
%%  ~~further notes~~
 There is no tie correction applied in this function.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{kruskal.test}},
\code{\link{vanWaerden.test}},
\code{\link{posthoc.kruskal.nemenyi.test}},
\code{\link{posthoc.kruskal.dunn.test}},
\code{\link{TDist}},
\code{\link{p.adjust}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~htest }
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
