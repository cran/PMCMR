\name{posthoc.kruskal.nemenyi.test}
\alias{posthoc.kruskal.nemenyi.test}
\alias{posthoc.kruskal.nemenyi.test.default}
\alias{posthoc.kruskal.nemenyi.test.formula}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise Test for Multiple Comparisons of Mean Rank Sums (Nemenyi-Tests)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise multiple comparisons between group levels. These tests are sometimes referred to as Nemenyi-tests for multiple comparisons of (mean) rank sums of independent samples.
}
\usage{
posthoc.kruskal.nemenyi.test(x, \dots)

\method{posthoc.kruskal.nemenyi.test}{default}( x, g, dist =
c("Tukey", "Chisquare"), \dots)

\method{posthoc.kruskal.nemenyi.test}{formula}(formula, data, subset,
na.action, dist =
c("Tukey", "Chisquare"), \dots)

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
  \item{\dots}{further arguments to be passed to or from methods.}
  \item{dist}{
  the method for determining the p-value. The default distribution is \code{"Tukey"}, else \code{"Chisq"}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For one-factorial designs with samples that do not meet the assumptions for one-way-ANOVA and subsequent post-hoc tests, the Kruskal-Wallis-Test  \code{\link{kruskal.test}} can be employed that is also referred to as the Kruskal–Wallis one-way analysis of variance by ranks. Provided that significant differences were detected by this global test, one may be interested in applying post-hoc tests according to Nemenyi for pairwise multiple comparisons of the ranked data.

See \code{vignette("PMCMR")} for details.
}

\value{
A list with class \code{"PMCMR"}
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The p-value according to the studentized range distribution.}
  \item{statistic}{The estimated upper quantile of the studentized range distribution. (or quantile of Chisq distribution)}
  \item{p.adjust.method}{Defaults to "none"}


%% ...
}
\references{
%% ~put references to the literature/web site here ~

Lothar Sachs (1997), \emph{Angewandte Statistik}. Berlin: Springer. Pages: 395-397, 662-664.
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
\note{
%%  ~~further notes~~
 Only for \emph{method = "Chisq"} a tie correction is employed.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{kruskal.test}},
\code{\link{friedman.test}},
\code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{Tukey}},
\code{\link{Chisquare}} 
}
\examples{
##
require(stats) 
data(InsectSprays)
attach(InsectSprays)
kruskal.test(count, spray)
posthoc.kruskal.nemenyi.test(count, spray)
posthoc.kruskal.nemenyi.test(count, spray, "Chisq")
detach(InsectSprays)
rm(InsectSprays)
## Formula Interface
posthoc.kruskal.nemenyi.test(count ~ spray, data = InsectSprays, dist="Tukey")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{nonparametric}

