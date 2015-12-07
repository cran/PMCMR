\name{posthoc.kruskal.conover.test}
\alias{posthoc.kruskal.conover.test}
\alias{posthoc.kruskal.conover.test.default}
\alias{posthoc.kruskal.conover.test.formula}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise Test for Multiple Comparisons of Mean Rank Sums (Conover's-Test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise multiple comparisons between group levels according to Conover.
}
\usage{
posthoc.kruskal.conover.test(x, \dots)

\method{posthoc.kruskal.conover.test}{default}( x, g, p.adjust.method =
p.adjust.methods, \dots)

\method{posthoc.kruskal.conover.test}{formula}(formula, data, subset,
na.action, p.adjust.method = p.adjust.methods, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%  \item{x}{
%%     ~~Describe \code{x} here~~
%%   response vector. 
%%}
%%  \item{g}{
%%     ~~Describe \code{g} here~~
%%  grouping vector or factor.
%}
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
For one-factorial designs with samples that do not meet the assumptions for one-way-ANOVA and subsequent post-hoc tests, the Kruskal-Wallis-Test  \code{\link{kruskal.test}} can be employed that is also referred to as the Kruskal–Wallis one-way analysis of variance by ranks. Provided that significant differences were detected by this global test, one may be interested in applying post-hoc tests according to Conover for pairwise multiple comparisons of the ranked data.

See \code{vignette("PMCMR")} for details.
}

\value{
A list with class \code{"PMCMR"}
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
 A tie correction will be employed according to Conover (1979).
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{kruskal.test}},
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
kruskal.test(count, spray)
posthoc.kruskal.conover.test(count, spray, "bonferroni")
detach(InsectSprays)
rm(InsectSprays)
## Formula Interface
posthoc.kruskal.conover.test(count ~ spray, data = InsectSprays, p.adjust="bonf")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ nonparametric}
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
