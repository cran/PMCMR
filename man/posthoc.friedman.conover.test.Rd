\name{posthoc.friedman.conover.test}
\alias{posthoc.friedman.conover.test}
\alias{posthoc.friedman.conover.test.default}
\alias{posthoc.friedman.conover.test.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise post-hoc Test for Multiple Comparisons of Rank Sums for Unreplicated Blocked Data (Conover-Test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise comparisons using Conover post-hoc test for
unreplicated blocked data. This test is usually conducted post-hoc after
significant results of the \code{friedman.test}. The statistics refer to
the student-t-distribution (\code{TDist}).
}
\usage{
posthoc.friedman.conover.test(y, \dots)

\method{posthoc.friedman.conover.test}{default} (y, groups, blocks,
p.adjust.method = p.adjust.methods, \dots)

\method{posthoc.friedman.conover.test}{formula} (formula, data, subset,
na.action, p.adjust.method = p.adjust.methods,\ldots)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{x} here~~
   either a numeric vector of data values, or a data matrix. 
}
  \item{groups}{
%%     ~~Describe \code{g} here~~
  a vector giving the group for the corresponding elements of \code{y} if this is a vector; ignored if \code{y} is a matrix. If not a factor object, it is coerced to one.
 }
\item{blocks}{ a vector giving the block for the corresponding elements
  of \code{y} if this is a vector; ignored if \code{y} is a matrix. If
  not a factor object, it is coerced to one.}
\item{formula}{a formula of the form \code{a ~ b | c}, where \code{a},
    \code{b} and \code{c} give the data values and corresponding groups
    and blocks, respectively.}
\item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
\item{subset}{an optional vector specifying a subset of observations
  to be used.}
\item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{p.adjust.method}{Method for adjusting p values
    (see \code{\link{p.adjust}}).}
\item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
A one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the \code{friedman.test}. The consequent post-hoc pairwise multiple comparison test according to Conover is conducted with this function.

If \emph{y} is a matrix, than the colums refer to the treatment and the rows indicate the block.

See \code{vignette("PMCMR")} for details.
}

\value{
Object of class "pairwise.htest"
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The two-sided p-value according to the student-t-distribution.}
  \item{statistic}{The estimated quantiles of the
    student-t-distribution.}
  \item{p.adjust.method}{The applied method for p-value adjustment.}
%%  \item{p.adjust.method}{Defaults to "none"}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
W. J. Conover and R. L. Iman (1979), \emph{On multiple-comparisons
  procedures}, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.

W. J. Conover (1999), \emph{Practical nonparametric Statistics}, 3rd. Edition, Wiley.
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
\note{
%%  ~~further notes~~
This function does not test for ties.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{friedman.test}},
\code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{TDist}}
\code{\link{p.adjust}}
}
\examples{
##
## Sachs, 1997, p. 675
## Six persons (block) received six different diuretics
## (A to F, treatment).
## The responses are the Na-concentration (mval)
## in the urine measured 2 hours after each treatment.
##
y <- matrix(c(
3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45, 
26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
26.65),nrow=6, ncol=6, 
dimnames=list(1:6,c("A","B","C","D","E","F")))
print(y)
friedman.test(y)
posthoc.friedman.conover.test(y=y, p.adjust="none")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~htest }
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
