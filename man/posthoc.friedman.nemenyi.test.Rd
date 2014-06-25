\name{posthoc.friedman.nemenyi.test}
\alias{posthoc.friedman.nemenyi.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise post-hoc Test for Multiple Comparisons of Mean Rank Sums for Unreplicated Blocked Data (Nemenyi-Test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise comparisons using Nemenyi post-hoc test for unreplicated blocked data. This test is usually conducted post-hoc after significant results of the \code{friedman.test}. The statistics refer to upper quantiles of the studentized range distribution (\code{Tukey}).
}
\usage{
posthoc.friedman.nemenyi.test(y, groups, blocks)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{x} here~~
   either a numeric vector of data values, or a data matrix. 
}
  \item{groups}{
%%     ~~Describe \code{g} here~~
  a vector giving the group for the corresponding elements of \emph{y} if this is a vector; ignored if \emph{y} is a matrix. If not a factor object, it is coerced to one.
 }
\item{blocks}{
 a vector giving the block for the corresponding elements of \emph{y} if this is a vector; ignored if \emph{y} is a matrix. If not a factor object, it is coerced to one.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
A one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the \code{friedman.test}. The consequent post-hoc pairwise multiple comparison test according to Nemenyi is conducted with this function.

If \emph{y} is a matrix, than the colums refer to the treatment and the rows indicate the block.

See the vignette for details.

}

\value{
Object of class "pairwise.htest"
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The p-value according to the studentized range distribution.}
  \item{statistic}{The estimated upper quantile of the studentized range distribution.}
  \item{p.adjust.method}{Defaults to "none"}


%% ...
}
\references{
%% ~put references to the literature/web site here ~
Janez Demsar (2006), Statistical comparisons of classifiers over multiple data sets, \emph{Journal of Machine Learning Research}, 7, 1-30. 

Lothar Sachs (1997), \emph{Angewandte Statistik}. Berlin: Springer. Pages: 668-675. 
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
\code{\link{kruskal.test}},
\code{\link{posthoc.kruskal.nemenyi.test}},
\code{\link{Tukey}} 
}
\examples{
##
## Sachs, 1997, p. 675
## Six persons (block) received six different diuretics (A to F, treatment).
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
posthoc.friedman.nemenyi.test(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~htest }
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
