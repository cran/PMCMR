\name{posthoc.friedman.nemenyi.test}
\alias{posthoc.friedman.nemenyi.test}
\alias{posthoc.friedman.nemenyi.test.default}
\alias{posthoc.friedman.nemenyi.test.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise post-hoc Test for Multiple Comparisons of Mean Rank Sums for Unreplicated Blocked Data (Nemenyi-Test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise comparisons using Nemenyi post-hoc test for
unreplicated blocked data. This test is usually conducted post-hoc after
significant results of the \code{friedman.test}. The statistics refer to
upper quantiles of the studentized range distribution (\code{Tukey}).


}
\usage{
posthoc.friedman.nemenyi.test(y, \dots)

\method{posthoc.friedman.nemenyi.test}{default} (y, groups, blocks,
\dots)

\method{posthoc.friedman.nemenyi.test}{formula} (formula, data, subset,
na.action, \ldots)
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
\item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
A one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the \code{friedman.test}. The consequent post-hoc pairwise multiple comparison test according to Nemenyi is conducted with this function.

If \emph{y} is a matrix, than the columns refer to the treatment and the rows indicate the block.

See \code{vignette("PMCMR")} for details.

Let \eqn{R_j} and \eqn{n_j} denote the sum of Friedman-ranks and the sample
size of the \eqn{j}-th group, respectively, then a difference between
two groups is significant on the level of \eqn{\alpha}, if the following
inequality is met:

\deqn{\left| \frac{R_i}{n_i} - \frac{R_j}{n_j} \right| >
  \frac{q_{\infty;k;\alpha}}{\sqrt{2}} \sqrt{\frac{k \left( k + 1
      \right)}{6  n}}}{%
  |R_i / n_i - R_j / n_j| > q(\infty; k; \alpha) / 2^0.5 *
  (k (k + 1) / (6 n))^0.5}

with \eqn{k} the number of groups (or treatments) and \eqn{n} the total
  number of data.
}

\value{
A list with class "PMCMR"
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

P. Nemenyi (1963) \emph{Distribution-free Multiple Comparisons}. Ph.D. thesis, Princeton University.

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
\keyword{ htest }
\keyword{ nonparametric}
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
