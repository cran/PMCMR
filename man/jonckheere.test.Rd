\name{jonckheere.test}
\alias{jonckheere.test}
\alias{jonckheere.test.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Jonckheere-Terpstrata test for testing k independent samples
      against ordered alternatives
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Applies the Jonckheere-Terpstrata test. 
}
\usage{
jonckheere.test (x, \dots)

\method{jonckheere.test}{default} (x, g, alternative = c("monotonic", "increasing", "decreasing"), \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{alternative}{The alternative hypothesis. Defaults to \code{monotonic}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The global H0, the k medians (med) are
equal, is tested against the HA: med_1 <= med_2 <= ... <= med_k (increasing
trend; one-sided test). It can also be tested for decreasing trend
(one-sided test) and monotonic trend (two sided test). 

See the vignette for details.
}

\value{
A list with class \code{"PMCMR"}
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method}{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The p-value of the standard normal distribution.}
  \item{statistic}{The estimated quantile of the standard normal distribution.}
  \item{alternative}{The alternative hypothesis.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~ 
A. R. Jonckheere (1954). A distribution-free k-sample test against
ordered alternatives. \emph{Biometrica}, 41, 133-145.

L. Sachs (1997), \emph{Angewandte Statistik}. Berlin: Springer.
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
%%\note{
%%  ~~further notes~~
%%}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{kruskal.test}}
}
\examples{
##
## Example from Sachs (1997, p. 402)
x <- c(106, 114, 116, 127, 145, 110, 125,
       143, 148, 151, 136, 139, 149, 160,
       174)
g <- as.factor(c(rep(1,5), rep(2,5), rep(3,5)))
levels(g) <- c("A", "B", "C")
jonckheere.test(x , g, "increasing")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{nonparametric}

