\name{posthoc.quade.test}
\alias{posthoc.quade.test}
\alias{posthoc.quade.test.default}
%\alias{posthoc.quade.conover.test.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise post-hoc Test for Multiple Comparisons of Rank Sums for
Unreplicated Blocked Data (Quade multiple comparison test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise comparisons using Quade's post-hoc test for
unreplicated blocked data. This test is usually conducted post-hoc after significant results of the omnibus test, \code{quade.test}.
}
\usage{
posthoc.quade.test(y, \dots)

\method{posthoc.quade.test}{default} (y, groups, blocks, dist =
 c("TDist", "Normal"), p.adjust.method = p.adjust.methods, \dots)
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
\item{dist}{
  the method for determining the p-value. The default distribution is \code{"TDist"}, else \code{"Normal"}.}
\item{p.adjust.method}{Method for adjusting p values
    (see \code{\link{p.adjust}}).}
\item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
A one-way ANOVA with repeated measures that is also referred to as ANOVA with unreplicated block design can also be conducted via the \code{quade.test}. The consequent post-hoc pairwise multiple comparison test according to Quade is conducted with this function.

If \emph{y} is a matrix, than the columns refer to the treatment and the rows indicate the block.

See \code{vignette("PMCMR")} for details.
}

\value{
A list with class "PMCMR"
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The two-sided p-value according to the selected distribution.}
  \item{statistic}{The estimated quantiles of the
    selected distribution.}
  \item{p.adjust.method}{The applied method for p-value adjustment.}
%%  \item{p.adjust.method}{Defaults to "none"}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
W. J. Conover (1999), \emph{Practical nonparametric Statistics},
3rd. Edition, Wiley.

N. A. Heckert and J. J. Filliben (2003). NIST Handbook 148: Dataplot Reference Manual, Volume 2: Let Subcommands and Library Functions. National Institute of Standards and Technology Handbook Series, June 2003.

D. Quade (1979), Using weighted rankings in the analysis of complete
blocks with additive block effects. \emph{Journal of the American
  Statistical Association}, 74, 680-683.
}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
%%\note{
%%  ~~further notes~~
%%This function does not test for ties.
%%}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{quade.test}},
\code{\link{Normal}},
\code{\link{TDist}}
\code{\link{p.adjust}}
}
\examples{
## Conover (1999, p. 375f):
## Numbers of five brands of a new hand lotion sold in seven stores
## during one week.
y <- matrix(c( 5,  4,  7, 10, 12,
               1,  3,  1,  0,  2,
              16, 12, 22, 22, 35,
               5,  4,  3,  5,  4,
              10,  9,  7, 13, 10,
              19, 18, 28, 37, 58,
              10,  7,  6,  8,  7),
            nrow = 7, byrow = TRUE,
            dimnames =
            list(Store = as.character(1:7),
                 Brand = LETTERS[1:5]))
y
quade.test(y)
posthoc.quade.test(y, dist="TDist", p.adj="none")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest }
\keyword{nonparametric}
