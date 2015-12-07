\name{posthoc.durbin.test}
\alias{posthoc.durbin.test}
\alias{posthoc.durbin.test.default}
%\alias{posthoc.durbin.test.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Posthoc Durbin test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Pairwise post-hoc test for multiple comparisons of rank sums according
to Durbin and Conover for a
two-way balanced incomplete block design (BIBD).
}
\usage{
posthoc.durbin.test(y, \dots)

\method{posthoc.durbin.test}{default} (y, groups, blocks,
p.adjust.method = p.adjust.methods, \dots)



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
  \item{p.adjust.method}{Method for adjusting p values
    (see \code{\link{p.adjust}}).}
\item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
 In the case of an two-way balanced incomplete block
design, the Durbin test, \code{durbin.test} can be employed. The H0 is rejected, if at least one group (treatment) is
significantly different. The pairwise multiple comparisons are conducted
with this function. The \code{posthoc.durbin.test} is equivalent to the
\code{posthoc.friedman.conover.test} in
the case of a two-way balanced complete block design.

If \emph{y} is a matrix, than the columns refer to the groups (treatment)
and the rows indicate the block.

The statistics refer to the student-t-distribution (\code{TDist}).

See \code{vignette("PMCMR")} for details.
}

\value{
A list with class \code{"PMCMR"}
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The two-sided p-value according to the student-t-distribution.}
  \item{statistic}{The estimated quantiles of the
    student-t-distribution.}
  \item{p.adjust.method}{The applied method for p-value adjustment.}
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
The function does not test, whether it is a true BIBD.

This function does not test for ties.
}

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{durbin.test}},
  \code{\link{friedman.test}},
  \code{\link{posthoc.friedman.nemenyi.test}},
  \code{\link{posthoc.friedman.conover.test}},
  \code{\link{TDist}}
  \code{\link{p.adjust}}
}
\examples{
## Example for an incomplete block design:
## Data from Conover (1999, p. 391).
y <- matrix(c(
2,NA,NA,NA,3, NA,  3,  3,  3, NA, NA, NA,  3, NA, NA,
  1,  2, NA, NA, NA,  1,  1, NA,  1,  1,
NA, NA, NA, NA,  2, NA,  2,  1, NA, NA, NA, NA,
  3, NA,  2,  1, NA, NA, NA, NA,  3, NA,  2,  2
), ncol=7, nrow=7, byrow=FALSE,
dimnames=list(1:7, LETTERS[1:7]))
y
durbin.test(y)
posthoc.durbin.test(y, p.adj="none")

## Example for a complete block design:
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
dimnames=list(1:6,LETTERS[1:6]))
print(y)
friedman.test(y)
durbin.test(y)
posthoc.durbin.test(y, p.adj="none")
posthoc.friedman.conover.test(y, p.adj="none")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ nonparametric}

