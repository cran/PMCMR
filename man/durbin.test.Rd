\name{durbin.test}
\alias{durbin.test}
\alias{durbin.test.default}
\alias{durbin.test.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Durbin test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The omnibus test according to Durbin tests whether k groups (or
treatments) in a two-way balanced incomplete block design (BIBD) have identical effects.
}
\usage{
durbin.test(y, \dots)

\method{durbin.test}{default} (y, groups, blocks,
\dots)

\method{durbin.test}{formula} (formula, data, subset,
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
The \code{friedman.test} can be used to test k groups (treatments) for
identical effects in a two-way balanced complete
block design. In the case of an two-way balanced incomplete block
design, the Durbin test 
can be employed. The H0 is rejected, if at least one group (treatment) is
significantly different. The Durbin test is equivalent to the Friedman test in
the case of a two-way balanced complete block design.

If \emph{y} is a matrix, than the columns refer to the groups (treatment) and the rows indicate the block.

See \code{vignette("PMCMR")} for details.
}

\value{
A list with class "PMCMR":
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The p-value according to the studentized range distribution.}
  \item{statistic}{The estimated upper quantile of the studentized range distribution.}
  \item{p.adjust.method}{Defaults to "none"}
}
\references{
%% ~put references to the literature/web site here ~
W. J. Conover (1999), \emph{Practical nonparametric Statistics},
3rd. Edition, Wiley.

N. A. Heckert and J. J. Filliben (2003). NIST Handbook 148: Dataplot Reference Manual, Volume 2: Let Subcommands and Library Functions. National Institute of Standards and Technology Handbook Series, June 2003.
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
\code{\link{friedman.test}},
\code{\link{posthoc.durbin.test}}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ nonparametric}

