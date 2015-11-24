\name{dunn.test.control}
\alias{dunn.test.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Pairwise Test for Multiple Comparisons of Mean Rank Sums with one
control (Dunn's-Test)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate pairwise multiple comparisons with one control according to Dunn.
}
\usage{
dunn.test.control (x, g, p.adjust.method = p.adjust.methods, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{p.adjust.method}{Method for adjusting p values (see \code{\link{p.adjust}}).}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For one-factorial designs with samples that do not meet the assumptions
for one-way-ANOVA and subsequent post-hoc tests, the Kruskal-Wallis-Test
\code{\link{kruskal.test}} can be employed that is also referred to as
the Kruskal–Wallis one-way analysis of variance by ranks. Provided that
significant differences were detected by this global test, one may be
interested in applying post-hoc tests according to Dunn for pairwise
multiple comparisons with one control.

See the vignette for details.
}

\value{
Object of class \code{"pairwise.htest"}
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{method }{The applied method.}
  \item{data.name}{The name of the data.}
  \item{p.value}{The two-sided p-value of the standard normal distribution.}
  \item{statistic}{The estimated quantile of the standard normal distribution.}
  \item{p.adjust.method}{The applied method for p-value adjustment.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~

O.J. Dunn (1964). Multiple comparisons using rank sums. \emph{Technometrics}, 6, 241-252. 

S. A. Glantz (2012), \emph{Primer of Biostatistics}. New York: McGraw
Hill.

S. Siegel, N. J. Castellan Jr. (1988), \emph{Nonparametric Statistics
  for The Behavioral Sciences}. New York: McGraw-Hill.

}
\author{
%%  ~~who you are~~
Thorsten Pohlert
}
\note{
%%  ~~further notes~~
A tie correction will be employed according to Glantz (2012).
As it is the case for multiple testing with one control using
\code{aov}, the user must make sure that the control appears as the first
level in the group vector. There is no formula method enclosed.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{kruskal.test}},
\code{\link{friedman.test}},
\code{\link{posthoc.friedman.nemenyi.test}},
\code{\link{pnorm}},
\code{\link{p.adjust}}
}
\examples{
##
require(stats) 
data(PlantGrowth)
attach(PlantGrowth)
kruskal.test(weight, group)
dunn.test.control(weight,group, "bonferroni")
detach(PlantGrowth)
rm(PlantGrowth)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~htest }
%\keyword{ ~htest }% __ONLY ONE__ keyword per line
