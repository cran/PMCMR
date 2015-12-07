% File get.pvalues.Rd
% Part of the R package PMCMR
% Copyright 2015 Thorsten Pohlert
% Distributed under GPL 3 or later

\name{get.pvalues}
\alias{get.pvalues}
\title{Prints PMCMR objects}
\usage{
get.pvalues(object, \dots)
}
\arguments{
  \item{object}{either an object of class \code{"PMCMR"}, usually, a result of a
    call to any of the posthoc-tests included in the package PMCMR. Or
    an object of class \code{"pairwise.htest"}, a result of a call to
    \code{\link{pairwise.prop.test}},
    \code{\link{pairwise.t.test}} or
    \code{\link{pairwise.wilcox.test}}.}
  
   \item{\dots}{further arguments, currently ignored.}
}
\description{
  Returns a vector of pvalues that includes the names of the pairwise
  groups (i.e. the null hypothesis). The output can be used by
  \code{\link[multcompView]{multcompLetters}} to find homogeneous groups.
}
%%\details{
%%
%%}
\value{
  \item{out}{a named vector with p-values}
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link[multcompView]{multcompLetters}},
\code{\link[xtable]{xtable}},
\code{\link{pairwise.prop.test}},
\code{\link{pairwise.t.test}},
\code{\link{pairwise.wilcox.test}}
}
\examples{
data(InsectSprays)
attach(InsectSprays)
out <- posthoc.kruskal.dunn.test(count ~ spray, p.adjust="bonf")
out.p <- get.pvalues(out)
out.p

### a barplot, significant level at p < 0.05
require(multcompView)
out.mcV <- multcompLetters(out.p, threshold=0.05)
Rij <- rank(count)
Rj.mean <- tapply(Rij, spray, mean)
ti <- paste(out$method, "\nP-adjustment method:", out$p.adjust.method)
xx <- barplot(Rj.mean, ylim=c(0, 1.2* max(Rj.mean)),
xlab="Spray", ylab="Mean rank", main=ti)
yy <- Rj.mean + 3
text(xx, yy, lab=out.mcV$Letters)

## table format
dat <- data.frame(Group = names(Rj.mean),
                  meanRj = Rj.mean,
                  M = out.mcV$Letters)
dat

## LaTeX table
require(xtable)
xtable(dat, caption=ti, digits=1)

detach(InsectSprays)
}
\keyword{ utilities }

