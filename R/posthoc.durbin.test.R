#  posthoc.durbin.test.R
#  Part of the R package PMCMR
#
#  Copyright (C) 2015, 2016 Thorsten Pohlert
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

#' @name PMCMR-deprecated
#' @aliases posthoc.durbin.test
#' @title Deprecated Functions in Package \pkg{PMCMR}
#' @description
#' These functions are provided for reverse-dependencies
#' issues of other R-packages. They should no longer be used,
#' as actively maintained functions can be found in the package
#' \pkg{PMCMRplus}.
#' The functions may be defunct as soon as the next release.
#'
#' @param y either a numeric vector of data values,
#' or a data matrix.
#' @param groups a vector giving the group for
#' the corresponding elements of \code{y} if this is a vector;
#' ignored if \code{y} is a matrix.
#' If not a factor object, it is coerced to one.
#' @param blocks a vector giving the block for the
#' corresponding elements of \code{y} if this is a vector;
#' ignored if \code{y} is a matrix. If not a factor object,
#' it is coerced to one.
#' @param p.adjust.method Method for adjusting p values
#' (see \code{\link[stats]{p.adjust}}).
#' @param \dots further arguments to be passed to or
#' from methods.
#'
#' @return
#' A list with class \code{"PMCMR"}
#' \itemize{
#'    \item{method }{The applied method.}
#'    \item{data.name}{The name of the data.}
#'    \item{p.value}{The two-sided p-value according to the student-t-distribution.}
#'    \item{statistic}{The estimated quantiles of the
#'        student-t-distribution.}
#'    \item{p.adjust.method}{The applied method for p-value adjustment.}
#' }
#'
#' @references
#' W. J. Conover and R. L. Iman (1979), \emph{On multiple-comparisons
#'  procedures}, Tech. Rep. LA-7677-MS, Los Alamos Scientific Laboratory.
#'
#'  W. J. Conover (1999), \emph{Practical nonparametric Statistics}, 3rd. Edition, Wiley.
#'
#' @note
#' The function does not test, whether it is a true BIBD.
#'
#' This function does not test for ties.
#'
#' @examples
#'
#' \dontrun{
#' ## Example for an incomplete block design:
#' ## Data from Conover (1999, p. 391).
#' y <- matrix(c(2, NA, NA, NA, 3, NA, 3, 3,
#' 3, NA, NA, NA,  3, NA, NA,
#' 1,  2, NA, NA, NA,  1,  1, NA,  1,  1,
#' NA, NA, NA, NA,  2, NA,  2,  1, NA, NA, NA, NA,
#' 3, NA,  2,  1, NA, NA, NA, NA,  3, NA,  2,  2),
#' ncol=7, nrow=7, byrow=FALSE,
#' dimnames=list(1:7, LETTERS[1:7]))
#'
#' posthoc.durbin.test(y)
#' }
#'
#' @importFrom stats pt pairwise.table p.adjust.methods
#' @export
posthoc.durbin.test <- function(y, ...) UseMethod("posthoc.durbin.test")

#' @rdname PMCMR-deprecated
#' @aliases posthoc.durbin.test.default
#' @method posthoc.durbin.test default
#' @export
posthoc.durbin.test.default <-
function(y, groups, blocks,  p.adjust.method = p.adjust.methods, ...)
{

    .Deprecated(new = "PMCMRplus::durbinAllPairsTest",
                package = "PMCMR",
                old = "posthoc.durbin.test")

    DNAME <- deparse(substitute(y))

    if (is.matrix(y)) {
        GRPNAME <- colnames(y)
        g <- factor(c(col(y)))
        b <- factor(c(row(y)))
        yy <- as.vector(y)
        datf1 <- data.frame(yy, b, g)
        datf2 <- datf1[!is.na(datf1[,1]),]
        blocks <- factor(datf2[,2])
        groups <- factor(datf2[,3])
        y <- datf2[,1]
        ## Clean up
        rm(b, g, yy, datf1, datf2)
    }
    else {
        if (anyNA(groups) || anyNA(blocks))
            stop("NA's are not allowed in 'groups' or 'blocks'")
        if (any(diff(c(length(y), length(groups), length(blocks))) != 0L))
            stop("'y', 'groups' and 'blocks' must have the same length")
        DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                       " and ", deparse(substitute(blocks)), sep = "")
        groups <- factor(groups)
        blocks <- factor(blocks)
        GRPNAME <- levels(groups)
    }

    ## Need to ensure consistent order.
    o <- order(blocks, groups)
    y <- y[o]
    groups <- groups[o]
    blocks <- blocks[o]

    p.adjust.method = match.arg(p.adjust.method)
    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks))
    rij <- unlist(tapply(y, blocks, rank))
    Rj <- tapply(rij, groups, sum)
    ## Taken from NIST
    A <- sum(rij^2)
    C <- (b * k * (k + 1)^2) / 4
    D <- sum(Rj^2) - r * C
    T1 <- (t - 1) / (A - C) * D

    denom <- sqrt(((A - C) * 2 * r) / (b * k - b - t + 1) *
                      (1 - T1 / (b * (k -1))))
    df <- b * k - b - t + 1
   # Pairwise comparisons
    compare.stats <- function(i,j) {
        dif <- abs(Rj[i] - Rj[j])
        tval <- dif / denom
        return(tval)
    }
    PSTAT <- pairwise.table(compare.stats,levels(groups),
                            p.adjust.method="none" )

    compare.levels <- function(i,j) {
        dif <- abs(Rj[i] - Rj[j])
        tval <- dif / denom
        pval <- 2 * pt(q=abs(tval), df=df, lower.tail=FALSE)
        return(pval)
    }
    PVAL <- pairwise.table(compare.levels,levels(groups),
                           p.adjust.method=p.adjust.method)

    METHOD <- paste("Durbin's test for a two-way", "
                 balanced incomplete block design", sep="\t")
    colnames(PSTAT) <- GRPNAME[1:(t-1)]
    rownames(PSTAT) <- GRPNAME[2:t]
    colnames(PVAL) <- colnames(PSTAT)
    rownames(PVAL) <- rownames(PSTAT)
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
                statistic = PSTAT, p.adjust.method =p.adjust.method)
    class(ans) <- "PMCMR"
    ans
}
