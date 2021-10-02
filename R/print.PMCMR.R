#  print.PMCMR.R
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

#' @name summary.PMCMR
#' @aliases summary.PMCMR
#' @method summary PMCMR
#' @title Summarizing PMCMR objects
#' @description
#' \code{summary} method for class \code{"PMCMR"}.
#'
#' @param object an object of class \code{"PMCMR"},
#' usually, a result of a call to any of the posthoc-tests
#' included in the package PMCMR.
#' @param \dots further arguments, currently ignored.
#'
#' @return
#' The function \code{summary.PMCMR} computes and returns a list of the
#' pairwise comparisons including the H0, the corresponding statistic and
#' the (adjusted) p-value.
#'
#' @keywords methods print
summary.PMCMR <-
function(object, ...)
{
    OK <- inherits(object, "PMCMR")
    if (!OK)
        stop ("Not an object of class PMCMR")
    if (!is.matrix(object$statistic))
        stop ("Matrix object$statistic not found.")
    pval <- as.numeric(object$p.value)
    stat <- as.numeric(object$statistic)
    grp1 <- as.numeric(c(col(object$p.value)))
    cnam <- colnames(object$p.value)
    grp2 <- as.numeric(c(row(object$p.value)))
    rnam <- rownames(object$p.value)
    H0 <- paste(cnam[grp1], " = ", rnam[grp2])
    OK <- !is.na(pval)
    cat("\n\tPairwise comparisons using", object$method, "\n\n")
    cat("data: ", object$data.name, "\n\n")
    cat("\nP value adjustment method:", object$p.adjust.method, "\n")
    xdf <- data.frame(H0 = H0[OK], statistic = stat[OK],
                      p.value = format.pval(pval[OK], 2))
#    out <- list(x = xdf,
#                p.adjust.method = object$p.adjust.method,
#                method = object$method,
#                data.name = object$data.name)
#    class(out) <- "summary.PMCMR"
#    out
    print(xdf)
    invisible(object)
}


## This was taken from package stat
## file pairwise.R
## (C) 2014 R Core Team, GPL >= 2
#' @name print.PMCMR
#' @aliases  print.PMCMR
#' @title Prints PMCMR objects
#' @description
#' \code{print} method for class \code{"PMCMR"}.
#'
#' @param x an object of class \code{"PMCMR"}, usually,
#' a result of a call to any of the posthoc-tests included
#' in the package PMCMR.
#' @param \ldots further arguments, currently ignored.
#'
#' @return
#' The function \code{print.PMCMR} returns the lower
#' triangle of the (adjusted) p-values from any of
#' the posthoc tests included in the package PMCMR.
#'
#' @method print PMCMR
print.PMCMR <-
function(x, ...)
{
    cat("\n\tPairwise comparisons using", x$method, "\n\n")
    cat("data: ", x$data.name, "\n\n")
    pp <- format.pval(x$p.value, 2, na.form="-")
    attributes(pp) <- attributes(x$p.value)
    print(pp, quote=FALSE, ...)
    cat("\nP value adjustment method:", x$p.adjust.method, "\n")
    invisible(x)
}

##

#' @name get.pvalues
#' @aliases get.pvalues
#' @title Get Pvalues from PMCMR Objects
#' @param object either an object of class \code{"PMCMR"}, usually, a result of a
#' call to any of the posthoc-tests included in the package PMCMR. Or
#' an object of class \code{"pairwise.htest"}, a result of a call to
#' \code{\link[stats]{pairwise.prop.test}}, \code{\link[stats]{pairwise.t.test}} or
#' \code{\link[stats]{pairwise.wilcox.test}}.
#' @param \dots further arguments, currently ignored.
#'
#' @description
#' Returns a vector of pvalues that includes the names of the pairwise
#' groups (i.e. the null hypothesis). The output can be used by
#' \code{\link[multcompView]{multcompLetters}} to find homogeneous groups.
#'
#' @return
#' a named vector with p-values
#' @keywords utilities
#' @export
get.pvalues <-
function(object, ...)
{
    OK <- inherits(object, c("PMCMR", "pairwise.htest"))
    if (!OK)
        stop ("Not an object of class PMCMR or pairwise.htest")
    if (!is.matrix(object$p.value))
        stop ("Matrix object$p.value not found.")
    pval <- as.numeric(object$p.value)
    grp1 <- as.numeric(c(col(object$p.value)))
    cnam <- colnames(object$p.value)
    grp2 <- as.numeric(c(row(object$p.value)))
    rnam <- rownames(object$p.value)
    H0 <- paste(cnam[grp1],"-",rnam[grp2], sep="")
    OK <- !is.na(pval)
    out <- pval[OK]
    names(out) <- H0[OK]
    out
}
