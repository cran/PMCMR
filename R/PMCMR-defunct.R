# dunn.test.control.R
# Part of the R package: PMCMR
#
# Copyright (C) 2015, 2016 Thorsten Pohlert
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

#' @title PMCMR-defunct
#' @name PMCMR-defunct
#' @aliases PMCMR-defunct
#' @aliases dunn.test.control
#'
#' @description
#' The functions or methods listed here are no
#' longer part of \pkg{PMCMR}. You will find
#' functions and methods in the \pkg{PMCMRplus}
#' package \url{https://cran.r-project.org/package=PMCMRplus}.
#'
#' @param x a numeric vector of data values,
#' or a list of numeric data vectors.
#' @param g a vector or factor object giving
#' the group for the corresponding elements of \code{x}.
#' Ignored if \code{x} is a list.
#' @param p.adjust.method Method for adjusting
#' p values (see \code{\link[stats]{p.adjust}}).
#' @param \dots further arguments to be passed to or from methods.
#'
#' @section
#' \code{\link{.Defunct}}
#'
#' @export
dunn.test.control <-
function(x, g, p.adjust.method = p.adjust.methods, ...){
        ##
        .Defunct(new = "PMCMRplus::kwManyOneDunnTest",
                    package = "PMCMR")
}

##
#' @rdname PMCMR-defunct
#' @aliases jonckheere.test
#' @export
jonckheere.test <- function(x, ...) UseMethod("jonckheere.test")

#' @rdname PMCMR-defunct
#' @aliases jonckheere.test.default
#' @method jonckheere.test default
#' @param alternative The alternative hypothesis.
#' @export
jonckheere.test.default <-
        function(x, g, alternative = c("monotonic", "increasing", "decreasing"), ...)
        {

                .Defunct(new = "PMCMRplus::JonckheereTest",
                         package = "PMCMR")
        }

##
#' @rdname PMCMR-defunct
#' @aliases posthoc.friedman.conover.test
#' @param y either a numeric vector of data values, or a data matrix.
#' @export
posthoc.friedman.conover.test <- function(y, ...)
        UseMethod("posthoc.friedman.conover.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.friedman.conover.test.default
#' @method  posthoc.friedman.conover.test default
#' @param groups a vector giving the group for
#' the corresponding elements of \code{y} if this is a vector;
#' ignored if \code{y} is a matrix. If not a factor object,
#' it is coerced to one.
#' @param blocks a vector giving the block for
#' the corresponding elements of \code{y} if this is a vector;
#' ignored if \code{y} is a matrix. If not a factor object,
#' it is coerced to one
#' @export
posthoc.friedman.conover.test.default <-
        function(y, groups, blocks, p.adjust.method = p.adjust.methods, ...){

                .Defunct(new = "PMCMRplus::frdAllPairsConoverTest",
                         package = "PMCMR")
        }

##
#' @rdname PMCMR-defunct
#' @aliases posthoc.friedman.nemenyi.test
#' @export
posthoc.friedman.nemenyi.test <- function(y, ...) UseMethod("posthoc.friedman.nemenyi.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.friedman.nemenyi.test.default
#' @method posthoc.friedman.nemenyi.test default
#' @export
posthoc.friedman.nemenyi.test.default <-
        function(y, groups, blocks, ...){

                .Defunct(new = "PMCMRplus::frdAllPairsNemenyiTest",
                         package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.friedman.nemenyi.test.formula
#' @method posthoc.friedman.nemenyi.test formula
#' @param formula a formula of the form \code{a ~ b | c},
#' where \code{a}, \code{b} and \code{c} give the data
#' values and corresponding groups and blocks, respectively.
#' @param data an optional matrix or data frame
#' (or similar: see \code{\link[stats]{model.frame}}) containing the
#' variables in the formula \code{formula}.
#' By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a
#' subset of observations to be used.
#' @param na.action a function which indicates what
#' should happen when the data contain \code{NA}s.
#' Defaults to \code{getOption("na.action")}.´
#' @export
posthoc.friedman.nemenyi.test.formula <-
        function(formula, data, subset, na.action, ...)
        {
                if(missing(formula))
                        stop("formula missing")
                ## <FIXME>
                ## Maybe put this into an internal rewriteTwoWayFormula() when
                ## adding support for strata()
                if((length(formula) != 3L)
                   || (length(formula[[3L]]) != 3L)
                   || (formula[[3L]][[1L]] != as.name("|"))
                   || (length(formula[[3L]][[2L]]) != 1L)
                   || (length(formula[[3L]][[3L]]) != 1L))
                        stop("incorrect specification for 'formula'")
                formula[[3L]][[1L]] <- as.name("+")
                ## </FIXME>
                m <- match.call(expand.dots = FALSE)
                m$formula <- formula
                if(is.matrix(eval(m$data, parent.frame())))
                        m$data <- as.data.frame(data)
                m[[1L]] <- quote(stats::model.frame)
                mf <- eval(m, parent.frame())
                DNAME <- paste(names(mf), collapse = " and ")
                names(mf) <- NULL
                y <- do.call("posthoc.friedman.nemenyi.test", as.list(mf))
                y$data.name <- DNAME
                y
        }

#' @rdname PMCMR-defunct
#' @aliases durbin.test
#' @export
durbin.test <- function(y, ...) UseMethod("durbin.test")

#' @rdname PMCMR-defunct
#' @aliases durbin.test.default
#' @method durbin.test default
#' @export
durbin.test.default <-
        function(y, groups, blocks, ...)
        {

                .Defunct(new = "PMCMRplus::durbinTest",
                            package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases durbin.test.formula
#' @method durbin.test formula
#' @export
durbin.test.formula <-
        function(formula, data, subset, na.action, ...)
        {
                if(missing(formula))
                        stop("formula missing")
                ## <FIXME>
                ## Maybe put this into an internal rewriteTwoWayFormula() when
                ## adding support for strata()
                if((length(formula) != 3L)
                   || (length(formula[[3L]]) != 3L)
                   || (formula[[3L]][[1L]] != as.name("|"))
                   || (length(formula[[3L]][[2L]]) != 1L)
                   || (length(formula[[3L]][[3L]]) != 1L))
                        stop("incorrect specification for 'formula'")
                formula[[3L]][[1L]] <- as.name("+")
                ## </FIXME>
                m <- match.call(expand.dots = FALSE)
                m$formula <- formula
                if(is.matrix(eval(m$data, parent.frame())))
                        m$data <- as.data.frame(data)
                m[[1L]] <- quote(stats::model.frame)
                mf <- eval(m, parent.frame())
                DNAME <- paste(names(mf), collapse = " and ")
                names(mf) <- NULL
                y <- do.call("durbin.test", as.list(mf))
                y$data.name <- DNAME
                y
        }


##
#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.conover.test
#' @export
posthoc.kruskal.conover.test <- function(x, ...) UseMethod("posthoc.kruskal.conover.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.conover.test.default
#' @method posthoc.kruskal.conover.test default
#' @export
posthoc.kruskal.conover.test.default <-
        function(x, g, p.adjust.method = p.adjust.methods, ...){

                .Defunct(new = "PMCMRplus::kwAllPairsConoverTest",
                         package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.conover.test.formula
#' @method posthoc.kruskal.conover.test formula
#' @export
posthoc.kruskal.conover.test.formula <-
        function(formula, data, subset, na.action,
                 p.adjust.method = p.adjust.methods, ...)
        {
                mf <- match.call(expand.dots=FALSE)
                m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
                mf <- mf[c(1L, m)]
                mf[[1L]] <- quote(stats::model.frame)

                if(missing(formula) || (length(formula) != 3L))
                        stop("'formula' missing or incorrect")
                mf <- eval(mf, parent.frame())
                if(length(mf) > 2L)
                        stop("'formula' should be of the form response ~ group")
                DNAME <- paste(names(mf), collapse = " by ")
                p.adjust.method <- match.arg(p.adjust.method)
                names(mf) <- NULL
                y <- do.call("posthoc.kruskal.conover.test", c(as.list(mf),
                                                               p.adjust.method))
                y$data.name <- DNAME
                y
        }

#
#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.dunn.test
#' @export
posthoc.kruskal.dunn.test <- function(x, ...) UseMethod("posthoc.kruskal.dunn.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.dunn.test.default
#' @method posthoc.kruskal.dunn.test default
#' @export
posthoc.kruskal.dunn.test.default <-
        function(x, g, p.adjust.method = p.adjust.methods, ...){

                .Defunct(new = "PMCMRplus::kwAllPairsDunnTest",
                         package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.dunn.test.formula
#' @method posthoc.kruskal.dunn.test formula
#' @export
posthoc.kruskal.dunn.test.formula <-
        function(formula, data, subset, na.action, p.adjust.method = p.adjust.methods, ...)
        {
                mf <- match.call(expand.dots=FALSE)
                m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
                mf <- mf[c(1L, m)]
                mf[[1L]] <- quote(stats::model.frame)

                if(missing(formula) || (length(formula) != 3L))
                        stop("'formula' missing or incorrect")
                mf <- eval(mf, parent.frame())
                if(length(mf) > 2L)
                        stop("'formula' should be of the form response ~ group")
                DNAME <- paste(names(mf), collapse = " by ")
                p.adjust.method <- match.arg(p.adjust.method)
                names(mf) <- NULL
                y <- do.call("posthoc.kruskal.dunn.test", c(as.list(mf), p.adjust.method))
                y$data.name <- DNAME
                y
        }

#
#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.nemenyi.test
#' @export
posthoc.kruskal.nemenyi.test <- function(x, ...) UseMethod("posthoc.kruskal.nemenyi.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.nemenyi.test.default
#' @method posthoc.kruskal.nemenyi.test default
#' @export
posthoc.kruskal.nemenyi.test.default <-
        function(x, g, dist = c("Tukey","Chisquare"), ...){
                .Defunct(new = "PMCMRplus::kwAllPairsNemenyiTest",
                         package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.kruskal.nemenyi.test.formula
#' @method posthoc.kruskal.nemenyi.test formula
#' @export
posthoc.kruskal.nemenyi.test.formula <-
        function(formula, data, subset, na.action, dist = c("Tukey","Chisquare"), ...)
        {
                mf <- match.call(expand.dots=FALSE)
                m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
                mf <- mf[c(1L, m)]
                dist <- match.arg(dist)
                mf[[1L]] <- quote(stats::model.frame)

                if(missing(formula) || (length(formula) != 3L))
                        stop("'formula' missing or incorrect")
                mf <- eval(mf, parent.frame())
                if(length(mf) > 2L)
                        stop("'formula' should be of the form response ~ group")
                DNAME <- paste(names(mf), collapse = " by ")
                names(mf) <- NULL
                y <- do.call("posthoc.kruskal.nemenyi.test", c(as.list(mf), dist))
                y$data.name <- DNAME
                y
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.quade.test
#' @export
posthoc.quade.test <- function(y, ...) UseMethod("posthoc.quade.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.quade.test.default
#' @method posthoc.quade.test default
#' @param dist the test distribution
#' @export
posthoc.quade.test.default <-
        function(y, groups, blocks, dist=c("TDist", "Normal"),
                 p.adjust.method = p.adjust.methods,  ...)
        {
               .Defunct(new = "PMCMRplus::quadeAllPairsTest",
                        package = "PMCMR")
        }


#' @rdname PMCMR-defunct
#' @aliases posthoc.vanWaerden.test
#' @export
posthoc.vanWaerden.test <- function(x, ...) UseMethod("posthoc.vanWaerden.test")

#' @rdname PMCMR-defunct
#' @aliases posthoc.vanWaerden.test.default
#' @method posthoc.vanWaerden.test default
#' @export
posthoc.vanWaerden.test.default <-
        function(x, g,  p.adjust.method = p.adjust.methods, ...)
        {
                .Defunct(new = "PMCMRplus::vanWaerdenAllPairsTest",
                         package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases posthoc.vanWaerden.test.formula
#' @method posthoc.vanWaerden.test formula
#' @export
posthoc.vanWaerden.test.formula <-
        function(formula, data, subset, na.action, p.adjust.method = p.adjust.methods, ...)
        {
                mf <- match.call(expand.dots=FALSE)
                m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
                mf <- mf[c(1L, m)]
                mf[[1L]] <- quote(stats::model.frame)

                if(missing(formula) || (length(formula) != 3L))
                        stop("'formula' missing or incorrect")
                mf <- eval(mf, parent.frame())
                if(length(mf) > 2L)
                        stop("'formula' should be of the form response ~ group")
                DNAME <- paste(names(mf), collapse = " by ")
                p.adjust.method <- match.arg(p.adjust.method)
                names(mf) <- NULL
                y <- do.call("posthoc.vanWaerden.test", c(as.list(mf), p.adjust.method))
                y$data.name <- DNAME
                y
        }

#' @rdname PMCMR-defunct
#' @aliases vanWaerden.test
#' @export
vanWaerden.test <- function(x, ...) UseMethod("vanWaerden.test")

#' @rdname PMCMR-defunct
#' @aliases vanWaerden.test.default
#' @method vanWaerden.test default
#' @export
vanWaerden.test.default <-
        function(x, g, ...)
        {
         .Defunct(new = "PMCMRplus::vanWaerdenTest",
                  package = "PMCMR")
        }

#' @rdname PMCMR-defunct
#' @aliases vanWaerden.test.formula
#' @method vanWaerden.test formula
#' @export
vanWaerden.test.formula <-
        function(formula, data, subset, na.action, ...)
        {
                if(missing(formula) || (length(formula) != 3L))
                        stop("'formula' missing or incorrect")
                m <- match.call(expand.dots = FALSE)
                if(is.matrix(eval(m$data, parent.frame())))
                        m$data <- as.data.frame(data)
                m[[1L]] <- quote(stats::model.frame)
                mf <- eval(m, parent.frame())
                if(length(mf) > 2L)
                        stop("'formula' should be of the form response ~ group")
                DNAME <- paste(names(mf), collapse = " by ")
                names(mf) <- NULL
                y <- do.call("vanWaerden.test", as.list(mf))
                y$data.name <- DNAME
                y
        }
