posthoc.friedman.nemenyi.test <- function(y, groups, blocks){
         if ((is.matrix(y)) | (is.data.frame(y))) {
        groups <- factor(c(col(y)))
        blocks <- factor(c(row(y)))
        DNAME <- paste(deparse(substitute(y)))
        GRPNAMES <- colnames(y)
        }
        else {
        if (any(is.na(groups)) || any(is.na(blocks))) 
            stop("NA's are not allowed in groups or blocks")
        if (any(diff(c(length(y), length(groups), length(blocks))))) 
            stop("y, groups and blocks must have the same length")
        if (any(table(groups, blocks) != 1)) 
            stop("Not an unreplicated complete block design")

	 DNAME <- paste(deparse(substitute(y)), ",", deparse(substitute(groups)), "and", deparse(substitute(blocks)) )
         groups <- factor(groups)
         blocks <- factor(blocks)
         GRPNAMES <- as.character(levels(groups))
        }
    	n <- length(levels(blocks))
        k <- length(levels(groups))
    	y <- y[order(groups, blocks)]
    	mat <- matrix(y, nrow = n, ncol = k, byrow = FALSE)
    	 for (i in 1:length(mat[, 1])) mat[i, ] <- rank(mat[i, ])
        R.mnsum <- colMeans(mat)
        p.adjust.method = "none"
       METHOD <- "Nemenyi post-hoc test with q approximation for unreplicated blocked data"
         compare.stats <- function(i,j) {
            dif <- abs(R.mnsum[i] - R.mnsum[j])
            qval <- dif / sqrt(k * (k + 1) / (6 * n))
            return(qval)
        }
        PSTAT <- pairwise.table(compare.stats,levels(groups), p.adjust.method="none" ) * sqrt(2)
        PVAL <- 1 - ptukey(PSTAT, nmeans=k, df=1000000)
        colnames(PSTAT) <- GRPNAMES[1:(k-1)]
        rownames(PSTAT) <- GRPNAMES[2:k]
        colnames(PVAL) <- GRPNAMES[1:(k-1)]
        rownames(PVAL) <- GRPNAMES[2:k]
        ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
               statistic = PSTAT, p.adjust.method = p.adjust.method)
        class(ans) <- "pairwise.htest"
        ans
}
