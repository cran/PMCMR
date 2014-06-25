posthoc.kruskal.nemenyi.test <-
function(x, g, method = c("Tukey","Chisquare")){
        method <- match.arg(method)
        p.adjust.method = "none"
        DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
        g <- factor(g)
        x.rank <- rank(x)
        R.bar <- tapply(x.rank, g, mean,na.rm=T)
        R.n <- tapply(!is.na(x), g, length)
        g.unique <- unique(g)
        k <- length(g.unique)
        n <- sum(R.n)
        getties <- function(x, n) {
           x.sorted <- sort(x)
       	   pos <- 1
           tiesum <- 0
	     while (pos <= n) {
	       	val <- x.sorted[pos]  
			nt <- length(!is.na(x.sorted[x.sorted==val]))
			pos <- pos + nt
			 if (nt > 1){
			     tiesum <- tiesum + nt^3  - nt
			 }	     
                }
		C <- 1 - tiesum / (n^3 - n)
        	C <- min(c(1,C))
        	C
        }
	if(method == "Chisquare") {
          METHOD <- paste("Nemenyi-test with Chi-squared", "
                       approximation for independent samples", sep="\t")
         compare.stats <- function(i,j) {
            dif <- abs(R.bar[i] - R.bar[j]) 
            A <- n * (n+1) / 12
            B <- (1 / R.n[i] + 1 / R.n[j])
            chisqval <- dif^2 / (A * B)
            return(chisqval)
        }
        PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none" )

        C <- getties(x.rank, n)
        if (C != 1) warning("Ties are present. Chi-sq was corrected for ties.")
        PVAL <- 1 - pchisq((C * PSTAT), df=(k-1))
        } else {
                 METHOD <- paste("Tukey and Kramer (Nemenyi) test", "
                   with Tukey-Dist approximation for independent samples", sep="\t")
         compare.stats <- function(i,j) {
            dif <- abs(R.bar[i] - R.bar[j])
            qval <- dif / sqrt((n * (n + 1) / 12) * (1/R.n[i] + 1/R.n[j] ))
            return(qval)
        }
        PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none" )*sqrt(2)
        C <- getties(x.rank, n)
        if (C != 1) warning("Ties are present, p-values are not corrected.")
        PVAL <- 1 - ptukey(PSTAT, nmeans=k, df=1000000)
        }
        ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
               statistic = PSTAT, p.adjust.method = p.adjust.method)
        class(ans) <- "pairwise.htest"
        ans
}
