### R code from vignette source 'PMCMR.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PMCMR.Rnw:88-90
###################################################
library("graphics")
boxplot(count ~ spray, data=InsectSprays)


###################################################
### code chunk number 2: PMCMR.Rnw:98-99
###################################################
kruskal.test(count ~ spray, data=InsectSprays)


###################################################
### code chunk number 3: PMCMR.Rnw:104-108
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.nemenyi.test(x=count, g=spray, dist="Tukey")


###################################################
### code chunk number 4: PMCMR.Rnw:112-113
###################################################
posthoc.kruskal.nemenyi.test(count ~ spray, data=InsectSprays, dist="Tukey")


###################################################
### code chunk number 5: PMCMR.Rnw:118-119
###################################################
(out <- posthoc.kruskal.nemenyi.test(x=count, g=spray, dist="Chisquare"))


###################################################
### code chunk number 6: PMCMR.Rnw:124-125
###################################################
print(out$statistic)


###################################################
### code chunk number 7: PMCMR.Rnw:178-187
###################################################
require(PMCMR)
y <- matrix(c(
3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45, 
26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
26.65),nrow=6, ncol=6, 
dimnames=list(1:6,c("A","B","C","D","E","F")))
print(y)


###################################################
### code chunk number 8: PMCMR.Rnw:194-197
###################################################
library("graphics")
groups <- gl(6,6,labels=colnames(y))
boxplot(as.vector(y) ~ groups)


###################################################
### code chunk number 9: PMCMR.Rnw:204-205
###################################################
friedman.test(y)


###################################################
### code chunk number 10: PMCMR.Rnw:210-211
###################################################
posthoc.friedman.nemenyi.test(y)


