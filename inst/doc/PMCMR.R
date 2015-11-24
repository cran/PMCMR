### R code from vignette source 'PMCMR.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PMCMR.Rnw:89-91
###################################################
library("graphics")
boxplot(count ~ spray, data=InsectSprays)


###################################################
### code chunk number 2: PMCMR.Rnw:99-100
###################################################
kruskal.test(count ~ spray, data=InsectSprays)


###################################################
### code chunk number 3: PMCMR.Rnw:105-109
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.nemenyi.test(x=count, g=spray, dist="Tukey")


###################################################
### code chunk number 4: PMCMR.Rnw:113-114
###################################################
posthoc.kruskal.nemenyi.test(count ~ spray, data=InsectSprays, dist="Tukey")


###################################################
### code chunk number 5: PMCMR.Rnw:119-120
###################################################
(out <- posthoc.kruskal.nemenyi.test(x=count, g=spray, dist="Chisquare"))


###################################################
### code chunk number 6: PMCMR.Rnw:125-126
###################################################
print(out$statistic)


###################################################
### code chunk number 7: PMCMR.Rnw:168-172
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.dunn.test(x=count, g=spray, p.adjust.method="none")


###################################################
### code chunk number 8: PMCMR.Rnw:177-181
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.dunn.test(x=count, g=spray, p.adjust.method="bonferroni")


###################################################
### code chunk number 9: PMCMR.Rnw:204-208
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.conover.test(x=count, g=spray, p.adjust.method="none")


###################################################
### code chunk number 10: PMCMR.Rnw:211-215
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.kruskal.conover.test(x=count, g=spray, p.adjust.method="bonferroni")


###################################################
### code chunk number 11: PMCMR.Rnw:233-238
###################################################
require(stats) 
data(PlantGrowth)
attach(PlantGrowth)
kruskal.test(weight, group)
dunn.test.control(x=weight,g=group, p.adjust="bonferroni")


###################################################
### code chunk number 12: PMCMR.Rnw:244-245
###################################################
summary.lm(aov(weight ~ group))


###################################################
### code chunk number 13: PMCMR.Rnw:273-277
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
vanWaerden.test(x=count, g=spray)


###################################################
### code chunk number 14: PMCMR.Rnw:292-296
###################################################
require(PMCMR)
data(InsectSprays)
attach(InsectSprays)
posthoc.vanWaerden.test(x=count, g=spray, p.adjust.method="none")


###################################################
### code chunk number 15: PMCMR.Rnw:331-340
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
### code chunk number 16: PMCMR.Rnw:347-350
###################################################
library("graphics")
groups <- gl(6,6,labels=colnames(y))
boxplot(as.vector(y) ~ groups)


###################################################
### code chunk number 17: PMCMR.Rnw:357-358
###################################################
friedman.test(y)


###################################################
### code chunk number 18: PMCMR.Rnw:363-364
###################################################
posthoc.friedman.nemenyi.test(y)


###################################################
### code chunk number 19: PMCMR.Rnw:381-391
###################################################
require(PMCMR)
y <- matrix(c(
3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45, 
26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
26.65),nrow=6, ncol=6, 
dimnames=list(1:6,c("A","B","C","D","E","F")))
friedman.test(y)
posthoc.friedman.conover.test(y=y, p.adjust="none")


