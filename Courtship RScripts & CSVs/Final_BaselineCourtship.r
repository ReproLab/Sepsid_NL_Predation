#Import Data
coact <- read.csv("~/Baseline_behavioural data.csv")

library(dplyr)
str(coact)
coact$id <- as.character(coact$id)
coact$treatment <- ordered(coact$treatment, levels=c("Control","Beetle","Mantid"))

shapiro.test(coact$courtshipnumber) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality


#Kruskal Wallis	
kruskal.test(courtshipnumber ~ treatment, data = coact) #p-value less than sig 0.05, can conclude sig diffs between groups.
#Kruskal-Wallis rank sum test
#data:  courtshipnumber by treatment
#Kruskal-Wallis chi-squared = 6.2816, df = 2, p-value = 0.04325



#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use dunn_test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
library(rstatix)

dunn_test(coact, courtshipnumber ~ treatment)
#.y.             group1  group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr>           <chr>   <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 courtshipnumber Control Beetle     4     4     0.442 0.658  0.658  ns          
#2 courtshipnumber Control Mantid     4     4    -1.92  0.0554 0.111  ns          
#3 courtshipnumber Beetle  Mantid     4     4    -2.36  0.0184 0.0552 ns  



#Plot mean with SE
CAmean <- by(coact$courtshipnumber, coact$treatment, mean)
CAsd <- by(coact$courtshipnumber, coact$treatment, sd)
CAn <- summary(coact$treatment) # Sample sizes
CASE <- CAsd / sqrt(CAn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
#with SE
plotCI(y=CAmean, x=1:3, uiw=CASE, err='y', xaxt='n', xlab="", ylab="Number of Courtship",cex=1.5,pch=15,pt.bg="black",ylim=c(0,200))
axis(1, at=1:3, labels=levels(coact$treatment), las=1)
title(main="Baseline Number of Courtship")


####Courtship plots squeezed####
#load all necessary objects (baseline courtship & courtship no.s plots) first
par(mfrow=c(3,1), mar=c(5, 4.5, 3, 1) + 0.1)


##baseline
plotCI(y=CAmean, x=1:3, uiw=CASE, err='y', xaxt='n', xlab="Behavioural Assay", ylab="Number of Courtship",cex.lab=1.4, cex.axis=1.2, cex=1.5,pch=15,pt.bg="black",ylim=c(0,150), xlim=c(0.75,3.25))
axis(1, at=1:3, labels=levels(coact$treatment), las=1, cex.axis=1.2)
title(main="Baseline Courtship No.", cex.main=2)
