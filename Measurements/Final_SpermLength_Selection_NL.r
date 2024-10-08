#Import Data
SL <- read.csv("~/MSpermLengthOnly_G0510.csv",na.strings = "NA",header=T)

library(dplyr)
str(SL)
SL$replicate <- as.factor(SL$replicate)
SL$id <- as.character(SL$id)
SL$Generation <- as.factor(SL$Generation)

################################################# SPERM LENGTH ###################################################


### SPERM LENGTH with SE under RELAXED SEL
SLRS <- filter(SL, SelectionStatus == "RS")
View(SLRS)

SLRSmean <- by(SLRS$avgss, SLRS$Generation, mean)
SLRSsd <- by(SLRS$avgss, SLRS$Generation, sd)
SLRSn <- summary(SLRS$Generation) # Sample sizes
SLRSSE <- SLRSsd / sqrt(SLRSn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
#offset=0.05
plotCI(y=SLRSmean, x=(1:3), uiw=SLRSSE, err='y', xaxt='n', xlab="Generation", ylab=expression("Length of Sperm " ~(mu ~ M)), pch=15,col="#FF6600",xlim=c(0.9,3.1),
       ylim=c(240,320), pt.bg="#FF6600", lty=1,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Sperm Length")

#to plot trendline
plotCI(y=SLRSmean, x=(1:3), uiw=SLRSSE, err='y', xaxt='n', xlab="", pch=15,col="#FF6600",  pt.bg="#FF6600", lty=1, type="b",add=TRUE)


### SPERM LENGTH with SE under SEL
SLS <- filter(SL, SelectionStatus == "S")
View(SLS)

SLSmean <- by(SLS$avgss, SLS$Generation, mean)
SLSsd <- by(SLS$avgss, SLS$Generation, sd)
SLSn <- summary(SLS$Generation) # Sample sizes
SLSSE <- SLSsd / sqrt(SLSn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=SLSmean, x=(1:3), uiw=SLSSE, err='y', xaxt='n', xlab="", pch=15,pt.bg="#3399CC",col="#3399CC",
       cex=1.5,lty=1,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=250, # y coordinate of the top left of the legend
  legend=c("Control","Treatment"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col=c("#FF6600","#3399CC"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)
