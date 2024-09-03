#Import Data
TV <- read.csv("~/MTestesVolumeOnly_G0510.csv",na.strings = "NA",header=T)

library(dplyr)
str(TV)
TV$replicate <- as.factor(TV$replicate)
TV$id <- as.character(TV$id)
TV$Generation <- as.factor(TV$Generation)
TV$SelectionStatus <- as.factor(TV$SelectionStatus)

View(TV)

################################################# TESTES VOLUME G0 G5 G10 ###################################################


### TESTES VOLUME with SE under RELAXED SEL
TVRS <- filter(TV, SelectionStatus == "RS")
View(TVRS)

TVRSmean <- by(TVRS$avgts, TVRS$Generation, mean)
TVRSsd <- by(TVRS$avgts, TVRS$Generation, sd)
TVRSn <- summary(TVRS$Generation) # Sample sizes
TVRSSE <- TVRSsd / sqrt(TVRSn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
#offset=0.05
plotCI(y=TVRSmean, x=(1:3), uiw=TVRSSE, err='y', xaxt='n', xlab="Generation", ylab=expression("Volume of Testes " ~ (mm^{3})), pch=15,col="#FF6600",ylim=c(0.003,0.008), xlim=c(0.9,3.1),
       pt.bg="#FF6600", lty=1,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Testes Volume")

#to plot trendline
plotCI(y=TVRSmean, x=(1:3), uiw=TVRSSE, err='y', gap=0,xaxt='n', xlab="", pch=15, pt.bg="#FF6600",  col="#FF6600",lty=1,type="b",add=TRUE)


##################################
### TESTES VOLUME with SE under SEL
TVS <- filter(TV, SelectionStatus == "S")
View(TVS)

TVSmean <- by(TVS$avgts, TVS$Generation, mean)
TVSsd <- by(TVS$avgts, TVS$Generation, sd)
TVSn <- summary(TVS$Generation) # Sample sizes
TVSSE <- TVSsd / sqrt(TVSn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
#offset=0.05
plotCI(y=TVSmean, x=(1:3), uiw=TVSSE, err='y', gap=0, xaxt='n', xlab="", pch=15,pt.bg="#3399CC",col="#3399CC",
       cex=1.5,add=TRUE,lty=1,type="b")

legend(
  x=1, # x coordinate of the top left of the legend
  y=0.0035, # y coordinate of the top left of the legend
  legend=c("Control","Treatment"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col=c("#FF6600","#3399CC"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)


# printed as filename = "MTestesVolume Only_G0510.png"
