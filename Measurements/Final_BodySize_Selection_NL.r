################################################# M + F BODY SIZE G0510 ###################################################
BS1<- read.csv("~/Bodysize Only_G0510.csv",na.strings = "NA",header=T)

library(ggplot2)
library(dplyr)
library(plotrix)
#need remove NAs
BS<-na.omit(BS1) 
str(BS)
BS$replicate <- as.factor(BS$replicate)
BS$Generation <- as.factor(BS$Generation)
BS$sex <- factor(BS$sex, levels = c("male", "female"))

View(BS)

#change pch for M+F plots to 16 to differentiate M+F from M/F


##BODY SIZE SPLIT BY SEX. (Want to plot diffs in sex)
### BODY SIZE with SE RELAXED
MBSRS <- filter(BS, SelectionStatus == "RS", sex == "male") #M only
View(MBSRS)

MBSRSmean <- by(MBSRS$bs, MBSRS$Generation, mean)
MBSRSsd <- by(MBSRS$bs, MBSRS$Generation, sd)
MBSRSn <- summary(MBSRS$Generation) # Sample sizes
MBSRSSE <- MBSRSsd / sqrt(MBSRSn)



FBSRS <- filter(BS, SelectionStatus == "RS", sex == "female") #F only
View(FBSRS)

FBSRSmean <- by(FBSRS$bs, FBSRS$Generation, mean)
FBSRSsd <- by(FBSRS$bs, FBSRS$Generation, sd)
FBSRSn <- summary(FBSRS$Generation) # Sample sizes
FBSRSSE <- FBSRSsd / sqrt(FBSRSn)

#Plot M only and F only side by side
par(mfrow=c(1,2))

plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="Black", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Relaxed Selection Male Body Size")
#to plot trendline
plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="", pch=15, lty=1,type="b",add=TRUE)

plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="Black", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
#to plot trendline
plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="", pch=15, lty=1,type="b",add=TRUE)
title(main="Relaxed Selection Female Body Size")




### BODY SIZE with SE under SEL
MBSS <- filter(BS, SelectionStatus == "S", sex == "male") #M only
View(MBSS)

MBSSmean <- by(MBSS$bs, MBSS$Generation, mean)
MBSSsd <- by(MBSS$bs, MBSS$Generation, sd)
MBSSn <- summary(MBSS$Generation) # Sample sizes
MBSSSE <- MBSSsd / sqrt(MBSSn)



FBSS <- filter(BS, SelectionStatus == "S", sex == "female") #F only
View(FBSS)

FBSSmean <- by(FBSS$bs, FBSS$Generation, mean)
FBSSsd <- by(FBSS$bs, FBSS$Generation, sd)
FBSSn <- summary(FBSS$Generation) # Sample sizes
FBSSSE <- FBSSsd / sqrt(FBSSn)

#Plot M only and F only side by side
par(mfrow=c(1,2))

plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="Black", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Selection Male Body Size")
#to plot trendline
plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', xlab="", pch=15, lty=1,type="b",add=TRUE)

plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="Black", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
#to plot trendline
plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="", pch=15, lty=1,type="b",add=TRUE)
title(main="Selection Female Body Size")



## Sex plots with RS & S on same plot
#change F pch to 18 to differentiate btwn M & F
#Plot M only 
par(mfrow=c(1,1))
plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="#FF6600", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Male Body Size")
#to plot trendline
plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="", pch=15, col="#FF6600",lty=1,type="b",add=TRUE)

plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="#3399CC", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=1.5,add=TRUE)
#to plot trendline
plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', pch=15, col="#3399CC", lty=1,cex=1.5,type='b',add=TRUE)

legend(
  x=0.8, # x coordinate of the top left of the legend
  y=0.78, # y coordinate of the top left of the legend
  legend=c("Control","Treatment"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col = c("#FF6600","#3399CC"),# sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)


#Plot F only
plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=18,
       col="#FF6600", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=2)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Female Body Size")
#to plot trendline
plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="",  col="#FF6600", pch=18, lty=1,type="b",add=TRUE)

plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=18,
       col="#3399CC", xlim=c(0.75,3.25), ylim=c(0.75,0.95), lty=3,cex=2, add=TRUE)
#to plot trendline
plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="", col="#3399CC", pch=18, lty=1,type="b",add=TRUE)


legend(
  x=0.8, # x coordinate of the top left of the legend
  y=0.78, # y coordinate of the top left of the legend
  legend=c("Control","Treatment"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col = c("#FF6600","#3399CC"),# sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)
