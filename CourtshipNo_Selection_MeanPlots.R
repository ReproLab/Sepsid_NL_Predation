#Import and Prep Data
court <- read.csv("~/courtship_combinedCSV.csv")

library(dplyr)
str(court)
court$rep <- as.factor(court$rep)
court$Generation <- ordered(court$Generation, levels=c("Parental","After 5th","After 10th"))
View(court)

################################################# RELAXED SEL ###################################################

### RELAXED SELECTION with SE -- Empty Vial
RSemp <- filter(court, Type == "RS", Treatment == "Empty Vial")
View(RSemp)

RSempmean <- by(RSemp$CourtshipNumber, RSemp$Generation, mean)
RSempsd <- by(RSemp$CourtshipNumber, RSemp$Generation, sd)
RSempn <- summary(RSemp$Generation) # Sample sizes
RSempSE <- RSempsd / sqrt(RSempn)
t.crit <- qt(0.95, RSempn-1)
int.width <- RSempSE * t.crit
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=RSempmean, x=1:3, uiw=RSempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,600),col="#FF6600", 
pt.bg="#FF6600", lty=1,cex=1.5)
axis(1, at=1:3, labels=levels(RSemp$Generation), las=1)
title(main="Relaxed Selection")

### RELAXED SELECTION with SE-- Mantis Vial
RSman <- filter(court, Type == "RS", Treatment == "Mantis Vial")
View(RSman)

RSmanmean <- by(RSman$CourtshipNumber, RSman$Generation, mean)
RSmansd <- by(RSman$CourtshipNumber, RSman$Generation, sd)
RSmann <- summary(RSman$Generation) # Sample sizes
RSmanSE <- RSmansd / sqrt(RSmann)
t.crit <- qt(0.95, RSmann-1)
int.width <- RSmanSE * t.crit
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=RSmanmean, x=1:3, uiw=RSmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="maroon",lty=1,col="maroon",
cex=1.5,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=600, # y coordinate of the top left of the legend
 legend=c("Empty Vial","Vial with Mantid"), # sequence of text for the legend
  pch=c(24,21), # sequence of point types for the legend; -1 is a nonexistent point
  pt.bg=c("#FF6600","maroon"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.8),
  lty=c(1,1)
  )



################################################ SELECTION ##############################################################
  

 ###SELECTION with SE -- Empty vial
Semp <- filter(court, Type == "S", Treatment == "Empty Vial")
View(Semp)

Sempmean <- by(Semp$CourtshipNumber, Semp$Generation, mean)
Sempsd <- by(Semp$CourtshipNumber, Semp$Generation, sd)
Sempn <- summary(Semp$Generation) # Sample sizes
SempSE <- RSempsd / sqrt(Sempn)
Sempt.crit <- qt(0.95, Sempn-1)
Sempint.width <- SempSE * Sempt.crit
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=Sempmean, x=1:3, uiw=SempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,600),col="#3399CC",
pt.bg="#3399CC", lty=1,cex=1.5)
axis(1, at=1:3, labels=levels(Semp$Generation), las=1)
title(main="Selection under Predation")

#					-- Mantis vial
Sman <- filter(court, Type == "S", Treatment == "Mantis Vial")
View(Sman)

Smanmean <- by(Sman$CourtshipNumber, Sman$Generation, mean)
Smansd <- by(Sman$CourtshipNumber, Sman$Generation, sd)
Smann <- summary(Sman$Generation) # Sample sizes
SmanSE <- RSmansd / sqrt(Smann)
Smant.crit <- qt(0.95, Smann-1)
Smanint.width <- SmanSE * Smant.crit
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=Smanmean, x=1:3, uiw=SmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="dark blue",lty=1,col="dark blue",
cex=1.5,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=600, # y coordinate of the top left of the legend
 legend=c("Empty Vial","Vial with Mantid"), # sequence of text for the legend
  pch=c(24,21), # sequence of point types for the legend; -1 is a nonexistent point
  pt.bg=c("#3399CC","dark blue"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.8),
  lty=c(1,1)
  )
  
