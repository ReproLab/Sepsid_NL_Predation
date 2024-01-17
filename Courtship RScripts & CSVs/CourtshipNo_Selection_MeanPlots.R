#Import and Prep Data
court <- read.csv("~/courtship_combinedCSV.csv")

library(dplyr)
str(court)
court$rep <- as.factor(court$rep)
court$Generation <- ordered(court$Generation, levels=c("Parental","After 5th","After 10th"))

#to standardise terms, rename Generations
levels(court$Generation)[levels(court$Generation) == "Parental"]  <- "G0"
levels(court$Generation)[levels(court$Generation) == "After 5th"]  <- "G5"
levels(court$Generation)[levels(court$Generation) == "After 10th"]  <- "G10"

View(court)

################################################# RELAXED SEL ###################################################

### RELAXED SELECTION with SE -- Empty Vial
RSemp <- filter(court, Type == "RS", Treatment == "Empty Vial")
View(RSemp)

RSempmean <- by(RSemp$CourtshipNumber, RSemp$Generation, mean)
RSempsd <- by(RSemp$CourtshipNumber, RSemp$Generation, sd)
RSempn <- summary(RSemp$Generation) # Sample sizes
RSempSE <- RSempsd / sqrt(RSempn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=RSempmean, x=1:3, uiw=RSempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500),col="#FF6600", 
       pt.bg="#FF6600", lty=1,cex=1.5)
#to plot trendline
plotCI(y=RSempmean, x=1:3, uiw=RSempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500),col="#FF6600", 
       pt.bg="#FF6600", lty=1,cex=1.5, type="b",add=TRUE)
axis(1, at=1:3, labels=levels(RSemp$Generation), las=1)
title(main="Control")


### RELAXED SELECTION with SE-- Mantis Vial
RSman <- filter(court, Type == "RS", Treatment == "Mantis Vial")
View(RSman)

RSmanmean <- by(RSman$CourtshipNumber, RSman$Generation, mean)
RSmansd <- by(RSman$CourtshipNumber, RSman$Generation, sd)
RSmann <- summary(RSman$Generation) # Sample sizes
RSmanSE <- RSmansd / sqrt(RSmann)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=RSmanmean, x=1:3, uiw=RSmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="maroon",lty=1,col="maroon",
       cex=1.5,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=500, # y coordinate of the top left of the legend
  legend=c("Empty Vial","Mantid"), # sequence of text for the legend
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
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=Sempmean, x=1:3, uiw=SempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500),col="#3399CC",
       pt.bg="#3399CC", lty=1,cex=1.5)
#to plot trendline
plotCI(y=Sempmean, x=1:3, uiw=SempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500),col="#3399CC",
       pt.bg="#3399CC", lty=1,cex=1.5, type="b",add=TRUE)
axis(1, at=1:3, labels=levels(Semp$Generation), las=1)
title(main="Treatment")

#					-- Mantis vial
Sman <- filter(court, Type == "S", Treatment == "Mantis Vial")
View(Sman)

Smanmean <- by(Sman$CourtshipNumber, Sman$Generation, mean)
Smansd <- by(Sman$CourtshipNumber, Sman$Generation, sd)
Smann <- summary(Sman$Generation) # Sample sizes
SmanSE <- RSmansd / sqrt(Smann)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=Smanmean, x=1:3, uiw=SmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="dark blue",lty=1,col="dark blue",
       cex=1.5,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=500, # y coordinate of the top left of the legend
  legend=c("Empty Vial","Mantid"), # sequence of text for the legend
  pch=c(24,21), # sequence of point types for the legend; -1 is a nonexistent point
  pt.bg=c("#3399CC","dark blue"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.8),
  lty=c(1,1)
)

####Courtship plots squeezed for publishing####
#load all necessary objects first


par(mfrow=c(1,2), mar=c(5, 4, 3, 1) + 0.1)

##Control
plotCI(y=RSempmean, x=1:3, uiw=RSempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500), xlim=c(0.75,3.25),col="#FF6600", 
       pt.bg="#FF6600", lty=1,cex=1.5)
#to plot trendline
plotCI(y=RSempmean, x=1:3, uiw=RSempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500), xlim=c(0.75,3.25),col="#FF6600", 
       pt.bg="#FF6600", lty=1,cex=1.5, type="b",add=TRUE)
axis(1, at=1:3, labels=levels(RSemp$Generation), las=1)
title(main="Control Courtship No.")

plotCI(y=RSmanmean, x=1:3, uiw=RSmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="maroon",lty=1,col="maroon",
       cex=1.5,type="b",add=TRUE)

legend(
  x=0.75, # x coordinate of the top left of the legend
  y=500, # y coordinate of the top left of the legend
  legend=c("Empty Vial","Mantid"), # sequence of text for the legend
  pch=c(24,21), # sequence of point types for the legend; -1 is a nonexistent point
  pt.bg=c("#FF6600","maroon"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.8),
  lty=c(1,1)
)


##Treatment
plotCI(y=Sempmean, x=1:3, uiw=SempSE, err='y', xaxt='n', xlab="Generation", ylab="", pch=24, ylim=c(0,500), xlim=c(0.75,3.25),col="#3399CC",
       pt.bg="#3399CC", lty=1,cex=1.5)
#to plot trendline
plotCI(y=Sempmean, x=1:3, uiw=SempSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=24, ylim=c(0,500), xlim=c(0.75,3.25),col="#3399CC",
       pt.bg="#3399CC", lty=1,cex=1.5, type="b",add=TRUE)
axis(1, at=1:3, labels=levels(Semp$Generation), las=1)
title(main="Treatment Courtship No.")

plotCI(y=Smanmean, x=1:3, uiw=SmanSE, err='y', xaxt='n', xlab="", ylab="Number of courtship", pch=21,pt.bg="dark blue",lty=1,col="dark blue",
       cex=1.5,type="b",add=TRUE)

legend(
  x=0.75, # x coordinate of the top left of the legend
  y=500, # y coordinate of the top left of the legend
  legend=c("Empty Vial","Mantid"), # sequence of text for the legend
  pch=c(24,21), # sequence of point types for the legend; -1 is a nonexistent point
  pt.bg=c("#3399CC","dark blue"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.8),
  lty=c(1,1)
)

################################################# Plot individual replicates ###################################################

#### CONTROL ####
RS <- filter(court, Type == "RS")

RSC <- summarySE(RS, measurevar="CourtshipNumber", groupvars=c("Generation","Assay", "Replicate"))

# Standard error of the mean
ggplot(RSC, aes(x=Generation, y=CourtshipNumber, color=Assay, shape=Replicate)) + 
  geom_errorbar(aes(ymin=CourtshipNumber-se, ymax=CourtshipNumber+se), width=.1) +
  geom_point()+
  geom_line(aes(group=Assay))+
  facet_wrap(~ Replicate) +
  scale_color_manual(values=c("#FF6600","maroon")) +
  ggtitle("Control Courtship No.")+ 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### TREATMENT ####
S <- filter(court, Type == "S")

SC <- summarySE(S, measurevar="CourtshipNumber", groupvars=c("Generation","Assay", "Replicate"))

# Standard error of the mean
ggplot(SC, aes(x=Generation, y=CourtshipNumber, color=Assay, shape=Replicate)) + 
  geom_errorbar(aes(ymin=CourtshipNumber-se, ymax=CourtshipNumber+se), width=.1) +
  geom_point()+
  geom_line(aes(group=Assay))+
  facet_wrap(~ Replicate) +
  scale_color_manual(values=c("#3399CC","dark blue")) +
  ggtitle("Treatment Courtship No.")+ 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
