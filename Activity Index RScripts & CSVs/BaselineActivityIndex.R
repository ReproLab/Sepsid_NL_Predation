Import & Prep Data
coact <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\Behaviour tests\\Baseline_behavioural data.csv")

library(dplyr)
str(coact)
coact$id <- as.character(coact$id)
coact$treatment <- ordered(coact$treatment, levels=c("Control","Beetle","Mantid"))


shapiro.test(coact$activityIndex) #p-value > 0.05 implying that the data is not significantly different from normal dist. AKA assume normality.
#http://www.sthda.com/english/wiki/normality-test-in-r


# Compute the analysis of variance
res.aov <- aov(activityIndex ~ treatment, data = coact)
# Summary of the analysis
summary(res.aov)
#           Df Sum Sq Mean Sq F value Pr(>F)
#treatment    2   1328   663.9   0.829  0.467  #not significant
#Residuals    9   7210   801.1  


##Plot mean with SE
AImean <- by(coact$activityIndex, coact$treatment, mean)
AIsd <- by(coact$activityIndex, coact$treatment, sd)
AIn <- summary(coact$treatment) # Sample sizes
AISE <- AIsd / sqrt(AIn)
library(plotrix)
par(mar=c(5,7,4,2)+0.1)
plotCI(y=AImean, x=1:3, uiw=AISE, err='y', xaxt='n', xlab="", ylab="Activity Index",cex=1.5,pch=15,pt.bg="black",ylim=c(1600,1800))
axis(1, at=1:3, labels=levels(coact$treatment), las=1)
title(main="Baseline Activity Index")


#Boxplot
library(ggplot2)
ggplot(coact, aes(x=treatment, y=activityIndex)) + 
    geom_boxplot() + ylab("Activity Index") + xlab("Treatment") +
	ggtitle("Baseline Activity Index") + theme_light() +
	theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
	legend.title = element_text(size=11, face="bold"), 
	legend.text = element_text(size = 11),
	axis.text.x = element_text(size = 13),
	axis.text.y = element_text(size = 13),
	axis.title.x = element_text(size=13),
	axis.title.y = element_text(size=13)) +
	#scale_y_continuous(limits = c(1600,1750),expand = c(0,0))+
  stat_summary(fun = mean, geom = "point", shape=4, col = "black", position=position_dodge(0.75)) +theme_light() #+ 

