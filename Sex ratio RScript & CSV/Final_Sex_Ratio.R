library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

sex<-read.csv("~\Survivors & Sex Ratio\\Survivorship_NL.csv", na.strings= c('#DIV/0!', '', 'NA'),stringsAsFactors = F)
head(sex)
sex$Replicate <- as.factor(sex$Replicate)
sex$Generation <- as.factor(sex$Generation)


              
####ALL GENERATIONS####

control<-ggplot(sex, aes(x=Generation, y=C.M.F, group=Replicate)) + 
        scale_y_continuous(name="Control Survivors", limits=c(0, 2)) + geom_line(aes(color=Replicate)) +  geom_point(aes(color=Replicate)) + theme_classic()
treatment<-ggplot(sex, aes(x=Generation, y=T.M.F, group=Replicate)) + 
      scale_y_continuous(name="Treatment Survivors", breaks = seq(0, 3, by = 0.5), limits=c(0, 3)) + geom_line(aes(color=Replicate)) +  geom_point(aes(color=Replicate)) + theme_classic()

grid.arrange(control, treatment, nrow=2, top=textGrob("Sex Ratio with All Generations", gp=gpar(fontface="bold")))

####G0510####

G0510 <- filter(sex, Generation %in% c("0","5","10"))

control<-ggplot(G0510, aes(x=Generation, y=C.M.F, group=Replicate)) + 
      scale_y_continuous(name="Control Survivors", limits=c(0, 2)) +   scale_x_discrete(expand = c(0.05, 0.05)) + geom_line(aes(color=Replicate)) +  geom_point(aes(color=Replicate))  + theme_classic()
treatment<-ggplot(G0510, aes(x=Generation, y=T.M.F, group=Replicate)) + 
     scale_y_continuous(name="Treatment Survivors", breaks = seq(0, 3, by = 0.5), limits=c(0, 3)) + scale_x_discrete(expand = c(0.05, 0.05)) + geom_line(aes(color=Replicate)) +  geom_point(aes(color=Replicate))  + theme_classic()

grid.arrange(control, treatment, nrow=2, top=textGrob("Sex Ratio G0510", gp=gpar(fontface="bold")))

