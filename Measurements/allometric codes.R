library(dplyr)
library(ggplot2)

###################################### Body size vs Testes Volume ######################################

data<-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\allometric plots\\MTestesVolumeOnly_G0510_addTreatmentcol.csv")

#### G0510 ####
ggplot(data=data,aes(x=logbs,y=logtestes,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (testes volume (mm))") +
  ylim(-0.96, -0.65) + xlim(-0.19, 0)+
  ggtitle("G0510 Body Size against Testes Volume") +theme_light()



#### Plot G5 ( + baseline G0 for comparison) only ####
G5 <- filter(data, Generation < 10)

##allometric plot
testesG5 <- ggplot(data=G5,aes(x=logbs,y=logtestes,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (testes volume (mm))") +
  ylim(-0.96, -0.65) + xlim(-0.19, 0)+
  ggtitle("G5 Body Size against Testes Volume") +
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.776, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

G5B <- filter(G5, Treatment=="Baseline population")	
G5RS <- filter(G5, Treatment=="Relaxed Selection")	
G5S <- filter(G5, Treatment=="Selection")	

shapiro.test(G5B$logtestes) #p-value = 0.08192 aka NORMAL
shapiro.test(G5RS$logtestes) #p-value = 0.02464 aka NOT NORMAL
shapiro.test(G5S$logtestes) #p-value = 0.02464 aka NOT NORMAL


#models
model1<-lm(logtestes~logbs,subset=(Treatment=="Baseline population"),data=G5)
par(mfrow=c(2,2))
plot(model1)
summary(model1)
###y = 0.91607x + -0.72289 
## R^2=0.2294 
##p-value: 9.722e-05

model2<-lm(logtestes~logbs,subset=(Treatment=="Relaxed Selection"),data=G5)
par(mfrow=c(2,2))
plot(model2)
summary(model2)
###y = 1.96865x + -0.62062 
## R^2=0.4464
## p-value: 6.134e-05

model3<-lm(logtestes~logbs,subset=(Treatment=="Selection"),data=G5)
par(mfrow=c(2,2))
plot(model3)
summary(model3)
###y = 0.98932x + -0.69505 
## R^2=0.4031  
##p-value: 1.69e-06



#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model1)
#                 2.5 %     97.5 %
#(Intercept) -0.7589222 -0.6868609
#logbs        0.4793803  1.3527696

confint(model2)
#               2.5 %     97.5 %
#(Intercept) -0.6746668 -0.5665672
#logbs        1.1206658  2.8166405

confint(model3)
#                 2.5 %    97.5 %
#(Intercept) -0.7191119 -0.670980
#logbs        0.6292953  1.349353



#### Plot G10 ( + baseline G0 for comparison) only ####
target <- c("0", "10")
G10 <- filter(data, Generation  %in% target)

##allometric plot
testesG10<- ggplot(data=G10,aes(x=logbs,y=logtestes,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (testes volume (mm))") +
  ylim(-0.96, -0.65) + xlim(-0.19, 0)+
  ggtitle("G10 Body Size against Testes Volume") +theme_light()+
  geom_abline(intercept =-0.776, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

##Combined testes plots
library(cowplot)
plot_grid(testesG5, testesG10)

G10B <- filter(G10, Treatment=="Baseline population")	
G10RS <- filter(G10, Treatment=="Relaxed Selection")	
G10S <- filter(G10, Treatment=="Selection")	

shapiro.test(G10B$logtestes) #p-value = 0.08192 aka NORMAL
shapiro.test(G10RS$logtestes) #p-value = 0.06978 aka NORMAL
shapiro.test(G10S$logtestes) #p-value = 0.6237 aka NORMAL


#models
model4<-lm(logtestes~logbs,subset=(Treatment=="Baseline population"),data=G10)
par(mfrow=c(2,2))
plot(model4)
summary(model4)
###y = 0.91607x + -0.72289 
## R^2=0.2294 
##p-value: 9.722e-05

model5<-lm(logtestes~logbs,subset=(Treatment=="Relaxed Selection"),data=G10)
par(mfrow=c(2,2))
plot(model5)
summary(model5)
###y = 0.28046x + -0.73484 
## R^2=-0.0008365  
##p-value: 0.3322

model6<-lm(logtestes~logbs,subset=(Treatment=="Selection"),data=G10)
par(mfrow=c(2,2))
plot(model6)
summary(model6)
###y = 0.67658 x + -0.74920
## R^2=0.1804 
## p-value: 0.003677

#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model4)
#                 2.5 %     97.5 %
#(Intercept) -0.7589222 -0.6868609
#logbs        0.4793803  1.3527696

confint(model5)
#               2.5 %     97.5 %
#(Intercept) -0.7695945 -0.7000946
#logbs       -0.2954192  0.8563320

confint(model6)
#                 2.5 %    97.5 %
#(Intercept) -0.7968374 -0.7015568
#logbs        0.2341528  1.1190166


###################################### Body size vs Sperm Length ######################################

data<-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\allometric plots\\MSpermLengthOnly_G0510_addTreatmentcol.csv")

data$Treatment <- ordered(data$Treatment, levels=c("Baseline population","Relaxed Selection","Selection"))
levels(data$Treatment)[levels(data$Treatment) == "Baseline population"]  <- "Baseline"
levels(data$Treatment)[levels(data$Treatment) == "Relaxed Selection"]  <- "Control"
levels(data$Treatment)[levels(data$Treatment) == "Selection"]  <- "Treatment"

#### G0510 ####
ggplot(data=data,aes(x=logbs,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (sperm length (µm))") +
  ylim(2.4, 2.5145) + xlim(-0.19, 0) +
  ggtitle("G0510 Body Size against Sperm length") +theme_light()


#### Plot G5 ( + baseline G0 for comparison) only ####
G5 <- filter(data, Generation < 10)

##allometric plot
spermG5 <-  ggplot(data=G5,aes(x=logbs,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (sperm length (µm))") + 
  ylim(2.4, 2.5145) + xlim(-0.19, 0) +
  ggtitle("G5 Body Size against Sperm length") +theme_light()+
  geom_abline(intercept =2.593, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.778, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position ="none")


G5B <- filter(G5, Treatment=="Baseline population")	
G5RS <- filter(G5, Treatment=="Relaxed Selection")	
G5S <- filter(G5, Treatment=="Selection")	

shapiro.test(G5B$logsperm) #p-value = 0.8593 aka NORMAL
shapiro.test(G5RS$logsperm) #p-value = 0.01976 aka NOT NORMAL
shapiro.test(G5S$logsperm) #p-value = 0.0619 aka NORMAL

##models
model7<-lm(logsperm~logbs,subset=(Treatment=="Baseline population"),data=G5)
par(mfrow=c(2,2))
plot(model7)
summary(model7)
##y= -0.078710x + 2.461427
##R^2= -0.0007342 
##p-value: 0.3314

model8<-lm(logsperm~logbs,subset=(Treatment=="Relaxed Selection"),data=G5)
par(mfrow=c(2,2))
plot(model8)
summary(model8)
##y= -0.06207x + 2.46681
##R^2= -0.03535  
##p-value: 0.7053

#point79 looks like outlier in QQ, does removing make a difference?: 
#model2a<-lm(logsperm~logbs,subset=(Treatment=="Relaxed Selection"),data=G5[-c(79),])
#par(mfrow=c(2,2))
#plot(model2a)
#summary(model2a)
##y= -0.044851x + 2.469858
##R^2= -0.03735 

#not much diff. R2 values only increased abit: -0.03535 to -0.03735. Decided to keep point79

model9<-lm(logsperm~logbs,subset=(Treatment=="Selection"),data=G5)
par(mfrow=c(2,2))
plot(model9)
summary(model9)
###y = 0.346470x + 2.480163 
## R^2=0.4215
##p-value: 8.455e-07

#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model7)
#                 2.5 %     97.5 %
#(Intercept)  2.4479581 2.47489680
#logbs       -0.2398308 0.08241051

confint(model8)
#               2.5 %     97.5 %
#(Intercept)  2.4453527 2.488273
#logbs       -0.3967273 0.272597

confint(model9)
#                 2.5 %    97.5 %
#(Intercept) 2.4720390 2.4882862
#logbs       0.2249399 0.4680009



#### Plot G10 ( + baseline G0 for comparison) only ####
target <- c("0", "10")
G10 <- filter(data, Generation  %in% target)

##allometric plot
spermG10 <- ggplot(data=G10,aes(x=logbs,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (sperm length (µm))") + 
  ylim(2.4, 2.5145) + xlim(-0.19, 0) +
  ggtitle("G10 Body Size against Sperm length") +theme_light()+
  geom_abline(intercept =2.593, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.778, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none")


##Combined sperm plots
library(cowplot)
plot_grid(spermG5, spermG10)

G10B <- filter(G10, Treatment=="Baseline population")	
G10RS <- filter(G10, Treatment=="Relaxed Selection")	
G10S <- filter(G10, Treatment=="Selection")	

shapiro.test(G10B$logsperm) #p-value = 0.8593 aka NORMAL
shapiro.test(G10RS$logsperm) #p-value = 0.03772 aka NOT NORMAL
shapiro.test(G10S$logsperm) #p-value = 0.1793 aka NORMAL


##models
model10<-lm(logsperm~logbs,subset=(Treatment=="Baseline population"),data=G10)
par(mfrow=c(2,2))
plot(model10)
summary(model10)
##y= -0.078710x + 2.461427
##R^2= -0.0007342 
## p-value: 0.3314

model11<-lm(logsperm~logbs,subset=(Treatment=="Relaxed Selection"),data=G10)
par(mfrow=c(2,2))
plot(model11)
summary(model11)
##y= -0.112848x + 2.447457
##R^2= -0.01455   
##p-value: 0.5109

model12<-lm(logsperm~logbs,subset=(Treatment=="Selection"),data=G10)
plot(model12)
summary(model12)
###y = 0.277865x + 2.478935
## R^2=0.218
##p-value: 0.002101

#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model10)
#                 2.5 %     97.5 %
#(Intercept)  2.4479581 2.47489680
#logbs       -0.2398308 0.08241051

confint(model11)
#               2.5 %     97.5 %
#(Intercept)  2.4278441 2.4670709
#logbs       -0.4570278 0.2313312

confint(model12)
#                 2.5 %    97.5 %
#(Intercept) 2.4604220 2.4974470
#logbs       0.1080568 0.4476722




###################################### Testes Volume vs Sperm Length ######################################

data<-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\allometric plots\\MSpermLengthOnly_G0510_addTreatmentcol.csv")

#### G0510 ####
ggplot(data=data,aes(x=logtestes,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (testes volume (mm))") + ylab("log (sperm length (µm))") +
  ggtitle("G0510 Testes Volume against Sperm length") +theme_light()


#### Plot G5 ( + baseline G0 for comparison) only ####
G5 <- filter(data, Generation < 10)

##allometric plot
ggplot(data=G5,aes(x=logtestes,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (testes volume (mm))") + ylab("log (sperm length (µm))") + 
  ggtitle("G5 Testes Volume against Sperm length") +theme_light()+
  #ylim(2.3, 2.5) +
  geom_abline(intercept =3.375, slope = 1, color="gray55",linetype="dashed")

G5B <- filter(G5, Treatment=="Baseline population")	
G5RS <- filter(G5, Treatment=="Relaxed Selection")	
G5S <- filter(G5, Treatment=="Selection")	

##models
model13<-lm(logsperm~logtestes,subset=(Treatment=="Baseline population"),data=G5)
par(mfrow=c(2,2))
plot(model13)
summary(model13)
##y= 0.03241x + 2.49298 
##R^2= -0.008885   
##p-value: 0.465

model14<-lm(logsperm~logtestes,subset=(Treatment=="Relaxed Selection"),data=G5)
par(mfrow=c(2,2))
plot(model14)
summary(model14)
##y= -0.05132x + 2.43241
##R^2= -0.007904  
##p-value: 0.3788

model15<-lm(logsperm~logtestes,subset=(Treatment=="Selection"),data=G5)
par(mfrow=c(2,2))
plot(model15)
summary(model15)
###y = 0.12407x + 2.55402 
## R^2= 0.1107 
##p-value: 0.01459

#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model13)
#                 2.5 %     97.5 %
#(Intercept)  2.42275370 2.5632112
#logtestes   -0.05595839 0.1207716

confint(model14)
#               2.5 %     97.5 %
#(Intercept)  2.3442509 2.52057861
#logtestes   -0.1694542 0.06681146

confint(model15)
#                 2.5 %    97.5 %
#(Intercept) 2.48009083 2.6279433
#logtestes   0.02576828 0.2223802



#### Plot G10 ( + baseline G0 for comparison) only ####
target <- c("0", "10")
G10 <- filter(data, Generation  %in% target)

##allometric plot
ggplot(data=G10,aes(x=logtestes,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (testes volume (mm))") + ylab("log (sperm length (µm))") + 
  ggtitle("G10 Testes Volume against Sperm length") +theme_light()+
  #ylim(2.3, 2.5) +
  geom_abline(intercept =3.365, slope = 1, color="gray55",linetype="dashed")


##models
model16<-lm(logsperm~logtestes,subset=(Treatment=="Baseline population"),data=G10)
par(mfrow=c(2,2))
plot(model16)
summary(model16)
##y=  0.03241x + 2.49298
##R^2= -0.008885 
## p-value: 0.465

model17<-lm(logsperm~logtestes,subset=(Treatment=="Relaxed Selection"),data=G10)
par(mfrow=c(2,2))
plot(model17)
summary(model17)
##y=  0.20208x + 2.60460 
##R^2= 0.1595   
##p-value: 0.006199

model18<-lm(logsperm~logtestes,subset=(Treatment=="Selection"),data=G10)
plot(model18)
summary(model18)
###y = 0.17365x + 2.59203
## R^2=0.1926 
##p-value: 0.003847

#find CI to see whether significant. if CI for model no 0, means allometry significant for that population. doesnt overlap--> sig pop level differenece.
confint(model16)
#                 2.5 %     97.5 %
#(Intercept)  2.42275370 2.5632112
#logtestes   -0.05595839 0.1207716

confint(model17)
#               2.5 %     97.5 %
#(Intercept)  2.49888028 2.7103195
#logtestes   0.06092889 0.3432221

confint(model18)
#                 2.5 %    97.5 %
#(Intercept) 2.499127 2.6849272
#logtestes   0.059786 0.2875073



#### G0510 testing base relationship####
ggplot(data=data,aes(x=cbrtestes,y=avgss,colour=Treatment))+ geom_point(shape=19)+ geom_smooth(se=F)+ xlab("testes size (mm)") + ylab("sperm length (µm)") +
  ggtitle("G0510 Body Size against Sperm length") +theme_light()


#### Plot G5 ( + baseline G0 for comparison) only ####
G5 <- filter(data, Generation < 10)

##allometric plot
ggplot(data=G5,aes(x=cbrtestes,y=avgss, colour=Treatment))+ geom_point(shape=19)+ geom_smooth(se=F)+ xlab("testes volume (mm)") + ylab("sperm length (µm)") +
  ggtitle("G5 Testes Volume against Sperm length") +theme_light()+
  #ylim(2.3, 2.5) +
  geom_abline(intercept =3.375, slope = 1, color="gray55",linetype="dashed")

#### Plot G10 ( + baseline G0 for comparison) only ####
target <- c("0", "10")
G10 <- filter(data, Generation  %in% target)

##allometric plot
ggplot(data=G10,aes(x=cbrtestes,y=avgss,colour=Treatment))+ geom_point(shape=19)+ geom_smooth(se=F)+ xlab("testes volume (mm)") + ylab("sperm length (µm)") +
  ggtitle("G10 Testes Volume against Sperm length") +theme_light()+
  #ylim(2.3, 2.5) +
  geom_abline(intercept =3.365, slope = 1, color="gray55",linetype="dashed")




###################################### Replicates ######################################

#### Body size vs Testes Volume ####

repdata<-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\allometric plots\\MTestesVolumeOnly_G0510_addTreatmentcol.csv")
repdata <- filter(repdata, replicate != 0)

## Plot G5 ( + baseline G0 for comparison) only
repG5 <- filter(repdata, Generation < 10)

testesrepG5 <- ggplot(data=repG5,aes(x=logbs,y=logtestes,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (testes volume (mm))") +
  facet_wrap(~ replicate) +
  ggtitle("G5 Body Size against Testes Volume") +theme_light()+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.82, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")
#rep 4 only has 1 control male with testes measured

## Plot G10 ( + baseline G0 for comparison) only
reptarget <- c("0", "10")
repG10 <- filter(repdata, Generation  %in% reptarget)

testesrepG10 <- ggplot(data=repG10,aes(x=logbs,y=logtestes,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (testes volume (mm))") +
  facet_wrap(~ replicate) +
  ggtitle("G10 Body Size against Testes Volume") +theme_light()+
  geom_abline(intercept =-0.776, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

##Combined testes plots
library(cowplot)
plot_grid(testesrepG5, testesrepG10)  


#### Body size vs Sperm Length ####
repdata<-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\allometric plots\\MSpermLengthOnly_G0510_addTreatmentcol.csv")
repdata <- filter(repdata, replicate != 0)

## Plot G5 ( + baseline G0 for comparison) only
repG5 <- filter(repdata, Generation < 10)

spermrepG5 <-  ggplot(data=repG5,aes(x=logbs,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (sperm length (µm))") + 
  ylim(2.4, 2.5145) + xlim(-0.19, 0) +
  facet_wrap(~ replicate) +
  ggtitle("G5 Body Size against Sperm length") +theme_light()+
  geom_abline(intercept =2.593, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.778, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

## Plot G10 ( + baseline G0 for comparison) only
reptarget <- c("0", "10")
repG10 <- filter(repdata, Generation  %in% reptarget)

spermrepG10 <- ggplot(data=repG10,aes(x=logbs,y=logsperm,colour=Treatment))+ geom_point(shape=19)+ stat_smooth(method=lm,level=0)+ xlab("log (body size (mm))") + ylab("log (sperm length (µm))") + 
  ylim(2.4, 2.5145) + xlim(-0.19, 0) +
  facet_wrap(~ replicate) +
  ggtitle("G10 Body Size against Sperm length") +theme_light()+
  geom_abline(intercept =2.593, slope = 1, color="gray55",linetype="dashed")+
  scale_color_manual(values = c("#000000", "#FF6600", "#3399CC")) +
  geom_abline(intercept =-0.778, slope = 1, color="gray55",linetype="dashed") + 
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill =NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

##Combined sperm plots
library(cowplot)
plot_grid(spermrepG5, spermrepG10)