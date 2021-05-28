#Import Data
file.choose()
data <-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\Behaviour tests\\new_modeling_courtshipMST(issame).csv",header=T)
data<-read.csv("D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\csv data\\new_modeling_courtship.csv",header=T)
head(data)
str(data)
data$rep<-as.factor(data$rep)
# data$gen<-ordered(data$gen,levels=c("baseline","mid","end")) (shouldn't use. messes up model summary terms)

library(lme4)
maxmodel<-glmer(courtshipnumber~gen*treatment*selection + (1|rep),data=data,family=poisson)
summary(maxmodel)

##accounting for overdispersion 
maxmodel2<-glmer.nb(courtshipnumber~gen*treatment*selection + (1|rep),data=data)
summary(maxmodel2)
install.packages("RVAideMemoire")
library(RVAideMemoire)
overdisp.glmer(maxmodel2)

m3<-update(maxmodel2,~.-gen:treatment:selection)
summary(m3)
anova(maxmodel2,m3)
m4<-update(m3,~.-treatment:selection)
summary(m4)
anova(m3,m4)
#####courtshipnumber ~ gen + treatment + selection + (1 | rep) + gen:treatment +  gen:selection

#########################################################-----QH-------############################################################


library(MASS)
m1 <- glmer.nb(courtshipnumber~gen + (1|rep),data=data)
m2 <- glmer.nb(courtshipnumber~treatment + (1|rep),data=data)
m3 <- glmer.nb(courtshipnumber~selection + (1|rep),data=data)
m4 <- glmer.nb(courtshipnumber~gen+treatment+selection + (1|rep),data=data)
m5 <- glmer.nb(courtshipnumber~gen*treatment*selection + (1|rep),data=data) #missing from QH's AICtable
m6 <- glmer.nb(courtshipnumber~gen+treatment + (1|rep),data=data)
m7 <- glmer.nb(courtshipnumber~treatment+selection + (1|rep),data=data)
m8 <- glmer.nb(courtshipnumber~gen+selection + (1|rep),data=data)
m9 <- glmer.nb(courtshipnumber~gen*treatment + (1|rep),data=data)
m10 <- glmer.nb(courtshipnumber~treatment*selection + (1|rep),data=data)
m11 <- glmer.nb(courtshipnumber~gen*selection + (1|rep),data=data)
mNULL <- glmer.nb(courtshipnumber~1 + (1|rep),data=data)

library(HDInterval)
library(wiqid)
library(MuMIn)

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,mNULL)
AIC(m1,m2,m3,m4, m5, m6,m7,m8,m9,m10,m11,mNULL)
AICtable(AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,mNULL))

summary(m11)
plot(m11)
qqnorm(resid(m11))
###checking for overdispersion
install.packages("blmeco")
library(blmeco)
dispersion_glmer(m11)

# blmco dispersion_glmer function appears to be outdated https://stackoverflow.com/questions/22842017/model-checking-and-test-of-overdispersion-for-glmer/28672386
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(m11)

plot(fitted(m11),resid(m11))

#      df      AIC    Delta ModelLik ModelWt
#m5    11 1479.966    0.000        1       1
#m11    7 1606.959  126.993        0       0
#m9     7 2357.740  877.774        0       0
#m8     5 2578.134 1098.168        0       0
#m4     6 2578.700 1098.734        0       0
#m1     4 2579.235 1099.269        0       0
#m6     5 2579.576 1099.610        0       0
#m10    5 3765.503 2285.537        0       0
#m3     3 4059.974 2580.008        0       0
#m7     4 4061.061 2581.095        0       0
#mNULL  2 4061.074 2581.108        0       0
#m2     3 4061.415 2581.449        0       0

summary(m5)
#											 Estimate Std. Error z value	Pr(<|z|)
#genend:treatmentno insect                    -2.15136    0.14041 -15.322	<2e-16 ***
#genmid:treatmentno insect                    -1.71949    0.13289 -12.939	<2e-16 ***
#genend:selectionpredation                    -1.40722    0.07666 -18.358	<2e-16 ***

####interaction results tell me what explains my result. 
###genend:selectionpredation tells me that end generation with predation explains the courtship im seeing. 

confint(m5)
												 2.5 %       97.5 %
#genend:treatmentno insect                    -2.4301821494 -1.879667975
#genmid:treatmentno insect                    -1.9822516891 -1.464508493
#genend:selectionpredation                    -1.5582466114 -1.258497885


### Kruskal Wallis	
file.choose()
cs <- read.csv("C:\\Users\\dbshqh\\Documents\\ReproLab NUS\\Project Data\\2018-19\\Pamela\\Post-UROPS\\Data Files\\courtship_combinedCSV.csv")
cs <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\Behaviour tests\\courtship_combinedCSV.csv")
library(dplyr)
str(cs)
cs$rep <- as.factor(cs$rep)
View(cs)

#### Test across Generations ####
##		Relaxed Selection 		##
csRS <- filter(cs, Type == "RS")
shapiro.test(csRS$CourtshipNumber)
csS <- filter(cs, Type == "S")
shapiro.test(csS$CourtshipNumber)

# 		---Empty Vial---
csRSEV <- filter(cs, Type == "RS", Treatment == "Empty Vial")		
View(csRSEV)
pairwise.wilcox.test(csRSEV$CourtshipNumber, csRSEV$Generation)
#	          After 10th After 5th
#	After 5th 0.40       -        
#	Parental  0.17       0.49 

# 		---Mantis Vial---
csRSMV <- filter(cs, Type == "RS", Treatment == "Mantis Vial")		
View(csRSMV)
pairwise.wilcox.test(csRSMV$CourtshipNumber, csRSMV$Generation)
#	          After 10th After 5th
#	After 5th 0.114      -        
#	Parental  0.086      0.114 


##			Selection 			##
# 		---Empty Vial---
csSEV <- filter(cs, Type == "S", Treatment == "Empty Vial")		
View(csSEV)
shapiro.test(csSEV$CourtshipNumber)

pairwise.wilcox.test(csSEV$CourtshipNumber, csSEV$Generation)
#	          After 10th After 5th
#	After 5th 0.40       -        
#	Parental  0.40       0.17   


# 		---Mantis Vial---
csSMV <- filter(cs, Type == "S", Treatment == "Mantis Vial")		
View(csSMV)
shapiro.test(csSMV$CourtshipNumber)
pairwise.wilcox.test(csSMV$CourtshipNumber, csSMV$Generation)
#	          After 10th After 5th
#	After 5th 0.886      -        
#	Parental  0.491      0.086   


#### Test BETWEEN PREDATION EXPERIMENT GROUPS ####
##		Relaxed Selection 		##
# 		---Parental---
csRSP <- filter(cs, Type == "RS", Generation == "Parental")		
View(csRSP)
pairwise.wilcox.test(csRSP$CourtshipNumber, csRSP$Treatment)
#            Empty Vial
#Mantis Vial 0.057

# 		---After 5th---
csRS5 <- filter(cs, Type == "RS", Generation == "After 5th")		
View(csRS5)
pairwise.wilcox.test(csRS5$CourtshipNumber, csRS5$Treatment)
#            Empty Vial
#Mantis Vial 0.89

# 		---After 10th---
csRS10 <- filter(cs, Type == "RS", Generation == "After 10th")		
View(csRS10)
pairwise.wilcox.test(csRS10$CourtshipNumber, csRS10$Treatment)
#            Empty Vial
#Mantis Vial 0.69

##			Selection 			##
# 		---Parental---
csSP <- filter(cs, Type == "S", Generation == "Parental")		
View(csSP)
pairwise.wilcox.test(csSP$CourtshipNumber, csSP$Treatment)
#            Empty Vial
#Mantis Vial 0.057 


# 		---After 5th---
csS5 <- filter(cs, Type == "S", Generation == "After 5th")		
View(csS5)
pairwise.wilcox.test(csS5$CourtshipNumber, csS5$Treatment)
#            Empty Vial
#Mantis Vial 0.49

# 		---After 10th---
csS10 <- filter(cs, Type == "S", Generation == "After 10th")		
View(csS10)
pairwise.wilcox.test(csS10$CourtshipNumber, csS10$Treatment)
#            Empty Vial
#Mantis Vial 1


#####################################
####for prediction
#################

library(ggplot2)
install.packages("emmeans")
library(emmeans)
install.packages("viridis")
library(viridis)
emmeans(m11,~gen*selection,type="response")
emmeans(m11,pairwise~gen*selection,type="response") 
#results
cols<-viridis(6,begin=0.3,end=0.9)
pr <- emmeans::emmeans(m11,~gen*selection,type="response")
g0<-ggplot(summary(pr), 
       aes(gen, response,fill=selection, ymin = asymp.LCL, ymax = asymp.UCL,color=selection)) +
	    geom_pointrange(position=position_dodge(1))
g0	   


g0+ theme_bw()+
theme(panel.border = element_blank(), panel.grid.major = element_blank(),legend.position="none",
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
axis.text.y = element_text(color="black"),
axis.text.x = element_text(color="black")) + 
labs(y = "Least-square mean frequency of visitors on flowers") + labs(x = "Flower-visitors")
