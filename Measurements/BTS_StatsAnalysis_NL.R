#Load packages
library(dplyr)
library(MASS)
library(lmerTest)
library(HDInterval)
library(wiqid)

#### G0 G5 G10 only ####
#--------Body Size------------
#cleaned up "D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\Manuscript\\Data Files\\full_model_bs.csv" -> "MBodysize Only_G0510.csv"
BS<- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\BS Significance\\Bodysize Only_G0510.csv",na.strings = "NA",header=T)

#firstly, test for normality
shapiro.test(BS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Since data not normal, use Kruskal Wallis	to test if there is significant differences between groups
kruskal.test(bs ~ Generation, data = BS) #p-value less than sig 0.05, can conclude sig diffs between groups.

#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RSbs <- filter(BS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
View(RSbs)
pairwise.wilcox.test(RSbs$bs, RSbs$Generation)
#   0       5    
#5  0.033   -    
#10 1.1e-05 0.033

Sbs <- filter(BS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
View(Sbs)
pairwise.wilcox.test(Sbs$bs, Sbs$Generation)
#   0       5      
#5  0.69    -      
#10 3.1e-05 4.3e-09

#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0bs <- filter(BS,  Generation == "0")		#testing only G0
pairwise.wilcox.test(G0bs$bs, G0bs$SelectionStatus)
#  RS    
#S 0.0028

G5bs <- filter(BS,   Generation == "5")		#testing only G5
pairwise.wilcox.test(G5bs$bs, G5bs$SelectionStatus)
#  RS  
#S 0.04

G10bs <- filter(BS,  Generation == "10")		#testing only G10
pairwise.wilcox.test(G10bs$bs, G10bs$SelectionStatus)
#  RS     
#S 1.9e-11

#3rd, sort by both GENERATION & SELECTION STATUS. (testing if there are differences btwn sexes)
G0bsRS <- filter(G0bs, SelectionStatus == "RS")		#testing only G0 RS
pairwise.wilcox.test(G0bsRS$bs, G0bsRS$sex)
#   female
#male 0.01

G0bsS <- filter(G0bs, SelectionStatus == "S")		#testing only G0 S
pairwise.wilcox.test(G0bsS$bs, G0bsS$sex)
#   female
#male 0.23 

G5bsRS <- filter(G5bs, SelectionStatus == "RS")		#testing only G5 RS
pairwise.wilcox.test(G5bsRS$bs, G5bsRS$sex)
#   female
#male 3e-05

G5bsS <- filter(G5bs, SelectionStatus == "S")		#testing only G5 S
pairwise.wilcox.test(G5bsS$bs, G5bsS$sex)
#   female
#male 0.042

G10bsRS <- filter(G10bs, SelectionStatus == "RS")		#testing only G10 RS
pairwise.wilcox.test(G10bsRS$bs, G10bsRS$sex)
#   female
#male 0.0015

G10bsS <- filter(G10bs, SelectionStatus == "S")		#testing only G10 S
pairwise.wilcox.test(G10bsS$bs, G10bsS$sex)
#   female
#male 0.0018 

####Model####
str(BS)
BS$Generation <- as.factor(BS$Generation)
BS$replicate <- as.factor(BS$replicate)
BS$id <- as.character(BS$id)
View(BS)


bs1 <- lmer(bs~Generation*SelectionStatus + (1|replicate),data=BS)
bs2 <- lmer(bs~Generation + (1|replicate),data=BS)
bs3 <- lmer(bs~SelectionStatus + (1|replicate),data=BS)
bs4 <- lmer(bs~Generation + SelectionStatus + (1|replicate),data=BS)
bsNULL <- lmer(bs~1 + (1|replicate),data=BS)

AIC(bs1,bs2,bs3,bs4,bsNULL)
AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))

qqnorm(resid(bs1)) #https://stats.stackexchange.com/questions/77891/checking-assumptions-lmer-lme-mixed-models-in-r
qqline(resid(bs1))
plot(bs1)
summary(bs1) #all significant

#################################################################################

Random effects:
    Groups    Name        Variance  Std.Dev.
replicate (Intercept) 0.0004679 0.02163 
Residual              0.0041764 0.06463 
Number of obs: 525, groups:  replicate, 6

Fixed effects:
    Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)                     0.848084   0.011158  10.331104  76.006 1.54e-15 ***
    Generation5                     0.023878   0.010510 498.110443   2.272   0.0235 *  
    Generation10                    0.046699   0.009846 491.194180   4.743 2.77e-06 ***
    SelectionStatusS                0.052378   0.013346 514.971455   3.925 9.86e-05 ***
    Generation5:SelectionStatusS   -0.033339   0.016282 518.468827  -2.048   0.0411 *  
    Generation10:SelectionStatusS  -0.123227   0.016303 518.592859  -7.559 1.86e-13 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#################################################################################

#--------MBody Size------------

#cleaned up "D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\Manuscript\\Data Files\\full_model_bs.csv" -> "MBodysize Only_G0510.csv"
BSmod <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\BS Significance\\MBodysize Only_G0510.csv") 

shapiro.test(BSmod$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = BSmod) #p-value less than sig 0.05, can conclude sig diffs between groups.



#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RSbs <- filter(BSmod, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
View(RSbs)

pairwise.wilcox.test(RSbs$bs, RSbs$Generation)
#	   	0      5     
#	5  0.1839 -     
#	10 0.0012 0.0467

Sbs <- filter(BSmod, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
View(Sbs)

pairwise.wilcox.test(Sbs$bs, Sbs$Generation)
#	   0      5      
#	5  0.9758 -      
#	10 0.0059 1.7e-06


#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0bs <- filter(BSmod,  Generation == "0")		#testing only G0

pairwise.wilcox.test(G0bs$bs, G0bs$SelectionStatus)
#  RS  
#S 0.069

G5bs <- filter(BSmod,   Generation == "5")		#testing only G5

pairwise.wilcox.test(G5bs$bs, G5bs$SelectionStatus)
#  RS  
#S 0.017

G10bs <- filter(BSmod,  Generation == "10")		#testing only G10

pairwise.wilcox.test(G10bs$bs, G10bs$SelectionStatus)
#  RS  
#S 8.5e-08


####Model####
str(BSmod)
BSmod$Generation <- as.factor(BSmod$Generation)
BSmod$replicate <- as.factor(BSmod$replicate)
BSmod$id <- as.character(BSmod$id)
View(BSmod)


bs1 <- lmer(bs~Generation*SelectionStatus + (1|replicate),data=BSmod)
bs2 <- lmer(bs~Generation + (1|replicate),data=BSmod)
bs3 <- lmer(bs~SelectionStatus + (1|replicate),data=BSmod)
bs4 <- lmer(bs~Generation + SelectionStatus + (1|replicate),data=BSmod)
bsNULL <- lmer(bs~1 + (1|replicate),data=BSmod)

AIC(bs1,bs2,bs3,bs4,bsNULL)
AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))


qqnorm(resid(bs1)) #https://stats.stackexchange.com/questions/77891/checking-assumptions-lmer-lme-mixed-models-in-r
qqline(resid(bs1))
plot(bs1)
summary(bs1) #only G5 & G5:SelectionStatus not significant

#################################################################################
Random effects:
Groups    Name        Variance Std.Dev.
replicate (Intercept) 0.000448 0.02117 
Residual              0.003734 0.06111 
Number of obs: 259, groups:  replicate, 6

Fixed effects:
Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)                     0.82970    0.01244  14.32894  66.687  < 2e-16 ***
Generation5                     0.01954    0.01367 249.65380   1.429 0.154290    
Generation10                    0.04840    0.01293 247.79544   3.742 0.000227 ***
SelectionStatusS                0.04886    0.01854 252.93413   2.635 0.008936 ** 
Generation5:SelectionStatusS   -0.02191    0.02225 252.74778  -0.985 0.325708    
Generation10:SelectionStatusS  -0.13170    0.02257 252.74396  -5.834 1.65e-08 ***
 ---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
(Intr) Gnrtn5 Gnrt10 SlctSS G5:SSS
Generation5 -0.481                            
Generatin10 -0.510  0.514                     
SelctnSttsS -0.358  0.355  0.381              
Gnrtn5:SlSS  0.298 -0.611 -0.318 -0.832       
Gnrtn10:SSS  0.294 -0.291 -0.575 -0.822  0.683

################################################################################

#--------Testes Size------------
#cleaned up "D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\Manuscript\\Data Files\\full_model_avgts_updated.csv" -> "MTestesVolumeOnly_G0510.csv"
TVmod <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\TV Significance\\MTestesVolumeOnly_G0510.csv")

str(TVmod)
TVmod$Generation <- as.factor(TVmod$Generation)
TVmod$replicate <- as.factor(TVmod$replicate)
TVmod$id <- as.character(TVmod$id)

shapiro.test(TVmod$avgts) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r


#Kruskal Wallis	
kruskal.test(avgts ~ Generation, data = TVmod) #p-value less than sig 0.05, can conclude sig diffs between groups.

#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RStv <- filter(TVmod,  SelectionStatus == "RS")		#testing only Relaxed Selection Parental,5th and 10th gen
View(RStv)

pairwise.wilcox.test(RStv$avgts, RStv$Generation)
#	   	0      5     
#	5  0.0027 -     
#	10 0.0030 0.2294

Stv <- filter(TVmod,  SelectionStatus == "S")		#testing only Selection	Parental,5th and 10th gen
View(Stv)

pairwise.wilcox.test(Stv$avgts, Stv$Generation)
#	   0    5      
#	5  0.16 -      
#	10 0.35 4.5e-05

#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0tv <- filter(TVmod,  Generation == "0")		#testing only G0
View(G0tv)

pairwise.wilcox.test(G0tv$avgts, G0tv$SelectionStatus)
#  RS  
#S 0.88

G5tv <- filter(TVmod,  Generation == "5")		#testing only G5
View(G5tv)

pairwise.wilcox.test(G5tv$avgts, G5tv$SelectionStatus)
#  RS  
#S 0.54

G10tv <- filter(TVmod,  Generation == "10")		#testing only G10
View(G10tv)

pairwise.wilcox.test(G10tv$avgts, G10tv$SelectionStatus)
#  RS  
#S 9.4e-07



####Model####
str(TVmod)
TVmod$Generation <- as.factor(TVmod$Generation)
TVmod$replicate <- as.factor(TVmod$replicate)
TVmod$id <- as.character(TVmod$id)
View(TVmod)


tv1 <- lmer(avgts~Generation*SelectionStatus*bs + (1|replicate),data=TVmod)
tv2 <- lmer(avgts~Generation + (1|replicate),data=TVmod)
tv3 <- lmer(avgts~SelectionStatus + (1|replicate),data=TVmod)
tv4 <- lmer(avgts~bs + (1|replicate),data=TVmod)
tv5 <- lmer(avgts~bs*Generation + (1|replicate),data=TVmod)
tv6 <- lmer(avgts~bs*SelectionStatus + (1|replicate),data=TVmod)
tv7 <- lmer(avgts~SelectionStatus*Generation + (1|replicate),data=TVmod)
tv8 <- lmer(avgts~Generation + SelectionStatus + bs +(1|replicate),data=TVmod)
tvNULL <- lmer(avgts~1 + (1|replicate),data=TVmod)

AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL)
AICtable(AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL))	#tv4 is the best model

plot(tv4)
qqnorm(resid(tv4))
qqline(resid(tv4))
summary(tv4)  #bs is significant

################################################################################
Random effects:
Groups    Name        Variance  Std.Dev. 
replicate (Intercept) 4.020e-09 0.0000634
Residual              2.969e-06 0.0017231
Number of obs: 219, groups:  replicate, 6

Fixed effects:
Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)  -0.009786   0.001428 192.956034  -6.854  9.4e-11 ***
bs            0.017619   0.001663 194.521463  10.592  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
(Intr)
bs -0.996
################################################################################



#--------Sperm Length------------
SLmod <- read.csv("D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\Manuscript\\Data Files\\full_model_avgspermlength_updated.csv")

SLmod1 <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\SL Significance\\MSpermLengthOnly_G0510.csv")

str(SLmod1)
SLmod1$Generation <- as.factor(SLmod1$Generation)
SLmod1$replicate <- as.factor(SLmod1$replicate)
SLmod1$id <- as.character(SLmod1$id)

shapiro.test(SLmod1$avgss) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Kruskal Wallis	
kruskal.test(avgss ~ Generation, data = SLmod1) #p-value less than sig 0.05, can conclude sig diffs between groups.

#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RSsl <- filter(SLmod1, SelectionStatus == "RS")		#testing only Relaxed Selection Parental,5th and 10th gen
View(RSsl)

pairwise.wilcox.test(RSsl$avgss, RSsl$Generation)

#   0      5     
#5  0.9200 -     
#10 0.0021 0.0015


Ssl <- filter(SLmod1, SelectionStatus == "S")		#testing only Selection Parental,5th and 10th gen
View(Ssl)

pairwise.wilcox.test(Ssl$avgss, Ssl$Generation)
#	   0     5    
#	5  0.839 -    
#	10 0.489 0.092


#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0sl <- filter(SLmod1, Generation == "0")	#testing only G0

pairwise.wilcox.test(G0sl$avgss, G0sl$SelectionStatus)

#  RS  
#S 0.18


G5sl <- filter(SLmod1, Generation == "5")	#testing only G5

pairwise.wilcox.test(G5sl$avgss, G5sl$SelectionStatus)

#  RS  
#S 0.036

G10sl <- filter(SLmod1, Generation == "10")	#testing only G10

pairwise.wilcox.test(G10sl$avgss, G10sl$SelectionStatus)

#  RS  
#S 0.62

####Model####
str(SLmod1)
SLmod1$Generation <- as.factor(SLmod1$Generation)
SLmod1$replicate <- as.factor(SLmod1$replicate)
SLmod1$id <- as.character(SLmod1$id)
View(SLmod1)


sl1 <- lmer(avgss~Generation*SelectionStatus*bs + (1|replicate),data=SLmod1)
sl2 <- lmer(avgss~Generation + (1|replicate),data=SLmod1)
sl3 <- lmer(avgss~SelectionStatus + (1|replicate),data=SLmod1)
sl4 <- lmer(avgss~bs + (1|replicate),data=SLmod1)
sl5 <- lmer(avgss~Generation + SelectionStatus + (1|replicate),data=SLmod1)
sl6 <- lmer(avgss~bs + Generation + (1|replicate),data=SLmod1)
sl7 <- lmer(avgss~bs + SelectionStatus + (1|replicate),data=SLmod1)
sl8 <- lmer(avgss~bs*Generation + (1|replicate),data=SLmod1)
sl9 <- lmer(avgss~bs*SelectionStatus + (1|replicate),data=SLmod1)
sl10 <- lmer(avgss~SelectionStatus*Generation + (1|replicate),data=SLmod1)
sl11 <- lmer(avgss~Generation + SelectionStatus + bs +(1|replicate),data=SLmod1)
slNULL <- lmer(avgss~1 + (1|replicate),data=SLmod1)


sl12 <- lmer(avgss~Generation*SelectionStatus*bs*avgts + (1|replicate),data=SLmod1) ##removed. too many interactions. reduce to "+"

sl24 <-  lmer(avgss~Generation*SelectionStatus*bs + avgts + (1|replicate),data=SLmod1)
 
sl25 <- lmer(avgss~Generation + SelectionStatus*bs*avgts + (1|replicate),data=SLmod1)
 
sl26 <-lmer(avgss~Generation*bs*avgts + SelectionStatus + (1|replicate),data=SLmod1)
 
sl27 <-lmer(avgss~Generation*SelectionStatus*avgts + bs + (1|replicate),data=SLmod1)

sl13 <- lmer(avgss~Generation + SelectionStatus + bs + avgts + (1|replicate),data=SLmod1)
sl14	<-	lmer(avgss~SelectionStatus + bs + avgts + (1|replicate),data=SLmod1)
sl15	<-	lmer(avgss~bs + avgts + (1|replicate),data=SLmod1)
sl16	<-	lmer(avgss~avgts + (1|replicate),data=SLmod1)
sl17	<-	lmer(avgss~Generation + bs + avgts + (1|replicate),data=SLmod1)
sl18	<-	lmer(avgss~Generation + avgts + (1|replicate),data=SLmod1)
sl19	<-	lmer(avgss~bs*avgts + (1|replicate),data=SLmod1)
sl20	<-	lmer(avgss~SelectionStatus*bs*avgts + (1|replicate),data=SLmod1)
sl21	<-	lmer(avgss~SelectionStatus*avgts + (1|replicate),data=SLmod1)
sl22	<-	lmer(avgss~Generation*avgts + (1|replicate),data=SLmod1)
sl23	<-	lmer(avgss~avgts + (1|replicate),data=SLmod1)



AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, sl13, sl14, sl15, sl16, sl17, sl18, sl19, sl20, sl21, sl22, sl23, sl24, sl25, sl26, sl27, slNULL)
AICtable(AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, sl13, sl14, sl15, sl16, sl17, sl18, sl19, sl20, sl21, sl22, sl23,  sl24, sl25, sl26, sl27,slNULL))

plot(sl26)
qqnorm(resid(sl26))
qqline(resid(sl26))
summary(sl26)	#only intercept significant
summary(sl1)	#  significant

##################################################################################################################
Random effects:
Groups    Name        Variance Std.Dev.
replicate (Intercept)  26.9     5.186  
Residual              145.3    12.052  
Number of obs: 201, groups:  replicate, 6

Fixed effects:
Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)              374.705     52.346    187.245   7.158  1.8e-11 ***
Generation5             -191.656     76.109    187.038  -2.518  0.01263 *  
Generation10            -105.807     64.793    183.657  -1.633  0.10418    
bs                      -108.182     62.870    186.890  -1.721  0.08696 .  
avgts                  -6723.607   9340.025    185.358  -0.720  0.47251    
SelectionStatusS          -3.500      2.105    185.669  -1.663  0.09809 .  
Generation5:bs           238.432     89.761    187.090   2.656  0.00858 ** 
Generation10:bs          115.103     78.110    183.955   1.474  0.14230    
Generation5:avgts       9873.449  13213.581    186.113   0.747  0.45587    
Generation10:avgts      9736.203  13053.126    183.821   0.746  0.45669    
bs:avgts               10469.613  10723.640    185.661   0.976  0.33018    
Generation5:bs:avgts  -14662.025  14921.445    186.221  -0.983  0.32707    
Generation10:bs:avgts -11658.792  15032.132    183.781  -0.776  0.43899   
##################################################################################################################
