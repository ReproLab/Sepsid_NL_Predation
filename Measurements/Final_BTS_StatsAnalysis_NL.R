#Load packages
library(dplyr)
library(MASS)
library(lmerTest)
library(HDInterval)
library(wiqid)
library(rstatix)


#### G0 G5 G10 only ####
#--------Body Size------------

BS<- read.csv("~/Bodysize Only_G0510.csv",na.strings = "NA",header=T)

shapiro.test(BS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = BS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> use dunn_test() to calculate post hoc pairwise multiple comparison

#1st, sort by Selection Status (Group). To test differences between generations
RSbs <- filter(BS, SelectionStatus == "RS")
dunn_test(RSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic          p      p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
# 1 bs    0      5         91    76      2.32 0.0203     0.0406     *           
# 2 bs    0      10        91    99      4.70 0.00000263 0.00000788 ****        
# 3 bs    5      10        76    99      2.11 0.0350     0.0406     * 


Sbs <- filter(BS, SelectionStatus == "S")		
dunn_test(Sbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic             p         p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>         <dbl>         <dbl> <chr>       
#1 bs    0      5         34   133    -0.349 0.727         0.727         ns          
#2 bs    0      10        34    92    -4.41  0.0000105     0.0000209     ****        
#3 bs    5      10       133    92    -6.03  0.00000000165 0.00000000496 ****   


#2nd, also sort by Generation. To test differences between selection status
G0bs <- filter(BS,  Generation == "0")	
dunn_test(G0bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    RS     S         91    34      2.99 0.00280 0.00280 **


G5bs <- filter(BS,   Generation == "5")	
dunn_test(G5bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    RS     S         76   133      2.05 0.0403 0.0403 *  

G10bs <- filter(BS,  Generation == "10")		
dunn_test(G10bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    RS     S         99    92     -6.72 1.84e-11 1.84e-11 ****

#3rd, sort by BOTH Generation & Selection Status (Group). To test differences between sexes
G0bsRS <- filter(G0bs, SelectionStatus == "RS")
dunn_test(G0bsRS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    female male      44    47     -2.57 0.0103 0.0103 *

G0bsS <- filter(G0bs, SelectionStatus == "S")		
dunn_test(G0bsS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 bs    female male      19    15     -1.21 0.226 0.226 ns

G5bsRS <- filter(G5bs, SelectionStatus == "RS")	
dunn_test(G5bs, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    female male     103   106     -3.62 0.000296 0.000296 *** 

G5bsS <- filter(G5bs, SelectionStatus == "S")		
dunn_test(G5bsS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    female male      66    67     -2.04 0.0413 0.0413 *  

G10bsRS <- filter(G10bs, SelectionStatus == "RS")
dunn_test(G10bsRS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    female male      50    49     -3.17 0.00151 0.00151 **  

G10bsS <- filter(G10bs, SelectionStatus == "S")		
dunn_test(G10bsS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    female male      50    42     -3.12 0.00182 0.00182 ** 

#Model
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
bs5 <- lmer(bs~Generation*SelectionStatus*sex + (1|replicate),data=BS)
bs6 <- lmer(bs~Generation+SelectionStatus+sex + (1|replicate),data=BS)
bs7 <- lmer(bs~Generation*sex + (1|replicate),data=BS)
bs8 <- lmer(bs~Generation+sex + (1|replicate),data=BS)
bs9 <- lmer(bs~SelectionStatus*sex + (1|replicate),data=BS)
bs10 <- lmer(bs~SelectionStatus+sex + (1|replicate),data=BS)
bs11 <- lmer(bs~sex + (1|replicate),data=BS)


AIC(bs1,bs2,bs3,bs4,bsNULL)
#AICc(bs1,bs2,bs3,bs4,bsNULL)
AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))
library(MuMIn)
model.sel(bs1,bs2,bs3,bs4,bsNULL)
model.sel(bs1,bs2,bs3,bs4,bsNULL, bs5,bs6,bs7,bs8,bs9,bs10,bs11)


qqnorm(resid(bs1))
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
BS<- read.csv("~/Bodysize Only_G0510.csv",na.strings = "NA",header=T)
MBS<- filter(BS, sex == "male")

MBS$Generation <- as.factor(MBS$Generation)
MBS$replicate <- as.factor(MBS$replicate)
MBS$id <- as.character(MBS$id)

shapiro.test(MBS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = MBS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> use dunn_test() to calculate post hoc pairwise multiple comparison
#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS

MRSbs <- filter(MBS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
kruskal.test(bs ~ Generation, data = MRSbs) #p-value less than sig 0.05, can conclude sig diffs between groups.
dunn_test(MRSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    0      5         47    39      1.25 0.212    0.212    ns          
#2 bs    0      10        47    49      3.61 0.000303 0.000908 ***         
#3 bs    5      10        39    49      2.18 0.0294   0.0587   ns  

MSbs <- filter(MBS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
kruskal.test(bs ~ Generation, data = MSbs) #p-value less than sig 0.05, can conclude sig diffs between groups.

dunn_test(MSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic           p      p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>       <dbl>      <dbl> <chr>       
#1 bs    0      5         15    67    0.0745 0.941       0.941      ns          
#2 bs    0      10        15    42   -3.17   0.00155     0.00309    **          
#3 bs    5      10        67    42   -4.95   0.000000756 0.00000227 **** 


#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS

G0Mbs <- filter(MBS,  Generation == "0")		#testing only G0
dunn_test(G0Mbs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    RS     S         47    15      1.83 0.0678 0.0678 ns 

G5Mbs <- filter(MBS,  Generation == "5")		#testing only G5
dunn_test(G5Mbs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    RS     S         39    67      2.40 0.0164 0.0164 *   

G10Mbs <- filter(MBS,  Generation == "10")		#testing only G10
dunn_test(G10Mbs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic            p        p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>        <dbl>        <dbl> <chr>       
#1 bs    RS     S         49    42     -5.36 0.0000000828 0.0000000828 **** 

#Model
str(MBS)
MBS$Generation <- as.factor(MBS$Generation)
MBS$replicate <- as.factor(MBS$replicate)
MBS$id <- as.character(MBS$id)
View(MBS)


bs1 <- lmer(bs~Generation*SelectionStatus + (1|replicate),data=MBS)
bs2 <- lmer(bs~Generation + (1|replicate),data=MBS)
bs3 <- lmer(bs~SelectionStatus + (1|replicate),data=MBS)
bs4 <- lmer(bs~Generation + SelectionStatus + (1|replicate),data=MBS)
bsNULL <- lmer(bs~1 + (1|replicate),data=MBS)

AIC(bs1,bs2,bs3,bs4,bsNULL)

AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))


qqnorm(resid(bs1))
qqline(resid(bs1))
plot(bs1)
summary(bs1) #only G5 & G5:SelectionStatusS not significant

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
#--------F Body Size------------
BS<- read.csv("~/Bodysize Only_G0510.csv",na.strings = "NA",header=T)
FBS<- filter(BS, sex == "female")
FBS<-na.omit(FBS) 
shapiro.test(FBS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = FBS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> use dunn_test() to calculate post hoc pairwise multiple comparison

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS

FRSbs <- filter(FBS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
kruskal.test(bs ~ Generation, data = FRSbs) #p-value less than sig 0.05, can conclude sig diffs between groups.
dunn_test(FRSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    0      5         44    37      2.01 0.0446  0.0893  ns          
#2 bs    0      10        44    50      3.26 0.00110 0.00331 **          
#3 bs    5      10        37    50      1.04 0.296   0.296   ns 

FSbs <- filter(FBS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
kruskal.test(bs ~ Generation, data = FSbs) #p-value less than sig 0.05, can conclude sig diffs between groups.
dunn_test(FSbs, bs ~ Generation)
#  .y.   group1 group2    n1    n2 statistic         p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>     <dbl>    <dbl> <chr>       
#1 bs    0      5         19    66    -0.341 0.733     0.733    ns          
#2 bs    0      10        19    50    -3.11  0.00188   0.00377  **          
#3 bs    5      10        66    50    -3.99  0.0000650 0.000195 ***   


#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
FG0bs <- filter(FBS,  Generation == "0")		#testing only G0
dunn_test(FG0bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    RS     S         44    19      2.11 0.0347 0.0347 *      

FG5bs <- filter(FBS,   Generation == "5")		#testing only G5
dunn_test(FG5bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 bs    RS     S         37    66     0.718 0.473 0.473 ns 


FG10bs <- filter(FBS,  Generation == "10")		#testing only G10
dunn_test(FG10bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic          p      p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
#1 bs    RS     S         50    50     -4.67 0.00000299 0.00000299 ****      


#Model
str(FBS)
FBS$Generation <- as.factor(FBS$Generation)
FBS$replicate <- as.factor(FBS$replicate)
FBS$id <- as.character(FBS$id)
View(FBS)


bs1 <- lmer(bs~Generation*SelectionStatus + (1|replicate),data=FBS)
bs2 <- lmer(bs~Generation + (1|replicate),data=FBS)
bs3 <- lmer(bs~SelectionStatus + (1|replicate),data=FBS)
bs4 <- lmer(bs~Generation + SelectionStatus + (1|replicate),data=FBS)
bsNULL <- lmer(bs~1 + (1|replicate),data=FBS)

AIC(bs1,bs2,bs3,bs4,bsNULL)

AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))


qqnorm(resid(bsNULL))
qqline(resid(bsNULL))
plot(bsNULL)
summary(bsNULL)
################################################################################
Random effects:
  Groups    Name        Variance Std.Dev.
replicate (Intercept) 0.000489 0.02211 
Residual              0.003960 0.06293 
Number of obs: 266, groups:  replicate, 6

Fixed effects:
  Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  0.88770    0.01043 4.97336   85.11 4.62e-09 ***
  
################################################################################

#--------Testes Size------------
TVmod <- read.csv("~/MTestesVolumeOnly_G0510.csv")

str(TVmod)
TVmod$Generation <- as.factor(TVmod$Generation)
TVmod$replicate <- as.factor(TVmod$replicate)
TVmod$id <- as.character(TVmod$id)

shapiro.test(TVmod$avgts) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.


#Kruskal Wallis	
kruskal.test(avgts ~ Generation, data = TVmod) #p-value less than sig 0.05, can conclude sig diffs between groups.

#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> use dunn_test() to calculate post hoc pairwise multiple comparison

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS

RStv <- filter(TVmod,  SelectionStatus == "RS")		#testing only Relaxed Selection Parental,5th and 10th gen
kruskal.test(avgts ~ Generation, data = RStv)
dunn_test(RStv, avgts ~ Generation)
#.y.   group1 group2    n1    n2 statistic        p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
#1 avgts 0      5         42    28     3.57  0.000354 0.00106 **          
#2 avgts 0      10        42    49     3.02  0.00252  0.00504 **          
#3 avgts 5      10        28    49    -0.997 0.319    0.319   ns   


Stv <- filter(TVmod,  SelectionStatus == "S")		#testing only Selection	Parental,5th and 10th gen
dunn_test(Stv, avgts ~ Generation)
#.y.   group1 group2    n1    n2 statistic         p     p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
#1 avgts 0      5         15    45      1.94 0.0527    0.105     ns          
#2 avgts 0      10        15    40     -1.14 0.254     0.254     ns          
#3 avgts 5      10        45    40     -4.25 0.0000216 0.0000649 **** 

#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS

G0tv <- filter(TVmod,  Generation == "0")		#testing only G0
dunn_test(G0tv, avgts ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 avgts RS     S         42    15    -0.163 0.870 0.870 ns 

G5tv <- filter(TVmod,  Generation == "5")		#testing only G5
dunn_test(G5tv, avgts ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 avgts RS     S         28    45    -0.613 0.540 0.540 ns 

G10tv <- filter(TVmod,  Generation == "10")		#testing only G10
dunn_test(G10tv, avgts ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic           p       p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>       <dbl>       <dbl> <chr>       
#1 avgts RS     S         49    40     -4.91 0.000000921 0.000000921 ****   


#Model
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
tv9 <- lmer(avgts~Generation + SelectionStatus + (1|replicate),data=TVmod)
tv10 <- lmer(avgts~Generation + bs + (1|replicate),data=TVmod)
tv11 <- lmer(avgts~SelectionStatus + bs + (1|replicate),data=TVmod)

AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL)
AICtable(AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL))	#tv4 is the best model
#AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL, tv9, tv10, tv11)
#AICtable(AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL, tv9, tv10, tv11))	#tv4 is the best model

model.sel(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL, tv9, tv10, tv11) #tv4 is the best model

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
SLmod1 <- read.csv("~/MSpermLengthOnly_G0510.csv")

str(SLmod1)
SLmod1$Generation <- as.factor(SLmod1$Generation)
SLmod1$replicate <- as.factor(SLmod1$replicate)
SLmod1$id <- as.character(SLmod1$id)

shapiro.test(SLmod1$avgss) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.

#Kruskal Wallis	
kruskal.test(avgss ~ Generation, data = SLmod1) #p-value less than sig 0.05, can conclude sig diffs between groups.

#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> use dunn_test() to calculate post hoc pairwise multiple comparison

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RSsl <- filter(SLmod1, SelectionStatus == "RS")		#testing only Relaxed Selection Parental,5th and 10th gen
kruskal.test(avgss ~ Generation, data = RSsl)
dunn_test(RSsl, avgss ~ Generation)
#.y.   group1 group2    n1    n2 statistic        p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
#1 avgss 0      5         39    26     0.163 0.871    0.871   ns          
#2 avgss 0      10        39    40    -3.44  0.000585 0.00175 **          
#3 avgss 5      10        26    40    -3.24  0.00121  0.00243 ** 

Ssl <- filter(SLmod1, SelectionStatus == "S")		#testing only Selection Parental,5th and 10th gen
kruskal.test(avgss ~ Generation, data = Ssl)
dunn_test(Ssl, avgss ~ Generation)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 avgss 0      5         14    45     0.266 0.790  0.790  ns          
#2 avgss 0      10        14    37    -1.25  0.212  0.424  ns          
#3 avgss 5      10        45    37    -2.13  0.0330 0.0990 ns 

#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0sl <- filter(SLmod1, Generation == "0")	#testing only G0
dunn_test(G0sl, avgss ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 avgss RS     S         39    14     -1.37 0.170 0.170 ns  


G5sl <- filter(SLmod1, Generation == "5")	#testing only G5
dunn_test(G5sl, avgss ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 avgss RS     S         26    45     -2.10 0.0357 0.0357 * 

G10sl <- filter(SLmod1, Generation == "10")	#testing only G10
dunn_test(G10sl, avgss ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 avgss RS     S         40    37    -0.510 0.610 0.610 ns   

#Model
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

#did not add testes interactions as do not make biological sense (testes size is proxy for sperm no. not sperm length https://royalsocietypublishing.org/doi/10.1098/rstb.2020.0064)

#Without testes
AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, slNULL)
AICtable(AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, slNULL))
model.sel(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, slNULL)
plot(sl1)
qqnorm(resid(sl1))
qqline(resid(sl1))
summary(sl1)	

##################################################################################################################
Formula: avgss ~ Generation * SelectionStatus * bs + (1 | replicate)
Data: SLmod1

REML criterion at convergence: 1485.8

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-3.03646 -0.53804  0.06088  0.59606  2.87223 

Random effects:
Groups    Name        Variance Std.Dev.
replicate (Intercept)  31.73    5.633  
Residual              139.15   11.796  
Number of obs: 201, groups:  replicate, 6

Fixed effects:
                                 Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)                        347.61      26.64  188.45  13.049  < 2e-16 ***
Generation5                        -66.26      62.03  184.44  -1.068  0.28684    
Generation10                       -90.19      52.63  188.72  -1.714  0.08823 .  
SelectionStatusS                  -115.60      42.45  185.30  -2.723  0.00709 ** 
bs                                 -62.94      31.88  186.28  -1.975  0.04980 *  
Generation5:SelectionStatusS        22.99      74.19  185.61   0.310  0.75702    
Generation10:SelectionStatusS       79.11      66.82  188.52   1.184  0.23792    
Generation5:bs                      79.70      71.88  184.43   1.109  0.26895    
Generation10:bs                     92.11      60.44  188.71   1.524  0.12917    
SelectionStatusS:bs                124.45      49.09  185.35   2.535  0.01208 *  
Generation5:SelectionStatusS:bs    -28.26      85.17  185.42  -0.332  0.74042    
Generation10:SelectionStatusS:bs   -76.95      77.14  188.45  -0.998  0.31979    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
            (Intr) Gnrtn5 Gnrt10 SlctSS bs     Gn5:SSS Gn10:SSS Gnrt5: Gnr10: SlcSS:
Generation5 -0.402                                                                  
Generatin10 -0.522  0.240                                                           
SelctnSttsS -0.622  0.238  0.307                                                    
bs          -0.994  0.402  0.522  0.622                                             
Gnrtn5:SlSS  0.334 -0.835 -0.183 -0.557 -0.333                                      
Gnrtn10:SSS  0.410 -0.180 -0.787 -0.642 -0.409  0.376                               
Genertn5:bs  0.417 -0.999 -0.249 -0.247 -0.419  0.832   0.186                       
Genrtn10:bs  0.544 -0.249 -0.998 -0.321 -0.546  0.190   0.786    0.259              
SlctnSttsS:  0.645 -0.247 -0.316 -0.996 -0.648  0.553   0.635    0.257  0.332       
Gnrtn5:SSS: -0.350  0.841  0.191  0.558  0.351 -0.998  -0.376   -0.841 -0.199 -0.559
Gnrt10:SSS: -0.425  0.185  0.781  0.641  0.426 -0.376  -0.997   -0.192 -0.782 -0.640
            G5:SSS:
Generation5        
Generatin10        
SelctnSttsS        
bs                 
Gnrtn5:SlSS        
Gnrtn10:SSS        
Genertn5:bs        
Genrtn10:bs        
SlctnSttsS:        
Gnrtn5:SSS:        
Gnrt10:SSS:  0.379 
##################################################################################################################

