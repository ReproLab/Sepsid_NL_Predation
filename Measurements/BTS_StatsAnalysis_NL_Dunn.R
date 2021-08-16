#Load packages
library(dplyr)
library(MASS)
library(lmerTest)
library(HDInterval)
library(wiqid)
library(rstatix)


#### G0 G5 G10 only ####
#--------Body Size------------
#cleaned up "D:\\UG UROPS Pamela Kuan\\POST UROPS SAVE ME\\Manuscript\\Data Files\\full_model_bs.csv" -> "MBodysize Only_G0510.csv"
BS<- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\BS Significance\\Bodysize Only_G0510.csv",na.strings = "NA",header=T)
BS<-na.omit(BS) 

shapiro.test(BS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = BS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use dunn_test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS
RSbs <- filter(BS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
View(RSbs)

pairwise.wilcox.test(RSbs$bs, RSbs$Generation)

#F only:	   	0      5     
#	5  0.0666 -     
#  10 0.0049 0.2454


#   0       5    
#5  0.033   -    
#10 1.1e-05 0.033

dunn_test(RSbs, bs ~ Generation)
#M only
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    0      5         47    39      1.25 0.212    0.212    ns          
#2 bs    0      10        47    49      3.61 0.000303 0.000908 ***         
#3 bs    5      10        39    49      2.18 0.0294   0.0587   ns  


#.y.   group1 group2    n1    n2 statistic          p      p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>      <dbl>      <dbl> <chr>       
# 1 bs    0      5         91    76      2.32 0.0203     0.0406     *           
# 2 bs    0      10        91    99      4.70 0.00000263 0.00000788 ****        
# 3 bs    5      10        76    99      2.11 0.0350     0.0406     *  



Sbs <- filter(BS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
View(Sbs)

pairwise.wilcox.test(Sbs$bs, Sbs$Generation)
#M only:
#.y.   group1 group2    n1    n2 statistic           p      p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>       <dbl>      <dbl> <chr>       
#1 bs    0      5         15    67    0.0745 0.941       0.941      ns          
#2 bs    0      10        15    42   -3.17   0.00155     0.00309    **          
#3 bs    5      10        67    42   -4.95   0.000000756 0.00000227 **** 


# F only:  0      5      
#	5  0.78549 -      
#  10 0.00304 0.00023


#   0       5      
#5  0.69    -      
#10 3.1e-05 4.3e-09


dunn_test(Sbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic             p         p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>         <dbl>         <dbl> <chr>       
#1 bs    0      5         34   133    -0.349 0.727         0.727         ns          
#2 bs    0      10        34    92    -4.41  0.0000105     0.0000209     ****        
#3 bs    5      10       133    92    -6.03  0.00000000165 0.00000000496 ****   


#2nd, also sort by GENERATION.TO TEST DIFFERENCES BTWN SELECTION STATUS
G0bs <- filter(BS,  Generation == "0")		#testing only G0

pairwise.wilcox.test(G0bs$bs, G0bs$SelectionStatus)
#  M only: RS  
#S 0.069

# F only: RS  
#S 0.035


#  RS    
#S 0.0028

dunn_test(G0bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    RS     S         91    34      2.99 0.00280 0.00280 **


G5bs <- filter(BS,   Generation == "5")		#testing only G5

pairwise.wilcox.test(G5bs$bs, G5bs$SelectionStatus)
#  M only: RS  
#S 0.017

# F only: RS  
#S 0.48


#  RS  
#S 0.04

dunn_test(G5bs, bs ~ SelectionStatus)

G10bs <- filter(BS,  Generation == "10")		#testing only G10
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    RS     S         76   133      2.05 0.0403 0.0403 *  

pairwise.wilcox.test(G10bs$bs, G10bs$SelectionStatus)
#  M only: RS 
#S 8.5e-08

#  RS     
#S 1.9e-11

dunn_test(G10bs, bs ~ SelectionStatus)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    RS     S         99    92     -6.72 1.84e-11 1.84e-11 **** 

#3rd, sort by both GENERATION & SELECTION STATUS. TO TEST DIFFERENCES BTWN SEXES
G0bsRS <- filter(G0bs, SelectionStatus == "RS")		#testing only G0

pairwise.wilcox.test(G0bsRS$bs, G0bsRS$sex)
#   female
#male 0.01

dunn_test(G0bsRS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    female male      44    47     -2.57 0.0103 0.0103 *  

G0bsS <- filter(G0bs, SelectionStatus == "S")		#testing only G0

pairwise.wilcox.test(G0bsS$bs, G0bsS$sex)
#   female
#male 0.23 

dunn_test(G0bsS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 bs    female male      19    15     -1.21 0.226 0.226 ns 

G5bsRS <- filter(G5bs, SelectionStatus == "RS")		#testing only G5

pairwise.wilcox.test(G5bsRS$bs, G5bsRS$sex)
#   female
#male 3e-05

dunn_test(G5bs, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    female male     103   106     -3.62 0.000296 0.000296 *** 


G5bsS <- filter(G5bs, SelectionStatus == "S")		#testing only G5

pairwise.wilcox.test(G5bsS$bs, G5bsS$sex)
#   female
#male 0.042

dunn_test(G5bsS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic      p  p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#1 bs    female male      66    67     -2.04 0.0413 0.0413 *    

G10bsRS <- filter(G10bs, SelectionStatus == "RS")		#testing only G10

pairwise.wilcox.test(G10bsRS$bs, G10bsRS$sex)
#   female
#male 0.0015

dunn_test(G10bsRS, bs ~ sex)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    female male      50    49     -3.17 0.00151 0.00151 **    

G10bsS <- filter(G10bs, SelectionStatus == "S")		#testing only G10

pairwise.wilcox.test(G10bsS$bs, G10bsS$sex)
#   female
#male 0.0018 

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

AIC(bs1,bs2,bs3,bs4,bsNULL)
#AICc(bs1,bs2,bs3,bs4,bsNULL)
AICtable(AIC(bs1,bs2,bs3,bs4,bsNULL))
library(MuMIn)
model.sel(bs1,bs2,bs3,bs4,bsNULL)

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

BS<- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\BS Significance\\Bodysize Only_G0510.csv",na.strings = "NA",header=T)
MBS<- filter(BS, sex == "male")

MBS$Generation <- as.factor(MBS$Generation)
MBS$replicate <- as.factor(MBS$replicate)
MBS$id <- as.character(MBS$id)

shapiro.test(MBS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = MBS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS

MRSbs <- filter(MBS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
dunn_test(MRSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic        p    p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 bs    0      5         47    39      1.25 0.212    0.212    ns          
#2 bs    0      10        47    49      3.61 0.000303 0.000908 ***         
#3 bs    5      10        39    49      2.18 0.0294   0.0587   ns  

MSbs <- filter(MBS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
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
#--------F Body Size------------
BS<- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\BS Significance\\Bodysize Only_G0510.csv",na.strings = "NA",header=T)
FBS<- filter(BS, sex == "female")
FBS<-na.omit(FBS) 
shapiro.test(FBS$bs) #p-value < 0.05 implying that the data significantly different from normal dist. AKA assume non-normality.
#http://www.sthda.com/english/wiki/normality-test-in-r

#Kruskal Wallis	
kruskal.test(bs ~ Generation, data = FBS) #p-value less than sig 0.05, can conclude sig diffs between groups.


#Multiple pairwise-comparison between groups
# From Kruskal wallis, we don't know which pairs of groups are different -> can use pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.

#1st, sort by Selection Status. TO TEST DIFFERENCES BTWN GENERATIONS

FRSbs <- filter(FBS, SelectionStatus == "RS")	#testing only Relaxed Selection for Parental,5th and 10th gen
dunn_test(FRSbs, bs ~ Generation)
#.y.   group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 bs    0      5         44    37      2.01 0.0446  0.0893  ns          
#2 bs    0      10        44    50      3.26 0.00110 0.00331 **          
#3 bs    5      10        37    50      1.04 0.296   0.296   ns 

FSbs <- filter(FBS, SelectionStatus == "S")		#testing only Selection for Parental,5th and 10th gen
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


qqnorm(resid(bsNULL)) #https://stats.stackexchange.com/questions/77891/checking-assumptions-lmer-lme-mixed-models-in-r
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

AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL)
AICtable(AIC(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL))	#tv4 is the best model
AICc(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL)
model.sel(tv1,tv2,tv3,tv4,tv5,tv6,tv7,tv8,tvNULL)

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
dunn_test(RSsl, avgss ~ Generation)
#.y.   group1 group2    n1    n2 statistic        p   p.adj p.adj.signif
#* <chr> <chr>  <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
#1 avgss 0      5         39    26     0.163 0.871    0.871   ns          
#2 avgss 0      10        39    40    -3.44  0.000585 0.00175 **          
#3 avgss 5      10        26    40    -3.24  0.00121  0.00243 ** 

Ssl <- filter(SLmod1, SelectionStatus == "S")		#testing only Selection Parental,5th and 10th gen
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


# sl1 summary makes more biological sense. remove testes interactions
#sl12 <- lmer(avgss~Generation*SelectionStatus*bs*avgts + (1|replicate),data=SLmod1) ##removed. too many interactions. reduce to "+"
#sl24 <-  lmer(avgss~Generation*SelectionStatus*bs + avgts + (1|replicate),data=SLmod1)
#sl25 <- lmer(avgss~Generation + SelectionStatus*bs*avgts + (1|replicate),data=SLmod1)
#sl26 <-lmer(avgss~Generation*bs*avgts + SelectionStatus + (1|replicate),data=SLmod1)
#sl27 <-lmer(avgss~Generation*SelectionStatus*avgts + bs + (1|replicate),data=SLmod1)
#sl13 <- lmer(avgss~Generation + SelectionStatus + bs + avgts + (1|replicate),data=SLmod1)
#sl14	<-	lmer(avgss~SelectionStatus + bs + avgts + (1|replicate),data=SLmod1)
#sl15	<-	lmer(avgss~bs + avgts + (1|replicate),data=SLmod1)
#sl16	<-	lmer(avgss~avgts + (1|replicate),data=SLmod1)
#sl17	<-	lmer(avgss~Generation + bs + avgts + (1|replicate),data=SLmod1)
#sl18	<-	lmer(avgss~Generation + avgts + (1|replicate),data=SLmod1)
#sl19	<-	lmer(avgss~bs*avgts + (1|replicate),data=SLmod1)
#sl20	<-	lmer(avgss~SelectionStatus*bs*avgts + (1|replicate),data=SLmod1)
#sl21	<-	lmer(avgss~SelectionStatus*avgts + (1|replicate),data=SLmod1)
#sl22	<-	lmer(avgss~Generation*avgts + (1|replicate),data=SLmod1)
#sl23	<-	lmer(avgss~avgts + (1|replicate),data=SLmod1)


#Without testes
AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, slNULL)
AICtable(AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, slNULL))

#All models except sl12 (too many interactions)
#AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11,sl12, sl13, sl14, sl15, sl16, sl17, sl18, sl19, sl20, sl21, sl22, sl23, sl24, sl25, sl26, sl27, slNULL)
#AICtable(AIC(sl1,sl2,sl3,sl4,sl5,sl6,sl7,sl8,sl9,sl10,sl11, sl12, sl13, sl14, sl15, sl16, sl17, sl18, sl19, sl20, sl21, sl22, sl23,  sl24, sl25, sl26, sl27,slNULL))


plot(sl1)
qqnorm(resid(sl1))
qqline(resid(sl1))
summary(sl1)	#  significant

#summary(sl12)	
#summary(sl26)	
#plot(sl26)
#qqnorm(resid(sl26))
#qqline(resid(sl26))

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



