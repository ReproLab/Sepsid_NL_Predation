#Import Data
data <-read.csv("~\\new_modeling_courtshipMST(issame).csv",header=T)
head(data)
str(data)
data$rep<-as.factor(data$rep)

####Find the best fit model####
#Using glmm instead of lme cuz courtship no. is COUNT DATA.
library(lme4)
maxmodel<-glmer(courtshipnumber~gen*treatment*selection + (1|rep),data=data,family=poisson)
summary(maxmodel)
###checking for overdispersion
library(RVAideMemoire)
overdisp.glmer(maxmodel) #ratio=40.26!! overdispersed

#used negative binomial due to overdispersion
##accounting for overdispersion 
maxmodel2<-glmer.nb(courtshipnumber~gen*treatment*selection + (1|rep),data=data)
summary(maxmodel2)
overdisp.glmer(maxmodel2) #ratio=1.339, ~ok

library(MASS)
m1 <- glmer.nb(courtshipnumber~gen + (1|rep),data=data)
m2 <- glmer.nb(courtshipnumber~treatment + (1|rep),data=data)
m3 <- glmer.nb(courtshipnumber~selection + (1|rep),data=data)
m4 <- glmer.nb(courtshipnumber~gen+treatment+selection + (1|rep),data=data)
m5 <- glmer.nb(courtshipnumber~gen*treatment*selection + (1|rep),data=data)
m6 <- glmer.nb(courtshipnumber~gen+treatment + (1|rep),data=data)
m7 <- glmer.nb(courtshipnumber~treatment+selection + (1|rep),data=data)
m8 <- glmer.nb(courtshipnumber~gen+selection + (1|rep),data=data)
m9 <- glmer.nb(courtshipnumber~gen*treatment + (1|rep),data=data)
m10 <- glmer.nb(courtshipnumber~treatment*selection + (1|rep),data=data)
m11 <- glmer.nb(courtshipnumber~gen*selection + (1|rep),data=data)
mNULL <- glmer.nb(courtshipnumber~1 + (1|rep),data=data)

library(MuMIn)
model.sel(m1,m2,m3,m4, m5, m6,m7,m8,m9,m10,m11,mNULL)
#################################################################################
#Model selection table 
#                             family df   logLik  AICc delta weight
#m11   Negative Binomial(3.4426,log)  8 -227.844 476.3  0.00  0.968
#m1     Negative Binomial(2.263,log)  5 -236.496 484.8  8.42  0.014
#m6    Negative Binomial(2.3182,log)  6 -235.975 486.5 10.16  0.006
#m8    Negative Binomial(2.3097,log)  6 -236.219 487.0 10.65  0.005
#m9    Negative Binomial(2.6139,log)  8 -233.402 487.4 11.12  0.004
#m4    Negative Binomial(2.3501,log)  7 -235.808 489.1 12.78  0.002
#m5    Negative Binomial(3.5981,log) 12 -226.966 489.5 13.15  0.001
#mNULL Negative Binomial(1.4438,log)  3 -246.272 499.2 22.88  0.000
#m3      Negative Binomial(1.45,log)  4 -246.231 501.6 25.27  0.000
#m2    Negative Binomial(1.4451,log)  4 -246.260 501.7 25.33  0.000
#m7    Negative Binomial(1.4513,log)  5 -246.219 504.2 27.87  0.000
#m10   Negative Binomial(1.5126,log)  6 -245.149 504.8 28.51  0.000
#Models ranked by AICc(x) 
#Random terms (all models): 
#‘1 | rep’
#################################################################################

summary(m11)
#################################################################################
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
#Family: Negative Binomial(3.4426)  ( log )
#Formula: courtshipnumber ~ gen * selection + (1 | rep)
#Data: data

#AIC      BIC   logLik deviance df.resid 
#471.7    485.2   -227.8    455.7       32 

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.56972 -0.63964 -0.07533  0.36245  2.96874 

#Random effects:
#  Groups Name        Variance Std.Dev.
#rep    (Intercept) 0.07402  0.2721  
#Number of obs: 40, groups:  rep, 4

#Fixed effects:
#                            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 4.1713     0.3096  13.474  < 2e-16 ***
#genend                      1.5400     0.3404   4.524 6.07e-06 ***
#genmid                      0.4948     0.3440   1.438 0.150330    
#selectionpredation         -1.0515     0.4014  -2.620 0.008799 ** 
#genend:selectionpredation   0.3772     0.4880   0.773 0.439564    
#genmid:selectionpredation   1.8780     0.4904   3.830 0.000128 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Correlation of Fixed Effects:
#  (Intr) genend genmid slctnp gnnd:s
#genend      -0.736                            
#genmid      -0.726  0.645                     
#selctnprdtn -0.622  0.569  0.557              
#gnnd:slctnp  0.518 -0.696 -0.468 -0.827       
#gnmd:slctnp  0.508 -0.450 -0.705 -0.816  0.678

plot(m11)
qqnorm(resid(m11))
###checking for overdispersion
library(RVAideMemoire)
overdisp.glmer(m11) #ratio=1.178 ~ok

plot(fitted(m11),resid(m11))

confint(m11)
#2.5 %     97.5 %
#.sig01                     0.04747857  0.7815292
#(Intercept)                3.56692887  4.8239160
#genend                     0.84533368  2.1907403 *
#genmid                    -0.20594890  1.1531730
#selectionpredation        -1.84590567 -0.2575848 *
#genend:selectionpredation -0.58409398  1.3381861
#genmid:selectionpredation  0.91187198  2.8434831*

#### post hoc ####
library(rstatix)
cs <- read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\Behaviour tests\\courtship_combinedCSV.csv")
library(dplyr)
str(cs)
cs$rep <- as.factor(cs$rep)
View(cs)

shapiro.test(cs$CourtshipNumber) #p-value = 1.951e-05, not normal

#Kruskal Wallis	
kruskal.test(CourtshipNumber ~ Generation, data = cs)
#	Kruskal-Wallis rank sum test
#data:  CourtshipNumber by Generation
#Kruskal-Wallis chi-squared = 20.249, df = 2, p-value = 4.008e-05
dunn_test(cs, CourtshipNumber ~ Generation)
#  .y.             group1     group2       n1    n2 statistic         p     p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>     <dbl>     <dbl> <chr>       
#1 CourtshipNumber After 10th After 5th    16    16    -0.846 0.397     0.397     ns          
#2 CourtshipNumber After 10th Parental     16    16    -4.25  0.0000213 0.0000639 ****        
#3 CourtshipNumber After 5th  Parental     16    16    -3.40  0.000663  0.00133   ** 
dunn_test(cs, CourtshipNumber ~ Type)
#  .y.             group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr>           <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 CourtshipNumber RS     S         24    24     0.289 0.773 0.773 ns   

##		Relaxed Selection 		##
csRS <- filter(cs, Type == "RS")
kruskal.test(CourtshipNumber ~ Generation, data = csRS)
#Kruskal-Wallis chi-squared = 13.42, df = 2, p-value = 0.001219
##p-value less than sig 0.05, can conclude sig diffs between Generations
##		Selection 		##
csS <- filter(cs, Type == "S")
kruskal.test(CourtshipNumber ~ Generation, data = csS)
#Kruskal-Wallis chi-squared = 10.736, df = 2, p-value = 0.004664
##p-value less than sig 0.05, can conclude sig diffs between Generations

####Dunn's Test BTWN PREDATION EXPERIMENT GROUPS ####
csParental <- filter(cs, Generation == "Parental")	
dunn_test(csParental, CourtshipNumber ~ Type)
# .y.             group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr>           <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 CourtshipNumber RS     S          8     8         0     1     1 ns    
csAfter5th <- filter(cs, Generation == "After 5th")	
dunn_test(csAfter5th, CourtshipNumber ~ Type)
#  .y.             group1 group2    n1    n2 statistic       p   p.adj p.adj.signif
#* <chr>           <chr>  <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 CourtshipNumber RS     S          8     8      2.63 0.00865 0.00865 **   
csAfter10th <- filter(cs, Generation == "After 10th")	
dunn_test(csAfter10th, CourtshipNumber ~ Type)
#  .y.             group1 group2    n1    n2 statistic     p p.adj p.adj.signif
#* <chr>           <chr>  <chr>  <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 CourtshipNumber RS     S          8     8     -1.58 0.115 0.115 ns  



####Dunn's Test WITHIN PREDATION EXPERIMENT GROUPS ####
##		Relaxed Selection 		##
csRS <- filter(cs, Type == "RS")		
View(csRS)
dunn_test(csRS, CourtshipNumber ~ Generation)
# .y.             group1     group2       n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 CourtshipNumber After 10th After 5th     8     8     -1.96 0.0497   0.0994   ns          
#2 CourtshipNumber After 10th Parental      8     8     -3.66 0.000252 0.000756 ***         
#3 CourtshipNumber After 5th  Parental      8     8     -1.70 0.0896   0.0994   ns  

dunn_test(csRS, CourtshipNumber ~ Treatment)
# .y.             group1     group2       n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 CourtshipNumber Empty Vial Mantis Vial    12    12    -0.664 0.507 0.507 ns  


# 		---Empty Vial---
csRSEV <- filter(cs, Type == "RS", Treatment == "Empty Vial")		
View(csRSEV)
dunn_test(csRSEV, CourtshipNumber ~ Generation)
# A tibble: 3 x 9
#.y.             group1     group2       n1    n2 statistic      p  p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>  <dbl>  <dbl> <chr>       
#  1 CourtshipNumber After 10th After 5th     4     4    -1.37  0.170  0.340  ns          
#2 CourtshipNumber After 10th Parental      4     4    -2.16  0.0310 0.0930 ns          
#3 CourtshipNumber After 5th  Parental      4     4    -0.784 0.433  0.433  ns    


# 		---Mantis Vial---
csRSMV <- filter(cs, Type == "RS", Treatment == "Mantis Vial")		
View(csRSMV)
dunn_test(csRSMV, CourtshipNumber ~Generation)
# A tibble: 3 x 9
#.y.             group1     group2       n1    n2 statistic       p  p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>   <dbl>  <dbl> <chr>       
#  1 CourtshipNumber After 10th After 5th     4     4     -1.27 0.202   0.233  ns          
#2 CourtshipNumber After 10th Parental      4     4     -2.84 0.00446 0.0134 *           
#  3 CourtshipNumber After 5th  Parental      4     4     -1.57 0.117   0.233  ns    


##			Selection 			##
csS <- filter(cs, Type == "S")		
View(csS)
dunn_test(csS, CourtshipNumber ~ Generation)
# .y.             group1     group2       n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 CourtshipNumber After 10th After 5th     8     8      1.08 0.281   0.281   ns          
#2 CourtshipNumber After 10th Parental      8     8     -2.14 0.0324  0.0647  ns          
#3 CourtshipNumber After 5th  Parental      8     8     -3.22 0.00129 0.00386 ** 

dunn_test(csS, CourtshipNumber ~ Treatment)
# .y.             group1     group2       n1    n2 statistic        p    p.adj p.adj.signif
#* <chr>           <chr>      <chr>     <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
#1 CourtshipNumber Empty Vial Mantis Vial    12    12    -0.838 0.402 0.402 ns

# 		---Empty Vial---
csSEV <- filter(cs, Type == "S", Treatment == "Empty Vial")		
View(csSEV)
pairwise.wilcox.test(csSEV$CourtshipNumber, csSEV$Generation)
#	          After 10th After 5th
#	After 5th 0.40       -        
#	Parental  0.40       0.17   


# 		---Mantis Vial---
csSMV <- filter(cs, Type == "S", Treatment == "Mantis Vial")		
View(csSMV)
pairwise.wilcox.test(csSMV$CourtshipNumber, csSMV$Generation)
#	          After 10th After 5th
#	After 5th 0.886      -        
#	Parental  0.491      0.086   
