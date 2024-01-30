#Import Data
data <-read.csv("C:\\Users\\Nicole Lee\\Desktop\\Pamela analyses check\\Behaviour tests\\new_modeling_courtshipMST(issame).csv",header=T)
head(data)
str(data)
data$rep<-as.factor(data$rep)

#Model simplification https://www.r-bloggers.com/2017/02/raccoon-ch-2-4-3-way-anova/
res.aov <- aov(courtshipnumber ~ treatment*gen*selection, data = data) #gives same summary
shapiro.test(residuals(res.aov))  #p-value is not significant (p = 0.4495), so we can assume normality. Fits model assumptions! https://www.datanovia.com/en/lessons/anova-in-r/
res.aov1<-update(res.aov,~.-treatment:gen:selection)
anova(res.aov, res.aov1)
res.aov2<-update(res.aov1,~.-treatment:selection)
anova(res.aov1, res.aov2)
res.aov3<-update(res.aov2,~.-treatment:gen)
anova(res.aov2, res.aov3)
res.aov3<-update(res.aov2,~.-treatment:gen)
res.aov4<-update(res.aov3,~.-treatment)
anova(res.aov4, res.aov3)

summary(res.aov4)
#################################################################################
Df Sum Sq Mean Sq F value  Pr(>F)   
gen            2 214941  107470   8.146 0.00129 **
  selection      1    555     555   0.042 0.83871   
gen:selection  2 180082   90041   6.825 0.00322 **
  Residuals     34 448576   13193                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#################################################################################

shapiro.test(residuals(res.aov4)) #p-value is not significant (p = 0.9643), so we can assume normality. Fits model assumptions! https://www.datanovia.com/en/lessons/anova-in-r/
TukeyHSD(res.aov4)

#################################################################################
> TukeyHSD(res.aov4)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = courtshipnumber ~ gen + selection + gen:selection, data = data)

$gen
diff         lwr       upr     p adj
end-baseline 200.6875   78.810345 322.56465 0.0008356
mid-baseline 130.0000    8.122845 251.87715 0.0345083
mid-end      -70.6875 -170.199780  28.82478 0.2050828

$selection
diff       lwr      upr     p adj
predation-no predation -7.45 -81.26665 66.36665 0.8387125

$`gen:selection`
diff        lwr       upr     p adj
end:no predation-baseline:no predation    250.500   38.20093 462.79907 0.0130747
mid:no predation-baseline:no predation     31.375 -180.92407 243.67407 0.9975743
baseline:predation-baseline:no predation  -46.500 -291.64185 198.64185 0.9921971
end:predation-baseline:no predation       104.375 -107.92407 316.67407 0.6764447
mid:predation-baseline:no predation       182.125  -30.17407 394.42407 0.1276569
mid:no predation-end:no predation        -219.125 -392.46646 -45.78354 0.0066762
baseline:predation-end:no predation      -297.000 -509.29907 -84.70093 0.0021785
end:predation-end:no predation           -146.125 -319.46646  27.21646 0.1397401
mid:predation-end:no predation            -68.375 -241.71646 104.96646 0.8382322
baseline:predation-mid:no predation       -77.875 -290.17407 134.42407 0.8750236
end:predation-mid:no predation             73.000 -100.34146 246.34146 0.7982585
mid:predation-mid:no predation            150.750  -22.59146 324.09146 0.1186890
end:predation-baseline:predation          150.875  -61.42407 363.17407 0.2894962
mid:predation-baseline:predation          228.625   16.32593 440.92407 0.0287005
mid:predation-end:predation                77.750  -95.59146 251.09146 0.7532817
