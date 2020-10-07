#Load file: SPL5min_Analysis.csv ####

Data<-read.csv(file.choose())
Data$Year<-as.factor(Data$Year)
head(Data)

#Renaming 20-1000Hz band as SPL_Fish
Data$SPL_Fish<-Data$SPL_20.1000Hz

#Renaming 20Hz-48kHz band as SPL_BB
Data$SPL_BB<-Data$SPL_20Hz.48kHz

#loading libraries

library(lme4)
library(lmerTest)
library(gamm4)
library(gamm4.test)

#Focus analysis on times when all recorders are recording
Data_reduced<-subset(Data,Include=="1")


#Analysis of SPL_Fish####
Model1<-lmer(SPL_Fish~WindSp+NShips10km+Recorder*Year+(1|Jday),data=Data_reduced)
Model2<-lmer(SPL_Fish~WindSp+NShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
Model3<-lmer(SPL_Fish~WindSp+NShips1km+Recorder*Year+(1|Jday),data=Data_reduced)

Model4<-lmer(SPL_Fish~WindSp+AnchoredShips10km+MovingShips10km+Recorder*Year+(1|Jday),data=Data_reduced)
Model5<-lmer(SPL_Fish~WindSp+AnchoredShips5km+MovingShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
Model6<-lmer(SPL_Fish~WindSp+AnchoredShips1km+MovingShips1km+Recorder*Year+(1|Jday),data=Data_reduced)

AIC(Model1,Model2,Model3,Model4,Model5,Model6)
#df      AIC
#Model1  8 119572.3
#Model2  8 119043.2
#Model3  8 119266.7
#Model4  9 118984.9
#Model5  9 118322.6***
#Model6  8 119266.7

summary(Model5)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_Fish ~ WindSp + AnchoredShips5km + MovingShips5km + Recorder *      Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 118304.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2739 -0.6409 -0.1604  0.4800  7.9025 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  3.175   1.782   
#  Residual             40.477   6.362   
# Number of obs: 18072, groups:  Jday, 34
# 
# Fixed effects:
#                            Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               9.992e+01  4.554e-01  3.637e+01 219.399  < 2e-16 ***
# WindSp                    9.670e-02  9.148e-03  1.780e+04  10.571  < 2e-16 ***
# AnchoredShips5km         -3.975e-01  6.071e-02  1.806e+04  -6.548  6.0e-11 ***
# MovingShips5km            1.889e+00  4.799e-02  1.805e+04  39.366  < 2e-16 ***
# RecorderRCA_Out           6.551e+00  1.365e-01  1.803e+04  47.987  < 2e-16 ***
# Year2020                 -6.769e+00  6.271e-01  3.268e+01 -10.795  2.6e-12 ***
# RecorderRCA_Out:Year2020 -4.370e+00  1.903e-01  1.803e+04 -22.958  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp AnchS5 MvngS5 RcRCA_O Yr2020
# WindSp      -0.188                                    
# AnchrdShps5 -0.076  0.048                             
# MvngShps5km -0.104 -0.021 -0.113                      
# RcrdrRCA_Ot -0.130 -0.010 -0.196  0.032               
# Year2020    -0.696  0.022  0.027  0.018  0.100        
# RRCA_O:Y202  0.095  0.003  0.105 -0.009 -0.710  -0.147



#Model examining the influence of min distance and ave speed on SPL_Fish####

Model7<-lmer(SPL_Fish~WindSp+MinDistMoving+MinDistAnchored+AveSpeedMoving+MinDistMoving:AveSpeedMoving+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model7)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_Fish ~ WindSp + MinDistMoving + MinDistAnchored + AveSpeedMoving +  
#     MinDistMoving:AveSpeedMoving + Recorder * Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 92529.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2518 -0.6420 -0.1375  0.5063  7.9993 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  2.775   1.666   
#  Residual             39.596   6.293   
# Number of obs: 14171, groups:  Jday, 34
# 
# Fixed effects:
#                                Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   1.011e+02  5.462e-01  9.461e+01 185.130  < 2e-16 ***
# WindSp                        7.258e-02  1.031e-02  1.373e+04   7.037 2.05e-12 ***
# MinDistMoving                -5.365e-04  5.689e-05  1.414e+04  -9.430  < 2e-16 ***
# MinDistAnchored               2.275e-04  4.463e-05  1.416e+04   5.099 3.46e-07 ***
# AveSpeedMoving                3.928e-01  2.597e-02  1.415e+04  15.122  < 2e-16 ***
# RecorderRCA_Out               6.344e+00  1.466e-01  1.413e+04  43.279  < 2e-16 ***
# Year2020                     -6.417e+00  5.933e-01  3.305e+01 -10.816 2.15e-12 ***
# MinDistMoving:AveSpeedMoving -5.185e-05  5.304e-06  1.414e+04  -9.776  < 2e-16 ***
# RecorderRCA_Out:Year2020     -3.008e+00  3.129e-01  1.415e+04  -9.613  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp MnDstM MnDstA AvSpdM RcRCA_O Yr2020 MDM:AS
# WindSp      -0.149                                                  
# MinDistMvng -0.461 -0.014                                           
# MnDstAnchrd -0.339 -0.033 -0.027                                    
# AveSpedMvng -0.474 -0.029  0.814 -0.036                             
# RcrdrRCA_Ot -0.127  0.006 -0.019 -0.004 -0.001                      
# Year2020    -0.536  0.019  0.005 -0.048  0.026  0.125               
# MnDstMv:ASM  0.406  0.016 -0.870  0.002 -0.869  0.001  -0.010       
# RRCA_O:Y202 -0.184 -0.029 -0.024  0.737 -0.028 -0.470  -0.156  0.002
# fit warnings:
# Some predictor variables are on very different scales: consider rescaling





#Analysis of SPL_BB####
Model11<-lmer(SPL_BB~WindSp+NShips10km+Recorder*Year+(1|Jday),data=Data_reduced)
Model12<-lmer(SPL_BB~WindSp+NShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
Model13<-lmer(SPL_BB~WindSp+NShips1km+Recorder*Year+(1|Jday),data=Data_reduced)

Model14<-lmer(SPL_BB~WindSp+AnchoredShips10km+MovingShips10km+Recorder*Year+(1|Jday),data=Data_reduced)
Model15<-lmer(SPL_BB~WindSp+AnchoredShips5km+MovingShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
Model16<-lmer(SPL_BB~WindSp+AnchoredShips1km+MovingShips1km+Recorder*Year+(1|Jday),data=Data_reduced)

AIC(Model11,Model12,Model13,Model14,Model15,Model16)
# df      AIC
# Model11  8 117025.4
# Model12  8 116511.5
# Model13  8 116679.7
# Model14  9 116465.6
# Model15  9 115801.1***
# Model16  8 116679.7


summary(Model15)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_BB ~ WindSp + AnchoredShips5km + MovingShips5km + Recorder *      Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 115783.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2730 -0.6397 -0.1631  0.4482  8.2365 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  2.874   1.695   
#  Residual             35.201   5.933   
# Number of obs: 18072, groups:  Jday, 34
# 
# Fixed effects:
#                            Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               1.021e+02  4.324e-01  3.629e+01 236.002  < 2e-16 ***
# WindSp                    1.053e-01  8.532e-03  1.783e+04  12.337  < 2e-16 ***
# AnchoredShips5km         -3.682e-01  5.662e-02  1.806e+04  -6.503 8.08e-11 ***
# MovingShips5km            1.742e+00  4.475e-02  1.805e+04  38.931  < 2e-16 ***
# RecorderRCA_Out           5.671e+00  1.273e-01  1.803e+04  44.544  < 2e-16 ***
# Year2020                 -6.981e+00  5.960e-01  3.274e+01 -11.713 3.00e-13 ***
# RecorderRCA_Out:Year2020 -3.799e+00  1.775e-01  1.803e+04 -21.400  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp AnchS5 MvngS5 RcRCA_O Yr2020
# WindSp      -0.185                                    
# AnchrdShps5 -0.075  0.048                             
# MvngShps5km -0.102 -0.021 -0.113                      
# RcrdrRCA_Ot -0.128 -0.010 -0.196  0.032               
# Year2020    -0.696  0.021  0.026  0.017  0.098        
# RRCA_O:Y202  0.094  0.003  0.105 -0.009 -0.710  -0.145



#Model examining the influence of min distance and ave speed on SPL_BB####
Model17<-lmer(SPL_BB~WindSp+MinDistMoving+MinDistAnchored+AveSpeedMoving+MinDistMoving:AveSpeedMoving+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model17)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_BB ~ WindSp + MinDistMoving + MinDistAnchored + AveSpeedMoving +  
#     MinDistMoving:AveSpeedMoving + Recorder * Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 90641.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3130 -0.6342 -0.1361  0.4864  8.3242 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  2.488   1.577   
#  Residual             34.651   5.887   
# Number of obs: 14171, groups:  Jday, 34
# 
# Fixed effects:
#                                Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   1.034e+02  5.144e-01  9.309e+01 201.068  < 2e-16 ***
# WindSp                        7.944e-02  9.650e-03  1.375e+04   8.232  < 2e-16 ***
# MinDistMoving                -5.423e-04  5.322e-05  1.414e+04 -10.191  < 2e-16 ***
# MinDistAnchored               2.225e-04  4.175e-05  1.415e+04   5.329  1.0e-07 ***
# AveSpeedMoving                3.373e-01  2.430e-02  1.415e+04  13.882  < 2e-16 ***
# RecorderRCA_Out               5.499e+00  1.371e-01  1.413e+04  40.100  < 2e-16 ***
# Year2020                     -6.689e+00  5.613e-01  3.312e+01 -11.917  1.6e-13 ***
# MinDistMoving:AveSpeedMoving -4.465e-05  4.962e-06  1.414e+04  -8.999  < 2e-16 ***
# RecorderRCA_Out:Year2020     -2.430e+00  2.927e-01  1.414e+04  -8.299  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp MnDstM MnDstA AvSpdM RcRCA_O Yr2020 MDM:AS
# WindSp      -0.148                                                  
# MinDistMvng -0.458 -0.014                                           
# MnDstAnchrd -0.337 -0.033 -0.027                                    
# AveSpedMvng -0.471 -0.029  0.814 -0.036                             
# RcrdrRCA_Ot -0.126  0.006 -0.019 -0.004 -0.001                      
# Year2020    -0.538  0.019  0.005 -0.048  0.026  0.123               
# MnDstMv:ASM  0.404  0.016 -0.870  0.002 -0.869  0.001  -0.010       
# RRCA_O:Y202 -0.183 -0.029 -0.024  0.737 -0.028 -0.470  -0.154  0.002
# fit warnings:
# Some predictor variables are on very different scales: consider rescaling