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


#Now examine N anchored ships 5 km as a binary variable

Model5B<-lmer(SPL_Fish~WindSp+Binary_AnchoredShips5km+MovingShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model5B)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_Fish ~ WindSp + Binary_AnchoredShips5km + MovingShips5km +      Recorder * Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 118297.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3106 -0.6429 -0.1573  0.4793  7.8905 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  3.178   1.783   
#  Residual             40.462   6.361   
# Number of obs: 18072, groups:  Jday, 34
# 
# Fixed effects:
#                            Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               1.000e+02  4.564e-01  3.660e+01 219.125  < 2e-16 ***
# WindSp                    9.647e-02  9.146e-03  1.781e+04  10.547  < 2e-16 ***
# Binary_AnchoredShips5km  -7.031e-01  1.000e-01  1.805e+04  -7.028 2.17e-12 ***
# MovingShips5km            1.888e+00  4.792e-02  1.805e+04  39.395  < 2e-16 ***
# RecorderRCA_Out           6.487e+00  1.348e-01  1.803e+04  48.133  < 2e-16 ***
# Year2020                 -6.766e+00  6.273e-01  3.268e+01 -10.785 2.66e-12 ***
# RecorderRCA_Out:Year2020 -4.298e+00  1.895e-01  1.803e+04 -22.688  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp Bn_AS5 MvngS5 RcRCA_O Yr2020
# WindSp      -0.189                                    
# Bnry_AnchS5 -0.094  0.048                             
# MvngShps5km -0.103 -0.021 -0.101                      
# RcrdrRCA_Ot -0.136 -0.006 -0.118  0.022               
# Year2020    -0.695  0.021  0.024  0.018  0.103        
# RRCA_O:Y202  0.099  0.000  0.044 -0.002 -0.707  -0.150


#What if we only examine anchored ships within 2 km of recorder?
#AnchoredShips2km is a binary variable, with 1 when there is a ship within 2 km.

Model5C<-lmer(SPL_Fish~WindSp+AnchoredShips2km+MovingShips5km+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model5C)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_Fish ~ WindSp + AnchoredShips2km + MovingShips5km + Recorder *      Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 118344
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3292 -0.6394 -0.1647  0.4767  7.9135 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  3.118   1.766   
#  Residual             40.575   6.370   
# Number of obs: 18072, groups:  Jday, 34
# 
# Fixed effects:
#                            Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               9.970e+01  4.506e-01  3.613e+01 221.265  < 2e-16 ***
# WindSp                    9.952e-02  9.158e-03  1.780e+04  10.868  < 2e-16 ***
# AnchoredShips2km         -4.533e-02  3.310e-01  1.806e+04  -0.137    0.891    
# MovingShips5km            1.854e+00  4.779e-02  1.805e+04  38.796  < 2e-16 ***
# RecorderRCA_Out           6.376e+00  1.341e-01  1.803e+04  47.557  < 2e-16 ***
# Year2020                 -6.660e+00  6.215e-01  3.271e+01 -10.715 3.11e-12 ***
# RecorderRCA_Out:Year2020 -4.240e+00  1.895e-01  1.803e+04 -22.369  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp AnchS2 MvngS5 RcRCA_O Yr2020
# WindSp      -0.188                                    
# AnchrdShps2 -0.026  0.047                             
# MvngShps5km -0.114 -0.018 -0.046                      
# RcrdrRCA_Ot -0.149 -0.001 -0.027  0.011               
# Year2020    -0.696  0.021  0.014  0.020  0.108        
# RRCA_O:Y202  0.105 -0.002  0.011  0.002 -0.707  -0.152



#Model examining the influence of min distance and ave speed on SPL_Fish####

Model7<-lmer(SPL_Fish~WindSp+log10(MinDistMoving+1)+log10(MinDistAnchored+1)+AveSpeedMoving+log10(MinDistMoving+1):AveSpeedMoving+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model7)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_Fish ~ WindSp + log10(MinDistMoving + 1) + log10(MinDistAnchored +  
#     1) + AveSpeedMoving + log10(MinDistMoving + 1):AveSpeedMoving +      Recorder * Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 92130.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.3951 -0.6230 -0.1260  0.4886  8.1881 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  2.801   1.674   
#  Residual             38.640   6.216   
# Number of obs: 14171, groups:  Jday, 34
# 
# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                              1.149e+02  2.213e+00  9.219e+03  51.943  < 2e-16 ***
# WindSp                                   7.921e-02  1.019e-02  1.376e+04   7.770 8.39e-15 ***
# log10(MinDistMoving + 1)                -5.511e+00  4.930e-01  1.414e+04 -11.179  < 2e-16 ***
# log10(MinDistAnchored + 1)               1.301e+00  3.626e-01  1.416e+04   3.588 0.000335 ***
# AveSpeedMoving                           1.416e+00  1.597e-01  1.414e+04   8.865  < 2e-16 ***
# RecorderRCA_Out                          6.417e+00  1.449e-01  1.413e+04  44.298  < 2e-16 ***
# Year2020                                -6.296e+00  5.952e-01  3.287e+01 -10.577 4.09e-12 ***
# log10(MinDistMoving + 1):AveSpeedMoving -3.573e-01  4.475e-02  1.414e+04  -7.984 1.53e-15 ***
# RecorderRCA_Out:Year2020                -4.108e+00  2.091e-01  1.413e+04 -19.643  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp lg10(MDM+1) l10(MDA+1 AvSpdM RcRCA_O Yr2020 l10(MDM+1):
# WindSp       0.001                                                               
# lg10(MDM+1) -0.781 -0.024                                                        
# lg10(MDA+1) -0.566 -0.038 -0.031                                                 
# AveSpedMvng -0.719 -0.023  0.905      -0.013                                     
# RcrdrRCA_Ot -0.017  0.007 -0.023       0.003    -0.004                           
# Year2020    -0.114  0.019  0.001      -0.039     0.009  0.122                    
# l10(MDM+1):  0.713  0.021 -0.898       0.007    -0.997  0.004  -0.007            
# RRCA_O:Y202  0.016 -0.007 -0.005       0.019     0.003 -0.690  -0.176 -0.003  





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
Model17<-lmer(SPL_BB~WindSp+log10(MinDistMoving+1)+log10(MinDistAnchored+1)+AveSpeedMoving+log10(MinDistMoving+1):AveSpeedMoving+Recorder*Year+(1|Jday),data=Data_reduced)
summary(Model17)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: SPL_BB ~ WindSp + log10(MinDistMoving + 1) + log10(MinDistAnchored +  
#     1) + AveSpeedMoving + log10(MinDistMoving + 1):AveSpeedMoving +      Recorder * Year + (1 | Jday)
#    Data: Data_reduced
# 
# REML criterion at convergence: 90204.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.4616 -0.6229 -0.1215  0.4706  8.5214 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Jday     (Intercept)  2.544   1.595   
#  Residual             33.724   5.807   
# Number of obs: 14171, groups:  Jday, 34
# 
# Fixed effects:
#                                           Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                              1.147e+02  2.186e+00  9.746e+03  52.499  < 2e-16 ***
# WindSp                                   8.541e-02  9.524e-03  1.380e+04   8.968  < 2e-16 ***
# log10(MinDistMoving + 1)                -5.476e+00  4.605e-01  1.414e+04 -11.890  < 2e-16 ***
# log10(MinDistAnchored + 1)               1.938e+00  3.905e-01  1.416e+04   4.963 7.01e-07 ***
# AveSpeedMoving                           1.227e+00  1.492e-01  1.414e+04   8.222  < 2e-16 ***
# RecorderRCA_Out                          5.569e+00  1.353e-01  1.413e+04  41.154  < 2e-16 ***
# Year2020                                -6.608e+00  5.666e-01  3.294e+01 -11.663 3.08e-13 ***
# log10(MinDistMoving + 1):AveSpeedMoving -3.109e-01  4.181e-02  1.414e+04  -7.437 1.09e-13 ***
# RecorderRCA_Out:Year2020                 3.597e+00  1.447e+00  1.416e+04   2.485    0.013 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) WindSp lg10(MDM+1) l10(MDA+1 AvSpdM RcRCA_O Yr2020 l10(MDM+1):
# WindSp       0.002                                                               
# lg10(MDM+1) -0.739 -0.025                                                        
# lg10(MDA+1) -0.625 -0.034 -0.027                                                 
# AveSpedMvng -0.680 -0.024  0.905      -0.011                                     
# RcrdrRCA_Ot -0.016  0.007 -0.023       0.003    -0.004                           
# Year2020    -0.104  0.018  0.001      -0.043     0.009  0.120                    
# l10(MDM+1):  0.674  0.021 -0.898       0.007    -0.997  0.004  -0.006            
# RRCA_O:Y202 -0.616 -0.035 -0.027       0.991    -0.011 -0.091  -0.066  0.006  