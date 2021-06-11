# script to work on models to correct fish detection to be closer to manual 

library(tidyverse)
library(ggeffects)
library(lubridate)
library(mgcv)
library(mgcViz)
library(glmmTMB)


# read in datasets
om19<-read_rds("wdata/one_minute_plus_followup.rds")%>%
  select(-int.old)
fm19<-read_rds("wdata/five_minute_review_reduceddata.rds")
om20<-read_rds("wdata/2020_review_reduceddata.rds")


# bring in vessel detections
vd19<-read.csv("boatnoise_detector/RCA-In_April_July2019_1342218252__20190410_20190625__AOutput.csv")
vd20<-bind_rows(read.csv("boatnoise_detector/RCA_in_2020_RCAin_200418_1505_5047__20200418_20200504__AOutput.csv"),
                read.csv("boatnoise_detector/RCA_in_2020_RCAin_200524_1149_5042__20200524_20200622__AOutput.csv"))

vd19<-vd19%>%
  mutate(Time=ymd_hm(Time),
         yr=year(Time),
         m=month(Time),
         d=day(Time),
         hr=hour(Time),
         min=minute(Time),
         boat = case_when(
           shipping.detection.flag==0~0,
           shipping.detection.flag==1~2,
           shipping.detection.flag==4~1))%>%
  rename(boat.close=vesselCPAFlag)

vd20<-vd20%>%
  mutate(Time=ymd_hm(Time),
         yr=year(Time),
         m=month(Time),
         d=day(Time),
         hr=hour(Time),
         min=minute(Time),
         boat = case_when(
           shipping.detection.flag==0~0,
           shipping.detection.flag==1~2,
           shipping.detection.flag==4~1))%>%
  rename(boat.close=vesselCPAFlag)

om19b<-left_join(om19,vd19)%>%
  select(-Confidence,-Time,-shipping.detection.flag,-man.type,-s)%>%
  mutate(utmDateTime = datetime + hours(7),
    doy = as.numeric(strftime(utmDateTime, format = "%j")),
    af=case_when(
    auto.class=="NN"~0,
    auto.class=="FS"~1,
    is.na(auto.class)~0),
    mf=case_when(
      man.class=="N"~0,
      man.class=="NN"~0,
      man.class=="M"~0,
      man.class=="F"~1,
      man.class=="FS"~1,
      man.class==""~0,
      man.class=="G"~0))%>%
  group_by(yr,m,d,hr,min,spl.interval)%>%
  summarize(spl=mean(SPL,na.rm = T),
         wave.ht=mean(wave.ht),na.rm = T,
         wave.prd=mean(wave.prd,na.rm = T),
         wind_spd=mean(wind_spd,na.rm = T),
         wind_dir=mean(wind_dir,na.rm = T),
         temp=mean(temp,na.rm = T),
         min=min(min,na.rm = T),
         hr=min(hr,na.rm = T),
         doy=min(doy,na.rm = T),
         month=min(m,na.rm = T),
         boat=mean(boat,na.rm=T),
         boat.close=mean(boat.close,na.rm=T),
         auto.fish=sum(af),
         man.fish=sum(mf))%>%
  filter(wave.prd<10)%>% # remove extreme outliers
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>% 
  ungroup()%>%
  mutate(log.auto.fish = log(auto.fish+1),
         log.fish.sc = scale(log.auto.fish),
         log.fish.sc2 = log.fish.sc^2,
         auto.fish2 = auto.fish^2,
         auto.fish.sc = scale(auto.fish),
         auto.fish.sc2 = auto.fish.sc^2,  
         spl.sc = scale(spl),
         spl2 = spl^2,
         spl.sc2 = spl.sc^2,
         boat.sc=scale(boat),
         boat.sc2=boat.sc^2,
         wind.spd = scale(wind_spd),
         wind.spd2 = wind.spd^2,
         wave.ht = scale(wave.ht),
         wave.ht2 = wave.ht^2,
         wind.dir.not.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 0, 1),
         wind.dir.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 1, 0),
         wind.dir.E = if_else(wind_dir >= 4.5 & wind_dir < 13.5, 1, 0),
         wind.dir.S = if_else(wind_dir >= 13.5 & wind_dir < 22.5, 1, 0),
         wind.dir.W = if_else(wind_dir >= 22.5 & wind_dir < 31.5, 1, 0),
         wave.prd = scale(wave.prd),
         wave.prd2 = wave.prd^2,
         ord = as.factor(spl.interval),
         hr = as.factor(hr))

fm19b<-left_join(fm19,vd19)%>%
  select(-Confidence,-Time,-shipping.detection.flag,-man.type,-s)%>%
  mutate(utmDateTime = datetime + hours(7),
         doy = as.numeric(strftime(utmDateTime, format = "%j")),
         af=case_when(
           auto.class=="NN"~0,
           auto.class=="FS"~1,
           is.na(auto.class)~0),
         mf=case_when(
           man.class=="N"~0,
           man.class=="NH"~0,
           man.class=="F"~1,
           man.class=="FS"~1))%>%
  group_by(yr,m,d,hr,min,spl.interval)%>%
  summarize(spl=mean(SPL,na.rm = T),
            wave.ht=mean(wave.ht),na.rm = T,
            wave.prd=mean(wave.prd,na.rm = T),
            wind_spd=mean(wind_spd,na.rm = T),
            wind_dir=mean(wind_dir,na.rm = T),
            temp=mean(temp,na.rm = T),
            min=min(min,na.rm = T),
            hr=min(hr,na.rm = T),
            doy=min(doy,na.rm = T),
            month=min(m,na.rm = T),
            boat=mean(boat,na.rm=T),
            boat.close=mean(boat.close,na.rm=T),
            auto.fish=sum(af),
            man.fish=sum(mf))%>%
  filter(wave.prd<10)%>% # remove extreme outliers
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>% 
  ungroup()%>%
  mutate(log.auto.fish = log(auto.fish+1),
    log.fish.sc = scale(log.auto.fish),
    log.fish.sc2 = log.fish.sc^2,
    auto.fish2 = auto.fish^2,
    auto.fish.sc = scale(auto.fish),
    auto.fish.sc2 = auto.fish.sc^2,  
    spl.sc = scale(spl),
    spl2 = spl^2,
    spl.sc2 = spl.sc^2,
    boat.sc=scale(boat),
    boat.sc2=boat.sc^2,
    wind.spd = scale(wind_spd),
    wind.spd2 = wind.spd^2,
    wave.ht = scale(wave.ht),
    wave.ht2 = wave.ht^2,
    wind.dir.not.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 0, 1),
    wind.dir.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 1, 0),
    wind.dir.E = if_else(wind_dir >= 4.5 & wind_dir < 13.5, 1, 0),
    wind.dir.S = if_else(wind_dir >= 13.5 & wind_dir < 22.5, 1, 0),
    wind.dir.W = if_else(wind_dir >= 22.5 & wind_dir < 31.5, 1, 0),
    wave.prd = scale(wave.prd),
    wave.prd2 = wave.prd^2,
    ord = as.factor(spl.interval),
    hr = as.factor(hr))
  

om20b<-left_join(om20,vd20)%>%
  select(-Confidence,-Time,-shipping.detection.flag,-man.type,-s)%>%
  mutate(utmDateTime = datetime + hours(7),
         doy = as.numeric(strftime(utmDateTime, format = "%j")),
         af=case_when(
           auto.class=="NN"~0,
           auto.class=="FS"~1,
           is.na(auto.class)~0),
         mf=case_when(
           man.class=="N"~0,
           man.class=="NF"~0,
           man.class=="F"~1,
           man.class=="B"~0,
           man.class=="K"~0,
           man.class=="FS"~1,))%>%
  group_by(yr,m,d,hr,min,spl.interval)%>%
  summarize(spl=mean(SPL,na.rm = T),
            wave.ht=mean(wave.ht),na.rm = T,
            wave.prd=mean(wave.prd,na.rm = T),
            wind_spd=mean(wind_spd,na.rm = T),
            wind_dir=mean(wind_dir,na.rm = T),
            temp=mean(temp,na.rm = T),
            min=min(min,na.rm = T),
            hr=min(hr,na.rm = T),
            doy=min(doy,na.rm = T),
            month=min(m,na.rm = T),
            boat=mean(boat,na.rm=T),
            boat.close=mean(boat.close,na.rm=T),
            auto.fish=sum(af),
            man.fish=sum(mf))%>%
  filter(wave.prd<10)%>% # remove extreme outliers
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>%
  filter(!is.na(boat))%>%  
  ungroup()%>%
  mutate(log.auto.fish = log(auto.fish+1),
         log.fish.sc = scale(log.auto.fish),
         log.fish.sc2 = log.fish.sc^2,
         auto.fish2 = auto.fish^2,
         auto.fish.sc = scale(auto.fish),
         auto.fish.sc2 = auto.fish.sc^2,  
         spl.sc = scale(spl),
         spl2 = spl^2,
         spl.sc2 = spl.sc^2,
         boat.sc=scale(boat),
         boat.sc2=boat.sc^2,
         wind.spd = scale(wind_spd),
         wind.spd2 = wind.spd^2,
         wave.ht = scale(wave.ht),
         wave.ht2 = wave.ht^2,
         wind.dir.not.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 0, 1),
         wind.dir.N = if_else(wind_dir < 4.5 | wind_dir >= 31.5, 1, 0),
         wind.dir.E = if_else(wind_dir >= 4.5 & wind_dir < 13.5, 1, 0),
         wind.dir.S = if_else(wind_dir >= 13.5 & wind_dir < 22.5, 1, 0),
         wind.dir.W = if_else(wind_dir >= 22.5 & wind_dir < 31.5, 1, 0),
         wave.prd = scale(wave.prd),
         wave.prd2 = wave.prd^2,
         ord = as.factor(spl.interval),
         hr = as.factor(hr))



all2019<-bind_rows(om19b,fm19b)

lm.model.choose<-function(data){
  for(i in 1:100){
    d1<-slice_sample(data,prop = .7, replace = FALSE)
    mod3<-lm(log(man.fish+1) ~doy+log.fish.sc+log.fish.sc2+boat.sc+boat.sc2+spl.sc+spl.sc2+boat.close+wind.spd+wave.ht,
             data=d1,na.action = na.fail)
    if(i==1)mresults<-MuMIn::dredge(mod3)[1,]
    if(i!=1)mresults<-bind_rows(mresults,MuMIn::dredge(mod3)[1,])
  }
  mresults<-data.frame(mresults)%>%
    select(-1,-df:-weight)%>%
    mutate(across(where(is.numeric),~ifelse(is.na(.x),0,1)))
  return(colSums(mresults))
}

lm.model.choosenolog<-function(data){
  for(i in 1:100){
    d1<-slice_sample(data,prop = .7, replace = FALSE)
    mod3<-lm(man.fish ~doy+log.fish.sc+log.fish.sc2+auto.fish.sc+auto.fish.sc2+boat.sc+boat.sc2+spl.sc+spl.sc2+boat.close+wind.spd+wave.ht,
             data=d1,na.action = na.fail)
    if(i==1)mresults<-MuMIn::dredge(mod3)[1,]
    if(i!=1)mresults<-bind_rows(mresults,MuMIn::dredge(mod3)[1,])
  }
  mresults<-data.frame(mresults)%>%
    select(-1,-df:-weight)%>%
    mutate(across(where(is.numeric),~ifelse(is.na(.x),0,1)))
  return(colSums(mresults))
}


lm.2019<-lm.model.choose(data=all2019)
lm.2020<-lm.model.choose(data=om20b)
lm.2020.nolog<-lm.model.choosenolog(data=om20b)

(lm.results<-data.frame(dataset=c("2019","2020","2020-no log"),
                       bind_rows(lm.2019,lm.2020,lm.2020.nolog)))

# 2019 model boat.sc2, log.fish.sc, log.fish.sc2, spl.sc,spl.sc2
#2020 model doy,log.fish.sc, spl.sc also try wind.spd, and boat.close

# 2020 no log - boat.close+spl.sc+auto.fish.sc
# now look at how these models 1) fit the data they are made with and 2) predict test data



lm19<-lm(log(man.fish+1)~boat.sc2+ log.fish.sc+ log.fish.sc2+ spl.sc+spl.sc2,data=all2019)
lm20.simple<-lm(log(man.fish+1)~doy+log.fish.sc+ spl.sc,data=om20b)
lm20.big<-lm(log(man.fish+1)~doy+log.fish.sc+ spl.sc+wind.spd+boat.close,data=om20b)
lm20.nolog<-lm(man.fish~boat.close+spl.sc+auto.fish.sc,data=om20b)


ten.fold.lm <- function(data,prop.d,model,logs=F) { 
  output.info = list(
    fit = data.frame(iteration = NA, MSE = NA,R2=NA)[-1, ],
    pred.values = data.frame(man.fish = NA, pred.fish = NA)[-1, ])
  for (i in 1:10) {
    data$ID<-seq(1:nrow(data))
    train.data <- data %>%
      slice_sample(prop = prop.d, replace = FALSE)
    
    test.data <- data %>%
      filter(!ID %in% train.data$ID)
    
    mod2<-update(model,.~.,data=train.data)
    
    if(logs==F){t1 <- data.frame(iteration = i, 
                                 MSE = mean((train.data$man.fish-predict(
                                   mod2, newdata = train.data, re.form = NA ))),
                                 R2=summary(mod2)$adj.r.squared)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = predict(
                       mod2, newdata = test.data, re.form = NA))
    }
    if(logs==T){t1 <- data.frame(iteration = i, 
                                 MSE = mean((train.data$man.fish-exp(predict(
                                   mod2, newdata = train.data, re.form = NA )))),
                                 R2=summary(mod2)$adj.r.squared)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = exp(predict(
                       mod2, newdata = test.data, re.form = NA)))
    }   
    
    output.info[[1]] <- bind_rows(output.info[[1]], t1)
    output.info[[2]] <- bind_rows(output.info[[2]], t2)
  }
  return(output.info)
}
ten.fold.gam <- function(data,prop.d) { 
  library(mgcv)
  library(mgcViz)
  library(glmmTMB)
  output.info = list(
    fit = data.frame(iteration = NA, MSE = NA,R2=NA)[-1, ],
    pred.values = data.frame(man.fish = NA, pred.fish = NA)[-1, ])
  for (i in 1:10) {
    data<-data%>%
      mutate(ID=row_number())
    
    train.data <- data %>%
      slice_sample(prop = prop.d, replace = FALSE)
    
    test.data <- data %>%
      filter(!ID %in% train.data$ID)
    
    mod2<-gamm(man.fish ~ te(spl.sc, log.fish.sc, bs = "ts") +
                 te(wind_dir, wind.spd, bs = c("cc", "ts")),
               family=nbinom2, 
               data=train.data)
    
    t1 <- data.frame(iteration = i, 
                     MSE = mean((train.data$man.fish-exp(predict(
                       mod2$gam, newdata = train.data, re.form = NA )))),
                     R2=summary(mod2$gam)$r.sq)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = exp(predict(
                       mod2$gam, newdata = test.data, re.form = NA)))
    
    output.info[[1]] <- bind_rows(output.info[[1]], t1)
    output.info[[2]] <- bind_rows(output.info[[2]], t2)
  }
  return(output.info)
}
ten.fold.gam.boat <- function(data,prop.d) { 
  library(mgcv)
  library(mgcViz)
  library(glmmTMB)
  output.info = list(
    fit = data.frame(iteration = NA, MSE = NA,R2=NA)[-1, ],
    pred.values = data.frame(man.fish = NA, pred.fish = NA)[-1, ])
  for (i in 1:10) {
    data<-data%>%
      mutate(ID=row_number())
    
    train.data <- data %>%
      slice_sample(prop = prop.d, replace = FALSE)
    
    test.data <- data %>%
      filter(!ID %in% train.data$ID)
    
    mod2<-gamm(man.fish ~ boat.sc+boat.sc2+
                 te(spl.sc, log.fish.sc, bs = "ts") +
                 te(wind_dir, wind.spd, bs = c("cc", "ts")),
               family=nbinom2, 
               data=train.data)
    
    t1 <- data.frame(iteration = i, 
                     MSE = mean((train.data$man.fish-exp(predict(
                       mod2$gam, newdata = train.data, re.form = NA )))),
                     R2=summary(mod2$gam)$r.sq)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = exp(predict(
                       mod2$gam, newdata = test.data, re.form = NA)))
    
    output.info[[1]] <- bind_rows(output.info[[1]], t1)
    output.info[[2]] <- bind_rows(output.info[[2]], t2)
  }
  return(output.info)
}

lm19.mse<-ten.fold.lm(data=all2019,prop.d = .9,model=lm19,logs=T)
lm20.simple.mse<-ten.fold.lm(data=om20b,prop.d = .9,model=lm20.simple,logs=T)
lm20.big.mse<-ten.fold.lm(data=om20b,prop.d = .9,model=lm20.big,logs=T)
lm20.nolog.mse<-ten.fold.lm(data=om20b,prop.d = .9,model=lm20.big,logs=F)
gam19<-ten.fold.gam(all2019,prop.d = .9)
gam20<-ten.fold.gam(om20b,prop.d = .9)
gam19.boat<-ten.fold.gam.boat(all2019,prop.d = .9)
gam20.boat<-ten.fold.gam.boat(om20b,prop.d = .9)




#2019 models
par(mfrow=c(3,2))
hist(lm19.mse$fit$R2)
hist(lm19.mse$fit$MSE)

hist(gam19[[1]]$R2)
hist(gam19[[1]]$MSE)

hist(gam19.boat[[1]]$R2)
hist(gam19.boat[[1]]$MSE)

#pred vs manual
par(mfrow=c(3,1))

plot(lm19.mse$pred.values$pred.fish~lm19.mse$pred.values$man.fish)
abline(a=0,b=1)

plot(gam19$pred.values$pred.fish~gam19$pred.values$man.fish)
abline(a=0,b=1)

plot(gam19.boat$pred.values$pred.fish~gam19.boat$pred.values$man.fish)
abline(a=0,b=1)


#2020 models
par(mfrow=c(5,2))
hist(lm20.simple.mse$fit$R2)
hist(lm20.simple.mse$fit$MSE)

hist(lm20.big.mse$fit$R2)
hist(lm20.big.mse$fit$MSE)

hist(lm20.nolog.mse$fit$R2)
hist(lm20.nolog.mse$fit$MSE)

hist(gam20[[1]]$R2)
hist(gam20[[1]]$MSE)

hist(gam20.boat[[1]]$R2)
hist(gam20.boat[[1]]$MSE)

#pred vs manual for simple and gams
par(mfrow=c(3,1))

plot(lm20.simple.mse$pred.values$pred.fish~lm20.simple.mse$pred.values$man.fish)
abline(a=0,b=1)

plot(gam20$pred.values$pred.fish~gam20$pred.values$man.fish)
abline(a=0,b=1)

plot(gam20.boat$pred.values$pred.fish~gam20.boat$pred.values$man.fish)
abline(a=0,b=1)

# incorporating boat in the 2020 gam does the best  at estimating the high fish calls
# I recommend using this model going forward. 
