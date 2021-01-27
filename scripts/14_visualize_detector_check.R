#Load packages
library(tidyverse)
library(ggeffects)

#read in data
om<-read_rds("wdata/one_minute_review_reduceddata.rds")
fm<-read_rds("wdata/five_minute_review_reduceddata.rds")

# first going to look at one minute data at old intervals and averaged spl
# for the purposes of this I'm going to assign calls that the detector did not find to an auto.cass of NN
om.old.auto<-om %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

#also for the purposes of this I'm going to assign the calls manually labeled FS as N because these are
#calls that were split across multiple detections
om.old.man<-om %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

# now get mean spl per interval

om.old.spl<-om%>%
  group_by(spl.interval)%>%
  summarize(spl=mean(SPL),
            wave.ht=mean(wave.ht),
            wave.prd=mean(wave.prd),
            wind_spd=mean(wind_spd),
            wind_dir=mean(wind_dir),
            hr=mean(hr))

# now link these

om.old<-left_join(om.old.auto,om.old.man)%>%
  left_join(om.old.spl)%>%
  mutate(tot.call=auto.fish+auto.non)

# get residuals of regressing man.fish~auto.fish

om.old$resids<-residuals(lm(man.fish~auto.fish,data=om.old))

om.old2<-om.old%>%
  pivot_longer(spl:tot.call,names_to="measure",values_to="values")

ggplot(data=om.old2)+
  geom_point(aes(x=values,y=resids))+
  facet_wrap(~measure,scales="free")




# Now going to do the same thing for the new spl intervals and only keep the intervals 
# with more than 5 calls
# for the purposes of this I'm going to assign calls that the detector did not find to an auto.cass of NN
om.auto<-om %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

#also for the purposes of this I'm going to assign the calls manually labeled FS as N because these are
#calls that were split across multiple detections
om.man<-om %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

# now get mean spl per interval

om.spl<-om%>%
  group_by(spl.interval)%>%
  summarize(spl=mean(SPL),
            wave.ht=mean(wave.ht),
            wave.prd=mean(wave.prd),
            wind_spd=mean(wind_spd),
            wind_dir=mean(wind_dir),
            hr=mean(hr))

# now link these

om.sum<-left_join(om.auto,om.man)%>%
  left_join(om.spl)%>%
  mutate(tot.call=auto.fish+auto.non)%>%
  filter(tot.call>5)

# get residuals of regressing man.fish~auto.fish

om.sum$resids<-residuals(lm(man.fish~auto.fish,data=om.sum))

om.sum2<-om.sum%>%
  pivot_longer(spl:tot.call,names_to="measure",values_to="values")

ggplot(data=om.sum2)+
  geom_point(aes(x=values,y=resids))+
  facet_wrap(~measure,scales="free")

om.sum$spl2<-om.sum$spl^2
om.nona<-om.sum %>%
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wave.ht))

# do a quick linear model
call.lm<-lm(man.fish~auto.fish*spl*spl2*wind_spd,data=om.nona)
summary(call.lm)

options(na.action = "na.fail")
x<-MuMIn::dredge(call.lm)
options(na.action = "na.omit")
head(x)

call.lm.best<-lm(man.fish~auto.fish+
                   spl+
                   spl2+
                   wind_spd+
                   auto.fish*spl+
                   auto.fish*spl2+
                   wind_spd*spl+
                   spl2*wind_spd,data=om.nona)
summary(call.lm.best)
om.nona$fits<-fitted(call.lm.best)

ggplot(om.nona)+
  geom_point(aes(x=man.fish,y=fits,color=spl))+
  geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
  scale_color_viridis_c()

ggplot(om.nona)+
  geom_point(aes(x=man.fish,y=auto.fish,color=spl))+
  geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
  scale_color_viridis_c()

# get dataset to point to check model on 5 min intervals
fm.auto<-fm %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

#also for the purposes of this I'm going to assign the calls manually labeled FS as N because these are
#calls that were split across multiple detections
fm.man<-fm %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class),
         man.class=ifelse(man.class=="NH","N",man.class),
         man.class=ifelse(man.class=="","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

# now get mean spl per interval

fm.spl<-fm%>%
  group_by(spl.interval)%>%
  summarize(spl=mean(SPL),
            wave.ht=mean(wave.ht),
            wave.prd=mean(wave.prd),
            wind_spd=mean(wind_spd),
            wind_dir=mean(wind_dir),
            hr=mean(hr))

fm.sum<-full_join(fm.auto,fm.man)%>%
  left_join(fm.spl)%>%
  mutate(spl2=spl^2)

fm.sum$preds<-predict(call.lm.best,fm.sum)

ggplot(fm.sum)+
  geom_point(aes(x=man.fish,y=preds,color=spl))+
  geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
  scale_color_viridis_c()

ggplot(fm.sum)+
  geom_point(aes(x=man.fish,y=auto.fish,color=spl))+
  geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
  scale_color_viridis_c()
