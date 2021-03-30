# cleaning up 14_visualize_detector_check

#Load packages
library(tidyverse)
library(ggeffects)
library(lubridate)
library(mgcv)
library(mgcViz)
library(glmmTMB)

#read in data
om<-read_rds("wdata/one_minute_review_reduceddata.rds")
fm<-read_rds("wdata/five_minute_review_reduceddata.rds")
# NOTE: in interval 17755 in the 2020 dataset there is something rubbing on the hydrophone
# if this is an outlier causing problems that may be a reason to remove it?
om20<-read_rds("wdata/2020_review_reduceddata.rds")

# organize data to get total calls per minute (both manual and auto)
#also for the purposes of this I'm going to assign the calls manually labeled FS as N because these are
#calls that were split across multiple detections
om.auto<-om %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

fm.auto<-fm %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

om20.auto<-om20 %>%
  mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
  group_by(spl.interval,auto.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
  rename(auto.fish=FS,auto.non=NN)

om.man<-om %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

fm.man <-fm %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

om20.man<-om20 %>%
  mutate(man.class=ifelse(man.class=="FS","N",man.class))%>%
  group_by(spl.interval,man.class)%>%
  summarize(ncall=n())%>%
  pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
  rename(man.fish=F,man.non=N)

# pull in downloaded tide data from
# https://www.pac.dfo-mpo.gc.ca/science/charts-cartes/obs-app/observed-eng.aspx?StationID=08545
filelist <-list.files("odata/nanaimo-tides/", pattern = "*.csv")

for(i in 1:length(filelist)){
  temp2<-read_csv(paste0("odata/nanaimo-tides/",filelist[i])) 
  if(i==1) tides<-temp2
  if(i!=1) tides<-dplyr::bind_rows(tides,temp2)
}

tides$datetime <- tides[[2]] - hours(1)

tides <- select(tides, datetime, tide=RADAR) %>% 
  mutate(
    yr = year(datetime),
    m = month(datetime),
    d = day(datetime),
    hr = hour(datetime)
  ) %>% select(-datetime)

# calculate mean spl per interval and join with tides
om.spl<-om%>%
  left_join(tides)%>%
  mutate(
    utmDateTime = datetime + hours(7),
    doy = as.numeric(strftime(utmDateTime, format = "%j"))
  )%>%
  group_by(spl.interval)%>%
  summarize(
    spl=mean(SPL,na.rm = T),
    tide = mean(tide,na.rm = T),
    wave.ht=mean(wave.ht),na.rm = T,
    wave.prd=mean(wave.prd,na.rm = T),
    wind_spd=mean(wind_spd,na.rm = T),
    wind_dir=mean(wind_dir,na.rm = T),
    temp=mean(temp,na.rm = T),
    min=min(min,na.rm = T),
    hr=min(hr,na.rm = T),
    doy=min(doy,na.rm = T),
    month=min(m,na.rm = T)
  )

fm.spl<-fm%>%
  left_join(tides)%>%
  mutate(
    utmDateTime = datetime + hours(7),
    doy = as.numeric(strftime(utmDateTime, format = "%j"))
  )%>%
  group_by(spl.interval)%>%
  summarize(
    spl=mean(SPL,na.rm = T),
    tide = mean(tide,na.rm = T),
    wave.ht=mean(wave.ht),na.rm = T,
    wave.prd=mean(wave.prd,na.rm = T),
    wind_spd=mean(wind_spd,na.rm = T),
    wind_dir=mean(wind_dir,na.rm = T),
    temp=mean(temp,na.rm = T),
    min=min(min,na.rm = T),
    hr=min(hr,na.rm = T),
    doy=min(doy,na.rm = T),
    month=min(m,na.rm = T)
  )


om20.spl<-om20%>%
  left_join(tides)%>%
  mutate(
    utmDateTime = datetime + hours(7),
    doy = as.numeric(strftime(utmDateTime, format = "%j"))
  )%>%
  group_by(spl.interval)%>%
  summarize(
    spl=mean(SPL,na.rm = T),
    tide = mean(tide,na.rm = T),
    wave.ht=mean(wave.ht),na.rm = T,
    wave.prd=mean(wave.prd,na.rm = T),
    wind_spd=mean(wind_spd,na.rm = T),
    wind_dir=mean(wind_dir,na.rm = T),
    temp=mean(temp,na.rm = T),
    min=min(min,na.rm = T),
    hr=min(hr,na.rm = T),
    doy=min(doy,na.rm = T),
    month=min(m,na.rm = T)
  )

# now link these

om.sum<-left_join(om.auto,om.man)%>%
  left_join(om.spl)%>%
  mutate(tot.call=auto.fish+auto.non)

fm.sum<-left_join(fm.auto,fm.man)%>%
  left_join(fm.spl)%>%
  mutate(tot.call=auto.fish+auto.non)

om20.sum<-left_join(om20.auto,om20.man)%>%
  left_join(om20.spl)%>%
  mutate(tot.call=auto.fish+auto.non)

# now refine datasets
om.sum$spl2<-om.sum$spl^2
om.nona<-om.sum %>%
  filter(wave.prd<10)%>% # remove two extreme outliers
  filter(!is.na(tide))%>%
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>%
  mutate(dataset="om.2019",
         yr=2019)
  

fm.sum$spl2<-fm.sum$spl^2
fm.nona<-fm.sum %>%
  filter(wave.prd<10)%>% # remove two extreme outliers
  filter(!is.na(tide))%>%
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>%
  mutate(dataset="fm.2019",
         yr=2019)

om20.sum$spl2<-om20.sum$spl^2
om20.nona<-om20.sum%>%
  filter(wave.prd<10)%>% # remove two extreme outliers
  filter(!is.na(tide))%>%
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))%>%
  mutate(dataset="om.2020",
         yr=2020)

# now create a single dataset to scale variables
allcheck<-bind_rows(om.nona,fm.nona,om20.nona)%>%
  select(-NH,-B,-K,-NF)

# scale variables
allcheck2 <- allcheck %>% ungroup() %>% mutate(
  # auto.fish = ifelse(auto.fish>35, 35, auto.fish),
  log.auto.fish = log(auto.fish+1),
  log.fish.sc = scale(log.auto.fish),
  auto.fish2 = auto.fish^2,
  auto.fish.sc = scale(auto.fish),
  auto.fish.sc2 = auto.fish.sc^2,  
  spl.sc = scale(spl),
  spl2 = spl^2,
  spl.sc2 = spl.sc^2,
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
  tide.sc = scale(tide),
  tide.sc2 = tide.sc^2,
  # temp.sc = scale(temp),
  ord = as.factor(spl.interval),
  hr = as.factor(hr)
)


# need varible types to match for prediction so change data
allcheck3 <- allcheck2 %>% select(dataset,spl.interval, man.fish, tide.sc,auto.fish.sc, log.auto.fish, log.fish.sc, spl.sc, wind.spd, wave.ht, wave.prd, wind_dir, month, doy, hr)

allcheck3[] <- lapply(allcheck3, function(x) { attributes(x) <- NULL; x })
str(allcheck3)
allcheck3$month.f <- as.factor(allcheck3$month)

allcheck2[] <- lapply(allcheck2, function(x) { attributes(x) <- NULL; x })
str(allcheck2)
allcheck2$month.f <- as.factor(allcheck2$month)


# now start looking at models
# Note that all models here predictions don't incorperate random effects, although the fixed effect estimates are with the random effect variation excluded

#GAM model
mod <- gamm(man.fish ~ 
               log.fish.sc + 
              # spl.sc + 
              # wind.spd +
              # s(wave.ht, bs = "ts") +
              # s(wave.prd, bs = "ts", k=3) +
              # s(spl.sc) +
              #  s(log.fish.sc) +
              # s(tide.sc, bs = "ts") +
              # ti(wave.ht, wave.prd, bs = "ts") + 
              te(spl.sc, log.fish.sc, bs = "ts") +#, k=c(3,3)
              te(wind_dir, wind.spd, bs = c("cc", "ts")),# + #, k=c(5,3)
              # s(hr, doy,bs=c("cc", "re"), k=c(5,3)) # doesn't work
             # s(doy, bs="re"),
            # s(wave.prd, k=3)
            #  s(wind.spd, wind.dir.not.N, k=4)
            # te(wind_dir, wind.spd, bs = "cc", k=5)
            # s(wind_dir, bs = "cc", k=8)
            # + te(wind.spd, spl.sc) + te(auto.fish, wind.spd) + te(auto.fish, spl.sc)
            # , correlation=corAR1(form=~1|doy:hr),
            #, correlation=corAR1(form=~1|spl.interval),
            # family=poisson(), 
            # family=nbinom1,
            family=nbinom2, # lower R2 and all effects become linear
            # data=om.nona2) # to experiment with other variables in full dataset
            data=allcheck3%>%
              filter(dataset=="om.2019"))

summary(mod$gam)

plot(residuals(mod$gam))

mod20 <- gamm(man.fish ~ 
              log.fish.sc +
              # spl.sc +
              # wind.spd +
              # s(wave.ht, bs = "ts") +
              # s(wave.prd, bs = "ts", k=3) +
              # s(spl.sc) +
              #  s(log.fish.sc) +
              # s(tide.sc, bs = "ts") +
              # ti(wave.ht, wave.prd, bs = "ts") +
              te(spl.sc, log.fish.sc, bs = "ts") +#, k=c(3,3)
              te(wind_dir, wind.spd, bs = c("cc", "ts")),# + #, k=c(5,3)
            # s(hr, doy,bs=c("cc", "re"), k=c(5,3)) # doesn't work
            # s(doy, bs="re"),
            # s(wave.prd, k=3)
            #  s(wind.spd, wind.dir.not.N, k=4)
            # te(wind_dir, wind.spd, bs = "cc", k=5)
            # s(wind_dir, bs = "cc", k=8)
            # + te(wind.spd, spl.sc) + te(auto.fish, wind.spd) + te(auto.fish, spl.sc)
            # , correlation=corAR1(form=~1|doy:hr),
            #, correlation=corAR1(form=~1|spl.interval),
            # family=poisson(), 
            # family=nbinom1,
            family=nbinom2, # lower R2 and all effects become linear
            # data=om.nona2) # to experiment with other variables in full dataset
            data=allcheck3%>%
              filter(dataset=="om.2020"))

summary(mod20$gam)
allcheck2$gampred<-exp(predict(mod$gam, newdata = allcheck2, re.form = NA ))
allcheck2$gampred20<-exp(predict(mod20$gam, newdata = allcheck2, re.form = NA ))

ggplot(data=allcheck2)+
  geom_point(aes(x=man.fish,y=gampred,color=dataset))+
  geom_abline(aes(intercept=0,slope=1))

ggplot(data=allcheck2)+
  geom_point(aes(x=man.fish,y=gampred20,color=dataset))+
  geom_abline(aes(intercept=0,slope=1))
# pull in all original data

calls<-read_rds("wdata/all_fish_calls_with_weather.rds")

calls2<-calls %>% ungroup() %>% mutate(
  # ncall = ifelse(ncall>35, 35, ncall),
  log.ncall = log(ncall+1),
  log.fish.sc = scale(log.ncall),
  ncall2 = ncall^2,
  ncall.sc = scale(ncall),
  ncall.sc2 = ncall.sc^2,  
  spl.sc = scale(spl),
  spl2 = spl^2,
  spl.sc2 = spl.sc^2,
  wind.spd = scale(wspeed),
  wind.spd2 = wind.spd^2,
  wave.ht = scale(wave.ht),
  wave.ht2 = wave.ht^2,
  wind.dir.not.N = if_else(wdir < 4.5 | wdir >= 31.5, 0, 1),
  wind.dir.N = if_else(wdir < 4.5 | wdir >= 31.5, 1, 0),
  wind.dir.E = if_else(wdir >= 4.5 & wdir < 13.5, 1, 0),
  wind.dir.S = if_else(wdir >= 13.5 & wdir < 22.5, 1, 0),
  wind.dir.W = if_else(wdir >= 22.5 & wdir < 31.5, 1, 0),
  wind_dir=wdir,
  wave.prd = scale(wave.prd),
  wave.prd2 = wave.prd^2,
  # tide.sc = scale(tide),
  # tide.sc2 = tide.sc^2,
  # temp.sc = scale(temp),
  # ord = as.factor(spl.interval),
  hr = as.factor(hr)
)
calls2[] <- lapply(calls2, function(x) { attributes(x) <- NULL; x })
str(calls2)
calls2$month.f <- as.factor(calls2$m)

calls2$cor.ncall<-exp(predict(mod$gam, newdata = calls2, re.form = NA ))

hist(calls2$cor.ncall)
hist(calls2$ncall)


fish2<-calls2 %>%
  mutate(DateTime=ymd_hms(paste(yr,m,d,hr,min,sec)),
         utmDateTime = DateTime + hours(7),
         doy = strftime(utmDateTime, format = "%j"),
         hr.min=as.numeric(hr)+(min/60)-1,
         ncall2=ifelse(cor.ncall<40,cor.ncall,40),
         ncall3=ifelse(ncall<40,ncall,40),
         doy2=as.numeric(doy))

# create different intervals to sum by

int.prds<-data.frame(hr.min=fish2$hr.min,five=NA,fifteen=NA,thirty=NA)%>%
  distinct()%>%
  arrange(hr.min)%>%
  mutate(five=rep(1:288,5)%>%sort(),
         fifteen=rep(1:96,15)%>%sort(),
         thirty=rep(1:48,30)%>%sort())

fish.5<-fish2%>%
  left_join(int.prds)%>%
  group_by(yr,doy,doy2,five)%>%
  summarize(ncall=sum(cor.ncall,na.rm =TRUE),
         ncall.perm=sum(cor.ncall,na.rm =TRUE)/5,
         tod=five/12,
         spl=mean(spl))


fish.15<-fish2%>%
  left_join(int.prds)%>%
  group_by(yr,doy,doy2,fifteen)%>%
  summarize(ncall=sum(cor.ncall,na.rm =TRUE),
            ncall.perm=sum(cor.ncall,na.rm =TRUE)/15,
            tod=fifteen/4,
            spl=mean(spl))


fish.30<-fish2%>%
  left_join(int.prds)%>%
  group_by(yr,doy,doy2,thirty)%>%
  summarize(ncall=sum(cor.ncall,na.rm =TRUE),
            ncall.perm=case_when(
              yr==2020 & doy >126 ~sum(cor.ncall,na.rm  =TRUE)/15,
              yr==2019~ sum(cor.ncall,na.rm =TRUE)/30,
              yr==2020 & doy <126 ~sum(cor.ncall,na.rm =TRUE)/30),
            tod=thirty/2,
            spl=mean(spl))


p19<-fish.15 %>% filter(yr == 2019)%>%
#  filter(doy2 >145 & doy2 <174)%>%
  filter(doy2 >111 & doy2 <123)%>%
  ggplot()+
  geom_tile(aes(doy,tod,color=ncall.perm,fill=ncall.perm),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "gray",limits=c(0,40)) +
  scale_fill_viridis_c(na.value = "gray",limits=c(0,40)) +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

p20<-fish.15 %>% filter(yr == 2020) %>%
#  filter(doy2 >145 & doy2 <174)%>%
  filter(doy2 >111 & doy2 <123)%>%
  ggplot()+
  geom_tile(aes(doy,tod,color=ncall.perm,fill=ncall.perm),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "gray",limits=c(0,40)) +
  scale_fill_viridis_c(na.value = "gray",limits=c(0,40)) +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

#library(patchwork)
p19/p20+plot_layout(widths = 1,heights = 1)

p19spl<-fish.15 %>% filter(yr == 2019)%>%
  #  filter(doy2 >145 & doy2 <174)%>%
  filter(doy2 >111 & doy2 <123)%>%
  ggplot()+
  geom_tile(aes(doy,tod,color=spl,fill=spl),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "gray",limits=c(85,120)) +
  scale_fill_viridis_c(na.value = "gray",limits=c(85,120)) +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

p20spl<-fish.15 %>% filter(yr == 2020) %>%
  #  filter(doy2 >145 & doy2 <174)%>%
  filter(doy2 >111 & doy2 <123)%>%
  ggplot()+
  geom_tile(aes(doy,tod,color=spl,fill=spl),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "gray",limits=c(85,120))+#,limits=c(0,120)) +
  scale_fill_viridis_c(na.value = "gray",limits=c(85,120))+#,limits=c(0,120)) +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

#library(patchwork)
p19spl/p20spl+plot_layout(widths = 1,heights = 1)

# make it easier to visualize

doytokeep<-fish2 %>%
  filter(yr==2020)%>%
  ungroup()%>%
  select(doy)%>%
  distinct()

fish3<-fish2 %>%
  filter(doy %in% doytokeep$doy)%>%
  mutate(hr=as.numeric(hr)-1)

ggplot(fish3)+
  geom_tile(aes(x=doy,y=hr.min,fill=ncall2),size=.5)+
  scale_y_reverse(expand=c(0,0))+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c() +
  gfplot::theme_pbs()

# I'm now going to go with average call per minute per hour to get rid of gaps
fish4<-fish3 %>%
  group_by(yr,doy,hr)%>%
  summarize(mean.call=mean(cor.ncall,rm.na=TRUE),
            mean.spl=mean(spl,rm.na=TRUE),
            mean.wvht=mean(wave.ht,rm.na=TRUE),
            mean.wvprd=mean(wave.prd,rm.na=TRUE),
            call.p.minute=sum(cor.ncall,rm.na=TRUE)/n())

# look per day

fish.d<-fish2%>%
  group_by(yr,doy,doy2)%>%
  summarize(tot.call=sum(cor.ncall,na.rm = TRUE),
            ns=n(),
            call.per.min=tot.call/ns)
  
fish.dchange<-fish.d%>%
  select(yr,doy,doy2,call.per.min)%>%
  pivot_wider(names_from=yr,values_from=call.per.min)%>%
#  filter(doy2 >145 & doy2 <174)%>%
  mutate(delta.call=`2019`-`2020`)

ggplot(fish.d%>%
         #filter(doy2 >145 & doy2 <174))+
         filter(doy2 >111 & doy2 <123))+
  geom_bar(aes(x=doy,y=call.per.min,fill=as.factor(yr)),
           position=position_dodge(),stat="identity")

ggplot(fish.dchange%>%
         filter(doy2>111 & doy2<123))+
  geom_point(aes(doy2,delta.call,color=ifelse(delta.call>0,"2019","2020")),size=3)+
  geom_hline(yintercept=0)


# look at call per minute during sunrise/sunset

fish.s<-fish2%>%
  ungroup()%>%
#  filter(hr %in% c(5,6,20,21))%>%
  mutate(tod=case_when(
    hr==5~"sunrise",
    hr==6~"sunrise",
    hr==20~"sunset",
    hr==21~"sunset",
    hr==22~"night",
    hr==23~"night",
    hr==24~"night",
    hr==0~"night",
    hr==1~"night",
    hr==2~"night",
    hr==3~"night",
    hr==4~"night",
    hr %in% c(7,8,9,10,11,12,13,14,15,16,17,18,19)~"day"))%>%
  group_by(yr,doy,doy2,tod)%>%
  summarise(call.per.min=sum(cor.ncall,na.rm = TRUE)/n(),
            mean.call.per.min=mean(cor.ncall,na.rm = TRUE),
            sd.call.per.min=sd(cor.ncall,na.rm = TRUE))%>%
  filter(doy2>111 & doy2<173)%>%
  mutate(prd=case_when(
    yr==2019~1,
    yr==2020 & doy2 <=125~2,
    yr==2020 & doy2 >125~3))

ggplot(fish.s)+
  geom_point(aes(x=doy,y=mean.call.per.min,color=as.factor(yr)),size=4)+
  geom_pointrange(aes(x=doy,y=mean.call.per.min,
    xmin=doy,xmax=doy, 
    ymin=mean.call.per.min-sd.call.per.min, ymax=mean.call.per.min+sd.call.per.min, 
    color=as.factor(yr))) +
  geom_smooth(aes(x=doy,y=mean.call.per.min,color=as.factor(yr),group=prd), se=F)+
  facet_wrap(~tod,ncol = 1) +
  ggsidekick::theme_sleek()
