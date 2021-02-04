#Load packages
library(tidyverse)
library(ggeffects)
library(lubridate)

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
  mutate(
    utmDateTime = datetime + hours(7),
    doy = as.numeric(strftime(utmDateTime, format = "%j"))
    )%>%
  group_by(spl.interval)%>%
  summarize(spl=mean(SPL,na.rm = T),
            wave.ht=mean(wave.ht),na.rm = T,
            wave.prd=mean(wave.prd,na.rm = T),
            wind_spd=mean(wind_spd,na.rm = T),
            wind_dir=mean(wind_dir,na.rm = T),
            temp=mean(temp,na.rm = T),
            min=min(min,na.rm = T),
            hr=mean(hr,na.rm = T),
            doy=mean(doy,na.rm = T),
            month=mean(m,na.rm = T)
    )

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

# now link these

om.sum<-left_join(om.auto,om.man)%>%
  left_join(om.spl)%>%
  mutate(tot.call=auto.fish+auto.non)
# %>%
#   filter(tot.call>5)

# get residuals of regressing man.fish~auto.fish

# reg <- glmmTMB(man.fish~auto.fish,family=nbinom2, data=om.sum)
reg <- lm(log(man.fish+1)~log(auto.fish+1), data=om.sum)

summary(reg)
# plot(reg)

om.sum$resids<-exp(residuals(reg, type = "response"))-1

om.sum2<-om.sum%>%filter(wave.prd<10)%>% 
  pivot_longer(spl:tot.call,names_to="measure",values_to="values") 


ggplot(data=om.sum2)+
  geom_point(aes(x=values,y=resids))+
  facet_wrap(~measure,scales="free")

om.sum$spl2<-om.sum$spl^2
om.nona<-om.sum %>%
  filter(wave.prd<10)%>% # remove two extreme outliers
  filter(!is.na(tide))%>%
  filter(!is.na(wind_spd))%>%
  filter(!is.na(wind_dir))%>%
  filter(!is.na(wave.prd))%>%
  filter(!is.na(wave.ht))

# # do a quick linear model
# call.lm<-lm(man.fish~auto.fish*spl*spl2*wind_spd,data=om.nona)
# summary(call.lm)
# 
# options(na.action = "na.fail")
# x<-MuMIn::dredge(call.lm)
# options(na.action = "na.omit")
# head(x)
# 
# call.lm.best<-lm(man.fish~auto.fish+
#                    spl+
#                    spl2+
#                    wind_spd+
#                    auto.fish*spl+
#                    auto.fish*spl2+
#                    wind_spd*spl+
#                    spl2*wind_spd,data=om.nona)
# summary(call.lm.best)
# om.nona$fits<-fitted(call.lm.best)
# 
# ggplot(om.nona)+
#   geom_point(aes(x=man.fish,y=fits,color=spl))+
#   geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
#   scale_color_viridis_c()

ggplot(om.nona)+
  geom_point(aes(x=man.fish,y=auto.fish,color=spl))+
  geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
  scale_color_viridis_c()

# # get dataset to point to check model on 5 min intervals
# fm.auto<-fm %>%
#   mutate(auto.class=ifelse(is.na(auto.class),"NN",auto.class))%>%
#   group_by(spl.interval,auto.class)%>%
#   summarize(ncall=n())%>%
#   pivot_wider(names_from = auto.class,values_from=ncall,values_fill=c(0))%>%
#   rename(auto.fish=FS,auto.non=NN)
# 
# #also for the purposes of this I'm going to assign the calls manually labeled FS as N because these are
# #calls that were split across multiple detections
# fm.man<-fm %>%
#   mutate(man.class=ifelse(man.class=="FS","N",man.class),
#          man.class=ifelse(man.class=="NH","N",man.class),
#          man.class=ifelse(man.class=="","N",man.class))%>%
#   group_by(spl.interval,man.class)%>%
#   summarize(ncall=n())%>%
#   pivot_wider(names_from = man.class,values_from=ncall,values_fill=c(0))%>%
#   rename(man.fish=F,man.non=N)
# 
# # now get mean spl per interval
# 
# fm.spl<-fm%>%
#   group_by(spl.interval)%>%
#   summarize(spl=mean(SPL),
#             wave.ht=mean(wave.ht),
#             wave.prd=mean(wave.prd),
#             wind_spd=mean(wind_spd),
#             wind_dir=mean(wind_dir),
#             hr=mean(hr))
# 
# fm.sum<-full_join(fm.auto,fm.man)%>%
#   left_join(fm.spl)%>%
#   mutate(spl2=spl^2)
# 
# fm.sum$preds<-predict(call.lm.best,fm.sum)
# 
# ggplot(fm.sum)+
#   geom_point(aes(x=man.fish,y=preds,color=spl))+
#   geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
#   scale_color_viridis_c()
# 
# ggplot(fm.sum)+
#   geom_point(aes(x=man.fish,y=auto.fish,color=spl))+
#   geom_abline(aes(slope=1,intercept=0),linetype="dashed")+
#   scale_color_viridis_c()
# 
# 
# 
# fm.spl <- fm.spl %>% mutate(
#   ncall_trim = ifelse(ncall>50, 50, ncall),
#   ord = as.factor(spl.interval),
#   hr = as.factor(hr),
#   yr = as.factor(yr),
#   doy.sc = scale(as.numeric(doy)),
#   doy.sc.2 = doy.sc^2,
#   spl.sc = scale(spl),
#   spl.sc.2 = spl.sc^2,
#   wdir.N = if_else(wdir < 4.5 | wdir >= 31.5, 1, 0),
#   wdir.E = if_else(wdir >= 4.5 & wdir < 13.5, 1, 0),
#   wdir.S = if_else(wdir >= 13.5 & wdir < 22.5, 1, 0),
#   wdir.W = if_else(wdir >= 22.5 & wdir < 31.5, 1, 0),
#   # wspeed.sc = scale(wspeed),
#   wspeed.2 =  wspeed^2,
#   # wave.ht.sc = scale(wave.ht),
#   wave.ht.2 = wave.ht^2,
#   wave.prd.sc = scale(wave.prd),
#   temp.sc = scale(temp)
# ) %>% filter(doy >110 & doy < 122)

# glimpse(om.nona)
hist(om.nona$tide, breaks = 30)
hist(om.nona$hr, breaks = 30)
hist(om.nona$spl.interval, breaks = 100)
hist(om.nona$wind_spd, breaks = 100)
hist(om.nona$wave.ht, breaks = 100)
hist(om.nona$spl, breaks = 100)

hist(om.nona$auto.fish, breaks = 100)
hist(om.nona$man.fish, breaks = 100)

om.nona2 <- om.nona %>% ungroup() %>% mutate(
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

# black is N wind, red = not N wind
ggplot(om.nona2, aes(as.factor(wind.dir.not.N), spl.sc, 
  color = as.factor(wind.dir.not.N))) + geom_boxplot() + 
  scale_color_manual(values = c("black", "red"))

ggplot(om.nona2, aes(wind.spd, spl.sc, color = as.factor(wind.dir.not.N))) + 
  geom_point() + geom_smooth(method = "lm") + 
  scale_color_manual(values = c("black", "red"))

ggplot(om.nona2, aes(wind.spd, spl.sc, color = wave.ht)) + geom_point() + 
  # geom_smooth(method = "lm")+ 
  scale_color_viridis_c(option = "A")

ggplot(om.nona2, aes(wave.prd, spl.sc, color = wind.spd)) + geom_point() + 
  scale_color_viridis_c(option = "A")

ggplot(om.nona2, aes(wave.ht, spl.sc, color = wind.spd)) + geom_point() + 
  scale_color_viridis_c(option = "A")

ggplot(om.nona2, aes(tide.sc, spl.sc, color = wind.spd)) + geom_point() + 
  scale_color_viridis_c(option = "A")

ggplot(om.nona2, aes(wave.prd, spl.sc, color = as.factor(wind.dir.not.N)
  )) + geom_point() + geom_smooth(method = "lm") + 
  scale_color_manual(values = c("black", "red"))

ggplot(om.nona2, aes(wave.ht, spl.sc, color = as.factor(wind.dir.not.N)
  )) + geom_point() + geom_smooth(method = "lm") + 
  scale_color_manual(values = c("black", "red"))


# ggplot(data=om.sum2)+
#   geom_point(aes(x=values,y=resids))+
#   facet_wrap(~measure,scales="free")


library(mgcv)
mod4 <- gamm(man.fish ~ log.fish.sc + #wind.dir.not.N + 
    spl.sc + wind.spd + 
    # s(spl.sc) + s(wind.spd) + 
    s(wave.ht) +
    s(wave.prd, k=3) #+
    # te(wave.ht, wave.prd, k=7) +
    # s(tide.sc) +
    #  s(wind.spd, wind.dir.not.N, k=4)
    # te(wind_dir, wind.spd, bs = "cc", k=10)
    # s(wind_dir, bs = "cc", k=8)
  # + te(wind.spd, spl.sc) + te(auto.fish, wind.spd) + te(auto.fish, spl.sc)
  # , correlation=corAR1(form=~1|doy:hr),
  , correlation=corAR1(form=~1|spl.interval),
  family=poisson,
  data=om.nona2)

summary(mod4$gam)

 
plot(mod4$gam)

# library(mgcViz)
# p <- getViz(mod4$gam)

# plot(sm(p, 1)) + l_fitRaster() + l_fitContour() + l_points()

# visreg::visreg(mod4$gam)

# visreg::visreg2d(mod4$gam, x=wave.ht, y=wave.prd)

om.nona2$predicted <- predict(mod4$gam)
om.nona2$resids <- residuals(mod4$gam)

plot(resids~predicted, data = om.nona2)

plot(exp(predicted)~(man.fish), data = om.nona2, ylim=c(0, 10), xlim=c(0, 10)) + abline(a=0, b=1)

plot(auto.fish~man.fish, data = om.nona2, ylim=c(0, 10), xlim=c(0, 10)) + abline(a=0, b=1)


ggplot(om.nona2, aes(man.fish, auto.fish)) +
  geom_jitter() +
  # coord_cartesian(ylim=c(0, 20), xlim=c(0, 20)) + 
  coord_cartesian(ylim=c(0, 60), xlim=c(0, 60)) + 
  geom_abline(intercept =0, slope=1)

ggplot(om.nona2, aes(man.fish, exp(predicted))) +
  geom_jitter() +
  # coord_cartesian(ylim=c(0, 20), xlim=c(0, 20)) + 
  coord_cartesian(ylim=c(0, 60), xlim=c(0, 60)) + 
  geom_abline(intercept =0, slope=1)


# predict(mod4$gam, newdata = )


library(glmmTMB)
mod1 <- glmmTMB(man.fish~
    log.fish.sc + spl.sc + 
    wind.spd +
    # wind.spd * wind.dir.not.N + 
    poly(wave.ht, 2)+
    # wave.ht * wind.dir.not.N + 
    # wave.ht2 +
    poly(wave.prd, 2) +
    # wave.prd * wind.dir.not.N + 
    # wave.prd2 +
    # tide.sc + 
    # tide.sc2 + 
    poly(tide.sc, 2) + 
    (1|doy) +
    (1|doy:hr), 
  family=nbinom2,
  data=om.nona2)

summary(mod1)
# x<-MuMIn::dredge(mod1)
# 
# subset(x, delta <2)

p1 <- ggpredict(mod1, terms = c("spl.sc", "log.fish.sc")) 
plot(p1)  + 
  stat_smooth(aes(colour = group), se = FALSE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, colour = NA), alpha = .1)+
  scale_fill_viridis_d(option = "C")+
  scale_colour_viridis_d(option = "C") +
  gfplot::theme_pbs() 

# ggplot(p1, aes(x, predicted, colour = group)) +
#   stat_smooth(method = "lm", se = FALSE) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, colour = NA), alpha = .1)+
#   scale_fill_viridis_d(option = "C")+ 
#   scale_colour_viridis_d(option = "C") +
#   gfplot::theme_pbs() 

p2 <- ggpredict(mod1, terms = c("wind.spd", "spl.sc")) 
plot(p2) + gfplot::theme_pbs() 


p3 <- ggpredict(mod1, terms = c("log.fish.sc", "spl.sc")) 
plot(p3) + scale_y_log10() + 
  geom_abline(
    # intercept = log10(exp(1.06102)), # model intercept
    intercept = log10(exp(-(log(1+1)/1.13)*1.62)), # attempting to calculate an intercept of 0
    slope = log(1+1)/1.13, lty = 2) +
  gfplot::theme_pbs() 
 

# p4 <- ggpredict(mod1, terms = c("tide.sc", "spl.sc")) 
p4 <- ggpredict(mod1, terms = c("tide.sc","log.fish.sc"))
plot(p4) + gfplot::theme_pbs() 



mod2 <- glmmTMB(man.fish~
    log.fish.sc + spl.sc + 
    wind.spd +
    tide.sc +
    tide.sc2 +
    (1|doy) +
    (1|doy:hr), 
  family=nbinom2,
  data=om.nona2)

library(DHARMa)
# check residuals: not amazing, but not terrible
mmod_simres <- simulateResiduals(mod2) 
testDispersion(mmod_simres) 
plot(mmod_simres)

