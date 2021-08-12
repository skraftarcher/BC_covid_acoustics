
library(tidyverse)
library(lubridate)
# library(readxl)


#script to do an initial exploration on the full dataset 
fish<-readRDS("wdata/all_fish_calls_with_weather.rds")
# first going to look at relationship between two different wave height and period measurements

ggplot(data=fish%>%
         filter(!is.na(wave.ht2))%>%
         select(wave.ht,wave.ht2)%>%
         distinct())+
  geom_point(aes(x=wave.ht,y=wave.ht2))

# wave.ht is definitely the way to go. 

# now look at period data
ggplot(data=fish%>%
         filter(!is.na(wave.prd2))%>%
         select(wave.prd,wave.prd2)%>%
         distinct())+
  geom_point(aes(x=wave.prd,y=wave.prd2))
# wave.prd is also definitely the way to go.

# now trying to make figure of fish calls by time of day/day
# need to create two variables: a decimal hour, and a day of year variable

# going to look at distribution on the number of fish calls first
hist(fish$ncall)
# looks like there's a bit of an outlier somewhere.

fish2<-fish %>%
  mutate(utmDateTime = DateTime + hours(7),
         doy = strftime(utmDateTime, format = "%j"),
         hr.min=hr+min/60,
         ncall2=ifelse(ncall<30,ncall,30))

fish2 %>% filter(yr == 2019) %>%
ggplot()+
  geom_tile(aes(doy,hr.min,color=ncall2),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

fish2 %>% filter(yr == 2019) %>%
  ggplot()+
  geom_tile(aes(doy,hr.min,color=spl),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()
  

fish2 %>% filter(yr == 2019) %>%
  ggplot()+
  geom_tile(aes(doy,hr.min,color=wspeed),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()


fish2 %>% filter(yr == 2020 & doy < 140) %>%
  ggplot()+
  geom_tile(aes(doy,hr.min,color=ncall2),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs()

fish2 %>% filter(yr == 2020 & doy > 140) %>%
  ggplot()+
  geom_tile(aes(doy, as.factor(hr.min), color=ncall2),
    size=.5)+
  # scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  gfplot::theme_pbs() +
  theme(axis.text.y = element_blank() )

fish2 %>% #filter(yr == 2019) %>%
  ggplot()+
  geom_tile(aes(doy,hr.min,color=ncall2),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  facet_wrap(~yr,nrow = 2) +
  gfplot::theme_pbs()

fish2 %>% #filter(yr == 2019) %>%
  ggplot()+
  geom_tile(aes(doy,hr.min,color=spl),size=.5)+
  scale_y_reverse(limits=c(24.2,-0.2), expand=c(0,0))+
  scale_color_viridis_c(na.value = "red") +
  ylab("Time of day") +
  xlab("Day of Year") +
  facet_wrap(~yr,nrow = 2) +
  gfplot::theme_pbs() + theme(panel.background = element_rect(fill = "black"))


# hard to visualize with the whole dataset because the second part of
# the 2020 dataset is 15 min on 15 off and there's the big gap.
# first I'm going to subset down to only doy in the 2020 dataset

doytokeep<-fish2 %>%
  filter(yr==2020)%>%
  ungroup()%>%
  select(doy)%>%
  distinct()

fish3<-fish2 %>%
  filter(doy %in% doytokeep$doy)

ggplot(fish3)+
  geom_tile(aes(x=doy,y=hr.min,fill=ncall2),size=.5)+
  scale_y_reverse(expand=c(0,0))+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c() +
  gfplot::theme_pbs()

# I'm now going to go with average call per minute per hour to get rid of gaps
fish4<-fish3 %>%
  group_by(yr,doy,hr)%>%
  summarize(mean.call=mean(ncall,rm.na=TRUE),
            mean.spl=mean(spl,rm.na=TRUE),
            mean.wvht=mean(wave.ht,rm.na=TRUE),
            mean.wvprd=mean(wave.prd,rm.na=TRUE))

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.call),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.spl),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.wvht),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()



# Try some preliminary models

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(DHARMa))install.packages("DHARMa");library(DHARMa)
if(!require(glmmTMB))install.packages("glmmTMB");library(glmmTMB)

doytokeep<-fish2 %>%
  filter(deployment=="2")%>%
  ungroup()%>%
  select(doy)%>%
  distinct()

fish3<-fish2 %>%
  filter(doy %in% doytokeep$doy)

fish <- fish3 %>% mutate(
  ncall_trim = ifelse(ncall>50, 50, ncall),
  ord = as.factor(spl.interval),
  hr = as.factor(hr),
  yr = as.factor(yr),
  doy.sc = scale(as.numeric(doy)),
  doy.sc.2 = doy.sc^2,
  spl.sc = scale(spl),
  spl.sc.2 = spl.sc^2,
  wdir.N = if_else(wdir < 4.5 | wdir >= 31.5, 1, 0),
  wdir.E = if_else(wdir >= 4.5 & wdir < 13.5, 1, 0),
  wdir.S = if_else(wdir >= 13.5 & wdir < 22.5, 1, 0),
  wdir.W = if_else(wdir >= 22.5 & wdir < 31.5, 1, 0),
  # wspeed.sc = scale(wspeed),
  wspeed.2 =  wspeed^2,
  # wave.ht.sc = scale(wave.ht),
  wave.ht.2 = wave.ht^2,
  wave.prd.sc = scale(wave.prd),
  temp.sc = scale(temp)
) %>% filter(doy >110 & doy < 122)

glimpse(fish)

mmod <- glmmTMB(ncall ~ yr +
  hr +
  # doy.sc + doy.sc.2 +
  spl.sc + #spl.sc.2 + 
  wspeed + wspeed.2 +
  wave.ht + #wave.ht.2 +
  wspeed * wdir.N + wspeed.2 * wdir.N +
  # wspeed * wdir.E + wspeed.2 * wdir.E +
  # wspeed * wdir.S + wspeed.2 * wdir.S +
  # wspeed * wdir.W + wspeed.2 * wdir.W +
  # wave.ht * wdir.N + 
  # wave.ht * wdir.S +
  wave.prd.sc +
  temp.sc +
  ar1(ord - 1|deployment), 
  family = nbinom2,
  # family = nbinom1,
  # family = poisson,
  data = fish)

summary(mmod)

# check residuals: not amazing, but not terrible
mmod_simres <- simulateResiduals(mmod) 
testDispersion(mmod_simres) 
plot(mmod_simres)

# ggeffects plots
library(ggeffects)

p1 <- ggpredict(mmod, terms = c("prd", "type","year","pm")) 
p1 <- ggpredict(mmod, terms = c("doy", "type", "prd", "year")) 
p1 <- ggpredict(mmod, terms = c("spl_mean", "type", "prd", "year")) 

plot(p1)

lm1 <- lm(fish_count~prd_type, data = d2 )
anova(aov1)
summary(aov1)






