# script to visualize the full dataset

source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="patchwork")
lp(pck="lubridate")


# load data
# all19<-read_rds("wdata/alldata2019.rds")
# all20<-read_rds("wdata/alldata2020.rds")

all3<-read_rds("wdata/alldata_bothyears_withtod.rds")

all4<-all3%>%
  pivot_longer(morning.golden:night.dark, names_to="tod",values_to="tod.val")%>%
  filter(tod.val!=0)

allboat<-all3%>%
  group_by(ship.close,yr)%>%
  summarize(n.call=sum(fish.pa),
            n.mins=n(),
            fish.call.mean=mean(fish.calls),
            fish.call.median=median(fish.calls),
            fish.call.se=sd(fish.calls)/sqrt(n.mins-1))%>%
  mutate(prop.mins=n.call/n.mins)

allboat2<-all3%>%
  group_by(ship.close,yr,hr)%>%
  summarize(n.call=sum(fish.pa),
            n.mins=n(),
            fish.call.mean=mean(fish.calls),
            fish.call.median=median(fish.calls),
            fish.call.se=sd(fish.calls)/sqrt(n.mins-1),
            spl.mean=mean(SPL),
            spl.median=median(SPL),
            spl.var=var(SPL))%>%
  mutate(prop.mins=n.call/n.mins)

allboat3<-all4%>%
  group_by(ship.close,yr,tod)%>%
  summarize(n.call=sum(fish.pa),
            n.mins=n(),
            fish.call.mean=mean(fish.calls),
            fish.call.median=median(fish.calls),
            fish.call.se=sd(fish.calls)/sqrt(n.mins-1),
            spl.mean=mean(SPL),
            spl.median=median(SPL),
            spl.var=var(SPL))%>%
  mutate(prop.mins=n.call/n.mins)

allboat$ship.close<-factor(allboat$ship.close,levels=c("no.ship","approaching","close"))
allboat2$ship.close<-factor(allboat2$ship.close,levels=c("no.ship","approaching","close"))
allboat3$ship.close<-factor(allboat3$ship.close,levels=c("no.ship","approaching","close"))
allboat3$tod<-factor(allboat3$tod,levels=c("day","evening.golden","morning.golden","night.light","night.dark"))


# start plotting
theme_set(theme_bw())
theme_update(panel.grid=element_blank())

(p1<-ggplot(allboat)+
  geom_bar(aes(x=ship.close,y=prop.mins,fill=as.factor(yr)),
           stat="identity",position=position_dodge(0.9))+
    ylab("Proportion of minutes with a fish calls"))


(p2<-ggplot(allboat)+
    geom_bar(aes(x=ship.close,y=fish.call.mean,fill=as.factor(yr)),
             stat="identity",position=position_dodge(0.9))+
    geom_errorbar(aes(group=as.factor(yr),x=ship.close,ymin=fish.call.mean-fish.call.se,ymax=fish.call.mean+fish.call.se),
                  position=position_dodge(0.9),width=.1)+
    ylab("Fish calls per minute"))

(p3<-ggplot(allboat2)+
    geom_bar(aes(x=hr,y=fish.call.mean,fill=ship.close),
             stat="identity",position=position_dodge(0.9))+
    geom_errorbar(aes(group=ship.close,x=hr,ymin=fish.call.mean-fish.call.se,ymax=fish.call.mean+fish.call.se),
                  position=position_dodge(0.9),width=.1)+
    facet_wrap(~yr,ncol=1)+
    ylab("Fish calls per minute"))

(p4<-ggplot(allboat2)+
    geom_bar(aes(x=hr,y=fish.call.mean,fill=spl.mean),
             stat="identity",position=position_dodge(0.9))+
    geom_errorbar(aes(x=hr,ymin=fish.call.mean-fish.call.se,ymax=fish.call.mean+fish.call.se),
                  position=position_dodge(0.9),width=.1)+
    facet_grid(ship.close~yr)+
    ylab("Fish calls per minute")+
    scale_fill_viridis_c())


(p5<-ggplot(allboat3)+
    geom_bar(aes(x=tod,y=fish.call.mean,fill=spl.mean),
             stat="identity",position=position_dodge(0.9))+
    geom_errorbar(aes(x=tod,ymin=fish.call.mean-fish.call.se,ymax=fish.call.mean+fish.call.se),
                  position=position_dodge(0.9),width=.1)+
    facet_grid(ship.close~yr)+
    ylab("Fish calls per minute")+
    scale_fill_viridis_c())
ggsave("figures/fish_calls_by_timeofday_ship_year.jpg")

(p5b<-ggplot(allboat3)+
    geom_bar(aes(x=tod,y=fish.call.mean,fill=ship.close),
             stat="identity",position=position_dodge(0.9))+
    geom_errorbar(aes(group=ship.close,x=tod,ymin=fish.call.mean-fish.call.se,ymax=fish.call.mean+fish.call.se),
                  position=position_dodge(0.9),width=.1)+
    facet_grid(~yr)+
    ylab("Fish calls per minute")+
    scale_fill_viridis_d())

(p6<-ggplot(all3%>%
             filter(night.light!=0))+
  geom_jitter(aes(x=night.light,y=fish.calls,color=SPL))+
    facet_grid(yr~ship.close))

# simplifying into dawn/dusk/day/night
all5<-all4%>%
  mutate(tod = case_when(
    tod=="night.light" | tod=="night.dark" ~ "night",
    tod!="night.light" & tod!="night.dark" ~ tod))

(p7<-ggplot(data=all5)+
    geom_density_ridges(aes(y=as.factor(yr),x=fish.calls,fill=tod),alpha=.5)+
    facet_grid(~ship.pa))

# make heatmap by tod
all5$tod<-factor(all5$tod,levels=c("morning.golden","day","evening.golden","night"))

all6<-all5%>%
  group_by(doy,tod,yr)%>%
  summarize(mfishcalls=mean(fish.calls),
            medfishcalls=median(fish.calls),
            vfishcalls=var(fish.calls),
            mwind=mean(wind_spd,na.rm=T),
            medwind=median(wind_spd,na.rm=T),
            vwind=var(wind_spd,na.rm=T),
            mspl=mean(SPL),
            medspl=median(SPL),
            vspl=var(SPL),
            mtide=mean(tide,na.rm=T),
            prop.ships=sum(ship.pa)/n())

bhp<-ggplot(data=all6,aes(x=doy,y=tod))+
  scale_fill_viridis_c(option = "B",end=.8)+
  facet_grid(cols = vars(yr))+
  scale_y_discrete(limits=c("night","evening.golden","day","morning.golden"))

(p8<-bhp+
    geom_tile(aes(fill=medfishcalls)))

(p9<-bhp+
    geom_tile(aes(fill=medwind)))

(p10<-bhp+
    geom_tile(aes(fill=prop.ships)))

(p11<-bhp+
    geom_tile(aes(fill=medspl)))

p8/p9/p10/p11

# make heat maps by hour
# make heatmap by tod
all7<-all3%>%
  group_by(doy,hr,yr)%>%
  summarize(fishcalls=sum(fish.calls),
            mfishcalls=mean(fish.calls),
            medfishcalls=median(fish.calls),
            vfishcalls=var(fish.calls),
            mwind=mean(wind_spd,na.rm=T),
            medwind=median(wind_spd,na.rm=T),
            vwind=var(wind_spd,na.rm=T),
            mspl=mean(SPL),
            medspl=median(SPL),
            vspl=var(SPL),
            mtide=mean(tide,na.rm=T),
            prop.ships=sum(ship.pa)/n())

bhp2<-ggplot(data=all7,aes(x=doy,y=hr))+
  scale_fill_viridis_c(option = "B",end=.8)+
  facet_grid(cols = vars(yr))+
  scale_y_reverse()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

bhp2b<-ggplot(data=all7,aes(x=doy,y=hr))+
  scale_fill_viridis_c(option = "B",end=.8)+
  facet_grid(cols = vars(yr))+
  scale_y_reverse()+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

bhp2c<-ggplot(data=all7,aes(x=doy,y=hr))+
  scale_fill_viridis_c(option = "B",end=.8)+
  facet_grid(cols = vars(yr))+
  scale_y_reverse()+
  theme(strip.background = element_blank(),
        strip.text = element_blank())

(p12<-bhp2+
    geom_tile(aes(fill=medfishcalls)))

(p13<-bhp2b+
    geom_tile(aes(fill=medwind)))

(p14<-bhp2b+
    geom_tile(aes(fill=prop.ships)))

(p15<-bhp2b+
    geom_tile(aes(fill=medspl)))

(p16<-bhp2c+
    geom_tile(aes(fill=mtide)))

p12/p13/p14/p15/p16

ggsave("figures/summary_heatmaps_byhr.jpg")


