# script to visualize the full dataset

source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="patchwork")
lp(pck="suncalc")
lp(pck="lubridate")


# load data
all19<-read_rds("wdata/alldata2019.rds")
all20<-read_rds("wdata/alldata2020.rds")

#organize data

all<-bind_rows(all19,all20)%>%
  mutate(fish.pa=ifelse(fish.calls<=1,0,1),
         SPL.start.time=force_tz(SPL.start.time,tz="America/Chicago"),
         #date1=ymd(paste(yr,mnth,d)),
         time.utc=with_tz(SPL.start.time,tz="UTC"))%>%
  filter(!is.na(ship.close))

all$date<-ymd(paste(all$yr,all$mnth,all$d))

moon.phase<-getMoonIllumination(date = all$time.utc, keep = c("phase"))
dawn<-getSunlightTimes(date = all$date, lat = 49.21445, lon = -123.892233, 
                           keep = c( "dawn","dusk"), tz = "UTC")%>%
  distinct()


colnames(moon.phase)<-c("time.utc","moon.phase")

all2<-left_join(all,moon.phase)%>%
  left_join(dawn)%>%
  mutate(
    dawn.end=dawn+3600,
    dusk.end=dusk+3600,
    tod=case_when(
    SPL.start.time>=dawn & SPL.start.time<dawn.end ~ "dawn",
    SPL.start.time>=dawn.end & SPL.start.time<dusk ~ "day",
    SPL.start.time>=dusk & SPL.start.time<dusk.end ~ "dusk",
    SPL.start.time>=dusk.end | SPL.start.time<dawn ~ "night"))

# write this out so we can use this dataset for analysis
write_rds(all2,"wdata/alldata_bothyears_withtod.rds")

allboat<-all2%>%
  group_by(ship.close,yr)%>%
  summarize(n.call=sum(fish.pa),
            n.mins=n(),
            fish.call.mean=mean(fish.calls),
            fish.call.median=median(fish.calls),
            fish.call.se=sd(fish.calls)/sqrt(n.mins-1))%>%
  mutate(prop.mins=n.call/n.mins)

allboat2<-all2%>%
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

allboat3<-all2%>%
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

(p5<-ggplot(allboat3)+
    geom_bar(aes(x=tod,y=prop.mins),
             stat="identity",position=position_dodge(0.9))+
    facet_grid(ship.close~yr)+
    scale_fill_viridis_c())




