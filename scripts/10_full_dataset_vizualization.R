# script to visualize the full dataset

source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="patchwork")
lp(pck="lubridate")


# load data
all19<-read_rds("wdata/alldata2019.rds")
all20<-read_rds("wdata/alldata2020.rds")



all3<-read_rds("wdata/alldata_bothyears_withtod.rds")

allboat<-all3%>%
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




