#This script is the initial data explore for the boat passage analysis

#load packages
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="patchwork")

# bring in data
boatpass<-read_rds("wdata/all_boat_passage_data.rds")
boatpass$prd<-factor(boatpass$prd,levels=c("pre","ferry","post"))

theme_set(theme_bw())
theme_update(panel.grid = element_blank())
pb<-ggplot(data = boatpass)
# now start visualizing
(p0<-pb+
    geom_boxplot(aes(y=fish.calls,x=as.factor(yr)),position=position_dodge(0.9)))

(p1<-pb+
    geom_boxplot(aes(y=fish.calls,x=prd,fill=prd))+
    facet_grid(yr~type))

(p2<-pb+
    geom_point(aes(x=wind_spd,y=fish.calls,color=prd))+
    facet_wrap(~type))

(p3<-pb+
    geom_point(aes(x=inter,y=fish.calls,color=type),size=3,alpha=.5)+
    facet_wrap(~prd,ncol=1))

# organize data to look at the difference between fish calling rate in quiet vs boat periods
boatpass2<-boatpass%>%
  select(inter,prd,yr,type,fish.calls)%>%
  pivot_wider(names_from = type,values_from = fish.calls)%>%
  mutate(fc.diff=quiet-boat)

(p4<-ggplot(boatpass2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
    geom_boxplot(aes(x=prd,y=fc.diff,fill=as.factor(yr)),position=position_dodge(0.9))+
    ylab("Quiet period fish calls - boat period fish calls")+
    scale_fill_viridis_d(option="D",end=.8,begin = .4))


(p1+p4)/p3
ggsave("figures/initial_data_explore_boat_passage.jpg",dpi=300)

# look at fish call power here

fcpower<-read_rds("wdata/all_boat_passage_data_power.rds")
fcpower$prd<-factor(fcpower$prd,levels=c("pre","ferry","post"))

(ip<-ggplot(fcpower)+
  geom_boxplot(aes(x=prd,y=inband.power,fill=prd))+
  facet_wrap(yr~type))


(pp<-ggplot(fcpower)+
  geom_boxplot(aes(x=prd,y=peak.power,fill=prd))+
  facet_wrap(yr~type))

ip/pp
ggsave("figures/call_power_distribution.jpg")

(ip2<-ggplot(fcpower)+
  geom_density(aes(x=inband.power,fill=prd),alpha=.25)+
  facet_wrap(yr~type))


(pp2<-ggplot(fcpower)+
  geom_density(aes(x=peak.power,fill=prd),alpha=.25)+
  facet_wrap(yr~type))

ip2/pp2+plot_layout(guides = "collect")
ggsave("figures/call_power_distribution2.jpg")
