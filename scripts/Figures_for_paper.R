# script to make figures for anthropause paper

source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp("patchwork")
lp("seewave")
lp("lubridate")
lp("sf")
lp("rgdal")
lp("sp")
lp("readxl")
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
# remotes::install_github("jlmelville/vizier")
library(vizier)

# site figure
#----load datasets----
land<-st_read("C:/Users/sarcher/Documents/Manuscripts/Abaco_Seagrass/original_data/land_polygons.shp")# read in shape file
sites<-read_xlsx("odata/site_locations.xlsx")
rca<-st_read("odata/NorthumberlandRCA.shp")
ferry<-st_read("odata/CHRFRRRTSS_line.shp")
bbymin<-47.5
bbxmin<- -121.5
bbymax<- 49.8
bbxmax<- -125

bbymax2<-49.25
bbxmin2<- -123.84703681153307
bbymin2<- 49.16
bbxmax2<- -123.97


cities<-sites[-1:-2,]
hydro<-sites[c(1:2),]

rca<-st_transform(rca,crs = 4326)
ferry<-st_transform(ferry,crs = 4326)
rca2<-st_crop(rca,xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2)
overview<-st_crop(land,xmin=bbxmin,xmax=bbxmax,ymin=bbymin,ymax=bbymax)
sitemap<-st_crop(overview,xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2)
ferry2<-st_crop(ferry,xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2)

ferry3<-ferry2%>%
  mutate(use=ifelse(FREQ_USE=="Unknown","High",FREQ_USE),
         useb=factor(use,levels = c("Low","Moderate","High","Very high")),
         use2=case_when(
           use %in% c("Low")~0.5,
           use %in% c("Moderate")~0.75,
           use %in% c("High","Very high","Unknown")~1),
         covid=c("Reduced","Reduced","Continued","Continued","Reduced","Stopped","Stopped"))
#ferry2<-ferry2[-5,]

(o1<-ggplot()+
  geom_sf(data=overview,fill="grey")+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    geom_rect(aes(xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2),fill="red")+
    geom_rect(aes(xmin=bbxmin,xmax=bbxmax,ymin=bbymin,ymax=bbymax),color="black",alpha=0)+
    geom_text(data=cities,aes(label=Site,x=Longitude-c(-0.35,-.31,0.15),y=Latitude-c(.08,.05,.08)),size=8)+
    geom_point(data=cities,aes(x=Longitude,y=Latitude),size=4)+
      theme(panel.background = element_rect(color="black",fill="white"),
        panel.grid=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()))
ggsave("figures/site_overview.jpg",dpi=600)


(sm1<-ggplot()+
    geom_sf(data=rca2,alpha=.1,fill="darkgoldenrod3")+
    geom_sf(data=ferry3,size=1.2,aes(linetype=covid,color=useb))+# aes(color=as.factor(FERRY_ID))
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    geom_sf(data=sitemap,fill="grey")+
    geom_point(data=hydro,aes(x=Longitude,y=Latitude,shape=Site),fill="red",color="black",size=3)+
    scale_shape_manual(values=c(24,25),name="Hydrophone Location")+
    scale_color_manual(values=turbo(n=4,start=.2,end=.8),name = "Pre-Covid Ferry Frequency")+
    scale_linetype_manual(values=c("solid","dashed","dotdash"),name="Covid Response")+
    #scale_size_manual(breaks = c(0.5,0.75,1),values = c(0.5,0.75,1))+
    geom_text(aes(y=49.198,x=-123.872,label="Northumberland \n Channel \n RCA"))+
    geom_text(aes(x=-123.96,y=49.203,label="Departure \n Bay"))+
    geom_text(aes(y=49.19,x=-123.855,label="Gabriola \n Island"))+
    geom_text(aes(y=49.185,x=-123.96,label="Nanaimo"))+
    geom_text(aes(label="Saysutshun \n (Newcastle Island)",y=49.192,x=-123.935))+
    #geom_label(data=hydro,aes(label=Site,x=Longitude-.01,y=Latitude-.01),alpha=.5)+
    theme_bw()+
    guides(colour = guide_legend(order = 2), 
           shape = guide_legend(order = 1),
           linetype = guide_legend(order=3))+
    theme(panel.grid=element_blank(),
          axis.title = element_blank(),
          #legend.position = c(.9,.9),
          #legend.background = element_rect(fill="white",color="black"),
          legend.title.align = 0.5,
          legend.key.width = unit(4, "lines")))  

ggsave("figures/site_closeup.jpg",dpi=600)


# load data 
alld<-read_rds("wdata/alldata_bothyears_withtod.rds")
bp<-read_rds("wdata/all_boat_passage_data_power.rds")

# make hourly heat maps

allhr<-alld%>%
  group_by(yr,mnth,d,hr)%>%
  summarize(wind.spd=mean(wind_spd,na.rm=T),
            bb.SPL=rms(SPL),
            tide=mean(tide,na.rm=T),
 #           temp=mean(temp,na.tm=T),
            ship=sum(ship.pa)/n(),
            ship.close=sum(ifelse(ship.close=="close",1,0))/60,
            fish.calls=sum(fish.calls),
            fish.present=sum(fish.pa)/n())%>%
  mutate(period=case_when(
    yr==2019 & mnth==4 & d > 18~1,
    yr==2019 & mnth==5 & d < 4~1,
    yr==2019 & mnth==5 & d > 24~2,
    yr==2019 & mnth==6 & d < 21 ~2,
    yr==2020 & mnth==4 & d > 18~1,
    yr==2020 & mnth==5 & d < 4~1,
    yr==2020 & mnth==5 & d > 24~2,
    yr==2020 & mnth==6 & d < 21 ~2),
    Date=ymd(paste(yr,mnth,d)))%>%
  filter(!is.na(period))

# ship present/absent

ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=ship))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="H",name="Proportion of time \n with a ship")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#ships close
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=ship.close))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="H",name="Proportion of time \n with a ship's \n close approach")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#spl
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=bb.SPL))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="H",name="Broadband SPL")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#wind
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=wind.spd))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="H",name="wind speed")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#fish calls 
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=fish.calls))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="H",name="Fish calls per hr")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

# boat passage figures

# make a plot to look at this
theme_set(theme_bw())
theme_update(panel.grid = element_blank())
col.pal=c("#403891ff","#de7065ff","6b4596ff")

boatpass.pp<-read_rds("wdata/boat_passage_trimmed_peakpower.rds")
boatpass.ip<-read_rds("wdata/boat_passage_trimmed_inbandpower.rds")

boatpass.pp2<-boatpass.pp%>%
  select(inter,prd,type,yr,fish.calls)%>%
  pivot_wider(names_from = type,values_from = fish.calls,values_fill = 0)%>%
  mutate(fc.diff=quiet-boat,
         fc.diff2=boat-quiet)

boatpass.ip2<-boatpass.ip%>%
  select(inter,prd,type,yr,fish.calls)%>%
  pivot_wider(names_from = type,values_from = fish.calls,values_fill = 0)%>%
  mutate(fc.diff=quiet-boat,
         fc.diff2=boat-quiet)


(p1<-ggplot(data=boatpass.pp%>%
              filter(type=="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=150,label="Boat Period"),size=5)+
    ylab("Total number of fish calls \n Boat Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p2<-ggplot(data=boatpass.pp%>%
              filter(type!="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=5)+
    ylab("Total number of fish calls\n Quiet Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p3<-ggplot(data=boatpass.pp2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
    geom_boxplot(aes(x=prd,y=fc.diff,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=8)+
    ylab("Difference in fish calls \n (Quiet - Boat)")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

p1 / p2 / p3 / plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

ggsave("figures/boat_passage_results_ppfiltered.jpg",height = 9,width=4)

# second way of displaying boat passage results

boatpass.pp$lgrp=paste0(boatpass.pp$inter,".",boatpass.pp$type)
(p4<-ggplot(data=boatpass.pp)+
  geom_line(aes(x=prd,y=fish.calls,group=lgrp,color=type),size=1.1,alpha=.6)+
    scale_color_manual(values=c("#91bfdb","#fc8d59"),labels=c("Quiet","Boat Passage"),name="")+
    scale_x_discrete(expand=c(0.05,0.05),labels=c("Pre-passage","Ferry Passage","Post-passage"))+
    ylab("Total number of fish calls")+
    theme(legend.position = "top",
          axis.title.x = element_blank()))


(p5<-ggplot(data=boatpass.pp2)+
    geom_line(aes(x=prd,y=fc.diff,group=inter),size=1.1,alpha=.6)+
    geom_hline(aes(yintercept=0),linetype="dashed",color="red",size=2)+
    #scale_color_manual(values=c("#91bfdb","#fc8d59"),labels=c("Quiet","Boat Passage"),name="")+
    scale_x_discrete(expand=c(0.05,0.05),labels=c("Pre-passage","Ferry Passage","Post-passage"))+
    ylab("Difference in fish calls \n (Quiet - Boat)")+
    theme(axis.title.x = element_blank()))

p4/p5

ggsave("figures/boat_passage_results_ppfiltered_line.jpg",height = 9,width=4)


(p1b<-ggplot(data=boatpass.pp%>%
              filter(type=="boat"))+
    geom_violin(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=150,label="Boat Period"),size=5)+
    ylab("Total number of fish calls \n Boat Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p2b<-ggplot(data=boatpass.pp%>%
              filter(type!="boat"))+
    geom_violin(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=5)+
    ylab("Total number of fish calls\n Quiet Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p3b<-ggplot(data=boatpass.pp2)+
    geom_violin(aes(x=prd,y=fc.diff2,fill=prd))+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=8)+
    ylab("Difference in fish calls \n (Boat - Quiet)")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p2b<-ggplot(data=boatpass.pp)+
    geom_violin_pattern(aes(x=prd,y=fish.calls,fill=prd,pattern=type))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=5)+
    ylab("Total number of fish calls")+
    scale_y_continuous(expand=c(0,0))+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

p1b / p2b / p3b / plot_layout(guides = 'collect')&
  theme(legend.position='bottom')
#color based on mean SPL
ggsave("figures/boat_passage_results_ppfiltered_violin.jpg",height = 9,width=5)
