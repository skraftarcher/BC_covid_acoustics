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

# site figure
#----load datasets----
land<-st_read("C:/Users/sarcher/Documents/Manuscripts/Abaco_Seagrass/original_data/land_polygons.shp")# read in shape file
sites<-read_xlsx("odata/site_locations.xlsx")
bbymin<-47.5
bbxmin<- -121.5
bbymax<- 49.8
bbxmax<- -125

bbymax2<-49.22308258078075
bbxmin2<- -123.84703681153307
bbymin2<- 49.15
bbxmax2<- -123.97


cities<-sites[-1:-2,]
hydro<-sites[c(1:2,5),]

overview<-st_crop(land,xmin=bbxmin,xmax=bbxmax,ymin=bbymin,ymax=bbymax)
sitemap<-st_crop(overview,xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2)

(o1<-ggplot()+
  geom_sf(data=overview,fill="grey")+
    geom_rect(aes(xmin=bbxmin2,xmax=bbxmax2,ymin=bbymin2,ymax=bbymax2),fill="red")+
    geom_rect(aes(xmin=bbxmin,xmax=bbxmax,ymin=bbymin,ymax=bbymax),color="black",alpha=0)+
    geom_text(data=cities,aes(label=Site,x=Longitude-c(-0.35,-.31,0.15),y=Latitude-c(.08,.05,.08)),size=8)+
    geom_point(data=cities,aes(x=Longitude,y=Latitude),size=4)+
      theme(panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()))
ggsave("figures/site_overview.jpg",dpi=600)

(sm1<-ggplot()+
    geom_sf(data=sitemap,fill="grey")+
    geom_point(data=hydro,aes(x=Longitude,y=Latitude,shape=Site),size=3)+
    #geom_label(data=hydro,aes(label=Site,x=Longitude-.01,y=Latitude-.01),alpha=.5)+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.title = element_blank()))  
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
  scale_fill_viridis_c(option="B",end=.8,name="Proportion of hour \n with a ship")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#ships close
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=ship.close))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="B",end=.8,name="Minutes with a ship's \n close approach")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#spl
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=bb.SPL))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="B",end=.8,name="Broadband SPL")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#wind
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=wind.spd))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="B",end=.8,name="wind speed")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

#fish calls 
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=fish.calls))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="B",end=.8,name="Fish calls per hr")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())

# fish calls present
ggplot(data=allhr)+
  geom_tile(aes(x=Date,y=hr,fill=fish.present))+
  facet_wrap(yr~period,scales = "free",ncol=2)+
  scale_fill_viridis_c(option="B",end=.8,name="Minutes with fish calls present")+
  scale_y_reverse()+
  theme(panel.background = element_rect(fill="black"),
        panel.grid = element_blank())


# boat passage figures

# make a plot to look at this
theme_set(theme_bw())
theme_update(panel.grid = element_blank())
col.pal=c("#403891ff","#de7065ff","6b4596ff")

boatpass2<-boatpass%>%
  select(inter,prd,yr,type,fish.calls)%>%
  pivot_wider(names_from = type,values_from = fish.calls)%>%
  mutate(fc.diff=quiet-boat)

(p1<-ggplot(data=boatpass%>%
              filter(type=="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=150,label="Boat Period"),size=5)+
    ylab("Total number of fish calls \n Boat Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p2<-ggplot(data=boatpass%>%
              filter(type!="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=5)+
    ylab("Total number of fish calls\n Quiet Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p3<-ggplot(data=boatpass2)+
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

ggsave("figures/boat_passage_results.jpg",height = 9,width=4)