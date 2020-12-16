# I'm not convinced by my matching. So I'm going to check the
# correlation between manual and automatic by looking at the # of calls per 
# minute in each dataset.

library(tidyverse)
library(lubridate)
# bring in newly classified autodetections

autod19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                         files = "USE_THIS_FOR_CHECKING_AUTODETECTOR.txt",
                         all.data = TRUE) 
#bring in manual detections
mand19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                        files = "boat_passage_random_selections_updated_Dec152020.txt",
                        all.data = TRUE) 

#rename columns
autod19<-autod19%>%
  select(fo=`File Offset (s)`,
         begin.file=`Begin File`,
         auto.class=Class,
         Confidence)
mand19<-mand19%>%
  select(fo=`File Offset (s)`,
         begin.file=`Begin File`,
         man.class=`Manual Class`,
         man.type=`Manual Type`,
         inter=Inter,
         prd=Period)


# bring in SPL data

spl<-readRDS("wdata/spl_by_min.rds")

year(spl$DateTime)<-spl$year
spl<-arrange(spl,DateTime)
spl$dt2<-spl$DateTime+1
spl$interval<-findInterval(spl$dt2,spl$DateTime)

spl2<-spl%>%
  select(SPL,interval)
# create a date time variable in each detection dataset
autod19<-separate(autod19,begin.file,into = c("st","yr","m","d","hr","min","s","ext"),sep = c(11,13,15,17,19,21,23),remove=FALSE,convert = TRUE)

autod19<- autod19 %>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime+fo,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))%>%
  arrange(datetime)

mand19<-separate(mand19,begin.file,into = c("st","yr","m","d","hr","min","s","ext"),sep = c(11,13,15,17,19,21,23),remove=FALSE,convert = TRUE)

mand19<- mand19 %>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime + fo,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))



# link detections to SPL
autod19$interval<-findInterval(autod19$datetime,spl$DateTime)

autod192<-left_join(autod19,spl2)%>%
  select(interval,auto.class,Confidence,SPL)

mand19$interval<-findInterval(mand19$datetime,spl$DateTime)

mand192<-left_join(mand19,spl2)%>%
  select(interval,man.class,man.type, inter,prd,SPL)

# summaries by minute

autod.min<-autod192%>%
  group_by(interval, SPL,auto.class)%>%
  summarize(n.auto.call=n())%>%
  pivot_wider(names_from = auto.class,values_from=n.auto.call,values_fill=0)%>%
  select(interval,SPL,n.auto.call=FS)%>%
  distinct()

mand.min<-mand192%>%
  filter(man.class=="F")%>%
  group_by(interval, SPL)%>%
  summarize(n.man.call=n())

call.min<-left_join(mand.min,autod.min)

round(cor(x=call.min$n.man.call,y=call.min$n.auto.call),2)

rplab<-expression(paste("r"[p]," = ",0.83))

ggplot(data=call.min)+
  geom_point(aes(y=n.auto.call,x=n.man.call,color=SPL),size=3)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Automatically detected fish calls per minute")+
  xlab("Manually detected fish calls per minute")+
  scale_color_distiller(palette = "Spectral",direction=-1)+
  geom_smooth(aes(y=n.auto.call,x=n.man.call),method="lm")+
  geom_text(aes(x=5,y=33),label=rplab)
  
