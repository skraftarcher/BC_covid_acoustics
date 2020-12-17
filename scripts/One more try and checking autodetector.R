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

rplab<-expression(paste("r"[p]," = ",0.81))

ggplot(data=call.min)+
  geom_point(aes(y=n.auto.call,x=n.man.call,color=SPL),size=3,position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Automatically detected fish calls per minute")+
  xlab("Manually detected fish calls per minute")+
  scale_color_distiller(palette = "Spectral",direction=-1)+
  geom_smooth(aes(y=n.auto.call,x=n.man.call),method="lm")+
  geom_text(aes(x=5,y=33),label=rplab)


ggsave("figures/Nice_autovmanual_figure.jpg")  

# I just realized there are more than likely minutes where I've only reviewed for part of the minute
# this is an attempt to deal with that.

mand.min2<-mand192%>%
  mutate(man.class=ifelse(man.class=="","no.obs",man.class))%>%
  group_by(interval, SPL,man.class)%>%
  summarize(n.man.call=n())%>%
  pivot_wider(names_from = man.class,values_from=n.man.call,values_fill=0)%>%
  mutate(p.noobs=no.obs/(no.obs+`F`+N+NH+FS+NN+M+G))%>%
  filter(p.noobs!=1)%>%
  rename(n.man.call=`F`)


call.min2<-left_join(mand.min2,autod.min)

ggplot(data=call.min2)+
  geom_point(aes(y=n.auto.call,x=n.man.call,color=p.noobs),size=3,position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Automatically detected fish calls per minute")+
  xlab("Manually detected fish calls per minute")+
  scale_color_distiller(palette = "Spectral",direction=-1)+
  geom_smooth(aes(y=n.auto.call,x=n.man.call),method="lm")#+
#  geom_text(aes(x=5,y=33),label=rplab)

# sure looks like that's part of the problem. Going to select down to minutes 
# where I've made observations on all of the detections

call.min3<-call.min2%>%
  filter(p.noobs==0)


round(cor(x=call.min3$n.man.call,y=call.min3$n.auto.call),2)

rplab<-expression(paste("r"["p"]," = 0.84"))

ggplot(data=call.min3)+
  geom_point(aes(y=n.auto.call,x=n.man.call,color=SPL),size=3,position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Automatically detected fish calls per minute")+
  xlab("Manually detected fish calls per minute")+
  scale_color_distiller(palette = "Spectral",direction=-1)+
  geom_smooth(aes(y=n.auto.call,x=n.man.call),method="lm")+
  geom_text(aes(x=5,y=33),label=rplab)+
  geom_abline(aes(intercept=0,slope=1),color="black",linetype="dashed")

ggsave("figures/Nice_autovmanual_figure_corrected.jpg") 

# I'm going to save this dataset so I know which minutes I've already analyzed any part of

write_rds(call.min2,"wdata/first_check_of_detector_Dec162020.rds")
