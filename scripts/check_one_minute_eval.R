# This script checks agreement between manual and automatic detectors in the 1 minute intervals

# load packages
library(tidyverse)
library(lubridate)

# load data
am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)
# get # of calls by interval and spl group
am.sum.auto<-am.check%>%
  filter(`Manual Class`!="")%>%
  group_by(interval,SPL,spl.group,Class)%>%
  summarize(a.call=n())%>%
  pivot_wider(names_from=Class,values_from=a.call,values_fill=0)%>%
  select(-NN,-`NA`)%>%
  rename(a.call=FS)

am.sum.man<-am.check%>%
  filter(`Manual Class`!="")%>%
  group_by(interval,SPL,spl.group,`Manual Class`)%>%
  summarize(m.call=n())%>%
  pivot_wider(names_from=`Manual Class`,values_from=m.call,values_fill=0)%>%
  select(-N,-FS,-N)%>%
  rename(m.call=`F`)

am.sum<-full_join(am.sum.auto,am.sum.man)%>%
  mutate(call.ratio=ifelse(m.call==0,a.call,a.call/m.call))
(cor(am.sum$m.call,am.sum$a.call))
am.sum$spl.group<-factor(am.sum$spl.group)
# make plot
ggplot(data=am.sum)+
  geom_point(aes(x=m.call,y=a.call),size=2)+
  geom_abline(aes(intercept=0,slope=1),color="grey",linetype="dashed")+
  facet_wrap(~spl.group)+
  theme_bw()+
  theme(panel.grid = element_blank())


# look at agreement by spl
ggplot(data=am.sum)+
  geom_point(aes(x=SPL,y=call.ratio),size=2)+
  geom_hline(aes(yintercept=1),color="darkgrey",linetype="dashed")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Number of automatic ID calls : Number of manual ID calls")

# correlation by spl group
spl.group.agree<-data.frame(spl.group=seq(1,10,1),correlation=NA,n.complete=NA,spl.min=NA,spl.max=NA)
for(i in 1:10){
  t1<-filter(am.sum,spl.group==i)
  spl.group.agree$correlation[i]<-cor(t1$m.call,t1$a.call)
  spl.group.agree$n.complete[i]<-nrow(t1)
  spl.group.agree$spl.min[i]<-min(t1$SPL)
  spl.group.agree$spl.max[i]<-max(t1$SPL)
}
#bring in wind

wind<-read_rds("wdata/trimmed_hourly_weather.rds")%>%
  filter(year==2019)%>%
  filter(rca=="in")%>%
  select("year","month","day","hr","wdir","wspeed")

# bring in spl data cause it has the intervals on it
spl<-read_rds("wdata/spl_by_min.rds")%>%
  filter(rca=="in")%>%
  filter(Year==2019)
year(spl$DateTime)<-spl$year
spl<-arrange(spl,DateTime)
spl$dt2<-spl$DateTime+1
spl$interval<-findInterval(spl$dt2,spl$DateTime)

spl2<-spl%>%
  select(DateTime,interval)%>%
  mutate(year=year(DateTime),
         month=month(DateTime),
         day=day(DateTime),
         hr=hour(DateTime))%>%
  select(-DateTime)
am.sum2<-am.sum%>%
  left_join(spl2)%>%
  left_join(wind)%>%
  filter(!is.na(wspeed))

# LINEAR MODEL
library(lme4)
library(lmerTest)
call.lm<-lmer(m.call~a.call+(a.call|spl.group),data=am.sum)
summary(call.lm)
plot(call.lm)
hist(resid(call.lm))
am.sum$fitted<-fitted(call.lm)


ggplot(data=am.sum)+
  geom_point(aes(x=a.call,y=m.call,color=spl.group),size=2)+
#  geom_smooth(aes(x=a.call,y=m.call),formula = y~x,method="lm")+
  geom_line(aes(x=a.call,y=fitted,group=spl.group))+
  scale_color_viridis_d()+
  geom_abline(aes(intercept=0,slope=1),color="grey",linetype="dashed")+
  facet_wrap(~spl.group)

# look at agreement
am.agree<-am.check %>%
  filter(Class=="FS" | `Manual Class` =="F")%>%
  filter(`Manual Class`!="")

table(am.agree$Class,am.agree$`Manual Class`)

# bring in 5 minute interval checks
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
spl<-arrange(spl,DateTime)%>%
  filter(rca=="in")
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

