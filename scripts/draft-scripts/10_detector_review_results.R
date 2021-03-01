# this script examines the results of the detector check for the 10% of 5 minute chunks

library(tidyverse)
library(lubridate)
# bring in selection table

check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE) 

colnames(check19)<-c("Selection","View","Channel","begin.time","end.time","delta.time","low.freq","high.freq","begin.path","file.offset",
                     "begin.file","class","sound.type","software","confidence","selec.file1","selection_x","m.class","m.type","coments",
                     "inter","prd","selec.file","updated.class","updated.confidence","selec.file2")
# create an agreement variable

check192<-check19[!is.na(check19$inter),] %>%
  mutate(agrmt = case_when(
    updated.class=="NN"& m.class=="N"~"agree.noise",
    updated.class=="NN" & m.class=="NH"~"noise.not.heard",
    updated.class=="NN" & m.class=="F"~"false.negative",
    updated.class=="NN" & m.class=="U"~"uncertain",
    updated.class=="NN" & is.na(m.class)~"not.annotated",
    updated.class=="NN" & m.class=="FS"~"split.detection",
    updated.class=="FS"& m.class=="N"~"false.positive",
    updated.class=="FS" & m.class=="NH"~"fish.not.heard",
    updated.class=="FS" & m.class=="F"~"agree.fish",
    updated.class=="FS" & m.class=="U"~"uncertain",
    updated.class=="FS" & is.na(m.class)~"not.annotated",
    updated.class=="FS" & m.class=="FS"~"split.detection",
    is.na(updated.class)& m.class=="N"~"new.noise",
    is.na(updated.class) & m.class=="NH"~"mistake",
    is.na(updated.class) & m.class=="F"~"new.fish",
    is.na(updated.class) & m.class=="U"~"new.uncertain",
    is.na(updated.class) & is.na(m.class)~"not.annotated",
    is.na(updated.class) & m.class=="FS"~"split.detection"))

check192<-separate(check192,begin.file,into = c("st","yr","m","d","hr","min","s","ext"),sep = c(11,13,15,17,19,21,23),remove=FALSE)

check192<- check192 %>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime + file.offset,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))

# bring in minute by minute spl

spl<-readRDS("wdata/spl_by_min.rds")

year(spl$DateTime)<-spl$year
spl<-arrange(spl,DateTime)
spl$dt2<-spl$DateTime+1
spl$interval<-findInterval(spl$dt2,spl$DateTime)

spl2<-spl%>%
  select(SPL,interval)

# link detections to SPL
check192$interval<-findInterval(check192$datetime,spl$DateTime)

check192<-left_join(check192,spl2)

# look at summaries by interval
sum19<-check192%>%
  group_by(interval,agrmt)%>%
  summarise(ncalls=n(),spl=mean(SPL),confidence=mean(confidence),.groups="rowwise")%>%
  ungroup()%>%
  group_by(interval)%>%
  mutate(totcalls=sum(ncalls),
         pcalls=round((ncalls/totcalls)*100,2))

ggplot(data=check192)+
  geom_violin(aes(x=confidence,y=agrmt))+
  facet_grid(~class)

ggplot(data=check192%>%
         filter(agrmt %in% c("false.positive")))+
  geom_point(aes(x=SPL,y=confidence))

ggplot(data=sum19%>%
         filter(agrmt %in% c("false.positive"))%>%
         filter(totcalls>5))+
  geom_point(aes(x=spl,y=pcalls))+
  geom_smooth(aes(x=spl,y=pcalls),method="lm")

ggplot(data=check192%>%
         filter(agrmt %in% c("false.negative")))+
  geom_point(aes(x=SPL,y=confidence))

# calculate % false positives and % false negatives

prcts<-check192%>%  
  group_by(class)%>%
  mutate(totdetect=n())%>%
  ungroup()%>%
  group_by(class,agrmt,totdetect)%>%
  summarize(ndetect=n(),conf=mean(confidence))%>%
  mutate(p.detect=ndetect/totdetect)


# look at patterns of calls in 5 minute periods in automatic detector vs manual

auto19<-check192%>%
  group_by(inter, prd, updated.class)%>%
  summarize(ncall.auto=n(),spl=mean(SPL))%>%
  filter(updated.class=="FS")%>%
  select(-updated.class)

man19<-check192%>%
  group_by(inter, prd, m.class)%>%
  summarize(ncall.man=n())%>%
  filter(m.class=="F")%>%
  select(-m.class)

all19<-left_join(auto19,man19)
summary(lm(ncall.man~ncall.auto,data=all19))
cor(x=all19$ncall.man,y=all19$ncall.auto,method="pearson")

ggplot(aes(x=ncall.auto,y=ncall.man),data=all19)+
  geom_point(aes(color=spl))+
  geom_smooth(method="lm")+
  geom_text(aes(x=20,y=150),label="rp = 0.88")

ggsave("figures/auto_mannual_updated.jpg")

# look at spl by minute

auto192<-check192%>%
  group_by(interval, updated.class, SPL)%>%
  summarize(ncall.auto=n())%>%
  filter(updated.class=="FS")%>%
  select(-updated.class)

man192<-check192%>%
  group_by(interval, m.class)%>%
  summarize(ncall.man=n())%>%
  filter(m.class=="F")%>%
  select(-m.class)

all192<-left_join(auto192,man192)%>%
  mutate(ncall.man=ifelse(is.na(ncall.man),0,ncall.man))
cor(x=all192$ncall.man,y=all192$ncall.auto,method="pearson")


ggplot(aes(x=ncall.auto,y=ncall.man),data=all192)+
  geom_point(aes(color=SPL))+
  geom_smooth(method="lm")+
  geom_text(aes(x=5,y=50),label="rp = 0.86")


ggsave("figures/auto_mannual_by_minute_updated.jpg")
