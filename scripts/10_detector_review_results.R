# this script examines the results of the detector check for the 10% of 5 minute chunks

library(tidyverse)
library(lubridate)
# bring in selection table

check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_amp_10_Nov32020.txt",
                           all.data = TRUE) 

colnames(check19)<-c("Selection","View","Channel","begin.time","end.time","delta.time","low.freq","high.freq","begin.path","file.offset",
                     "begin.file","class","sound.type","software","confidence","selec.file1","selection_x","m.class","m.type","coments",
                     "inter","prd","selec.file")
# create an agreement variable

check192<-check19[!is.na(check19$inter),] %>%
  mutate(agrmt = case_when(
    class=="NN"& m.class=="N"~"agree.noise",
    class=="NN" & m.class=="NH"~"noise.not.heard",
    class=="NN" & m.class=="F"~"false.negative",
    class=="NN" & m.class=="U"~"uncertain",
    class=="NN" & is.na(m.class)~"not.annotated",
    class=="NN" & m.class=="FS"~"split.detection",
    class=="FS"& m.class=="N"~"false.positive",
    class=="FS" & m.class=="NH"~"fish.not.heard",
    class=="FS" & m.class=="F"~"agree.fish",
    class=="FS" & m.class=="U"~"uncertain",
    class=="FS" & is.na(m.class)~"not.annotated",
    class=="FS" & m.class=="FS"~"split.detection",
    is.na(class)& m.class=="N"~"new.noise",
    is.na(class) & m.class=="NH"~"mistake",
    is.na(class) & m.class=="F"~"new.fish",
    is.na(class) & m.class=="U"~"new.uncertain",
    is.na(class) & is.na(m.class)~"not.annotated",
    is.na(class) & m.class=="FS"~"split.detection"))

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


