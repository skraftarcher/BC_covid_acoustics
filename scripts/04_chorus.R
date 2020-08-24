#script to look at the first 48 hours in 2019

#install packages----
# devtools::install_github("maRce10/warbleR")
# devtools::install_github("maRce10/Rraven")

#load packages----
library(tidyverse)
library(warbleR)
library(Rraven)
library(here)
library(lubridate)

#load data ----
in19q<-imp_raven(path = here("selection.tables"),
                 files = "1342218252.190421000157.Table.1.selections.txt",
                 all.data = TRUE)
autod<-imp_raven(path = here("selection.tables"),
                 files = "NanaimoRCA_FS_detections_raven-format_EditedCopy_05June2019.txt",
                 all.data = TRUE)
colnames(autod)<-c("selection","view","channel","begin.time","end.time","low.freq",
                      "high.freq","begin.path","file.off","begin.file","species","detector","confidence",
                   "agreement","sount.type","comments","selec.file")
oct19in<-imp_raven(path=here("selection.tables","RCA_In_Deployment_1"),all.data = TRUE)
oct19out<-imp_raven(path=here("selection.tables","RCA_Out_Deployment_1"),all.data = TRUE)
colnames(oct19in)<-c("selection","view","channel","begin.time","end.time","low.freq","peak.freq",
                     "high.freq","delta.time","file.off","begin.path","begin.file","class","sound.type",
                     "comments","selec.file")
colnames(oct19out)<-c("selection","view","channel","begin.time","end.time","low.freq","peak.freq",
                     "high.freq","delta.time","file.off","begin.path","begin.file","class","sound.type",
                     "comments","selec.file")
# organize data
in19q2<-in19q%>%
  separate("Begin File",into=c("st","y","m","d","h","min","s","e"),
           sep=c(11,13,15,17,19,21,23),
           remove=TRUE)%>%
  mutate(abund=case_when(
    comments==""~1,
    comments=="0"~0,
    comments=="1"~1,
    comments=="0-10"~1,
    comments=="2-10"~2,
    comments=="10-20"~10,
    comments=="20-50"~20,
    comments=="50-100"~50,
    comments==">100"~100))

oct19in2<-oct19in%>%
  separate(selec.file,into=c("st","y","m","d","h","min","s","e"),
         sep=c(9,11,13,15,17,19,21),
         remove=TRUE)%>%
  filter(class=="fish")%>%
  group_by(y,m,d,h)%>%
  summarize(ncall=n(),tcall=sum(delta.time),dt=ymd_h(paste(y,m,d,h)),site="In")%>%
  arrange(y,m,d,h)

oct19out2<-oct19out%>%
  separate(selec.file,into=c("st","y","m","d","h","min","s","e"),
           sep=c(9,11,13,15,17,19,21),
           remove=TRUE)%>%
  filter(class=="fish")%>%
  group_by(y,m,d,h)%>%
  summarize(ncall=n(),tcall=sum(delta.time),dt=ymd_h(paste(y,m,d,h)),site="Out")%>%
  arrange(y,m,d,h)

autod2<-autod%>%
  separate(begin.file,into=c("st","y","m","d","h","min","s","e"),
           sep=c(9,11,13,15,17,19,21),
           remove=TRUE)%>%
  filter(agreement=="yes"|agreement=="new")%>%
  group_by(y,m,d,h)%>%
  summarize(ncall=n(),tcall=sum(end.time-begin.time),dt=ymd_h(paste(y,m,d,h)),site="In2")%>%
  arrange(y,m,d,h)

all.oct<-bind_rows(oct19in2,oct19out2,autod2)

#plot data ----

ggplot(data=all.oct)+
  geom_line(aes(y=tcall,x=dt,color=site))

ggsave("figures/oct2019_patterns.jpg")
ggplot(data=in19q2)+
  geom_line(aes(y=abund,x=h,color=d,group=d))
ggsave("figures/april2019_patterns.jpg")

#bring in hourly spl to look at patterns by time for these two days in may----
spl<-read_rds("wdata/trimmed_hourly_weather.rds")%>%
  filter((date=="2019-04-21"|date=="2019-04-29") & rca=="in")

ggplot(data=spl)+
  geom_line(aes(y=spl50,x=hr,color=as.factor(date),group=date))
ggsave("figures/april2019_spl.jpg")
