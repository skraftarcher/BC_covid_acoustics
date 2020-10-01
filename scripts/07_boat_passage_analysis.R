# script to look at how ferry passage impacts fish calling rates

library(tidyverse)
# library(warbleR)
library(Rraven)
library(here)
library(lubridate)
library(tidyverse)

#load data ----
bp<-imp_raven(path = here("selection.tables"),
                 files = "boat_passage.txt",
                 all.data = TRUE)
colnames(bp)<-c("selection","view","channel","begin.time","end.time","low.freq","peak.freq",
                   "high.freq","begin.path","file.off","begin.file","species","detector","confidence",
                   "agreement","sound.type","int","prd","comments","selec.file")

bp2<-bp%>%
  mutate(sound.type=case_when(
    sound.type=="knock"~"knock",
    sound.type=="KNOCK"~"knock",
    sound.type=="grunt"~"grunt",
    sound.type=="grutn"~"grunt",
    sound.type=="grrunt"~"grunt"))%>%
  group_by(int,prd,sound.type)%>%
  summarize(ncalls=n())
bp2$prd<-factor(bp2$prd,levels=c("pre","ferry","post"))

ggplot(data=bp2%>%
         filter(prd!="ferry"))+
  geom_line(aes(x=prd,y=ncalls,group=int,color=as.factor(int)))+
  geom_point(aes(x=prd,y=ncalls,color=as.factor(int)))+
  facet_wrap(~sound.type)

ggplot(data=bp2)+
  geom_boxplot(aes(y=ncalls,fill=prd))

caov<-aov(ncalls~prd,data=bp2)
par(mfrow=c(2,2))
plot(caov)
summary(caov)
TukeyHSD(caov)
