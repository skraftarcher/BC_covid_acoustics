# script to compare automatic detections and mannual checking

# First- just some code to subset down to already made selections to check how the amplification worked.
library(tidyverse)

temp1<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                                files = "boat_passage_autoselect.txt",
                                all.data = TRUE) 

temp2<-temp1[!is.na(temp1[,21]),]
# to check 10db amplification yes I know I misspelled amplified in the file name, but I didn't notice until I had created these files
temp2[,9]<-paste0("E:\\RCA_IN\\April_July2019\\boat_passage\\amiplified_10\\",temp2[,11])

write.table(temp2, file = "w.selection.tables/boat_passage_check_amp_10.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# to check 20db amplification 
temp2[,9]<-paste0("E:\\RCA_IN\\April_July2019\\boat_passage\\amiplified_20\\",temp2[,11])

write.table(temp2, file = "w.selection.tables/boat_passage_check_amp_20.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Now bring in the 10 and 20 db amp selection tables and compare

a10<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                       files = "boat_passage_check_amp_10.txt",
                       all.data = TRUE) 

a20<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                       files = "boat_passage_check_amp_20.txt",
                       all.data = TRUE) 


a102<-a10%>%
  select(Selection,Class,Confidence,'Manual Class','Manual Type')%>%
  rename(class.10='Manual Class',type.10='Manual Type')%>%
  mutate(class.10=ifelse(Class=="NN" & class.10=="NH","N",class.10))

a202<-a20%>%
  select(Selection,Class,Confidence,'Manual Class','Manual Type')%>%
  rename(class.20='Manual Class',type.20='Manual Type')%>%
  mutate(class.20=ifelse(Class=="NN" & class.20=="NH","N",class.20))

a02<-temp2%>%
  select(Selection,Class,Confidence,'Manual Class','Manual Type')%>%
  rename(class.0='Manual Class',type.0='Manual Type')%>%
  filter(!is.na(class.0))

all<-full_join(a02,a102)%>%
  full_join(a202)%>%
  mutate(agree.0=case_when(
    Class=="NN"& class.0=="N"~"agree",
    Class=="NN" & class.0=="NH"~"noise.not.heard",
    Class=="NN" & class.0=="F"~"false.negative",
    Class=="NN" & class.0=="U"~"uncertain",
    Class=="NN" & is.na(class.0)~"not.annotated",
    Class=="FS"& class.0=="N"~"false.positive",
    Class=="FS" & class.0=="NH"~"fish.not.heard",
    Class=="FS" & class.0=="F"~"agree",
    Class=="FS" & class.0=="U"~"uncertain",
    Class=="FS" & is.na(class.0)~"not.annotated",
    is.na(Class)& class.0=="N"~"new.noise",
    is.na(Class) & class.0=="NH"~"mistake",
    is.na(Class) & class.0=="F"~"new.fish",
    is.na(Class) & class.0=="U"~"new.uncertain",
    is.na(Class) & is.na(class.0)~"not.annotated"),
    agree.10=case_when(
      Class=="NN"& class.10=="N"~"agree",
      Class=="NN" & class.10=="NH"~"noise.not.heard",
      Class=="NN" & class.10=="F"~"false.negative",
      Class=="NN" & class.10=="U"~"uncertain",
      Class=="NN" & is.na(class.10)~"not.annotated",
      Class=="FS"& class.10=="N"~"false.positive",
      Class=="FS" & class.10=="NH"~"fish.not.heard",
      Class=="FS" & class.10=="F"~"agree",
      Class=="FS" & class.10=="U"~"uncertain",
      Class=="FS" & is.na(class.10)~"not.annotated",
      is.na(Class)& class.10=="N"~"new.noise",
      is.na(Class) & class.10=="NH"~"mistake",
      is.na(Class) & class.10=="F"~"new.fish",
      is.na(Class) & class.10=="U"~"new.uncertain",
      is.na(Class) & is.na(class.10)~"not.annotated"),
    agree.20=case_when(
      Class=="NN"& class.20=="N"~"agree",
      Class=="NN" & class.20=="NH"~"noise.not.heard",
      Class=="NN" & class.20=="F"~"false.negative",
      Class=="NN" & class.20=="U"~"uncertain",
      Class=="NN" & is.na(class.20)~"not.annotated",
      Class=="FS"& class.20=="N"~"false.positive",
      Class=="FS" & class.20=="NH"~"fish.not.heard",
      Class=="FS" & class.20=="F"~"agree",
      Class=="FS" & class.20=="U"~"uncertain",
      Class=="FS" & is.na(class.20)~"not.annotated",
      is.na(Class)& class.20=="N"~"new.noise",
      is.na(Class) & class.20=="NH"~"mistake",
      is.na(Class) & class.20=="F"~"new.fish",
      is.na(Class) & class.20=="U"~"new.uncertain",
      is.na(Class) & is.na(class.20)~"not.annotated"))

total.ids<-all%>%
  group_by(Class)%>%
  summarize(tot.n=n())

all.summary<-all%>%
  select(Class,Confidence,agree.0,agree.10,agree.20)%>%
  pivot_longer(agree.0:agree.20,names_to="amplification",values_to="agreement")%>%
  mutate(amplification=case_when(
    amplification=="agree.0"~0,
    amplification=="agree.10"~10,
    amplification=="agree.20"~20))%>%
  group_by(Class,amplification,agreement)%>%
  summarize(m.conf=mean(Confidence),sd.conf=sd(Confidence),n=n())%>%
  left_join(total.ids)%>%
  mutate(p=n/tot.n)%>%
  filter(agreement!="not.annotated")

ggplot(data=all.summary,aes(y=agreement,x=p,fill=as.factor(amplification)))+
  geom_bar(stat="identity",position = position_dodge())+
  facet_grid(~Class)


