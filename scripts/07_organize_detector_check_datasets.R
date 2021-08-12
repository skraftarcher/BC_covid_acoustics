# Organize detector check data

# load packages
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="lubridate")
lp(pck="readxl")

# load data
# one minute chunks
am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)%>%
  select(-selec.file)
# five minute chunks
check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE)%>%
  select(-selec.file)
#follow up chunks
follow.check<-Rraven::imp_raven(path = "w.selection.tables/",
                                files = "followup_minutes_to_evaluate.txt",
                                all.data = TRUE)%>%
  select(-selec.file)

#2020 chunks
check20<-Rraven::imp_raven(path = "w.selection.tables/",
                           files = "2020review.txt",
                           all.data = TRUE)%>%
  select(-selec.file)%>%
  filter(man.class!="")%>%
  filter(!is.na(man.class))

all19<-read_rds("wdata/alldata2019.rds")
all20<-read_rds("wdata/alldata2020.rds")

# organize data

#look and see if there are typos in the manual class
table(am.check$`Manual Class`) # all good

am2<-am.check%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="original")

table(check19$`Manual Class`) #all good

five2<-check19%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="boatpassage")

table(follow.check$man.class)# there's a few typos

follow2<-follow.check%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(man.class %in% c("F","G",""),1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="followup")

table(check20$man.class)# there's a few typos

man20<-check20%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(man.class %in% c("F","K"),1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="2020")

# group all 2019 

man19<-bind_rows(am2,five2,follow2)

# link autodetection stuff

dcheck19<-left_join(man19,all19)%>%
  filter(!is.na(SPL))
dcheck20<-left_join(man20,all20)%>%
  filter(!is.na(SPL))

#write out these datasets
write_rds(dcheck19,"wdata/detector_check_2019.rds")
write_rds(dcheck20,"wdata/detector_check_2020.rds")
