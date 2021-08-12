# # script to determine which minutes to analyze to get a better range of SPLs for
# # checking the detector

source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="Rraven")
lp(pck="lubridate")

# # bring in full 2019 automatic dectection dataset

alldata2019<-read_rds("wdata/alldata2019.rds")
alldata2020<-read_rds("wdata/alldata2020.rds")

#bring in minutes already evaluated
# one minute chunks
am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)

# five minute chunks
check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE) 


# organize the already reviewed datasets
# going to match each call again to make sure initial code worked
am.2<-am.check%>%
  filter(`Manual Class`!="")%>%
  separate(`Begin File`,
           into = c("st","yr","m","d","hr","min","s","ext"), 
           sep = c(11,13,15,17,19,21,23), remove=FALSE, convert = TRUE)%>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime+`File Offset (s)`,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))%>%
  arrange(datetime)%>%
  rename(auto.class=Class,
         man.class=`Manual Class`,
         man.type=`Manual Type`,
         spl.old=SPL,
         spl.group.old=spl.group,
         int.old=interval)%>%
  mutate(man.class=case_when(
    man.class=="M"~"N",
    man.class=="B"~"N",
    man.class=="F"~"F",
    man.class=="N"~"N",
    man.class=="FS"~"FS"))

am.2$SPL.Int<-findInterval(am.2$datetime,alldata2019$SPL.start.time)

# for some reason check19 seems to be grumpy unless you rename the columns
colnames(check19)<-c("Selection","View","Channel","begin.time","end.time","delta.time","low.freq","high.freq","begin.path","file.offset",
                     "begin.file","class","sound.type","software","confidence","selec.file1","selection_x","m.class","m.type","coments",
                     "inter","prd","selec.file","updated.class","updated.confidence","selec.file2")

fm.2<-check19%>%
  filter(!is.na(inter))%>%
  separate(begin.file,
           into = c("st","yr","m","d","hr","min","s","ext"), 
           sep = c(11,13,15,17,19,21,23), remove=FALSE, convert = TRUE)%>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime+file.offset,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))%>%
  arrange(datetime)%>%
  rename(auto.class=updated.class,
         Confidence=updated.confidence,
         man.class=m.class,
         man.type=m.type)

fm.2$SPL.Int<-findInterval(fm.2$datetime,alldata2019$SPL.start.time)

om<-left_join(am.2,alldata2019[,colnames(alldata2019)%in%c("SPL.Int","SPL","wind_spd","wind_dir","precip_amt","tide","wave.ht","wave.prd","wave.ht2","wave.prd2","temp")])%>%
  select(datetime,
         yr,
         m,
         d,
         hr,
         min,
         s,
         auto.class,
         Confidence,
         man.class,
         man.type,
         int.old,
         SPL.Int,
         SPL,
         wind_dir,
         wind_spd,
         wave.ht,
         wave.prd,
         temp)

fm<-left_join(fm.2,alldata2019[,colnames(alldata2019)%in%c("SPL.Int","SPL","wind_spd","wind_dir","precip_amt","tide","wave.ht","wave.prd","wave.ht2","wave.prd2","temp")])%>%
  select(datetime,
         yr,
         m,
         d,
         hr,
         min,
         s,
         auto.class,
         Confidence,
         man.class,
         man.type,
         SPL.Int,
         SPL,
         wind_dir,
         wind_spd,
         wave.ht,
         wave.prd,
         temp)

# find 2020 minutes, and fill in some gaps in 2019 files

# create dataset of time intervals

deploy.ints<-data.frame(phase=c("dontuse","early","no2020","mid","late"),
                        start.dt=c(min(alldata2019$SPL.start.time),
                                   ymd_hms("2019-04-14 00:00:00"),
                                   ymd_hms("2019-05-11 00:00:00"),
                                   ymd_hms("2019-05-20 00:00:00"),
                                   ymd_hms("2019-06-01 23:59:59")),
                        start.dt2020=c(min(alldata2019$SPL.start.time),
                                   ymd_hms("2020-04-14 00:00:00"),
                                   ymd_hms("2020-05-11 00:00:00"),
                                   ymd_hms("2020-05-20 00:00:00"),
                                   ymd_hms("2020-06-01 23:59:59")),
                        phaseID=1:5)

alldata2019$phaseID<-findInterval(alldata2019$SPL.start.time,deploy.ints$start.dt)
alldata2020$phaseID<-findInterval(alldata2020$SPL.start.time,deploy.ints$start.dt2020)

# now create datasets to pick files by
om2019fs<-alldata2019 %>%
  left_join(deploy.ints)%>%
  select(`Begin File`,fish.calls,SPL.Int,SPL,phase)%>%
  filter(phase!="dontuse")%>%
  distinct()

om2020fs<-alldata2020 %>%
  left_join(deploy.ints)%>%
  select(`Begin File`,fish.calls,SPL.Int,SPL,phase)%>%
  filter(phase!="dontuse")%>%
  distinct()

# get distribution of # fish calls across both years

ncall<-c(om2019fs$fish.calls,om2020fs$fish.calls)
hist(ncall)

call.q<-quantile(ncall,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
call.q
# this isn't really helpfull. The majority of minutes have less than 20 calls. 
# Breaking data into low <10, mid 10-20, and high >20 calls

# bring in spl.quantiles
spl_groups<-read_rds("wdata/spl_groups_2019.rds")

om2019fs<-om2019fs%>%
  mutate(call.group=case_when(
    fish.calls %in% 0:9~"low",
    fish.calls %in% 10:19~"mid",
    fish.calls > 19~"high"),
    spl.group=case_when(
      SPL <= spl_groups[1]~1,
      SPL > spl_groups[1] & SPL <= spl_groups[2]~2,
      SPL > spl_groups[2] & SPL <= spl_groups[3]~3,
      SPL > spl_groups[3] & SPL <= spl_groups[4]~4,
      SPL > spl_groups[4] & SPL <= spl_groups[5]~5,
      SPL > spl_groups[5] & SPL <= spl_groups[6]~6,
      SPL > spl_groups[6] & SPL <= spl_groups[7]~7,
      SPL > spl_groups[7] & SPL <= spl_groups[8]~8,
      SPL > spl_groups[8] & SPL <= spl_groups[9]~9,
      SPL > spl_groups[9]~10),
    spl.group2 = case_when(
      spl.group==1~1,
      spl.group==2~1,
      spl.group==3~1,
      spl.group==4~2,
      spl.group==5~2,
      spl.group==6~2,
      spl.group==7~2,
      spl.group==8~3,
      spl.group==9~3,
      spl.group==10~3))

om2020fs<-om2020fs%>%
  mutate(call.group=case_when(
    fish.calls %in% 0:9~"low",
    fish.calls %in% 10:19~"mid",
    fish.calls > 19~"high"),
    spl.group=case_when(
      SPL <= spl_groups[1]~1,
      SPL > spl_groups[1] & SPL <= spl_groups[2]~2,
      SPL > spl_groups[2] & SPL <= spl_groups[3]~3,
      SPL > spl_groups[3] & SPL <= spl_groups[4]~4,
      SPL > spl_groups[4] & SPL <= spl_groups[5]~5,
      SPL > spl_groups[5] & SPL <= spl_groups[6]~6,
      SPL > spl_groups[6] & SPL <= spl_groups[7]~7,
      SPL > spl_groups[7] & SPL <= spl_groups[8]~8,
      SPL > spl_groups[8] & SPL <= spl_groups[9]~9,
      SPL > spl_groups[9]~10),
    spl.group2 = case_when(
      spl.group==1~1,
      spl.group==2~1,
      spl.group==3~1,
      spl.group==4~2,
      spl.group==5~2,
      spl.group==6~2,
      spl.group==7~2,
      spl.group==8~3,
      spl.group==9~3,
      spl.group==10~3))

# look at number of minutes per group
ngrp2019<-om2019fs%>%
  select(SPL.Int,phase,call.group,spl.group)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group)%>%
  summarize(n=n())%>%
  pivot_wider(names_from = spl.group,values_from=n,values_fill=0)

min(ngrp2019[,-1:-2])

# 2019 has a minimum of 27 minutes per group

ngrp2020<-om2020fs%>%
  select(SPL.Int,phase,call.group,spl.group)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group)%>%
  summarize(n=n())%>%
  pivot_wider(names_from = spl.group,values_from=n,values_fill=0)

min(ngrp2020[,-1:-2])

#2020 has a minimum of 12 calls per group

# now to see what I've already done

om$phaseID<-findInterval(om$datetime,deploy.ints$start.dt)
fm$phaseID<-findInterval(fm$datetime,deploy.ints$start.dt)

om2<-om%>%
  left_join(deploy.ints)%>%
  select(SPL.Int,SPL,Class=auto.class,phase)%>%
  group_by(SPL.Int,Class)%>%
  mutate(fish.calls=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()%>%
  mutate(call.group=case_when(
    fish.calls %in% 0:9~"low",
    fish.calls %in% 10:19~"mid",
    fish.calls > 19~"high"),
    spl.group=case_when(
      SPL <= spl_groups[1]~1,
      SPL > spl_groups[1] & SPL <= spl_groups[2]~2,
      SPL > spl_groups[2] & SPL <= spl_groups[3]~3,
      SPL > spl_groups[3] & SPL <= spl_groups[4]~4,
      SPL > spl_groups[4] & SPL <= spl_groups[5]~5,
      SPL > spl_groups[5] & SPL <= spl_groups[6]~6,
      SPL > spl_groups[6] & SPL <= spl_groups[7]~7,
      SPL > spl_groups[7] & SPL <= spl_groups[8]~8,
      SPL > spl_groups[8] & SPL <= spl_groups[9]~9,
      SPL > spl_groups[9]~10),
    spl.group2 = case_when(
      spl.group==1~1,
      spl.group==2~1,
      spl.group==3~1,
      spl.group==4~2,
      spl.group==5~2,
      spl.group==6~2,
      spl.group==7~2,
      spl.group==8~3,
      spl.group==9~3,
      spl.group==10~3))%>%
  ungroup()%>%
  select(SPL.Int,phase,call.group,spl.group2)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group2)%>%
  summarize(n=n())

fm2<-fm%>%
  left_join(deploy.ints)%>%
  select(SPL.Int,SPL,Class=auto.class,phase)%>%
  group_by(SPL.Int,Class)%>%
  mutate(fish.calls=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()%>%
  mutate(call.group=case_when(
    fish.calls %in% 0:9~"low",
    fish.calls %in% 10:19~"mid",
    fish.calls > 19~"high"),
    spl.group=case_when(
      SPL <= spl_groups[1]~1,
      SPL > spl_groups[1] & SPL <= spl_groups[2]~2,
      SPL > spl_groups[2] & SPL <= spl_groups[3]~3,
      SPL > spl_groups[3] & SPL <= spl_groups[4]~4,
      SPL > spl_groups[4] & SPL <= spl_groups[5]~5,
      SPL > spl_groups[5] & SPL <= spl_groups[6]~6,
      SPL > spl_groups[6] & SPL <= spl_groups[7]~7,
      SPL > spl_groups[7] & SPL <= spl_groups[8]~8,
      SPL > spl_groups[8] & SPL <= spl_groups[9]~9,
      SPL > spl_groups[9]~10),
    spl.group2 = case_when(
        spl.group==1~1,
        spl.group==2~1,
        spl.group==3~1,
        spl.group==4~2,
        spl.group==5~2,
        spl.group==6~2,
        spl.group==7~2,
        spl.group==8~3,
        spl.group==9~3,
        spl.group==10~3))%>%
  ungroup()%>%
  select(SPL.Int,phase,call.group,spl.group2)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group2)%>%
  summarize(n2=n())

alrdydone<-left_join(om2,fm2)%>%
  mutate(n=ifelse(is.na(n),0,n),
         n2=ifelse(is.na(n2),0,n2),
         ndone=n+n2)%>%
  select(-n,-n2)%>%
  pivot_wider(names_from = spl.group2,values_from=ndone,values_fill=0)

max(alrdydone[,-1:-2])
#maximum i've done per group is 15
write_rds(ngrp2019,"wdata/datadist_2019.rds")
write_rds(ngrp2020,"wdata/datadist_2020.rds")
write_rds(alrdydone,"wdata/datadist_alreadydone.rds")

# # remove periods I've already done from possible selections for 2019
# 


om2019fs.2<-filter(om2019fs,!SPL.Int %in% om$SPL.Int)

om2020fs.2<-filter(om2020fs,`Begin File` %in% review.2020.files$`Begin File`)

ad2<-alrdydone%>%
  pivot_longer(-1:-2,names_to = "spl.group2",values_to="n.done")
todo<-om2019fs[1,][-1,]
cgrp<-unique(ad2$call.group)
pgrp<-unique(ad2$phase)
sgrp<-c(1,2,3)
for(c in 1:3){
  for(p in 1:3){
    for(s in 1:3){
      n2do<-ad2%>%
        filter(call.group==cgrp[c])%>%
        filter(phase==pgrp[p])%>%
        filter(spl.group2==sgrp[s])
      if(n2do$n.done<5){
        do<-5-n2do$n.done
        t1<-om2019fs.2%>%
        filter(call.group==cgrp[c])%>%
        filter(phase==pgrp[p])%>%
        filter(spl.group2==sgrp[s])%>%
        slice_sample(n=do,replace=FALSE)
        todo<-bind_rows(todo,t1)
      }
    }
  }
}

follow.up.2019<-alldata2019%>%
  filter(SPL.Int %in% todo$SPL.Int)%>%
  select(SPL.Int,`Begin File`,SPL.start.time)%>%
  mutate(end.time=SPL.start.time+60)%>%
  pivot_longer(SPL.start.time:end.time,names_to="start.end",values_to="datetime")%>%
  distinct()%>%
  arrange(datetime)


follow.up.wavs<-unique(follow.up.2019$`Begin File`)
for(i in 1:length(follow.up.wavs)){
  if(i==1)dfiles<-list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern=follow.up.wavs[i])
  if(i!=1)dfiles<-c(dfiles,list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern=follow.up.wavs[i]))
}

follow.up.st<-Rraven::imp_raven(path="selection.tables/RCA_in_April_July2019_1342218252_updated",
                                files = dfiles,
                                all.data = TRUE)%>%
  select(-selec.file)

follow.up.st2<-follow.up.st%>%
  separate('Begin File',into=c("st","time","ext"),sep=c(-16,-4),remove = FALSE)%>%
  mutate(time=ymd_hms(time),
         time=time+`File Offset (s)`,
         okeep=findInterval(time,follow.up.2019$datetime))%>%
  filter(okeep %in% seq(1,9945,2))%>%
  select(-st,-time,-ext,-okeep)



follow.up.st2$`Begin Path`<-paste0("D:/RCA_IN/April_July2019/amplified_10/",follow.up.st2$`Begin Path`)
ftm<-follow.up.st2$`Begin File`[follow.up.st2$`Begin File`%in% list.files("D:/RCA_IN/April_July2019/1342218252/")]
ftm<-unique(ftm)

for (i in 1:length(ftm)){
    filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
      "D:/RCA_IN/April_July2019/toamplify")
}

write.table(follow.up.2019,file="w.selection.tables/followup_minutes_to_evaluate.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# now pull 5 per category for 2020

om2020.2do<-om2020fs%>%
  group_by(call.group,phase,spl.group2)%>%
  slice_sample(n=5,replace=FALSE)

files.2020<-data.frame(files=unique(om2020.2do$`Begin File`))

follow.up.2020<-alldata2020%>%
  filter(SPL.Int %in% todo$SPL.Int)%>%
  select(SPL.Int,`Begin File`,SPL.start.time)%>%
  mutate(end.time=SPL.start.time+60)%>%
  pivot_longer(SPL.start.time:end.time,names_to="start.end",values_to="datetime")%>%
  distinct()%>%
  arrange(datetime)

follow.up.wavs<-unique(follow.up.2020$`Begin File`)


for(i in 1:length(follow.up.wavs)){
  if(i==1){
    dfiles1<-list.files("selection.tables/RCA_In_200418_1205_5047",pattern=follow.up.wavs[i])
    dfiles2<-list.files("selection.tables/RCA_In_200524_1149_5042",pattern=follow.up.wavs[i])
  }
  if(i!=1){
    dfiles1<-c(dfiles1,list.files("selection.tables/RCA_In_200418_1205_5047",pattern=follow.up.wavs[i]))
    dfiles2<-c(dfiles2,list.files("selection.tables/RCA_In_200524_1149_5042",pattern=follow.up.wavs[i]))
  }
}

follow.up20.st<-bind_rows(Rraven::imp_raven(path="selection.tables/RCA_In_200418_1205_5047",
                                files = dfiles1,
                                all.data = TRUE)%>%
  select(-selec.file),
  Rraven::imp_raven(path="selection.tables/RCA_In_200524_1149_5042",
                    files = dfiles2,
                    all.data = TRUE)%>%
    select(-selec.file))
  

follow.up20.st2<-follow.up20.st%>%
  separate('Begin File',into=c("st","time","ext"),sep=c(-16,-4),remove = FALSE)%>%
  mutate(time=ymd_hms(time),
         time=time+`File Offset (s)`,
         okeep=findInterval(time,follow.up.2020$datetime))%>%
  filter(okeep %in% seq(1,9945,2))%>%
  select(-st,-time,-ext,-okeep)


follow.up20.st2$`Begin Path`<-paste0("D:/RCA_IN/April_July2020/amplified_10/",follow.up20.st2$`Begin Path`)
write.table(review.2020,file="w.selection.tables/2020review.txt", sep = "\t", row.names = FALSE, quote = FALSE)


