# # script to determine which minutes to analyze to get a better range of SPLs for
# # checking the detector
# library(tidyverse)
# library(lubridate)
# # bring in full 2019 automatic dectection dataset
# new.files<-Rraven::imp_raven(path = "w.selection.tables/",
#                              files = "Autodetect_Updated_April_July2019_Dec152020.txt",
#                              all.data = TRUE)
# # bring in dataset that tells me which minutes I've already done
# already.done<-read_rds("wdata/first_check_of_detector_Dec162020.rds")%>%
#   filter(p.noobs==0)
# ad2<-unique(already.done$interval)
# 
# # bring in SPL
# 
# spl<-readRDS("wdata/spl_by_min.rds")
# 
# year(spl$DateTime)<-spl$year
# spl<-arrange(spl,DateTime)%>%
#   filter(rca=="in")
# spl$dt2<-spl$DateTime+1
# spl$interval<-findInterval(spl$dt2,spl$DateTime)
# 
# spl2<-spl%>%
#   filter(DateTime<ymd("2019/6/25"))%>%
#   filter(DateTime>ymd("2019/4/10"))%>%
#   select(SPL,interval)%>%
#   mutate(already.done=ifelse(interval %in% ad2,1,0))
# 
# 
# 
# # look at histogram of SPL
# 
# hist(spl2$SPL)
# min(spl2$SPL)
# max(spl2$SPL)
# 
# # Maybe break it into 10 sections?
# spl.quantiles<-quantile(spl2$SPL,c(.10,.20,.30,.40,.50,.60,.70,.80,.90))
# 
# spl2<-spl2 %>%
#   mutate(spl.group=case_when(
#          SPL<=spl.quantiles[1]~1,
#          SPL<=spl.quantiles[2] & SPL>spl.quantiles[1]~2,
#          SPL<=spl.quantiles[3] & SPL>spl.quantiles[2]~3,
#          SPL<=spl.quantiles[4] & SPL>spl.quantiles[3]~4,
#          SPL<=spl.quantiles[5] & SPL>spl.quantiles[4]~5,
#          SPL<=spl.quantiles[6] & SPL>spl.quantiles[5]~6,
#          SPL<=spl.quantiles[7] & SPL>spl.quantiles[6]~7,
#          SPL<=spl.quantiles[8] & SPL>spl.quantiles[7]~8,
#          SPL<=spl.quantiles[9] & SPL>spl.quantiles[8]~9,
#          SPL>spl.quantiles[9]~10))
# # look at how many I've already done per "group"
# (spl3<-spl2%>%
#   group_by(spl.group)%>%
#   summarize(min.done=sum(already.done)))
# 
# spl4<-spl%>%
#   select(interval,DateTime)%>%
#   distinct()
# 
# # Do 25 per group
# mins.to.do_a<-spl2%>%
#   filter(already.done!=1)
# 
# mins.to.do<-mins.to.do_a%>%
#   group_by(spl.group)%>%
#   sample_n(25,replace=FALSE)
# 
# 
# 
# new.files2<-new.files%>%
#   select(-16:-17)%>%
#   distinct()%>%
#   rename(fo=`File Offset (s)`,
#          begin.file=`Begin File`)
# 
# new.files2<-separate(new.files2,begin.file,into = c("st","yr","m","d","hr","min","s","ext"),sep = c(11,13,15,17,19,21,23),remove=FALSE,convert = TRUE)
# 
# new.files2<- new.files2 %>%
#   mutate(yr=paste0(20,yr),
#          datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
#          dt2=datetime,
#          datetime=datetime+fo,
#          yr=year(datetime),
#          m=month(datetime),
#          d=day(datetime),
#          hr=hour(datetime),
#          min=minute(datetime),
#          s=second(datetime))%>%
#   arrange(datetime)
# 
# new.files2$interval<-findInterval(new.files2$datetime,spl$DateTime)
# 
# mins.to.do2<-new.files2 %>%
#   filter(interval %in% mins.to.do$interval)%>%
#   left_join(spl2)%>%
#   arrange(datetime)%>%
#   mutate(Selection_orig=Selection,
#          Selection=row_number())%>%
#   select(Selection,View,Channel,"Begin Time (s)","End Time (s)",
#          "Delta Time (s)", "Low Freq (Hz)" , "High Freq (Hz)",
#          "Begin Path",`File Offset (s)`=fo,"Begin File"=begin.file,Class,"Sound type",Confidence,interval,SPL,spl.group,Selection_orig)
# 
# 
# ints.doing<-new.files2%>%
#   select(interval,begin.file,dt2)%>%
#   filter(interval %in% mins.to.do2$interval)%>%
#   distinct()
# for.pages<-spl4[spl4$interval%in%ints.doing$interval,]%>%
#   left_join(ints.doing)%>%
#   select(interval,begin.file,DateTime,dt2)%>%
#   mutate(sec.into.file=DateTime-dt2)
# write.csv(for.pages,"wdata/minutes_to_evaluate_master_sheet.csv")
# 
# 
# 
# 
# mins.to.do2$`Begin Path`<-paste0("D:/RCA_IN/April_July2019/amplified_10/",mins.to.do2$`Begin Path`)
# ftm<-mins.to.do2$`Begin File`[mins.to.do2$`Begin File`%in% list.files("D:/RCA_IN/April_July2019/1342218252/")]
# ftm<-unique(ftm)
# 
# for (i in 1:length(ftm)){
#     filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
#       "D:/RCA_IN/April_July2019/toamplify")
# }
# 
# write.table(mins.to.do2,file="w.selection.tables/minutes_to_evaluate.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# find 2020 minutes, and fill in some gaps in 2019 files

all2019 <- Rraven::imp_raven(path = "w.selection.tables/",
                             files = "Autodetect_Updated_April_July2019_Jan2021.txt",
                             all.data = TRUE)[,-19]

new.files2<-Rraven::imp_raven(path = "w.selection.tables/",
                              files = "Autodetect_RCA_In_200418_1205_5047_Jan2021.txt",
                              all.data = TRUE)[,-19]

new.files3<-Rraven::imp_raven(path = "w.selection.tables/",
                              files = "Autodetect_RCA_In_200524_1149_5042_Jan2021.txt",
                              all.data = TRUE)[,-19]

all2020 <- bind_rows(new.files2,new.files3)

# create a date time variable in each detection dataset
#all2019 <- new.files

all2019 <-separate(all2019, stfile,
                   into = c("st","yr","m","d","hr","min","s","ext"), 
                   sep = c(11,13,15,17,19,21,23), remove=FALSE, convert = TRUE)

all2020 <- separate(all2020, stfile, 
                    into = c("st","yr","m","d","hr","min","s","ext"), 
                    sep = c(5,7,9,11,13,15,17), remove=FALSE, convert = TRUE)

# subset 2019 data to only data that overlaps 2020
# first create dates

all2019<-all2019%>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime+into.file,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))%>%
  arrange(datetime)

all2020<-all2020%>%
  mutate(yr=paste0(20,yr),
         datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
         datetime=datetime+into.file,
         yr=year(datetime),
         m=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         min=minute(datetime),
         s=second(datetime))%>%
  arrange(datetime)

min.day<-min(all2020$datetime)
year(min.day)<-2019
max.day<-max(all2020$datetime)
year(max.day)<-2019

sub2019<-all2019%>%
  filter(datetime>=min.day)%>%
  filter(datetime<=max.day)

# now link with SPL

spl1<-readxl::read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
spl2<-readxl::read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2020") 
year(spl1$DateTime)<-2019
year(spl2$DateTime)<-2020

spl19<-spl1%>%
  mutate(
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(Time),
    min=minute(Time),
    sec=second(Time),
    DateTime2=ymd_hms(paste(yr,m,d,hr,min,sec)),
    spl.interval=row_number())%>%
  select(DateTime2,SPL,spl.interval)%>%
  arrange(DateTime2)

spl20<-spl2%>%
  mutate(
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(Time),
    min=minute(Time),
    sec=second(Time),
    DateTime2=ymd_hms(paste(yr,m,d,hr,min,sec)),
    spl.interval=row_number())%>%
  select(DateTime2,SPL,spl.interval)%>%
  arrange(DateTime2)


all2019$spl.interval<-findInterval(all2019$datetime,spl19$DateTime2)
all2020$spl.interval<-findInterval(all2020$datetime,spl20$DateTime2)


om2019<-left_join(all2019,spl19)
om2020<-left_join(all2020,spl20)

# now create datasets to pick files by
om2019fs<-om2019 %>%
  select(`Begin File`,Class,datetime,spl.interval,SPL)%>%
  mutate(phase=case_when(
    datetime < ymd_hms("2019-04-14 00:00:00")~ "dontuse",
    datetime %within% interval(ymd_hms("2019-04-14 00:00:00"),
                               ymd_hms("2019-05-10 23:59:59"))~"early",
    datetime %within% interval(ymd_hms("2019-05-11 00:00:00"),
                               ymd_hms("2019-05-19 23:59:59"))~"no2020",
    datetime %within% interval(ymd_hms("2019-05-20 00:00:00"),
                               ymd_hms("2019-06-01 23:59:59"))~"mid",
    datetime > ymd_hms("2019-06-01 23:59:59") ~"late"))%>%
  select(-datetime)%>%
  group_by(spl.interval,Class)%>%
  mutate(n.call=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()

om2020fs<-om2020 %>%
  select(`Begin File`,Class,datetime,spl.interval,SPL)%>%
  mutate(phase=case_when(
    datetime < ymd_hms("2020-04-14 00:00:00")~ "dontuse",
    datetime %within% interval(ymd_hms("2020-04-14 00:00:00"),
                               ymd_hms("2020-05-10 23:59:59"))~"early",
    datetime %within% interval(ymd_hms("2020-05-11 00:00:00"),
                               ymd_hms("2020-05-20 23:59:59"))~"no2020",
    datetime %within% interval(ymd_hms("2020-05-20 00:00:00"),
                               ymd_hms("2020-06-01 23:59:59"))~"mid",
    datetime > ymd_hms("2020-06-01 23:59:59") ~"late"))%>%
  select(-datetime)%>%
  group_by(spl.interval,Class)%>%
  mutate(n.call=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()

# get distribution of # fish calls across both years

ncall<-c(om2019fs$n.call,om2020fs$n.call)
hist(ncall)

call.q<-quantile(ncall,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
call.q
# this isn't really helpfull. The majority of minutes have less than 20 calls. 
# Breaking data into low <10, mid 10-20, and high >20 calls

# bring in spl.quantiles
spl_groups<-read_rds("wdata/spl_groups_2019.rds")

om2019fs<-om2019fs%>%
  mutate(call.group=case_when(
    n.call %in% 0:9~"low",
    n.call %in% 10:19~"mid",
    n.call > 19~"high"),
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
    n.call %in% 0:9~"low",
    n.call %in% 10:19~"mid",
    n.call > 19~"high"),
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
  select(spl.interval,phase,call.group,spl.group)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group)%>%
  summarize(n=n())%>%
  pivot_wider(names_from = spl.group,values_from=n,values_fill=0)
min(ngrp2019[,-1:-2])
# 2019 has a minimum of 27 minutes per group
ngrp2020<-om2020fs%>%
  select(spl.interval,phase,call.group,spl.group)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group)%>%
  summarize(n=n())%>%
  pivot_wider(names_from = spl.group,values_from=n,values_fill=0)
min(ngrp2020[,-1:-2])
#2020 has a minimum of 12 calls per group

# now to see what I've already done
om<-read_rds("wdata/one_minute_review_reduceddata.rds")
fm<-read_rds("wdata/five_minute_review_reduceddata.rds")

om$spl.interval<-findInterval(om$datetime,spl19$DateTime2)
fm$spl.interval<-findInterval(fm$datetime,spl19$DateTime2)

om2<-om%>%mutate(phase=case_when(
  datetime < ymd_hms("2019-04-14 00:00:00")~ "dontuse",
  datetime %within% interval(ymd_hms("2019-04-14 00:00:00"),
                             ymd_hms("2019-05-10 23:59:59"))~"early",
  datetime %within% interval(ymd_hms("2019-05-11 00:00:00"),
                             ymd_hms("2019-05-20 23:59:59"))~"no2019",
  datetime %within% interval(ymd_hms("2019-05-20 00:00:00"),
                             ymd_hms("2019-06-01 23:59:59"))~"mid",
  datetime > ymd_hms("2019-06-01 23:59:59") ~"late"))%>%
  select(spl.interval,SPL,Class=auto.class,phase)%>%
  group_by(spl.interval,Class)%>%
  mutate(n.call=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()%>%
  mutate(call.group=case_when(
    n.call %in% 0:9~"low",
    n.call %in% 10:19~"mid",
    n.call > 19~"high"),
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
  select(spl.interval,phase,call.group,spl.group2)%>%
  distinct()%>%
  group_by(phase,call.group,spl.group2)%>%
  summarize(n=n())

fm2<-fm%>%mutate(phase=case_when(
  datetime < ymd_hms("2019-04-14 00:00:00")~ "dontuse",
  datetime %within% interval(ymd_hms("2019-04-14 00:00:00"),
                             ymd_hms("2019-05-10 23:59:59"))~"early",
  datetime %within% interval(ymd_hms("2019-05-11 00:00:00"),
                             ymd_hms("2019-05-20 23:59:59"))~"no2019",
  datetime %within% interval(ymd_hms("2019-05-20 00:00:00"),
                             ymd_hms("2019-06-01 23:59:59"))~"mid",
  datetime > ymd_hms("2019-06-01 23:59:59") ~"late"))%>%
  select(spl.interval,SPL,Class=auto.class,phase)%>%
  group_by(spl.interval,Class)%>%
  mutate(n.call=n())%>%
  ungroup()%>%
  filter(Class=="FS")%>%
  filter(phase!="dontuse")%>%
  distinct()%>%
  mutate(call.group=case_when(
    n.call %in% 0:9~"low",
    n.call %in% 10:19~"mid",
    n.call > 19~"high"),
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
  select(spl.interval,phase,call.group,spl.group2)%>%
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
# om2019fs.2<-filter(om2019fs,!spl.interval %in% om$spl.interval)
# ad2<-alrdydone%>%
#   pivot_longer(-1:-2,names_to = "spl.group2",values_to="n.done")
# todo<-om2019fs[1,][-1,]
# cgrp<-unique(ad2$call.group)
# pgrp<-unique(ad2$phase)
# sgrp<-c(1,2,3)
# for(c in 1:3){
#   for(p in 1:3){
#     for(s in 1:3){
#       n2do<-ad2%>%
#         filter(call.group==cgrp[c])%>%
#         filter(phase==pgrp[p])%>%
#         filter(spl.group2==sgrp[s])
#       if(n2do$n.done<5){
#         do<-5-n2do$n.done
#         t1<-om2019fs.2%>%
#         filter(call.group==cgrp[c])%>%
#         filter(phase==pgrp[p])%>%
#         filter(spl.group2==sgrp[s])%>%
#         slice_sample(n=do,replace=FALSE)
#         todo<-bind_rows(todo,t1)
#       }
#     }
#   }
# }
# 
# follow.up.2019<-all2019%>%
#   filter(`Begin File` %in% todo$`Begin File`)%>%
#   select(-selec.file,-stfile,-st,-ext,-into.file)%>%
#   mutate(old.selection=Selection,
#          Selection=row_number())
# 
# 
# follow.up.2019$`Begin Path`<-paste0("D:/RCA_IN/April_July2019/amplified_10/",follow.up.2019$`Begin Path`)
# ftm<-follow.up.2019$`Begin File`[follow.up.2019$`Begin File`%in% list.files("D:/RCA_IN/April_July2019/1342218252/")]
# ftm<-unique(ftm)
# 
# for (i in 1:length(ftm)){
#     filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
#       "D:/RCA_IN/April_July2019/toamplify")
# }
# 
# write.table(follow.up.2019,file="w.selection.tables/followup_minutes_to_evaluate.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# # now pull 5 per category for 2020
# 
# om2020.2do<-om2020fs%>%
#   group_by(call.group,phase,spl.group2)%>%
#   slice_sample(n=5,replace=FALSE)
# files.2020<-data.frame(files=unique(om2020.2do$`Begin File`))
# review.2020<-all2020%>%
#   filter(`Begin File` %in% files.2020)
# 
# review.2020$`Begin Path`<-paste0("D:/RCA_IN/April_July2020/amplified_10/",review.2020$`Begin Path`)
# write.table(review.2020,file="w.selection.tables/2020review.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# write_csv(files.2020,"wdata/2020filesforsteph.csv")
