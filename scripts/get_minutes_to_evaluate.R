# script to determine which minutes to analyze to get a better range of SPLs for
# checking the detector
library(tidyverse)
library(lubridate)
# bring in full 2019 automatic dectection dataset
new.files<-Rraven::imp_raven(path = "w.selection.tables/",
                             files = "Autodetect_Updated_April_July2019_Dec152020.txt",
                             all.data = TRUE)
# bring in dataset that tells me which minutes I've already done
already.done<-read_rds("wdata/first_check_of_detector_Dec162020.rds")%>%
  filter(p.noobs==0)
ad2<-unique(already.done$interval)

# bring in SPL

spl<-readRDS("wdata/spl_by_min.rds")

year(spl$DateTime)<-spl$year
spl<-arrange(spl,DateTime)
spl$dt2<-spl$DateTime+1
spl$interval<-findInterval(spl$dt2,spl$DateTime)

spl2<-spl%>%
  filter(DateTime<ymd("2019/6/25"))%>%
  filter(DateTime>ymd("2019/4/10"))%>%
  select(SPL,interval)%>%
  mutate(already.done=ifelse(interval %in% ad2,1,0))



# look at histogram of SPL

hist(spl2$SPL)
min(spl2$SPL)
max(spl2$SPL)

# Maybe break it into 10 sections?
spl.quantiles<-quantile(spl2$SPL,c(.10,.20,.30,.40,.50,.60,.70,.80,.90))

spl2<-spl2 %>%
  mutate(spl.group=case_when(
         SPL<=spl.quantiles[1]~1,
         SPL<=spl.quantiles[2] & SPL>spl.quantiles[1]~2,
         SPL<=spl.quantiles[3] & SPL>spl.quantiles[2]~3,
         SPL<=spl.quantiles[4] & SPL>spl.quantiles[3]~4,
         SPL<=spl.quantiles[5] & SPL>spl.quantiles[4]~5,
         SPL<=spl.quantiles[6] & SPL>spl.quantiles[5]~6,
         SPL<=spl.quantiles[7] & SPL>spl.quantiles[6]~7,
         SPL<=spl.quantiles[8] & SPL>spl.quantiles[7]~8,
         SPL<=spl.quantiles[9] & SPL>spl.quantiles[8]~9,
         SPL>spl.quantiles[9]~10))
# look at how many I've already done per "group"
spl3<-spl2%>%
  group_by(spl.group)%>%
  summarize(min.done=sum(already.done))

# Maximum I've already done is 77. Maybe do 80 per group?
need.to.do<-80-spl3$min.done
mins.to.do_a<-spl2%>%
  filter(already.done!=1)
for(i in 1:10){
  t1<-filter(mins.to.do_a,spl.group==i)%>%
    sample_n(need.to.do[i],replace=F)
  if(i==1)mins.to.do<-t1
  if(i!=1)mins.to.do<-bind_rows(mins.to.do,t1)
}


new.files2<-new.files%>%
  select(-16:-17)%>%
  distinct()%>%
  rename(fo=`File Offset (s)`,
         begin.file=`Begin File`)

new.files2<-separate(new.files2,begin.file,into = c("st","yr","m","d","hr","min","s","ext"),sep = c(11,13,15,17,19,21,23),remove=FALSE,convert = TRUE)

new.files2<- new.files2 %>%
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

new.files2$interval<-findInterval(new.files2$datetime,spl$DateTime)

mins.to.do2<-new.files2 %>%
  filter(interval %in% mins.to.do$interval)%>%
  left_join(spl2)%>%
  arrange(begin.file,fo)%>%
  select(Selection,View,Channel,"Begin Time (s)","End Time (s)",
         "Delta Time (s)", "Low Freq (Hz)" , "High Freq (Hz)",
         "Begin Path",`File Offset (s)`=fo,"Begin File"=begin.file,Class,"Sound type",Confidence,interval,SPL,spl.group)

mins.to.do2$`Begin Path`<-paste0("D:/RCA_IN/April_July2019/amplified_10/",mins.to.do2$`Begin Path`)
ftm<-mins.to.do2$`Begin File`[mins.to.do2$`Begin File`%in% list.files("D:/RCA_IN/April_July2019/1342218252/")]
ftm<-unique(ftm)

for (i in 1:length(ftm)){
    filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
      "D:/RCA_IN/April_July2019/toamplify")
}

write.table(mins.to.do2,file="w.selection.tables/minutes_to_evaluate.txt", sep = "\t", row.names = FALSE, quote = FALSE)





