# load packages
library(tidyverse)
library(lubridate)
library(readxl)

# load data
# one minute chunks
am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)
# five minute chunks
check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE) 
#follow up chunks
follow.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "followup_minutes_to_evaluate.txt",
                            all.data = TRUE)

# organize each dataset getting the time of each call to place into each minute
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

# rename some columns in the follow up check
follow.2<-follow.check%>%
  rename(auto.class=Class,
         old.spl.int=spl.interval)%>%
  select(-selec.file)%>%
  mutate(datetime=ymd_hms(datetime))
# do the SPL thing
# bring in SPL data

spl19<-read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
 
year(spl19$DateTime)<-2019


#create the date + time that we trust for the spl data
spl19.use<-spl19%>%
  filter(DateTime<ymd("2019/6/25"))%>%
  filter(DateTime>ymd("2019/4/10"))%>%
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

# find interval in fish calls

am.2$spl.interval<-findInterval(am.2$datetime,spl19.use$DateTime2)
fm.2$spl.interval<-findInterval(fm.2$datetime,spl19.use$DateTime2)
follow.2$spl.interval<-findInterval(follow.2$datetime,spl19.use$DateTime2)
#now link with spl
am.2<-left_join(am.2,spl19.use)
follow.2<-left_join(follow.2,spl19.use)
#check one minute intervals to see if old code worked (mostly out of curiosity)
plot(am.2$spl.old~am.2$SPL)
# it didn't entirely. Interesting. 
# look to see if there are "orphan" observations now
table(am.2$spl.interval)
# there are...does this matter? I don't think it does as long as the SPL 
# of the reviews I did is relatively evenly distributed

hist(am.2$SPL)

# compare this to the original intent

hist(am.2$spl.old)

# well.... its got more extreme values now if we look at ones in the new below 125

hist(am.2$SPL[am.2$SPL<=125])

# actually looks a bit better ok moving forward with matching the 5 minut intervals

fm.2<-left_join(fm.2,spl19.use)

ggplot(fm.2)+
  geom_histogram(aes(SPL))+
  facet_grid(~prd,scales="free")

# now we have some post periods with SPL over 100 should look at this more for 
# analysing the data for the "experiment" but these look like single minutes
# not the average SPL for the whole 5 minute period

# now bring in wind data
#update weather package

#devtools::install_github("ropensci/weathercan") 

wthr19<-weathercan::weather_dl(station_ids=29411, start="2019-04-10",end="2019-06-22")%>%
  select(time,yr=year,m=month,d=day,hr=hour,wind_dir,wind_spd)%>%
  separate(hr,into = c("hr","extra"),sep=":")%>%
  mutate(yr=as.double(yr),
         m=as.double(m),
         d=as.double(d),
         hr=as.double(hr))

am.2<-left_join(am.2,wthr19)
fm.2<-left_join(fm.2,wthr19)
follow.2<-left_join(follow.2,wthr19)

#Now bring in wave data
wave<-read.csv("odata/halibut_bank_wave_height.csv")%>%
  select(date=DATE,wave.ht=VWH.,wave.prd=VTP.,wave.ht2=VCAR,wave.prd2=VTPK,temp=SSTP)%>%
  separate(date,
           into=c("m","d","yr.time"),
           sep=c("/"))%>%
  separate(yr.time,
           into=c("yr","time"),
           sep=c(" "))%>%
  separate(time,
           into=c("hr","min"),
           sep=c(":"))%>%
  mutate(date=ymd_hm(paste(yr,m,d,hr,min)),
         yr=as.numeric(yr),
         m=as.numeric(m),
         d=as.numeric(d),
         hr=as.numeric(hr))%>%
  filter(yr>=2019)%>%
  select(-min)

#join to fish data
am.2<-left_join(am.2,wave)
fm.2<-left_join(fm.2,wave)
follow.2<-left_join(follow.2,wave)

# write out big ugly datasets
write_rds(am.2,"wdata/one_minute_review_allcolumns.rds")
write_rds(fm.2,"wdata/five_minute_review_allcolumns.rds")
write_rds(follow.2,"wdata/follow_up_2019_review_allcolumns.rds")

# now select down to most useful columns
am.3<-am.2%>%
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
         spl.interval,
         SPL,
         wind_dir,
         wind_spd,
         wave.ht,
         wave.prd,
         temp)

fm.3<-fm.2%>%
  select(
    datetime,yr,
    m,
    d,
    hr,
    min,
    s,
    auto.class,
    Confidence,
    man.class,
    man.type,
    spl.interval,
    SPL,
    wind_dir,
    wind_spd,
    wave.ht,
    wave.prd,
    temp)

    
follow.3<-follow.2%>%
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
         spl.interval,
         SPL,
         wind_dir,
         wind_spd,
         wave.ht,
         wave.prd,
         temp)

#write smaller datasets
write_rds(am.3,"wdata/one_minute_review_reduceddata.rds")
write_rds(fm.3,"wdata/five_minute_review_reduceddata.rds")
write_rds(follow.3,"wdata/follow_up_2019_review_reduceddata.rds")

#one minute dataset with original 1 minute reviews and followup
write_rds(bind_rows(am.3,follow.3),"wdata/one_minute_plus_followup.rds")
