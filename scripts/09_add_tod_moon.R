#script to add in tod variable to dataset

source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="suncalc")
lp(pck="lubridate")


# load data
all19<-read_rds("wdata/alldata2019.rds")
all20<-read_rds("wdata/alldata2020.rds")

#organize data

all<-bind_rows(all19,all20)%>%
  mutate(fish.pa=ifelse(fish.calls<=1,0,1),
         SPL.start.time=force_tz(SPL.start.time,tz="Canada/Pacific"),
         #date1=ymd(paste(yr,mnth,d)),
         time.utc=with_tz(SPL.start.time,tz="UTC"))%>%
  filter(!is.na(ship.close))

all$date<-ymd(paste(all$yr,all$mnth,all$d))

date<-c(unique(all$date),all$date[nrow(all)]+1)

moon.phase<-getMoonIllumination(date = all$time.utc, keep = c("fraction"))%>%
  distinct()

timeofday<-getSunlightTimes(date = date, lat = 49.21445, lon = -123.892233,
                           keep = c( "dawn","goldenHourEnd","goldenHour","dusk"), tz = "UTC")%>%
  distinct()

moon.times<-getMoonTimes(date = date, lat = 49.21445, lon = -123.892233,
                         inUTC=FALSE,
                         tz="Canada/Pacific",
                         keep=c("rise","set"))%>%
  distinct()

moon.times<-moon.times%>%
  mutate(moon.up=case_when(
    set<rise~interval(rise,lead(set)),
    set>rise~interval(rise,set),
    is.na(rise)~interval(lag(rise),set),
    is.na(set)~interval(rise,lead(set))),
    moon.down=case_when(
      set<rise~interval(set,rise),
      set>rise~interval(set,lead(rise)),
      is.na(rise)~interval(set,lead(rise)),
      is.na(set)~interval(lag(set),rise)))

moon.up<-unique(moon.times$moon.up)
moon.down<-unique(moon.times$moon.down)


moon.phase<-moon.phase%>%
  mutate(SPL.start.time=with_tz(date,tz="Canada/Pacific"))%>%
  select(-date)

timeofday<-timeofday%>%
  mutate(across(dawn:dusk,~with_tz(.x,tzone="Canada/Pacific")),
         morning.golden.int=interval(dawn+1,goldenHourEnd),
         day.int=interval(goldenHourEnd+1,goldenHour),
         evening.golden.int=interval(goldenHour+1,dusk),
         night.int=interval(dusk+1,lead(dawn)),
         night.int2=lag(night.int))%>%
  select(-lat:-dusk)

all2<-all
all2$moon<-NA

for(i in 1:nrow(all2)){
    t1<-length(which(all2$SPL.start.time[i] %within% moon.down))
    t2<-length(which(all2$SPL.start.time[i] %within% moon.up))
    all2$moon[i]<-case_when(t1>0~"down",
                            t2>0~"up")
}

all3<-all2%>%
  left_join(timeofday)%>%
  left_join(moon.phase)%>%
  mutate(morning.golden=ifelse(SPL.start.time %within% morning.golden.int,1,0),
         day=ifelse(SPL.start.time %within% day.int,1,0),
         evening.golden=ifelse(SPL.start.time %within% evening.golden.int,1,0),
         night=case_when(
           SPL.start.time %within% night.int~1,
           SPL.start.time %within% night.int2~1,
           !SPL.start.time%within% night.int~0,
           !SPL.start.time %within% night.int2~0),
         night.light=case_when(
           night==1 & moon=="up"~fraction,
           night==0 ~0,
           night==1 & moon=="down"~0),
         night.dark=case_when(
           night==1 & moon=="up" & fraction==0~1,
           night==1 & moon=="down" ~1,
           night==1 & moon=="up" & fraction!=0~0,
           night==0~0))%>%
  select(-morning.golden.int:-night.int)


all3$doy<-yday(all3$date)

# write this out so we can use this dataset for analysis
write_rds(all3,"wdata/alldata_bothyears_withtod.rds")
