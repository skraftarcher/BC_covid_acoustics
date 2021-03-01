#This script preps the data for the complete analysis

library(tidyverse)
library(lubridate)
library(readxl)

# if the number of columns changes above this needs to be updated

# ONLY NEED TO RUN THIS BIT ONCE, IF SECOND TIME START ON LINE 85
# load auto detector data
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
# going to subset down to only fish calls to make this go easier
# subset 2019 data to only data that overlaps 2020
# first create dates

all2019<-all2019%>%
  filter(Class=="FS")%>%
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
  filter(Class=="FS")%>%
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
# now get bounding days from 2020 dataset
min.day<-min(all2020$datetime)
year(min.day)<-2019
max.day<-max(all2020$datetime)
year(max.day)<-2019

sub2019<-all2019%>%
  filter(datetime>=min.day)%>%
  filter(datetime<=max.day)

#now we can bind the datasets and group by minute
bothyrs <- bind_rows(sub2019,all2020) %>%
  distinct()%>%
  group_by(yr,m,d,hr,min)%>%
  summarize(ncall=n())%>%
  distinct()

#save this
write_rds(bothyrs,"wdata/fish_calls_both_years_fulldataset.rds")


#CAN START HERE FROM NOW ON
bothyrs<-read_rds("wdata/fish_calls_both_years_fulldataset.rds")

#bring in SPL data
spl1<-read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
spl2<-read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2020") 
year(spl1$DateTime)<-2019
year(spl2$DateTime)<-2020
spl<-bind_rows(spl1,spl2)

# It looks like there are two records of SPL for the places where a file started/stopped. Now the code averages those
# investigating why we seem to lose some minutes 
spl.check<-spl%>%
  filter(yr==2019 & m==4 & d==30)

spl <- arrange(spl, DateTime) %>% # note: so far all datetime vars have different names
    mutate(
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(DateTime),
    hr2=hour(Time),
    min2=minute(DateTime),
    min=minute(Time))

spl.mismatch<-spl[spl$min!=spl$min2,]
# investigating why we seem to lose some minutes 

#It looks like there's a difference in rounding between DateTime and Time for
# minutes + seconds. Trying to see if using the minutes from Time corrects the problem

spl<-spl %>%
  group_by(yr,m,d,hr,min)%>%
  summarize(spl=mean(SPL,na.rm = TRUE))%>%
  distinct()

#join to fish call dataset
fish<-left_join(bothyrs,spl)%>%
  mutate(datetime=ymd_hm(paste(yr,m,d,hr,min)))

fish.check<-filter(fish,is.na(spl))

# using the minutes from Time rather than DateTime fixes it

# still have 850 observations without spl data
# figure this out. # all the observations missing an SPL are either at 12 or 42 minutes
# in 2019 or at 2 or 24 in the first deployment of 2020
# and at 19 minutes in the second deployment of 2020

#bring in wind
wind<-read_rds("wdata/trimmed_hourly_weather.rds")%>%
  filter(rca=="in")%>%
  select(yr=year,m=month,d=day,hr,wdir,wspeed)

#join to fish call dataset
fish<-left_join(fish,wind)

#bring in wave data
wave<-read.csv("odata/halibut_bank_wave_height.csv")%>%
  select(date=DATE,wave.ht=VWH.,wave.prd=VTP.,wave.ht2=VCAR,wave.prd2=VTPK)%>%
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
fish<-left_join(fish,wave)

#output this dataset
write_rds(fish,"wdata/all_fish_calls_with_weather.rds")
