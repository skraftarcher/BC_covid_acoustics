#This script preps the data for the complete analysis

library(tidyverse)
library(lubridate)
library(readxl)

# if the number of columns changes above this needs to be updated

# ONLY NEED TO RUN THIS BIT ONCE, IF SECOND TIME START ON LINE 95
# # load auto detector data
# all2019 <- Rraven::imp_raven(path = "w.selection.tables/",
#                              files = "Autodetect_Updated_April_July2019_Jan2021.txt",
#                              all.data = TRUE)[,-19]
# 
# new.files2<-Rraven::imp_raven(path = "w.selection.tables/",
#                               files = "Autodetect_RCA_In_200418_1205_5047_Jan2021.txt",
#                               all.data = TRUE)[,-19]
# 
# new.files3<-Rraven::imp_raven(path = "w.selection.tables/",
#                               files = "Autodetect_RCA_In_200524_1149_5042_Jan2021.txt",
#                               all.data = TRUE)[,-19]
# 
# # create a date time variable in each detection dataset
# #all2019 <- new.files
# 
# all2019 <-separate(all2019, stfile,
#                    into = c("st","yr","m","d","hr","min","s","ext"), 
#                    sep = c(11,13,15,17,19,21,23), remove=FALSE, convert = TRUE)
# 
# first2020 <- separate(new.files2, stfile, 
#                       into = c("st","yr","m","d","hr","min","s","ext"), 
#                       sep = c(5,7,9,11,13,15,17), remove=FALSE, convert = TRUE)
# 
# second2020 <- separate(new.files3, stfile, 
#                        into = c("st","yr","m","d","hr","min","s","ext"), 
#                        sep = c(5,7,9,11,13,15,17), remove=FALSE, convert = TRUE)
# 
# # # going to subset down to only fish calls to make this go easier
# # subset 2019 data to only data that overlaps 2020
# # first create datesets
# 
# all2019<-all2019%>%
#   mutate(yr=paste0(20,yr),
#          datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
#          datetime=datetime+into.file,
#          yr=year(datetime),
#          m=month(datetime),
#          d=day(datetime),
#          hr=hour(datetime),
#          min=minute(datetime),
#          s=second(datetime))%>%
#   arrange(datetime) 
# 
# all2019f<-all2019%>%filter(Class=="FS")
# 
# 
# first2020<-first2020%>%
#   mutate(yr=paste0(20,yr),
#          datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
#          datetime=datetime+into.file,
#          yr=year(datetime),
#          m=month(datetime),
#          d=day(datetime),
#          hr=hour(datetime),
#          min=minute(datetime),
#          s=second(datetime))%>%
#   arrange(datetime)
# 
# first2020f<-first2020%>%filter(Class=="FS")
#   
# 
# second2020<-second2020%>%
#   mutate(yr=paste0(20,yr),
#          datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
#          datetime=datetime+into.file,
#          yr=year(datetime),
#          m=month(datetime),
#          d=day(datetime),
#          hr=hour(datetime),
#          min=minute(datetime),
#          s=second(datetime))%>%
#   arrange(datetime)
# 
# second2020f<-second2020%>%filter(Class=="FS")
# 
# write_rds(all2019,"wdata/all_selections_spring2019.rds")
# write_rds(first2020,"wdata/all_selections_first2020.rds")
# write_rds(second2020,"wdata/all_selections_second2020.rds")
# 
# write_rds(all2019f,"wdata/fish_calls_spring2019.rds")
# write_rds(first2020f,"wdata/fish_calls_first2020.rds")
# write_rds(second2020f,"wdata/fish_calls_second2020.rds")
# 
# 
# RERUN STARTING HERE

# all2019a <- read_rds("wdata/all_selections_spring2019.rds")
# first2020a <- read_rds("wdata/all_selections_first2020.rds")

all2019 <- read_rds("wdata/fish_calls_spring2019.rds")
first2020 <- read_rds("wdata/fish_calls_first2020.rds")
second2020 <- read_rds("wdata/fish_calls_second2020.rds")


# bring in SPL data

spl19<-read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
spl20<-read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2020") 
year(spl19$DateTime)<-2019
year(spl20$DateTime)<-2020

#create the date + time that we trust for the spl data
spl19.use<-spl19%>%
  mutate(
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(Time),
    min=minute(Time),
    sec=second(Time),
    DateTime2=ymd_hms(paste(yr,m,d,hr,min,sec)))%>%
  select(DateTime2,SPL)%>%
  arrange(DateTime2)

# remove row with faulty datetime due to truncation in file 1342218252.190610220207.wav
spl19.use <- spl19.use[!(is.na(spl19.use$DateTime2)),]

spl20.use<-spl20%>%
  mutate(
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(Time),
    min=minute(Time),
    sec=second(Time),
    DateTime2=ymd_hms(paste(yr,m,d,hr,min,sec)))%>%
  select(DateTime2,SPL)%>%
  arrange(DateTime2)

# split spl data into separate deployments and exclude first 6 hrs post deployment to make eliminate impact of disturbance 
# need to use spl data to determine start times because fish data starts with first fish call...

trimmed_start19 <- min(spl19.use$DateTime2) + hours(6)
# trimmed_end19 <- max(spl19.use$DateTime2) - hours(6)


trimmed_start20 <- min(spl20.use$DateTime2) + hours(6)

spl2019.1 <- filter(spl19.use, 
  DateTime2 > trimmed_start19 #& DateTime2 < trimmed_end19
  )%>%
  mutate(spl.interval=row_number())

# fish data can be used to identify end of first deployment in 2020
end1 <- max(first2020$datetime)

spl2020.1 <- filter(spl20.use, 
  DateTime2 > trimmed_start20 & DateTime2 <= end1 
  )%>%
  mutate(spl.interval=row_number())

spl2020.2 <- filter(spl20.use, DateTime2 > end1)

trimmed_start20b <- min(spl2020.2$DateTime2) + hours(6)

spl2020.2<- filter(spl2020.2,
  DateTime2 > trimmed_start20b
  )%>%
  mutate(spl.interval=row_number())


#find intervals for matching fish calls to spl data
fish.2019<-all2019%>%
  select(Class,Confidence,datetime) %>% 
  filter(datetime > trimmed_start19)
  

fish.2019$spl.interval<-findInterval(fish.2019$datetime,spl2019.1$DateTime2)

fish.first2020<-first2020%>%
  select(Class,Confidence,datetime)%>% 
  filter(datetime > trimmed_start20)

fish.first2020$spl.interval<-findInterval(fish.first2020$datetime,spl2020.1$DateTime2)

fish.second2020<-second2020%>%
  select(Class,Confidence,datetime)%>% 
  filter(datetime > trimmed_start20b)

fish.second2020$spl.interval<-findInterval(fish.second2020$datetime,spl2020.2$DateTime2)


# creating the full dataset
# first we have to group by intervals then remove intervals
fish.19.spl<-left_join(fish.2019, spl2019.1)

fish.20.1.spl<-left_join(fish.first2020, spl2020.1)

fish.20.2.spl<-left_join(fish.second2020, spl2020.2)


# check the match for NAs
summary(is.na(fish.19.spl$SPL))
summary(is.na(fish.20.1.spl$SPL))
summary(is.na(fish.20.2.spl$SPL))

# no NAs so now checking for general match in date + times
# plot(fish.19.spl$datetime[1:1000]~fish.19.spl$DateTime2[1:1000])
# plot(fish.19.spl$datetime[10000:11000]~fish.19.spl$DateTime2[10000:11000])
# 
# # looks like 2019 matches well
# 
# plot(fish.20.1.spl$datetime~fish.20.1.spl$DateTime2)
# # so does  the first 2020 period
# 
# plot(fish.20.2.spl$datetime~fish.20.2.spl$DateTime2)
# 
# # as does the second period of 2020

# it looks like this worked


# writing the spl linked datasets
write_rds(fish.19.spl,"wdata/fish_spl_2019.rds")
write_rds(fish.20.1.spl,"wdata/fish_spl_2020_1.rds")
write_rds(fish.20.2.spl,"wdata/fish_spl_2020_2.rds")


fish.19.g<-fish.19.spl %>%
  group_by(spl.interval)%>%
  summarize(ncall=n(),spl=SPL,DateTime=DateTime2)%>%
  ungroup()%>%
  distinct()#%>%
  # select(-spl.interval)

fish.19.g <- left_join(spl2019.1, fish.19.g) %>% mutate(deployment = "1")
 

fish.20.1.g<-fish.20.1.spl %>%
  group_by(spl.interval)%>%
  summarize(ncall=n(),spl=SPL,DateTime=DateTime2)%>%
  ungroup()%>%
  distinct()#%>%
  # select(-spl.interval)

fish.20.1.g <- left_join(spl2020.1, fish.20.1.g) %>% mutate(deployment = "2")


fish.20.2.g<-fish.20.2.spl %>%
  group_by(spl.interval)%>%
  summarize(ncall=n(),spl=SPL,DateTime=DateTime2)%>%
  ungroup()%>%
  distinct()#%>%
  # select(-spl.interval)

fish.20.2.g <- left_join(spl2020.2, fish.20.2.g) %>% mutate(deployment = "3")

fish<-bind_rows(fish.19.g,fish.20.1.g,fish.20.2.g) %>%
  select(#spl.interval, 
    DateTime = DateTime2, deployment, spl.interval, spl = SPL, ncall) %>%
  mutate(yr=year(DateTime),
         m=month(DateTime),
         d=day(DateTime),
         hr=hour(DateTime),
         min=minute(DateTime),
         sec=second(DateTime))

fish[is.na(fish$ncall),]$ncall <- 0

write_rds(fish,"wdata/all_fish_calls_with_spl.rds")


# now moving on to adding in everything else

# get weather data

# note: may require updated version of tidyselect package and a reinstall of weathercan
# if(!require(weathercan)) devtools::install_github("ropensci/weathercan")
# 
# wthr19<-weathercan::weather_dl(station_ids=29411, start="2019-04-10",end="2019-06-25")
# wthr20<-weathercan::weather_dl(station_ids=29411, start="2020-04-10",end="2020-06-22")  
# 
# # all weather by hr 
# all_wthr <- bind_rows(wthr19, wthr20) %>%
#   mutate(time = hour) %>%
#   separate(hour, into = c("hr", "min"), sep = ":") %>%
#   select(date, 
#     yr = year,
#     m = month,
#     d = day,
#     hr, time,    
#     wdir = wind_dir,
#     wspeed = wind_spd
#   ) %>%
#   mutate(
#     yr = as.numeric(yr),
#     m = as.numeric(m),
#     d = as.numeric(d),
#     hr = as.numeric(hr)
#   ) 
# write_rds(all_wthr, "wdata/all_hourly_weather.rds")

wind<-read_rds("wdata/all_hourly_weather.rds")%>%
  select(yr,m,d,hr,wdir,wspeed)

#join to fish call dataset
fish<-left_join(fish,wind)

# bring in wave data from Halibut Bank station C46146
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
fish<-left_join(fish,wave)

#output this dataset
write_rds(fish,"wdata/all_fish_calls_with_weather.rds")
