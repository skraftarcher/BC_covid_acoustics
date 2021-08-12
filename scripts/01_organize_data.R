#updated script to organize data for pre-covid, post-covid 
# fish calling behavior paper

source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="Rraven")
lp(pck="lubridate")
lp(pck="readxl")

# First organize spl data

spl19<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2019")
spl20<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2020")

spl192<-spl19%>%
  mutate(hr=hour(Time),
         mins=minute(Time),
         sec=second(Time),
         SPL.start.time=ymd_hms(paste(Year,Month,Day,hr,mins,sec)))%>%
  select(SPL.start.time,SPL)%>%
  arrange(SPL.start.time)%>%
  mutate(SPL.Int=as.numeric(row.names(.)))

spl202<-spl20%>%
  mutate(hr=hour(Time),
         mins=minute(Time),
         sec=second(Time),
         SPL.start.time=ymd_hms(paste(Year,Month,Day,hr,mins,sec)))%>%
  select(SPL.start.time,SPL)%>%
  arrange(SPL.start.time)%>%
  mutate(SPL.Int=as.numeric(row.names(.)))


# Next bring in all detector results, link to SPL and save

fish.calls.19<-imp_raven(path="selection.tables/RCA_in_April_July2019_1342218252_updated",
                         all.data = TRUE )

fish.calls.20<-bind_rows(imp_raven(path="selection.tables/RCA_In_200418_1205_5047",
                         all.data = TRUE ),
                        imp_raven(path="selection.tables/RCA_In_200524_1149_5042",
                                  all.data = TRUE ))

# now organize that data so that it is fish calls per minute 
# and save it

fc19<-fish.calls.19%>%
  separate(`Begin File`,
           into = c("st","yr","mnth","d","hr","mins","sec","ext"),
           sep=c(-16,-14,-12,-10,-8,-6,-4),
           remove=FALSE)%>%
  mutate(file.datetime=ymd_hms(paste(yr,mnth,d,hr,mins,sec)),
         call.datetime=file.datetime+`File Offset (s)`,
         autoc=ifelse(Class=="FS",1,0),
         SPL.Int=findInterval(call.datetime,spl192$SPL.start.time))%>%
  group_by(SPL.Int,`Begin File`)%>%
  summarize(fish.calls=sum(autoc))%>%
  left_join(spl192)%>%
  mutate(yr=year(SPL.start.time),
         mnth=month(SPL.start.time),
         d=day(SPL.start.time),
         hr=hour(SPL.start.time),
         mins=minute(SPL.start.time))

write_rds(fc19,"wdata/detector_fishcalls_2019.rds")  

fc20<-fish.calls.20%>%
  separate(`Begin File`,
           into = c("st","yr","mnth","d","hr","mins","sec","ext"),
           sep=c(-16,-14,-12,-10,-8,-6,-4),
           remove=FALSE)%>%
  mutate(file.datetime=ymd_hms(paste(yr,mnth,d,hr,mins,sec)),
         call.datetime=file.datetime+`File Offset (s)`,
         autoc=ifelse(Class=="FS",1,0),
         SPL.Int=findInterval(call.datetime,spl202$SPL.start.time))%>%
  group_by(SPL.Int,`Begin File`)%>%
  summarize(fish.calls=sum(autoc))%>%
  left_join(spl202)%>%
  mutate(yr=year(SPL.start.time),
         mnth=month(SPL.start.time),
         d=day(SPL.start.time),
         hr=hour(SPL.start.time),
         mins=minute(SPL.start.time))

write_rds(fc20,"wdata/detector_fishcalls_2020.rds")

# collect and organize weather data

if (!require(weathercan)) devtools::install_github("ropensci/weathercan")

wthr19 <- weathercan::weather_dl(station_ids = 29411, start = "2019-04-14", end = "2019-06-25")
wthr20 <- weathercan::weather_dl(station_ids = 29411, start = "2020-04-19", end = "2020-06-20")

wthr192<-wthr19%>%
  select(yr=year,mnth=month,d=day,hr=hour,wind_spd,wind_dir,precip_amt)%>%
  separate(hr,into=c("hr","ext"),sep=-3)%>%
  select(-ext)%>%
  mutate(yr=as.numeric(yr),
         mnth=as.numeric(mnth),
         d=as.numeric(d),
         hr=as.numeric(hr))

wthr202<-wthr20%>%
  select(yr=year,mnth=month,d=day,hr=hour,wind_spd,wind_dir,precip_amt)%>%
  separate(hr,into=c("hr","ext"),sep=-3)%>%
  select(-ext)%>%
  mutate(yr=as.numeric(yr),
         mnth=as.numeric(mnth),
         d=as.numeric(d),
         hr=as.numeric(hr))

write_rds(wthr192,"wdata/weather_2019.rds")
write_rds(wthr202,"wdata/weather_2020.rds")

# collect and organize tide data

# pull in downloaded tide data from
# https://www.pac.dfo-mpo.gc.ca/science/charts-cartes/obs-app/observed-eng.aspx?StationID=08545
filelist <-list.files("odata/nanaimo-tides/", pattern = "*.csv")

for(i in 1:length(filelist)){
  temp2<-read_csv(paste0("odata/nanaimo-tides/",filelist[i])) 
  if(i==1) tides<-temp2
  if(i!=1) tides<-dplyr::bind_rows(tides,temp2)
}

tides$datetime <- tides[[2]] - hours(1)

tides <- select(tides, datetime, tide=RADAR) %>% 
  mutate(
    yr = year(datetime),
    mnth = month(datetime),
    d = day(datetime),
    hr = hour(datetime),
    mins=minute(datetime)) %>% select(-datetime)%>%
  distinct()

# write out tide data
write_rds(tides,"wdata/tides.rds")

#Now organize wave data
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
  mutate(yr=as.numeric(yr),
         mnth=as.numeric(m),
         d=as.numeric(d),
         hr=as.numeric(hr),
         mins=as.numeric(min))%>%
  filter(yr>=2019)%>%
  select(-min,-m)

write_rds(wave,"wdata/waves.rds")

# organize boat detector data
boat.19<-read.csv("boatnoise_detector/RCA-In_April_July2019_1342218252__20190410_20190625__AOutput.csv")
boat.20<-bind_rows(read.csv("boatnoise_detector/RCA_in_2020_RCAin_200418_1505_5047__20200418_20200504__AOutput.csv"),
                   read.csv("boatnoise_detector/RCA_in_2020_RCAin_200524_1149_5042__20200524_20200622__AOutput.csv"))

boat.19<-boat.19%>%
  mutate(yr=year(Time),
         mnth=month(Time),
         d=day(Time),
         hr=hour(Time),
         mins=minute(Time),
         ship.pa=ifelse(shipping.detection.flag==0,0,1),
         ship.close=case_when(
           shipping.detection.flag==0~"no.ship",
           shipping.detection.flag==4~"approaching",
           shipping.detection.flag==1~"close"))%>%
  select(-shipping.detection.flag,-Time)

boat.20<-boat.20%>%
  mutate(yr=year(Time),
         mnth=month(Time),
         d=day(Time),
         hr=hour(Time),
         mins=minute(Time),
         ship.pa=ifelse(shipping.detection.flag==0,0,1),
         ship.close=case_when(
           shipping.detection.flag==0~"no.ship",
           shipping.detection.flag==4~"approaching",
           shipping.detection.flag==1~"close"))%>%
  select(-shipping.detection.flag,-Time)
# make a combined dataset and write it out

alldata2019<-left_join(fc19,wthr192)%>%
  left_join(tides)%>%
  left_join(wave)%>%
  left_join(boat.19)

write_rds(alldata2019,"wdata/alldata2019.rds")

alldata2020<-left_join(fc20,wthr202)%>%
  left_join(tides)%>%
  left_join(wave)%>%
  left_join(boat.20)

write_rds(alldata2020,"wdata/alldata2020.rds")
