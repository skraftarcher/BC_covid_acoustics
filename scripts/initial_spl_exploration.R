# A project to determine how COVID-19 has impacted the soundscape around Nanaimo, BC 

# this script is just to look at the data 

#packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(lubridate))install.packages("lubridate");library(lubridate)

theme_set(theme_bw())


## data----
in19<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2019")
in20<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2020")
out19<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_Out_2019")
out20<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_Out_2020")

#data manipulation----
# add in identifiers that allow us to combine the datasets
in192<-in19 %>%
  mutate(grp="RCA In 2019",
         rca="in",
         year=2019)

out192<-out19 %>%
  mutate(grp="RCA Out 2019",
         rca="out",
         year=2019)

in202<-in20 %>%
  mutate(grp="RCA In 2020",
         rca="in",
         year=2020)

out202<-out20 %>%
  mutate(grp="RCA Out 2020",
         rca="out",
         year=2020)

# create a dataset that is aggregated by the hour, 
# several variables are calculated, 1,5,50,95, and 99 quantiles as well as the root mean square error


splhr<-bind_rows(in192,in202,out192,out202)%>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(year,rca,grp,Month,Day,hr)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day,"-",hr))%>%
  arrange(mdh)
write.csv(splhr,"wdata/splbyhour.csv")

# grouped by hour----
ggplot(splhr)+
  geom_line(aes(y=rmsspl,x=mdh,color=as.factor(year),group=grp))+
  facet_wrap(~rca)

# create a dataset that is aggregated by day, same variables are calculated 


spld<-bind_rows(in192,in202,out192,out202)%>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(year,rca,grp,Month,Day)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day))%>%
  arrange(mdh)

write.csv(bind_rows(in19d,in20d,out19d,out20d),"wdata/splbyday.csv")

# grouped by day----
ggplot(spld)+
  geom_line(aes(y=spl50,x=mdh,color=as.factor(year),group=grp))+
  facet_grid(~rca,scales="free")

# subset down to only time periods recorded in 2020
# create datasets that only include the time periods where there is data in 2020 then subset the
# larger datasets down to those time periods

d20<-spld%>%
  ungroup()%>%
  filter(year==2020)%>%
  select(mdh)%>%
  distinct()


hr20<-splhr%>%
  ungroup()%>%
  filter(year==2020)%>%
  select(mdh)%>%
  distinct()

spld2<-spld%>%
  filter(mdh %in% d20$mdh)

splhr2<-splhr%>%
  filter(mdh %in% hr20$mdh)


# plotting only data in both data sets

ggplot(spld2)+
  geom_line(aes(y=spl50,x=mdh,color=as.factor(year),group=grp),size=2)+
  facet_grid(~rca,scales="free")


ggplot(splhr2)+
  geom_line(aes(y=spl50,x=mdh,color=as.factor(year),group=grp))+
  facet_grid(~rca,scales="free")

# get additional weather data
devtools::install_github("ropensci/weathercan")

wthr19<-weathercan::weather_dl(station_ids=29411, start="2019-04-18",end="2020-06-22")
wthr20<-weathercan::weather_dl(station_ids=29411, start="2020-04-18",end="2020-06-22")   
wthr<-bind_rows(wthr19,wthr20)%>%
  select(date,year,Month=month,Day=day,wind_dir,wind_spd)%>%
  mutate(year=as.numeric(year),
         Month=as.numeric(Month),
         Day=as.numeric(Day))%>%
  group_by(date,year,Month,Day)%>%
  summarize(wd=mean(wind_dir,na.rm = TRUE),ws=mean(wind_spd,na.rm=TRUE))%>%
  left_join(spld)%>%
  filter(!is.na(rca))%>%
  filter(mdh %in% d20$mdh)%>%
  ungroup()%>%
  distinct()

wthr1.19<-wthr %>%
  filter(date<as.Date("2019-05-05")&date>as.Date("2019-04-20"))%>%
  mutate(tp="early")%>%
  arrange(date)%>%
  mutate(d2=seq_len(n()))
wthr1.20<-wthr %>%
  filter(date<as.Date("2020-05-05")&date>as.Date("2020-04-20"))%>%
  mutate(tp="early")%>%
  arrange(date)%>%
  mutate(d2=seq_len(n()))
wthr2.19<-wthr %>%
  filter(date>as.Date("2019-05-26")&date<as.Date("2019-06-18"))%>%
  mutate(tp="late")%>%
  arrange(date)%>%
  mutate(d2=seq_len(n()))
wthr2.20<-wthr %>%
  filter(date>as.Date("2020-05-26")&date<as.Date("2020-06-18"))%>%
  mutate(tp="late")%>%
  arrange(date)%>%
  mutate(d2=seq_len(n()))


wthr2<-bind_rows(wthr1.19,wthr1.20,wthr2.19,wthr2.20)

write.csv(wthr2,"wdata/trimmed_with_weather.csv")
