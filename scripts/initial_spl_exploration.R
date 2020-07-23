# A project to determine how COVID-19 has impacted the soundscape around Nanaimo, BC 

# this script is just to look at the data 

#packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(readxl))install.packages("readxl");library(readxl)

theme_set(theme_bw())


## data----
in19<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2019")
in20<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_In_2020")
out19<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_Out_2019")
out20<-read_xlsx("odata/Broadband SPL RCA.xlsx",sheet = "RCA_Out_2020")

#data manipulation----
in192<-in19 %>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(Month,Day,hr)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day,"-",hr),
         grp="RCA In 2019",
         rca="in",
         year=2019)

out192<-out19 %>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(Month,Day,hr)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day,"-",hr),
         grp="RCA Out 2019",
         rca="out",
         year=2019)

in202<-in20 %>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(Month,Day,hr)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day,"-",hr),
         grp="RCA In 2020",
         rca="in",
         year=2020)

out202<-out20 %>%
  separate(Time,into=c("hr","min","sec"),sep=":")%>%
  separate(hr,into=c("wste","hr"),sep=" ",remove=TRUE)%>%
  select(-wste)%>%
  group_by(Month,Day,hr)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  mutate(mdh=paste(Month,"-",Day,"-",hr),
         grp="RCA Out 2020",
         rca="out",
         year=2020)
splhr<-bind_rows(in192,in202,out192,out202)

# grouped by hour----
ggplot(splhr)+
  geom_line(aes(y=rmsspl,x=mdh,color=as.factor(year),group=grp))+
  facet_wrap(~rca)

#data manipulation2----
in19d<-in19 %>%
  group_by(Month,Day)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  pivot_longer(spl50:spl99,names_to = "measure",values_to = "spd")%>%
  mutate(mdh=paste(Month,"-",Day),
         grp="RCA In 2019",
         rca="in",
         year=2019)

out19d<-out19 %>%
  group_by(Month,Day)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  pivot_longer(spl50:spl99,names_to = "measure",values_to = "spd")%>%
  mutate(mdh=paste(Month,"-",Day),
         grp="RCA Out 2019",
         rca="out",
         year=2019)

in20d<-in20 %>%
  group_by(Month,Day)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  pivot_longer(spl50:spl99,names_to = "measure",values_to = "spd")%>%
  mutate(mdh=paste(Month,"-",Day),
         grp="RCA In 2020",
         rca="in",
         year=2020)

out20d<-out20 %>%
  group_by(Month,Day)%>%
  summarize(spl50=median(SPL),
            rmsspl=sqrt(mean(SPL^2)),
            spl01=quantile(SPL,.01),
            spl05=quantile(SPL,.05),
            spl95=quantile(SPL,.95),
            spl99=quantile(SPL,.99))%>%
  pivot_longer(spl50:spl99,names_to = "measure",values_to = "spd")%>%
  mutate(mdh=paste(Month,"-",Day),
         grp="RCA Out 2020",
         rca="out",
         year=2020)

d20<-in20d%>%
  ungroup()%>%
  select(mdh)%>%
  distinct()
spld<-bind_rows(in19d,in20d,out19d,out20d)%>%
  filter(mdh %in% d20$mdh)
# grouped by day----
ggplot(spld)+
  geom_line(aes(y=spd,x=mdh,color=as.factor(year),group=grp))+
  facet_grid(measure~rca,scales="free")
