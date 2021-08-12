# script to extract boat passages to examine further
library(tidyverse)
library(lubridate)
library(readxl)
if(!require(filesstrings))install.packages("filesstrings");library(filesstrings)


### taking midnight ferry approach----
# Bring in data ----
mv19<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2019_RCA_In")#2019 ais data
mv20<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2020_RCA_In")#2020 ais data
spl<-read_rds("wdata/spl_by_min.rds") %>%
  filter(rca=="in")# minute by minute spl data
# organize data ----
year(spl$DateTime)<-spl$Year #year in the date is messed up, fix that

mv<-bind_rows(mv19,mv20)%>%
  arrange (DateTime)%>% # make sure dataset goes from earliest to latest
  mutate(st=DateTime-minutes(60),et=DateTime+minutes(60))%>% # create the window to match to
  pivot_longer(st:et,names_to="tt",values_to="tint")%>% # turn it in to a long data format so we 
  mutate(inter=row_number()) # give each interval a number- using the find interval it assigns ID based on the row # of the first line in the
# interval i.e., the interval between rows 1 and 2 is interval 1

spl$inter<-findInterval(spl$DateTime,mv$tint)#assign intervals. 
# I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure 
mv2<-mv%>%
  select(-tint)%>%
  pivot_wider(names_from = tt,values_from=inter)%>%
  select(-et)%>%
  rename(inter=st,dt=DateTime) # we have 145 candidate intervals (less actually because not all have spl data)

#bring in wind data ----
wthr<-read_rds("wdata/trimmed_hourly_weather.rds")%>%
  select(wspeed,rca,datehr)%>%
  filter(rca=="in")%>%
  select(-rca)%>%
  arrange(datehr)

# Need to subset down to dates earlier than May 4th, 2020
spl3<-spl%>%
  filter(inter %in% mv2$inter)%>% # filter down only to intervals within 30 mins of the ferry passing (30 before or after)
  left_join(mv2)%>% # join the ferry passage info
  mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  group_by(inter)%>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2=mean(wspeed,na.rm = TRUE))%>% # create a mean wind value for the interval
  filter(!is.na(wsp2) & wsp2 <20 & DateTime < "2020-05-05")%>% # subset down to intervals where there's not much wind before 5/5/20
  mutate(dt2=dt-minutes(7),
         tperiods=case_when(
           DateTime<dt2~"pre",
           DateTime>dt~"post",
           DateTime>dt2&DateTime<dt~"oth"))%>% # create periods before the closest passage of the ferry and after
  group_by(inter,tperiods)%>% 
  mutate(
    ferry.int=interval(dt-minutes(6),dt-minutes(1)),# create a ferry interval that is the 5 minutes before the closest passage
    ferry.prd=if_else(DateTime %within% ferry.int,1,0), #create a new interval that indicates whether or not the minute is within the ferry interval
    pre.pst=case_when(
    tperiods=="pre" & ferry.prd!=1 & # find 5 minute periods in the period >5 min before closest passage 
        zoo::rollmax(SPL,k=5,fill=NA,align="left")<100~1, # where the maximum spl is < 100 
    tperiods=="pre"& zoo::rollmax(SPL,k=5,fill=NA,align="left")>100~0, 
    tperiods=="pre"& is.na(zoo::rollmax(SPL,k=5,fill=NA,align="left"))~0,
    tperiods=="pre"& ferry.prd==1~0,  
    tperiods=="post"~0,
    tperiods=="oth"~0),
    post.pst=case_when(
      tperiods=="post"& zoo::rollmax(SPL,k=5,fill=NA,align="right")<100~1,# find 5 minute periods in the post period where the maximum spl is < 100 
      tperiods=="post"& zoo::rollmax(SPL,k=5,fill=NA,align="right")>100~0,
      tperiods=="post"& is.na(zoo::rollmax(SPL,k=5,fill=NA,align="right"))~0,
      tperiods=="pre"~0,
      tperiods=="oth"~0),
    pt.int=ifelse(pre.pst==1|post.pst==1,1,0)# create a new variable that indicates whether or not the minute is part of a potential interval
  )%>% 
  group_by(inter,tperiods,pt.int)%>% 
  mutate(pre.pst=ifelse(pre.pst==1& int_start(ferry.int)-DateTime==min(int_start(ferry.int)-DateTime),1,0),# find the minute that starts the closest interval to the ferry passage where max spl < 100 in the pre-period
         post.pst=ifelse(post.pst==1& DateTime-dt==min(DateTime-dt),1,0))%>% # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the post-period
  ungroup(tperiods,pt.int)%>%
  mutate(keep.inter=ifelse(sum(pre.pst)!=0 & sum(post.pst)!=0,1,0))%>% # only keep intervals where there is a qualifying period in both pre and post ferry periods
  filter(keep.inter==1)%>% 
  group_by(inter,tperiods)

spl3a<-spl3 %>% 
  ungroup()%>%
  filter(ferry.prd==1)%>%
  group_by(inter, Year, Month, Day)%>%
  summarize(ferryspl=max(SPL))%>%
  filter(ferryspl>105)# find intervals where the ferry passage interval has a mean spl over 105

spl3<-filter(spl3,inter %in%spl3a$inter) # subset down to those intervals

# calculate the pre-ferry period to analyze
spl4<-spl3%>%
  filter(pre.pst==1)%>%
  ungroup()%>%
  select(inter,pre.dt=DateTime)%>%
  mutate(pre.int=interval(pre.dt,pre.dt+minutes(5)))
# calculate the post-ferry period to analyze
spl5<-spl3%>%
  filter(post.pst==1)%>%
  ungroup()%>%
  select(inter,post.dt=DateTime)%>%
  mutate(post.int=interval(post.dt-minutes(5),post.dt))

spl3<-spl3%>%
  ungroup()%>%
  left_join(spl4)%>%
  left_join((spl5))%>%
  mutate(pre.prd=if_else(DateTime %within% pre.int,1,0),# assign a 1 to minutes within the pre period
         post.prd=if_else(DateTime %within% post.int,1,0),# assign a 1 to minutes within the post period
         prd=case_when(
           pre.prd==1~"pre",
           post.prd==1~"post",
           ferry.prd==1~"ferry",
           pre.prd==0 & post.prd==0 & ferry.prd ==0~"none"))# create a variable that indicates which period the minute belongs to
# make figures to examine the spl profile of the candidate periods
interlist2<-unique(spl3$inter)
for(i in 1:length(interlist2)){
  p1<-spl3%>%
    filter(inter==interlist2[i])
  ggplot(data=p1)+
    geom_line(aes(y=SPL,x=DateTime,group=inter))+
    geom_line(aes(y=88,x=DateTime,group=inter,color=prd),size=1.5)+
    scale_color_manual(values=c("red","white","orange","orange"),name="Potential interval")+
    theme_bw()+
    geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
    scale_x_datetime(date_minor_breaks="5 mins")+
    theme(legend.position="none")
  ggsave(paste0("manual_figures/fig_",interlist2[i],"_",p1$Year[i],p1$Month[i],p1$Day[i],"_withperiods.jpg"))
}

# going to look at/listen to these potential ones. 
# First have to link to file created by 03-line-st-spl script 
spl_data <-read_rds("wdata/spl_file.rds")

spl.inters<-spl3%>%
  filter(prd!="none")

# create a dataset of sound trap files to use
ftu <- spl_data%>%
  filter(DateTime %in% spl.inters$DateTime)%>% 
  dplyr::select(stfile,DateTime,Year)%>%
  distinct()%>%
  left_join(spl.inters)%>%
  dplyr::select(stfile,DateTime,inter,Year,Deployment,pre.int,ferry.int,post.int)%>%
  distinct()%>%
   mutate(prd=case_when(
     DateTime %within% pre.int~"pre",
     DateTime %within% post.int~"post",
     DateTime %within% ferry.int~"ferry"),
     strt=case_when(
       prd=="pre"~int_start(pre.int),
       prd=="ferry"~int_start(ferry.int),
       prd=="post"~int_start(post.int)))%>%
  select(-pre.int,-ferry.int,-post.int)
  
ftu0 <- filter(ftu, Deployment==19.1)%>%
  group_by(stfile,inter,prd)%>%
  filter(DateTime==min(DateTime))%>%
  # select(stfile,inter,prd,strt)%>%
  separate(stfile,into=c("st","y","m","d","h","min","s","e"),
                           sep=c(11,13,15,17,19,21,23),
                           remove=FALSE)%>%
  mutate(y=paste0("20",y),
         strt.file=ymd_hms(paste(y,m,d,h,min,s)),
         into.file=strt-strt.file)

# I don't recall where Deployment==19.1 came from, but 0 makes more sense to me as these were all fixed stations in 2019
# its because you had RCA In and RCA out as 19.1 and 19.2 
ftu0$Deployment <- 0

ftu1 <- filter(ftu, Deployment==1)%>%
  group_by(stfile,inter,prd)%>%
  filter(DateTime==min(DateTime))%>%
  # select(stfile,inter,prd,strt)%>%
  separate(stfile,into=c("st","y","m","d","h","min","s","e"),
    sep=c(5,7,9,11,13,15,17),
    remove=FALSE)%>%
  mutate(y=paste0("20",y),
    strt.file=ymd_hms(paste(y,m,d,h,min,s)),
    into.file=strt-strt.file)
         
ftu <- rbind(ftu0,ftu1) %>% select(-st,-e,-y,-m,-d,-h,-min,-s, -DateTime, -Year)

write.csv(ftu, "wdata/periods_to_examine3.csv")


## create a list of files to place into a workspace, need 2019 and 2020 files separately
## since deployment 2 in 2020 was not recording continuously, we can't use it here

# have to get these down to a unique list of files

wf19<-filter(ftu, Deployment==0) %>%
  ungroup()%>%
  select(stfile)%>%
  distinct()
wf20<-filter(ftu, Deployment==1)%>%
  ungroup()%>%
  select(stfile)%>%
  distinct()

### move selected files to new folder
### for Philina, run just once
# for (i in 1:length(unique(wf19$stfile))){
# file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", unique(wf19$stfile)[i]),
#   "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/boat_passage")
# }
# 
# for (i in 1:length(unique(wf20$stfile))){
#   file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", unique(wf20$stfile)[i]),
#     "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/boat_passage_1")
# }
# 
# # for steph
for (i in 1:nrow(wf19)){
file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",wf19$stfile[i]),
  "E:/RCA_IN/April_July2019/boat_passage")
}


