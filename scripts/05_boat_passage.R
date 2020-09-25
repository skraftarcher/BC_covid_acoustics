# script to extract boat passages to examine further
library(tidyverse)
library(lubridate)
library(readxl)
if(!require(filesstrings))install.packages("filesstrings");library(filesstrings)
### taking midnight ferry approach----
# Bring in data ----
mv19<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2019_RCA_In")#2019 ais data
mv20<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2020_RCA_In")#2020 ais data
mv19am<-read_xlsx("odata/morning_vessel.xlsx",sheet = "2019_RCAIn")#2020 ais data
mv20am<-read_xlsx("odata/morning_vessel.xlsx",sheet = "2020_RCAIN")#2020 ais data


spl<-read_rds("wdata/spl_file.rds") %>%
  filter(site=="RCA_In")%>%
  rename(filedt=dt)# minute by minute spl data
# organize data ----

mnv<-bind_rows(mv19,mv20)%>%
  arrange (DateTime)%>% # make sure dataset goes from earliest to latest
  # mutate(first_ferry=1) %>%
  mutate(st=DateTime-minutes(60),et=DateTime+minutes(60))%>% # create the window to match to
  pivot_longer(st:et,names_to="tt",values_to="tint")%>% # turn it in to a long data format so we 
  mutate(direction="arriving") %>% 
  mutate(inter=row_number())%>%
    rename(boatdt=DateTime)%>%
  select(-MMSI) # currently removing this because missing form morning data (note: 3 vessels included)

amv<-bind_rows(mv19am,mv20am)%>% filter(first_ferry==1)%>%
  rename(DateTime=DateTime_PDT)%>%
  select(DateTime,Distance)%>%
  # select(DateTime,Distance,first_ferry)%>%
  arrange(DateTime)%>% # make sure dataset goes from earliest to latest
  mutate(st=DateTime-minutes(60),et=DateTime+minutes(60))%>% # create the window to match to
  pivot_longer(st:et,names_to="tt",values_to="tint")%>% # turn it in to a long data format so we
  mutate(direction="departing")%>%
    rename(boatdt=DateTime)%>%
  mutate(inter=row_number()) # trying just adding 1 so that morning passages are the even # following the odd for the prior midnight passage


# give each interval a number- using the find interval it assigns ID based on the row # of the first line in the
# interval i.e., the interval between rows 1 and 2 is interval 1
# mv<-bind_rows(mnv,amv)%>%  
#   # mutate(inter=row_number())%>%
#   arrange(DateTime)%>%
#   # 
#   rename(boatdt=DateTime)
# 
# mnv <- filter(mv, direction=="arriving")
# amv <- filter(mv, direction=="departing")
# hist(mnv$Distance)
# hist(amv$Distance)


spl$inter<-findInterval(spl$DateTime,mnv$tint)#assign intervals. 
# spl$inter<-findInterval(spl$DateTime,amv$tint)#assign intervals. 

# I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure 
mnv2<-mnv%>%
  select(-tint)%>%
  pivot_wider(names_from = tt,values_from=inter)%>%
  select(-et)%>%
  rename(inter=st)

mn_spl <- spl %>%
  filter(inter %in% mnv2$inter)%>% # filter down only to intervals within 60 mins of the ferry passing (60 before or after)
  left_join(mnv2)

# ### NEW CODE ### 
# ### define even inters for AM boat passages
# ### moved to after midnight selection is complete
# splam <- spl %>% select(-inter)
# splam$inter<-findInterval(splam$DateTime,sort(amv$tint)) + 1#assign intervals. 
# # spl$inter<-findInterval(spl$DateTime,amv$tint)#assign intervals. 
# # length(unique(mnv$inter))
# # length(unique(amv$inter))
# 
# # I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure 
# amv2<-amv%>%
#   select(-tint)%>%
#   pivot_wider(names_from = tt,values_from=inter)%>% # code used to find now corrected typo in xlsx: values_fn = length
#   mutate(inter=st+1)%>%
#   select(-et, -st)
# 
# # amv$inter <- amv$inter + 1
# 
# am_spl <- spl %>%
#   filter(inter %in% amv2$inter)%>% # filter down only to intervals within 60 mins of the ferry passing (60 before or after)
#   left_join(amv2)
# #############################################

wthr<-read_rds("wdata/trimmed_hourly_weather.rds")%>%
  select(wspeed,rca,Year=year,Month=month,Day=day,Hr=hr)%>%
  filter(rca=="in")%>%
  select(-rca)


# Need to subset down to dates earlier than May 4th, 2020
spl3<-mn_spl%>%
  mutate(Hr=hour(DateTime))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  group_by(inter)%>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2=mean(wspeed,na.rm = TRUE))%>% # create a mean wind value for the interval
  filter(!is.na(wsp2) & wsp2 <20 & DateTime < "2020-05-05")%>% # subset down to intervals where there's not much wind before 5/5/20
  mutate(dt2=boatdt-minutes(7),
         tperiods=case_when(
           DateTime<dt2~"pre",
           DateTime>boatdt~"post",
           DateTime>dt2&DateTime<boatdt~"oth",
           DateTime==boatdt~"oth"))%>% # create periods before the closest passage of the ferry and after
  group_by(inter,tperiods)%>% 
  mutate(
    ferry.int=interval(boatdt-minutes(6),boatdt-minutes(1)),# create a ferry interval that is the 5 minutes before the closest passage
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
         post.pst=ifelse(post.pst==1& DateTime-boatdt==min(DateTime-boatdt),1,0))%>% # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the post-period
  ungroup(tperiods,pt.int)%>%
  mutate(keep.inter=ifelse(sum(pre.pst)!=0 & sum(post.pst)!=0,1,0))%>% # only keep intervals where there is a qualifying period in both pre and post ferry periods
  filter(keep.inter==1)%>% 
  group_by(inter,tperiods)

spl3a<-spl3 %>% 
  ungroup()%>%
  filter(ferry.prd==1)%>%
  filter(Distance<3000)%>% # only removes #57
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
# interlist2<-unique(spl3$inter)
# for(i in 1:length(interlist2)){
#   p1<-spl3%>%
#     filter(inter==interlist2[i])
#   ggplot(data=p1)+
#     geom_line(aes(y=SPL,x=DateTime,group=inter))+
#     geom_line(aes(y=88,x=DateTime,group=inter,color=prd),size=1.5)+
#     scale_color_manual(values=c("red","white","orange","orange"),name="Potential interval")+
#     theme_bw()+
#     geom_vline(aes(xintercept=boatdt),color="red",linetype="dashed")+
#     scale_x_datetime(date_minor_breaks="5 mins")+
#     theme(legend.position="none")
#   ggsave(paste0("manual_figures/fig_",interlist2[i],"_",p1$Year[i],p1$Month[i],p1$Day[i],"_withperiods.jpg"))
# }


spl.inters<-spl3%>%
  filter(prd!="none")


# create a dataset of sound trap files to use
ftu <- spl.inters%>%
  dplyr::select(stfile,DateTime,filedt,inter,Year,Deployment,pre.int,ferry.int,post.int)%>%
  distinct()%>%
  mutate(prd=case_when(
    DateTime %within% pre.int~"pre",
    DateTime %within% post.int~"post",
    DateTime %within% ferry.int~"ferry"),
    strt=case_when(
      prd=="pre"~int_start(pre.int),
      prd=="ferry"~int_start(ferry.int),
      prd=="post"~int_start(post.int)),
    into.file=strt-filedt)%>%
  select(-pre.int,-ferry.int,-post.int)

# 
### trying to quantify longest period of quiet

splq<-spl%>%
  mutate(Hr=hour(DateTime),dymd=paste0(Year,Month,Day))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  filter(
    # wspeed <20 & !is.na(wspeed) & 
      DateTime < "2020-05-05" & Hr < 5 & Hr >=2) %>% 
  mutate(isq=ifelse(SPL<100,1,0),inter=inter-1)#assign isq (is quiet) a 1 if the spl is less than 105, 0 otherwise

# remove faulty inter assignment of 113 for both "201961" and "201962"
splq[splq$dymd=="201962",]$inter <- NA

dtl<-unique(splq$dymd)#the days to evaluate
splq$qpl<-NA  #create the qpl (quiet period length) variable

#for loop to calculate length of different quiet period stretches
for(i in 1:length(dtl)){
  t1=filter(splq,dymd==dtl[i])# subset down to a single night
  for(j in 2:nrow(t1)){
    t1$qpl[1]<-ifelse(t1$isq[1]==0,0,1)# assign the first qpl a 0 if isq = 0 otherwise 1
    t1$qpl[j]<-ifelse(t1$isq[j]==0,0,1+t1$qpl[j-1])# add to qpl as long as isq = 1
  }
  ifelse(i==1,splq2<-t1, splq2<-bind_rows(splq2,t1))# create splq2 first time through the loop otherwise append splq2
}

# this is where it gets super inefficient
fcp<-ftu%>% # get the files to use
  ungroup()%>% 
  select(inter,prd,strt)%>% # select only the interval, the period, and the start time
  distinct()%>% # go down to unique rows
  pivot_wider(names_from=prd,values_from=strt)%>% # make a wider data frame
  mutate(dymd=paste0(year(post),month(post),day(post)),
         pre2=pre+minutes(5),#get the end of the pre-period
         ferry2=ferry+minutes(5),#get the end of the ferry period
         pf=difftime(ferry,pre2,units="mins"),#calculate the length of time between the end of the pre-period and the start of the ferry period
         fp=difftime(post,ferry2,units="mins"),#calculate the length of time between the end of the ferry-period and the start of the post-period
         midtimediff=fp+10,
         passlen=15+pf+fp)%>% #calculate how long the boat period is
  select(inter, dymd, post,passlen, midtimediff)# only keep inter, post(start of the post period), and period length

splq3all<-splq2%>% #create a new splq dataset
  group_by(dymd)%>% # group by day
  mutate(qplength=max(qpl),
         eqp=ifelse(qpl==max(qpl),1,0))%>% # assign a 1 to the end (last minute) of longest quiet period (eqp)
  filter(eqp==1)%>%# only keep the end of the quiet period
  select(-Deployment)%>%# remove deployment
  left_join(fcp)%>%# join in the file to keep
  filter(!is.na(post))%>%# get rid of lines where there isn't a start to the post period
  mutate(epost=post+minutes(5),# calculate the end of the post period.
         tgap=difftime(DateTime,epost,units="mins"),# find the time gap (tgap) between the end of the post period and the end of the quiet period
         mintgap=ifelse(tgap==min(tgap),1,0),
         minquiet=qplength-passlen-11, # add in 6 min buffer at end of quiet period to allow for morning pre passage period
         midquiet=qplength-midtimediff-11, 
         maxquiet=qplength-5-11, 
         keep=ifelse(qpl>=(passlen+11) & mintgap==1,1,0))%>% # find intervals to keep, only keep those where the quiet period is at least as long as the boat pasage period.
  select(inter,tgap,eqtime=DateTime,qplength,passlen,minquiet, midquiet,maxquiet,keep)

splq3 <- splq3all %>% filter(keep==1)# remove deployment again

ftu2<-ftu%>%
  select(Year,Deployment,inter,prd,strt,boat.stfile=stfile,boat.intofile=into.file)%>%
  # filter(inter !%in% )%>% #remove passages with overlap probles
  distinct()%>%
  left_join(splq3all)%>%
  filter(!is.na(dymd))%>%
  ungroup()%>%
  mutate(quiet=strt+tgap-minutes(11),# calculate the start of of each control period (one for each pre,ferry,and post period)
         qstrt=quiet,# this is the start of the 5 minute period to analyze
         pend=quiet+minutes(5))%>%# this is the end of the 5 minute period to analyze
  pivot_longer(qstrt:pend,names_to="se",values_to="quiet2")%>%# pivot longer so that the start and end times are in a single variable
  filter(keep==1)# remove deployment again
second(ftu2$quiet2)<-0

splq4<-spl%>%
  mutate(quiet2=DateTime)%>%
  select(-inter)
second(splq4$quiet2)<-0

ftu.b<-ftu2%>%
  select(Year,Deployment,inter,prd, strt, stfile = boat.stfile, into.file = boat.intofile)%>%
  mutate(type = "boat")%>%
  distinct()

ftu.q<-ftu2%>%
  select(Year,Deployment,inter,prd,quiet,quiet2)%>%
  left_join(splq4)%>%
  mutate(into.file=quiet-filedt,
    type = "quiet")%>%
  select(Year,Deployment,inter,prd, strt=quiet, stfile, into.file, type)%>%
  distinct()

write.csv(ftu.b,"wdata/files_to_evaluate_boat_mn.csv")
write.csv(ftu.q,"wdata/files_to_evaluate_quiet_mn.csv")

### move selected files to new folder

wf19<-ftu.b%>%
  filter(Deployment==0)%>%
  select(stfile)%>%
  distinct()

wf20<-ftu.b%>%
  filter(Deployment==1)%>%
  select(stfile)%>%
  distinct()


qf19<-ftu.q%>%
  filter(Deployment==0)%>%
  select(stfile)%>%
  distinct()

qf20<-ftu.q%>%
  filter(Deployment==1)%>%
  select(stfile)%>%
  distinct()

### for Philina, run just once
for (i in 1:length(wf19$boat.stfile)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", wf19$boat.stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/boat_passage")
}

for (i in 1:length(wf20$boat.stfile)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", wf20$boat.stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/boat_passage_1")
}


for (i in 1:nrow(qf19)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", qf19$stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/quiet_period")
}

for (i in 1:nrow(qf20)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", qf20$stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/quiet_period_1")
}


### for Steph, run just once
for (i in 1:nrow(wf19)){
  file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",wf19$stfile.boat[i]),
    "E:/RCA_IN/April_July2019/boat_passage")
}

for (i in 1:nrow(qf19)){
  file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",qf19$stfile.strt[i]),
    "E:/RCA_IN/April_July2019/quiet_period")
}

# FIGURES FOR MN QUIET SAMPLES #####
# 
# splq3<-splq3all%>% filter(keep==1)
# 
# all.qp.lengths <- as.numeric(round(sort(c(splq3$minquiet,splq3$midquiet,splq3$maxquiet))))
# hist(all.qp.lengths, breaks = 30)
# 
# interlist2<-unique(ftu.q$inter)
# 
# 
# for(i in 1:length(interlist2)){
#   p1<-splq%>%
#     filter(inter==interlist2[i])
#   p2 <- ftu.q %>% filter(inter==interlist2[i])
#   
#   ggplot(data=p1)+
#     geom_line(aes(y=SPL,x=DateTime))+
#     geom_segment(data=p2, aes(y=88, yend=88, x=quiet, xend= (quiet + minutes(5)), color=prd),size=1.5, inherit.aes = F)+
#     scale_color_manual(values=c("red","orange","orange"),name="Potential interval")+
#     theme_bw()+
#     # geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
#     scale_x_datetime(date_minor_breaks="5 mins")+
#     coord_cartesian(ylim=c(85,120)) +
#     theme(legend.position="none")
#   ggsave(paste0("manual_figures/qfig_",interlist2[i],"_",p1$Year[i],p1$Month[i],p1$Day[i],"_100.jpg"))
# }

################
### NEW CODE ###
### define even inters for AM boat passages ####
splam <- spl %>% select(-inter)
splam$inter<-findInterval(splam$DateTime,sort(amv$tint)) + 1#assign intervals. 
# spl$inter<-findInterval(spl$DateTime,amv$tint)#assign intervals. 
# length(unique(mnv$inter))
# length(unique(amv$inter))

# I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure 
amv2<-amv%>%
  select(-tint)%>%
  pivot_wider(names_from = tt,values_from=inter)%>% # code used to find now corrected typo in xlsx: values_fn = length
  mutate(inter=st+1)%>%
  select(-et, -st)

# amv$inter <- amv$inter + 1

am_spl <- splam %>%
  filter(inter %in% amv2$inter)%>% # filter down only to intervals within 60 mins of the ferry passing (60 before or after)
  left_join(amv2)

spl3am <- am_spl%>%
  mutate(Hr=hour(DateTime))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  group_by(inter)%>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2=mean(wspeed,na.rm = TRUE))%>% # create a mean wind value for the interval
  filter(!is.na(wsp2) & wsp2 <20 & DateTime < "2020-05-05")%>% # subset down to intervals where there's not much wind before 5/5/20
  mutate(dt2=boatdt-minutes(7),
    tperiods=case_when(
      DateTime<dt2~"pre",
      DateTime>boatdt~"post",
      DateTime>dt2&DateTime<boatdt~"oth",
      DateTime==boatdt~"oth"))%>% # create periods before the closest passage of the ferry and after
  group_by(inter,tperiods)%>% 
  mutate(
    ferry.int=interval(boatdt-minutes(6),boatdt-minutes(1)),# create a ferry interval that is the 5 minutes before the closest passage
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
    post.pst=ifelse(post.pst==1& DateTime-boatdt==min(DateTime-boatdt),1,0))%>% # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the post-period
  ungroup(tperiods,pt.int)%>%
  mutate(keep.inter=ifelse(sum(pre.pst)!=0 & sum(post.pst)!=0,1,0))%>% # only keep intervals where there is a qualifying period in both pre and post ferry periods
  filter(keep.inter==1)%>% 
  group_by(inter,tperiods)

spl3a2 <- spl3am %>% 
  ungroup()%>%
  filter(ferry.prd==1)%>%
  filter(Distance<3000)%>% # only removes #57
  group_by(inter, Year, Month, Day)%>%
  summarize(ferryspl=max(SPL))%>%
  filter(ferryspl>105)# find intervals where the ferry passage interval has a mean spl over 105

spl3am<-filter(spl3am,inter %in%spl3a2$inter) # subset down to those intervals

# calculate the pre-ferry period to analyze
spl4am<-spl3am%>%
  filter(pre.pst==1)%>%
  ungroup()%>%
  select(inter,pre.dt=DateTime)%>%
  mutate(pre.int=interval(pre.dt,pre.dt+minutes(5)))

# calculate the post-ferry period to analyze
spl5am<-spl3am%>%
  filter(post.pst==1)%>%
  ungroup()%>%
  select(inter,post.dt=DateTime)%>%
  mutate(post.int=interval(post.dt-minutes(5),post.dt))

spl3am<-spl3am%>%
  ungroup()%>%
  left_join(spl4am)%>%
  left_join((spl5am))%>%
  mutate(pre.prd=if_else(DateTime %within% pre.int,1,0),# assign a 1 to minutes within the pre period
    post.prd=if_else(DateTime %within% post.int,1,0),# assign a 1 to minutes within the post period
    prd=case_when(
      pre.prd==1~"pre",
      post.prd==1~"post",
      ferry.prd==1~"ferry",
      pre.prd==0 & post.prd==0 & ferry.prd ==0~"none"))# create a variable that indicates which period the minute belongs to
# 
# # make figures to examine the spl profile of the candidate periods
# interlist2<-unique(spl3am$inter)
# for(i in 1:length(interlist2)){
#   p1<-spl3am%>%
#     filter(inter==interlist2[i])
#   ggplot(data=p1)+
#     geom_line(aes(y=SPL,x=DateTime,group=inter))+
#     geom_line(aes(y=88,x=DateTime,group=inter,color=prd),size=1.5)+
#     scale_color_manual(values=c("red","white","orange","orange"),name="Potential interval")+
#     theme_bw()+
#     geom_vline(aes(xintercept=boatdt),color="red",linetype="dashed")+
#     scale_x_datetime(date_minor_breaks="5 mins")+
#     theme(legend.position="none")
#   ggsave(paste0("manual_figures/fig_",interlist2[i],"_",p1$Year[i],p1$Month[i],p1$Day[i],"_am_boat.jpg"))
# }


spl.inters.am<-spl3am%>%
  filter(prd!="none")

##########
# create a dataset of AM sound trap files to use
ftuAM <- spl.inters.am %>%
  dplyr::select(stfile,DateTime,filedt,inter,Year,Deployment,pre.int,ferry.int,post.int)%>%
  distinct()%>%
  mutate(prd=case_when(
    DateTime %within% pre.int~"pre",
    DateTime %within% post.int~"post",
    DateTime %within% ferry.int~"ferry"),
    strt=case_when(
      prd=="pre"~int_start(pre.int),
      prd=="ferry"~int_start(ferry.int),
      prd=="post"~int_start(post.int)),
    into.file=strt-filedt)%>%
  select(-pre.int,-ferry.int,-post.int) 


##############################
### to quantify longest period of quiet after removing sampled times
inter.used <- unique(ftu.b$inter) 

fcp2 <- ftu2 %>% filter (inter %in% inter.used) %>% 
  select(dymd, prd, quiet, passlen) %>% distinct()


# calculate the post-ferry period to analyze
fcp3<-fcp2%>%
  ungroup()%>%
  mutate(quiet.int=interval(quiet-minutes(5),quiet)) %>% 
  select(-quiet) %>%
  pivot_wider(names_from = "prd", values_from = "quiet.int")


splqAM <- splq2%>% #create a new splq dataset
  left_join(fcp3)%>%# join in the file to keep
  group_by(dymd)%>%
  mutate(
    maxqpl= max(qpl, na.rm = T),
    passlength = if_else(!is.na(passlen), passlen, 0),
    remainingqpl = if_else(!is.na(passlen), maxqpl-passlength-6, maxqpl), # subtract 6 for buffer
    # truetgap=if_else(qpl==max(qpl, na.rm = T), tgap, NA_real_),
    # sampled = if_else( qpl>= max(qpl, na.rm = T)-passlength-6,1,0, missing = 0),
    # isq=ifelse(SPL<100 & sampled == 0,1,0)# appears to be working but loses too many samples
    sampled=if_else(DateTime %within% pre | DateTime %within% ferry  | DateTime %within% post,1,0, missing = 0), # also works, but don't know how to proceed
    isq=ifelse(SPL<100,1,0)
  ) 

dtl<-unique(splqAM$dymd)#the days to evaluate
splqAM$qpl<-NA  #create the qpl (quiet period length) variable


# this method requires continuous quiet without samples... 
# need to change to 
#for loop to calculate length of different quiet period stretches
for(i in 1:length(dtl)){
  t1=filter(splqAM,dymd==dtl[i])# subset down to a single night
  for(j in 2:nrow(t1)){
    t1$qpl[1]<-ifelse(t1$isq[1]==0,0,1)# assign the first qpl a 0 if isq = 0 otherwise 1
    t1$qpl[j]<-ifelse(t1$isq[j]==0,0,1+t1$qpl[j-1])# add to qpl as long as isq = 1
  }
  ifelse(i==1,splqAM2<-t1, splqAM2<-bind_rows(splqAM2,t1))# create splq2 first time through the loop otherwise append splq2
}

glimpse(splqAM2)
# 
# ###############
# # We will need rest of code pre figures to be also run for AM period
# # draft of subsequent code included hereafter
# #
fcpAM <- ftuAM %>% # get the files to use
  ungroup()%>%
  select(inter,prd,strt)%>% # select only the interval, the period, and the start time
  distinct()%>% # go town to unique rows
  pivot_wider(names_from=prd,values_from=strt)%>% # make a wider data frame
  mutate(dymd=paste0(year(post),month(post),day(post)),
    pre2=pre+minutes(5),#get the end of the pre-period
    ferry2=ferry+minutes(5),#get the end of the ferry period
    pf=difftime(ferry,pre2,units="mins"),#calculate the length of time between the end of the pre-period and the start of the ferry period
    fp=difftime(post,ferry2,units="mins"),#calculate the length of time between the end of the ferry-period and the start of the post-period
    midtimediff=fp+10,
    passlen=15+pf+fp)%>% #calculate how long the boat period is
  select(dymd, inter, pre, ferry, post, passlen, midtimediff)# only keep inter, post(start of the post period), and period length

# ##### working here
# 
splqAMall <- splqAM2 %>% #create a new splq dataset
  group_by(dymd)%>% # group by day
  mutate(qplength=max(qpl),
    eqp=ifelse(qpl==max(qpl),1,0))%>% # assign a 1 to the end (last minute) of longest quiet period (eqp)
  filter(eqp==1)%>%# only keep the end of the quiet period
  select(-Deployment, -inter, -passlen, -pre, -ferry, -post) %>%
  left_join(fcpAM)%>%# join in the file to keep
  filter(!is.na(post))%>%# get rid of lines where there isn't a start to the post period
  mutate(
    tgap=difftime(DateTime,pre,units="mins"),# find the time gap (tgap) between the end of the post period and the end of the quiet period
    # mintgap=ifelse(tgap==min(tgap),1,0),
    minquiet=qplength-passlen,
    midquiet=qplength-midtimediff,
    maxquiet=qplength-5,
    keep=ifelse(qplength>=passlen,1,0))%>% # find intervals to keep, only keep those where the quiet period is at least as long as the boat pasage period
  select(inter,tgap,eqtime=DateTime,qplength, passlen, minquiet, midquiet, maxquiet, keep)

ftuAM2 <- ftuAM %>%
  select(Year,Deployment,inter,prd,strt,boat.stfile=stfile,boat.intofile=into.file)%>%
  distinct()%>%
  left_join(splqAMall)%>%
  # filter(!is.na(dymd))%>%
  ungroup()%>%
  mutate(
    # calculate the start of of each control period (one for each pre,ferry,and post period)
    quiet=if_else(tgap>=0, strt - passlen - minutes(6) + tgap, strt - minutes(6) - passlen), # changed 5 min to 6 to give a min 1 min break between end of quiet-post sample and start of boat-pre sample
    # quiet_test=if_else(tgap>=0, eqtime - passlen - minutes(5), strt - passlen - minutes(5)), # test should be equal to quiet only for pre period
    qstrt=quiet,# this is the start of the 5 minute period to analyze
    pend=quiet+minutes(5))%>%# this is the end of the 5 minute period to analyze
  pivot_longer(qstrt:pend,names_to="se",values_to="quiet2")%>%# pivot longer so that the start and end times are in a single variable
  filter(keep==1)


second(ftuAM2$quiet2)<-0
splq4<-spl%>%
  mutate(quiet2=DateTime)%>%
  select(-inter)
second(splq4$quiet2)<-0

ftuAM.b<-ftuAM2%>%
  select(Year,Deployment,inter,prd, strt, stfile=boat.stfile, into.file=boat.intofile)%>%
  mutate(type="boat")%>%
  distinct()

ftuAM.q<-ftuAM2%>%
  select(inter,prd, quiet, quiet2)%>%
  left_join(splq4)%>%
  mutate(into.file=quiet-filedt, type="quiet")%>%
  select(Year,Deployment,inter,prd, strt=quiet, stfile, into.file, type)%>%
  distinct()

allAMfiles <- bind_rows(ftuAM.b, ftuAM.q) %>% arrange(strt)

write.csv(ftuAM.b,"wdata/files_to_evaluate_boat_am.csv")
write.csv(ftuAM.q,"wdata/files_to_evaluate_quiet_am.csv")
write.csv(allAMfiles,"wdata/files_to_evaluate_all_am.csv")


### move selected files to new folder

all19am<-allAMfiles%>%
  filter(Deployment==0)%>%
  select(stfile)%>%
  distinct()

all20am<-allAMfiles%>%
  filter(Deployment==1)%>%
  select(stfile)%>%
  distinct()

# 2019 conflicts... 
intersect(qf19, all19am)
# 2020 conflicts... 
intersect(qf20, all20am)
# duplicate these sets of files only for use in separate work spaces?


#list all files

allfiles <- bind_rows(ftu.b, ftu.q, ftuAM.b, ftuAM.q) %>% arrange(strt) %>% 
  mutate(timediff = signif((strt - lag(strt))/1, digits = 4), overlap = if_else(timediff<5*60 & timediff>0, T, F, missing = F))

samples_to_drop <- filter(allfiles, overlap == T)

allfiles2 <- allfiles %>% 
  mutate(year = year(strt), month = month(strt), day = day(strt), dymd=paste0(year(strt), month(strt),day(strt))) %>% 
  filter(!(inter %in% unique(samples_to_drop$inter)))

write.csv(allfiles2,"wdata/files_to_evaluate_all.csv")


# make figures to examine the spl profile of all quiet periods
morning_cutoff <- "1899-12-31 06:00:00 UTC"
"1899-12-31 06:29:56 UTC"

allinterspl <- spl %>% select(DateTime, Time, SPL) %>% 
  #bind_rows(mn_spl, am_spl) %>% select(inter, DateTime, Time, SPL) %>% 
  mutate(hr = hour(DateTime),
    dymd=paste0(year(DateTime),month(DateTime),day(DateTime)))  %>%
  filter(hr < 7) 

interlist2<-unique(allfiles2$dymd)

# allinterspl$DateTime[4036]
# allinterspl$Time[5639]


for(i in 1:length(interlist2)){
  
  p1<-allinterspl%>%
    filter(dymd==interlist2[i])
  
  p2 <- allfiles2 %>% 
    filter(dymd==interlist2[i] & type == "quiet") 
  
  p3 <- allfiles2 %>% 
    filter(dymd==interlist2[i] & type == "boat") %>% mutate(prd=paste0("x",prd))
  
  inters <- unique(p2$inter)
  
  ggplot(data=p1)+
    geom_line(aes(y=SPL,x=DateTime))+
    geom_segment(data=p2, aes(y=88, yend=88, x=strt, xend= (strt + minutes(5)), color=prd),size=1.5, inherit.aes = F)+
    geom_segment(data=p3, aes(y=88, yend=88, x=strt, xend= (strt + minutes(5)), color=prd),size=1.5, inherit.aes = F)+
    scale_color_manual(values=c("red","orange","orange", "black","purple","purple"),name="Potential interval")+
    theme_bw()+
    # geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
    scale_x_datetime(date_minor_breaks="5 mins")+
    coord_cartesian(ylim=c(85,120)) +
    theme(legend.position="none")
  ggsave(paste0("manual_figures/qfig_",paste0(inters[1]),"_",p2$year[1],p2$month[1],p2$day[1],".jpg"))
}

all_files19 <- bind_rows(wf19, qf19, all19am) %>% distinct()
all_files20 <- bind_rows(wf20, qf20, all20am) %>% distinct()


### for Philina, run just once
for (i in 1:length(wf19am$boat.stfile)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", wf19am$boat.stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/boat_passage_am")
}

for (i in 1:length(wf20am$boat.stfile)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", wf20am$boat.stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/boat_passage_1_am")
}


for (i in 1:nrow(qf19)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", qf19$stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/quiet_period")
}

for (i in 1:nrow(qf20)){
  file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", qf20$stfile[i]),
    "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/quiet_period_1")
}


### for Steph, run just once
for (i in 1:nrow(wf19)){
  file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",wf19$stfile.boat[i]),
    "E:/RCA_IN/April_July2019/boat_passage")
}

for (i in 1:nrow(qf19)){
  file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",qf19$stfile.strt[i]),
    "E:/RCA_IN/April_July2019/quiet_period")
}

# # FIGURES
# 
# splq3<-splq3all%>% filter(keep==1)
# 
# all.qp.lengths <- as.numeric(round(sort(c(splq3$minquiet,splq3$midquiet,splq3$maxquiet))))
# hist(all.qp.lengths, breaks = 30)
# 
# interlist2<-unique(ftu.q$inter)
# 
# allfiles2 <- allfiles %>% filter(!(inter %in% unique(samples_to_drop$inter)))
# 
# for(i in 1:length(interlist2)){
#   p1<-splq%>%
#     filter(inter==interlist2[i])
#   p2 <- ftu.q %>% filter(inter==interlist2[i])
#   
#   ggplot(data=p1)+
#     geom_line(aes(y=SPL,x=DateTime))+
#     geom_segment(data=p2, aes(y=88, yend=88, x=quiet, xend= (quiet + minutes(5)), color=prd),size=1.5, inherit.aes = F)+
#     scale_color_manual(values=c("red","orange","orange"),name="Potential interval")+
#     theme_bw()+
#     # geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
#     scale_x_datetime(date_minor_breaks="5 mins")+
#     coord_cartesian(ylim=c(85,120)) +
#     theme(legend.position="none")
#   ggsave(paste0("manual_figures/qfig_",interlist2[i],"_",p1$Year[i],p1$Month[i],p1$Day[i],"_100.jpg"))
# }
# 
# 
# 
