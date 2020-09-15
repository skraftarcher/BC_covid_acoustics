# find control periods ----
# Need to subset down to dates earlier than May 4th, 2020
# choose time of quiet required
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

spl$inter<-findInterval(spl$DateTime,mv$tint)

# quietmin <- 60
# quietmin <- 40
# quietmin <- 20 # only two missing
# 
# splc<-spl%>%
#   mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
#   left_join(wthr)%>% # join in wind
#   filter(!is.na(wspeed) & wspeed <20 & DateTime < "2020-05-05" & Hr < 5 & Hr >=2) %>% # subset down to intervals where there's not much wind before 5/5/20
#   mutate(pst=case_when(
#     zoo::rollmax(SPL,k=quietmin,fill=NA,align="left")<105~1,
#     zoo::rollmax(SPL,k=quietmin,fill=NA,align="left")>105~0),# find 60 minute periods where the maximum spl is < 100 
#     pst2=case_when(
#       zoo::rollmax(SPL,k=quietmin,fill=NA,align="right")<105~1,
#       zoo::rollmax(SPL,k=quietmin,fill=NA,align="right")>105~0),# find 60 minute periods maximum spl is < 100 
#     qp=case_when(
#       pst==1 | pst2==1~1,
#       is.na(pst) & pst2==1~1,
#       pst==1 & is.na(pst2)~1,
#       pst==0 & pst2==0~0,
#       is.na(pst) & pst2==0~0,
#       pst==0 & is.na(pst2)~0),
#     dymd=ymd(paste(Year,Month,Day)),
#     lint=interval(DateTime,DateTime+minutes(quietmin)),
#     rint=interval(DateTime-minutes(quietmin),DateTime))%>% # assign a 1 if either way of looking for a quiet 60 minute interval 
#   filter(qp==1)%>%
#   select(dymd,pst,pst2,lint,rint)
# 
# splc1<-splc%>%
#   filter(pst==1)%>%
#   select(dymd,int=lint)
# splc2<-splc%>%
#   filter(pst2==1)%>%
#   select(dymd,int=rint)
# 
# splc<-bind_rows(splc1,splc2)%>%
#   group_by(dymd)%>%
#   filter(int_start(int)==min(int_start(int)))%>%
#   arrange(dymd) # the closest hour period where the entire hour is less than 100 spl
# 
# fcp<-ftu%>%
#   ungroup()%>%
#   select(inter,prd,strt)%>%
#   distinct()%>%
#   pivot_wider(names_from=prd,values_from=strt)%>%
#   mutate(dymd=ymd(paste(year(post),month(post),day(post))),
#          pre=pre+minutes(5),
#          ferry.post=ferry+minutes(5),
#          pf=difftime(ferry,pre,units="mins"),
#          fp=difftime(post,ferry.post,units="mins"))%>%#,
#          # pf=ifelse(pf<0,0,pf),
#          # fp=ifelse(fp<0,0,fp))%>%
#   select(inter,dymd,pf,fp)%>%
#   left_join(splc)
# 
# 

### trying to quantify longest period of quiet

splq<-spl%>%
  mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  filter(!is.na(wspeed) & wspeed <20 & DateTime < "2020-05-05" & Hr < 5 & Hr >=2) %>% 
  mutate(isq=ifelse(SPL<105,1,0),dymd=ymd(paste(Year,Month,Day)))#assign isq (is quiet) a 1 if the spl is less than 105, 0 otherwise

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


fcp<-ftu%>% # get the files to use
  ungroup()%>% 
  select(inter,prd,strt)%>% # select only the interval, the period, and the start time
  distinct()%>% # go town to unique rows
  pivot_wider(names_from=prd,values_from=strt)%>% # make a wider data frame
  mutate(dymd=ymd(paste(year(post),month(post),day(post))),
         pre2=pre+minutes(5),#get the end of the pre-period
         ferry2t=ferry+minutes(5),#get the end of the ferry period
         pf=difftime(ferry,pre2,units="mins"),#calculate the length of time between the end of the pre-period and the start of the ferry period
         fp=difftime(post,ferry2,units="mins"),#calculate the length of time between the end of the ferry-period and the start of the post-period
         pl=15+pf+fp)%>% #calculate how long the boat period is
  select(inter,post,pl)# only keep inter, post(start of the post period), and period length

  
  
spl_data2 <-read_rds("wdata/spl_file.rds")%>%#bring in the file that links spl data to stfiles
  mutate(quiet2=DateTime)%>%#create a new variable quiet 2 that is the spl minute 
  rename(stfile.quiet=stfile,#rename some variables to make it clearer later
         strt.file.quiet=dt)
second(spl_data2$quiet2)<-0# remove the seconds from quiet2


splq3<-splq2%>% #create a new splq dataset
  group_by(dymd)%>% #group by day
  mutate(eqp=ifelse(qpl==max(qpl),1,0), #assign a 1 to the end (last minute) of longest quiet period (eqp)
         inter=inter-1)%>%# substract 1 from the interval here to make the boat passage intervals
  filter(eqp==1)%>%#only keep the end of the quiet period
  select(-Deployment)%>%#remove deployment
  left_join(fcp)%>%#join in the file to keep
  filter(!is.na(post))%>%# get rid of lines where there isn't a start to the post period
  mutate(epost=post+minutes(5),#calculate the end of the post period.
         tgap=difftime(DateTime,epost,units="mins"),#find the time gap (tgap) between the end of the post period and the end of the quiet period
         keep=ifelse(qpl>=pl,1,0))%>% # find intervals to keep, only keep those where the quiet period is at least as long as the boat pasage period.
  filter(keep==1)%>%
  select(inter,tgap,eqtime=DateTime)

ftu2<-left_join(ftu,splq3)%>%
  ungroup()%>%
  filter(!is.na(tgap))%>%# only keep periods where there is a time gap
  mutate(quiet=strt+tgap,#calculate the start of of each control period (one for each pre,ferry,and post period)
         qstrt=quiet,#this is the start of the 5 minute period to analyze
         pend=quiet+minutes(5))%>%#this is the end of the 5 minute period to analyze
  pivot_longer(qstrt:pend,names_to="se",values_to="quiet2")%>%#pivot longer so that the start and end times are in a single variable
  rename(stfile.boat=stfile)%>%#rename the current stfile to indicate that file is for the boat passage
  select(-Deployment) #remove deployment again

second(ftu2$quiet2)<-0#set the seconds in quiet2 to 0

ftu3<-left_join(ftu2,spl_data2)%>% # join stfile dataset
  mutate(into.file.quiet=quiet-strt.file.quiet)%>% #calculate how many seconds into the file the period starts
  select(Deployment,inter,stfile.boat,prd,strt,into.file,stfile.quiet,quiet,into.file.quiet)## select only columns we need

ftu.b<-ftu3%>%
  select(Deployment,inter,stfile.boat,prd,strt,into.file)%>%
  distinct()# create the file list to use for the boat passages

ftu.q<-ftu3%>%
  select(Deployment,inter,prd,stfile.strt=stfile.quiet,quiet,into.file.strt=into.file.quiet)%>%
  distinct()%>%
  mutate() #create the file list to use for the quiet period.

write.csv(ftu.b,"wdata/files_to_evaluate_boat.csv")
write.csv(ftu.q,"wdata/files_to_evaluate_quiet.csv")  
  
# separate(stfile.quiet,into=c("st","y","m","d","h","min","s","e"),
#          sep=c(-16,-14,-12,-10,-8,-6,-4),
#          remove=FALSE)

  



