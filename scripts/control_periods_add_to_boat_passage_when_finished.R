# find control periods ----
# Need to subset down to dates earlier than May 4th, 2020
splc<-spl%>%
  mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  filter(!is.na(wspeed) & wspeed <20 & DateTime < "2020-05-05" & Hr < 5 & Hr >=2) %>% # subset down to intervals where there's not much wind before 5/5/20
  mutate(pst=case_when(
    zoo::rollmax(SPL,k=60,fill=NA,align="left")<105~1,
    zoo::rollmax(SPL,k=60,fill=NA,align="left")>105~0),# find 60 minute periods where the maximum spl is < 100 
    pst2=case_when(
      zoo::rollmax(SPL,k=60,fill=NA,align="right")<105~1,
      zoo::rollmax(SPL,k=60,fill=NA,align="right")>105~0),# find 60 minute periods maximum spl is < 100 
    qp=case_when(
      pst==1 | pst2==1~1,
      is.na(pst) & pst2==1~1,
      pst==1 & is.na(pst2)~1,
      pst==0 & pst2==0~0,
      is.na(pst) & pst2==0~0,
      pst==0 & is.na(pst2)~0),
    dymd=ymd(paste(Year,Month,Day)),
    lint=interval(DateTime,DateTime+minutes(60)),
    rint=interval(DateTime-minutes(60),DateTime))%>% # assign a 1 if either way of looking for a quiet 60 minute interval 
  filter(qp==1)%>%
  select(dymd,pst,pst2,lint,rint)

splc1<-splc%>%
  filter(pst==1)%>%
  select(dymd,int=lint)
splc2<-splc%>%
  filter(pst2==1)%>%
  select(dymd,int=rint)

splc<-bind_rows(splc1,splc2)%>%
  group_by(dymd)%>%
  filter(int_start(int)==min(int_start(int)))%>%
  arrange(dymd) # the closest hour period where the entire hour is less than 100 spl

fcp<-ftu%>%
  ungroup()%>%
  select(inter,prd,strt)%>%
  distinct()%>%
  pivot_wider(names_from=prd,values_from=strt)%>%
  mutate(dymd=ymd(paste(year(post),month(post),day(post))),
         pre=pre+minutes(5),
         ferry.post=ferry+minutes(5),
         pf=difftime(ferry,pre,units="mins"),
         fp=difftime(post,ferry.post,units="mins"),
         pf=ifelse(pf<0,0,pf),
         fp=ifelse(fp<0,0,fp))%>%
  select(inter,dymd,pf,fp)%>%
  left_join(splc)






