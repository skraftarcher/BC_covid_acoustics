# script to extract boat passages to examine further
library(tidyverse)
library(lubridate)
library(readxl)


splm<-read_rds("wdata/spl_by_min.rds")%>%
  mutate(hr=hour(Time),
         mn=minute(Time),
         am_pm=if_else(hr<12, "am", "pm"),
         dspl=lead(SPL)-SPL) %>%
  filter(hr <= 6 | hr >= 21) %>% # 9 pm to 7 am
  group_by(Year, Month, rca) %>% 
  mutate(spl_scaled=scale(SPL)) %>% # scale SPL within site
  ungroup()

# quick example plot to look at data
# change from prior min
ggplot(data=splm%>%
    filter(Month==5 & Day ==1 & Year==2019 & hr<2),
  aes(y=dspl,x=DateTime))+
  geom_line(aes(group=grp,color=grp))#+
#  facet_wrap(~Year,scales="free",ncol=1)

# vs raw SPL timeseries
ggplot(data=splm%>%
    filter(Month==5 & Day==2 & Year==2019 & hr<2),
  aes(y=SPL,x=DateTime))+
  geom_line(aes(group=grp,color=grp))

# compared with scaled version of spl
ggplot(data=splm%>%
    filter(Month==5 & Day==2 & Year==2019 & hr<2),
  aes(y=spl_scaled,x=DateTime))+
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)

# maybe smaller blip shows up nearly simultaneously 

###############
### view multiple days at same time 
### includes scheduled ferry passage in both covid and normal times
###############

# daily 10:45 departure from tsawwassen should arrive around 12:45 am (passage ~12:30)
ggplot(data=splm%>%
    filter(Month==4 & Day >=22 & Year ==2019 & 
        hr <2 & 
        rca == "in"), 
  aes(y=spl_scaled,x=DateTime))+  
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day >=20 & Day <=28 & 
        Year ==2020 & 
        hr <2 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")


# daily 5:15 departure from duke point (peak passage ~5:30)
ggplot(data=splm%>%
    filter(Month==4 & Day >=22 & Year ==2019 & 
        hr>3 & hr<7 &
        rca == "in"), 
  aes(y=spl_scaled,x=DateTime))+  
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day >=20 & Day <=28 & 
        Year ==2020 & 
        hr>3 & hr<7 &
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")


# Mon-Fri 8:15 departure from Tsawwassen should pass around 10 pm and 10:45 departure from duke point (peak passage ~10:55 pm?) 
ggplot(data=splm%>%
    filter(Month==4 & Day>=20 & Day<=28 & 
        Year ==2019 & 
        hr >= 21 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day>=20 & Day<=28 & 
        Year ==2020 & 
        hr >= 22 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

################
#### try tsoutliers package to detect peaks 
################
splm2hr <- splm %>%
  filter(rca=="in" & Month==5 & Day ==2& Year ==2019& hr <4)

if(!require(tsoutliers)) install.packages("tsoutliers"); library("tsoutliers")
dat.ts<- ts(splm2hr$spl_scaled,frequency=1, deltat = 1/240)
plot(dat.ts)
data.ts.outliers <- tso(dat.ts, delta = 0.05, types = "TC", check.rank = T)
data.ts.outliers
plot(data.ts.outliers)


################
### simple function that finds peaks
################
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

# plot(splm2hr$SPL, type="l")
# p=find_peaks(splm2hr$SPL, m = 20)
# p2=find_peaks(-splm2hr$SPL, m = 20)
# points(p, splm2hr$SPL[p], col="red")
# points(p2, splm2hr$SPL[p2])

d <- c(20,21)
d <- c(21,22)
d <- c(22,23)
d <- c(23,24)
year <- 2019
year <- 2020

splm2hr <- splm %>%
  filter(rca=="in" & (
    Month==4 & Day==d[1]& Year==year & hr>=10 | 
      Month==4 & Day==d[2]& Year==year & hr<=6
  ))

splmIN <- splm %>% filter(rca=="in")

plot(splm2hr$SPL, type="l")
p=find_peaks(splm2hr$SPL, m = 20)
p <- p[ p> 20 & p < (max(p)-20)]
threshold <- quantile(splmIN$SPL, 0.75)
# threshold <- mean(splm$SPL)+ 2*(sd(splm$SPL))
# quiet_threshold <- mean(splm$SPL)
quiet_threshold <- quantile(splmIN$SPL, 0.25)
abline(h = threshold)
abline(h = quiet_threshold)
p <- p[splm2hr$SPL[p] > threshold]
points(p, splm2hr$SPL[p], col="red")

# p2=find_peaks(-splm2hr$SPL, m = 20)
# points(p2, splm2hr$SPL[p2])

################
# ### quantmod function that finds peaks ### NOT GOOD
# if(!require(quantmod)) install.packages("quantmod")
# 
# plot(splm2hr$spl_scaled, type="l")
# p=quantmod::findPeaks(splm2hr$spl_scaled, 0.5)
# points(p, splm2hr$spl_scaled[p])

### complex function with smoothing... but throws error
# myfindPeaks <- function (x, 
#   thresh=0.05, span=0.25, lspan=0.05, 
#   noisey=TRUE
# ){
#   n <- length(x)
#   y <- x
#   mu.y.loc <- y
#   if(noisey)
#   {
#     mu.y.loc <- (x[1:(n-2)] + x[2:(n-1)] + x[3:n])/3
#     mu.y.loc <- c(mu.y.loc[1], mu.y.loc, mu.y.loc[n-2])
#   }
#   y.loess <- loess(x~I(1:n), span=span)
#   y <- y.loess[[2]]
#   sig.y <- var(y.loess$resid, na.rm=TRUE)^0.5
#   DX.1 <- sign(diff(mu.y.loc, na.pad = FALSE))
#   pks <- which(diff(DX.1, na.pad = FALSE) < 0 & DX.1[-(n-1)] > 0) + 1
#   out <- pks
#   if(noisey)
#   {
#     n.w <- floor(lspan*n/2)
#     out <- NULL
#     # browser()
#     for(pk in pks)
#     {
#       inner <- (pk-n.w):(pk+n.w)
#       outer <- c((pk-2*n.w):(pk-n.w),(pk+2*n.w):(pk+n.w))
#       mu.y.outer <- mean(y[outer])
#       if(!is.na(mu.y.outer)) 
#         if (mean(y[inner])-mu.y.outer > thresh*sig.y) out <- c(out, pk)
#     }
#   }
#   out
# }


### taking midnight ferry approach----

mv19<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2019_RCA_In")#2019 ais data
mv20<-read_xlsx("odata/midnight_vessel.xlsx",sheet = "2020_RCA_In")#2020 ais data
spl<-read_rds("wdata/spl_by_min.rds") %>%
  filter(rca=="in")# minute by minute spl data
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
spl2<-spl%>%
  filter(inter %in% mv2$inter)%>% # filter down only to intervals within 30 mins of the ferry passing (30 before or after)
  left_join(mv2)%>% # join the ferry passage info
  mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  group_by(inter)%>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2=mean(wspeed,na.rm = TRUE))%>% # create a mean wind value for the interval
  filter(!is.na(wsp2) & wsp2 <20 & DateTime < "2020-05-05")


ggplot(data=spl2)+
  geom_line(aes(y=SPL,x=DateTime,group=inter))+
  geom_vline(aes(xintercept=dt),color="red",size=1.5,linetype="dashed")+
  facet_wrap(~inter,scales = "free_x")

interlist<-unique(spl2$inter)
#for(i in 1:length(interlist)){
  p1<-spl2%>%
    filter(inter==interlist[i])
    ggplot(data=p1)+
    geom_line(aes(y=SPL,x=DateTime,group=inter))+
    theme_bw()+
    geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
    scale_x_datetime(date_minor_breaks="5 mins")
  ggsave(paste0("manual_figures/fig_",p1$Year[i],p1$Month[i],p1$Day[i],".jpg"))
}

spl3<-spl%>%
  filter(inter %in% mv2$inter)%>% # filter down only to intervals within 30 mins of the ferry passing (30 before or after)
  left_join(mv2)%>% # join the ferry passage info
  mutate(Hr=hour(DateTime),datehr=ymd_h(paste(Year,Month,Day,Hr)))%>% # create a date hr variable to link with wind
  left_join(wthr)%>% # join in wind
  group_by(inter)%>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2=mean(wspeed,na.rm = TRUE))%>% # create a mean wind value for the interval
  filter(!is.na(wsp2) & wsp2 <20 & DateTime < "2020-05-05")%>%
  ungroup()%>%
  group_by(inter)%>%
  mutate(tperiods=ifelse(DateTime<dt,"pre","post"))%>%
  group_by(inter,tperiods)%>%
  mutate(pre.pst=case_when(
    tperiods=="pre"& zoo::rollmax(SPL,k=5,fill=NA,align="left")<100~1,
    tperiods=="pre"& zoo::rollmax(SPL,k=5,fill=NA,align="left")>100~0,
    tperiods=="pre"& is.na(zoo::rollmax(SPL,k=5,fill=NA,align="left"))~0,
    tperiods=="post"~0),
    post.pst=case_when(
      tperiods=="post"& zoo::rollmax(SPL,k=5,fill=NA,align="right")<100~1,
      tperiods=="post"& zoo::rollmax(SPL,k=5,fill=NA,align="right")>100~0,
      tperiods=="post"& is.na(zoo::rollmax(SPL,k=5,fill=NA,align="right"))~0,
      tperiods=="pre"~0),
    pt.int=ifelse(pre.pst==1|post.pst==1,1,0),
    ferry.int=interval(dt-minutes(6),dt-minutes(1)),
    ferry.prd=if_else(DateTime %within% ferry.int,1,0))%>%
  group_by(inter,tperiods,pt.int)%>%
  mutate(pre.pst=ifelse(pre.pst==1& dt-DateTime==min(dt-DateTime),1,0),
         post.pst=ifelse(post.pst==1& DateTime-dt==min(DateTime-dt),1,0))%>%
  ungroup(tperiods,pt.int)%>%
  mutate(keep.inter=ifelse(sum(pre.pst)!=0 & sum(post.pst)!=0,1,0))%>%
  filter(keep.inter==1)%>%
  group_by(inter,tperiods)

spl3a<-spl3 %>% 
  ungroup()%>%
  filter(ferry.prd==1)%>%
  group_by(inter)%>%
  summarize(ferryspl=mean(SPL))%>%
  filter(ferryspl>110)

spl3<-filter(spl3,inter %in%spl3a$inter)
  

spl4<-spl3%>%
  filter(pre.pst==1)%>%
  ungroup()%>%
  select(inter,pre.dt=DateTime)%>%
  mutate(pre.int=interval(pre.dt,pre.dt+minutes(5)))
spl5<-spl3%>%
  filter(post.pst==1)%>%
  ungroup()%>%
  select(inter,post.dt=DateTime)%>%
  mutate(post.int=interval(post.dt-minutes(5),post.dt))

spl3<-spl3%>%
  ungroup()%>%
  left_join(spl4)%>%
  left_join((spl5))%>%
  mutate(pre.prd=if_else(DateTime %within% pre.int,1,0),
         post.prd=if_else(DateTime %within% post.int,1,0),
         prd=case_when(
           pre.prd==1~"pre",
           post.prd==1~"post",
           ferry.prd==1~"ferry",
           pre.prd==0 & post.prd==0 & ferry.prd ==0~"none"))



  
  



interlist2<-unique(spl3$inter)
for(i in 1:length(interlist2)){
  p1<-spl3%>%
    filter(inter==interlist2[i])
    ggplot(data=p1)+
    geom_line(aes(y=SPL,x=DateTime,group=inter))+
    geom_line(aes(y=88,x=DateTime,group=inter,color=prd),size=1.5)+
      scale_color_manual(values=c("red","white","red","red"),name="Potential interval")+
    theme_bw()+
    geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
    scale_x_datetime(date_minor_breaks="5 mins")+
      theme(legend.position="none")
  ggsave(paste0("manual_figures/fig_",p1$Year[i],p1$Month[i],p1$Day[i],"_withperiods.jpg"))
}

# going to look at/listen to these potential ones. First have to link to file
r19in<-read_rds("wdata/spl_file.rds")

ftu<-r19in %>%
  filter(DateTime %in% spl3$DateTime)%>%
  dplyr::select(stfile,DateTime)%>%
  distinct()%>%
  left_join(spl3)%>%
  dplyr::select(stfile,inter,pre.int,ferry.int,post.int)%>%
  distinct()

write.csv(ftu,"wdata/periods_to_examine.csv")

