# put together selection tables for manual review of boat passages

source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="lubridate")
lp(pck="Rraven")

# bring in data
all19<-read.csv("wdata/periods_to_evaluate_19.csv", stringsAsFactors = F)[,-1]
all20<-read.csv("wdata/periods_to_evaluate_20.csv", stringsAsFactors = F)[,-1]

#collect files
files<-c(unique(all19$stfile),unique(all20$stfile))

for(i in 1:length(files)){
  if(i==1)dfiles<-list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern=files[i])
  if(i!=1)dfiles<-c(dfiles,list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern=files[i]))
  if(i!=1)dfiles<-c(dfiles,list.files("selection.tables/RCA_In_200418_1205_5047",pattern=files[i]))
  if(i!=1)dfiles<-c(dfiles,list.files("selection.tables/RCA_In_200524_1149_5042",pattern=files[i]))
}

# bring in files

selec.tables19<-imp_raven(path="selection.tables/RCA_in_April_July2019_1342218252_updated",
                        files=dfiles[files%in%unique(all19$stfile)],
                        all.data = TRUE)
                        
selec.tables20<-imp_raven(path="selection.tables/RCA_In_200418_1205_5047",
                                  files=dfiles[grep(dfiles,pattern="5047.20")],
                                  all.data = TRUE)

selec.tables192<-selec.tables19%>%
  separate(`Begin File`,into=c("st","filedt","ext"),sep=c(-16,-4),remove = FALSE)%>%
  mutate(filedt=ymd_hms(filedt),
         call.time=filedt+`File Offset (s)`)%>%
  select(-st,-ext)

selec.tables202<-selec.tables20%>%
  separate(`Begin File`,into=c("st","filedt","ext"),sep=c(-16,-4),remove = FALSE)%>%
  mutate(filedt=ymd_hms(filedt),
         call.time=filedt+`File Offset (s)`)%>%
  select(-st,-ext)

all19<-all19%>%
  mutate(strt=ymd_hms(strt),
         end=strt+300,
         int=interval(start=strt,end = end))

all20<-all20%>%
  mutate(strt=ymd_hms(strt),
         end=strt+300,
         int=interval(start=strt,end = end))
idxa<-NULL
idxb<-NULL

for(i in 1:nrow(selec.tables192)){
  idxa[i]<-ifelse(length(which(selec.tables192$call.time[i] %within% all19$int))==0,FALSE,TRUE)
  idxb[i]<-ifelse(length(which(selec.tables192$call.time[i] %within% all19$int))==0,0,which(selec.tables192$call.time[i] %within% all19$int))
  
}

selec.tables193<-selec.tables192[idxa,]%>%
  select(-filedt,-selec.file)%>%
  mutate(old.selection=Selection)

selec.tables193$Selection<-seq_len(nrow(selec.tables193))

idx2<-NULL
idx2b<-NULL

for(i in 1:nrow(selec.tables202)){
  idx2[i]<-ifelse(length(which(selec.tables202$call.time[i] %within% all20$int))==0,FALSE,TRUE)
  idx2b[i]<-ifelse(length(which(selec.tables202$call.time[i] %within% all20$int))==0,0,which(selec.tables202$call.time[i] %within% all20$int))
}

selec.tables203<-selec.tables202[idx2,]%>%
  select(-filedt,-selec.file)%>%
  mutate(old.selection=Selection)

selec.tables203$Selection<-seq_len(nrow(selec.tables203))

selec.tables193$`Begin Path`<-paste0("E:\\RCA_IN\\April_July2019\\1342218252\\",selec.tables193$`Begin Path`)
selec.tables203$`Begin Path`<-paste0("E:\\RCA_IN\\April_July2020\\",selec.tables203$`Begin Path`)




# write.table(selec.tables193[1:60000,], file = "w.selection.tables/five_minute_passages_2019filesa.txt",
#             sep = "\t", row.names = FALSE, quote = FALSE)
# write.table(selec.tables193[60001:120000,], file = "w.selection.tables/five_minute_passages_2019filesb.txt",
#             sep = "\t", row.names = FALSE, quote = FALSE)
# write.table(selec.tables193[120001:163731,], file = "w.selection.tables/five_minute_passages_2019filesc.txt",
#             sep = "\t", row.names = FALSE, quote = FALSE)

#write.table(selec.tables203, file = "w.selection.tables/five_minute_passages_2020files.txt",
#            sep = "\t", row.names = FALSE, quote = FALSE)


# once you've updated the selection tables in Raven to include peak power density and 
# inband power reload selec.table193 and selec.table203 here
selec.tables193b<-imp_raven(path="w.selection.tables",
                           files=c("five_minute_passages_2019filesa.txt",
                                   "five_minute_passages_2019filesb.txt",
                                   "five_minute_passages_2019filesc.txt"),
                           all.data = TRUE)%>%
  select(-selec.file)


selec.tables203b<-imp_raven(path="w.selection.tables",
                            files="five_minute_passages_2020files_PE.txt",
                            all.data=TRUE)%>%
  select(-selec.file)
# get fish calls per interval
idxb<-idxb[idxa]

fishcalls19<-bind_cols(selec.tables193b,all19[idxb,colnames(all19)%in% c("inter","prd","type")])%>%
  mutate(fcalls=ifelse(Class=="FS",1,0))%>%
  group_by(inter,prd,type)%>%
  summarize(fish.calls=sum(fcalls))%>%
  mutate(yr=2019)

fishcalls192<-bind_cols(selec.tables193b,all19[idxb,colnames(all19)%in% c("inter","prd","type")])%>%
  filter(Class=="FS")%>%
  select(inter,prd,type,Confidence,inband.power=`Inband Power (dB FS)`,peak.power= `Peak Power Density (dB FS)`)%>%
  mutate(inband.power=inband.power+175.9,
         peak.power=peak.power+175.9,
         yr=2019)

write_rds(fishcalls192,"wdata/fishcall_power_2019.rds")
  
  

idx2b<-idx2b[idx2]

fishcalls20<-bind_cols(selec.tables203,all20[idx2b,colnames(all20)%in% c("inter","prd","type")])%>%
  mutate(fcalls=ifelse(Class=="FS",1,0))%>%
  group_by(inter,prd,type)%>%
  summarize(fish.calls=sum(fcalls))%>%
  mutate(yr=2020)

fishcalls202<-bind_cols(selec.tables203b,all20[idx2b,colnames(all20)%in% c("inter","prd","type")])%>%
  filter(Class=="FS")%>%
  select(inter,prd,type,Confidence,inband.power=`Inband Power (dB FS)`,peak.power= `Peak Power Density (dB FS)`)%>%
  mutate(inband.power=inband.power+176.3,
         peak.power=peak.power+176.3,
         yr=2020)

write_rds(fishcalls202,"wdata/fishcall_power_2020.rds")

fish.call.all<-bind_rows(fishcalls19,fishcalls20)

fish.call.all.power<-bind_rows(fishcalls192,fishcalls202)

write_rds(fish.call.all.power,"wdata/fishcall_power_all.rds")

# now get spl, wind speed, wave height, tide, precip for each interval
all19mins<-read_rds("wdata/alldata2019.rds")
all20mins<-read_rds("wdata/alldata2020.rds")

idx3<-NULL
idx3b<-NULL
idx4<-NULL
idx4b<-NULL

for(i in 1:nrow(all19mins)){
  idx3[i]<-ifelse(length(which(all19mins$SPL.start.time[i] %within% all19$int))==0,FALSE,TRUE)
  idx3b[i]<-ifelse(length(which(all19mins$SPL.start.time[i] %within% all19$int))==0,0,which(all19mins$SPL.start.time[i] %within% all19$int))
}

for(i in 1:nrow(all20mins)){
  idx4[i]<-ifelse(length(which(all20mins$SPL.start.time[i] %within% all20$int))==0,FALSE,TRUE)
  idx4b[i]<-ifelse(length(which(all20mins$SPL.start.time[i] %within% all20$int))==0,0,which(all20mins$SPL.start.time[i] %within% all20$int))
}

idx3b<-idx3b[idx3]
idx4b<-idx4b[idx4]

spl19<-bind_cols(all19mins[idx3,],all19[idx3b,colnames(all19)%in% c("inter","prd","type")])%>%
 group_by(inter,prd,type)%>%
  summarize(SPL.mean=mean(SPL,na.rm=T),
            SPL.median=median(SPL,na.rm=T),
            SPL.var=var(SPL,na.rm = T),
            SPL.max=max(SPL,na.rm=T),
            wind_spd=mean(wind_spd,na.rm=T),
            wind_dir=mean(wind_dir,na.rm=T),
            precip=mean(precip_amt,na.rm=T),
            tide=mean(tide,na.rm=T))

spl20<-bind_cols(all20mins[idx4,],all20[idx4b,colnames(all20)%in% c("inter","prd","type")])%>%
  group_by(inter,prd,type)%>%
  summarize(SPL.mean=mean(SPL,na.rm=T),
            SPL.median=median(SPL,na.rm=T),
            SPL.var=var(SPL,na.rm = T),
            SPL.max=max(SPL,na.rm=T),
            wind_spd=mean(wind_spd,na.rm=T),
            wind_dir=mean(wind_dir,na.rm=T),
            precip=mean(precip_amt,na.rm=T),
            tide=mean(tide,na.rm=T))

# group spl data and bind it to fish calls
all.boat.passage<-left_join(fish.call.all,bind_rows(spl19,spl20))
all.boat.passage.power<-left_join(fish.call.all.power,bind_rows(spl19,spl20))

# write out this dataset
write_rds(all.boat.passage,"wdata/all_boat_passage_data.rds")
write_rds(all.boat.passage.power,"wdata/all_boat_passage_data_power.rds")
