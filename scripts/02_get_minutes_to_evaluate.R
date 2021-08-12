# # script to determine which minutes to analyze to get a better range of SPLs for
# # checking the detector

source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="Rraven")
lp(pck="lubridate")

# # bring in full 2019 automatic dectection dataset

alldata2019<-read_rds("wdata/alldata2019.rds")
alldata2020<-read_rds("wdata/alldata2020.rds")

# # bring in dataset that tells me which minutes I've already done
already.done<-read_rds("wdata/first_check_of_detector_Dec162020.rds")%>%
   filter(p.noobs==0)

ad2<-unique(already.done$interval)


alldata2019<-alldata2019%>%
  mutate(already.done=ifelse(SPL.Int %in% ad2,1,0))

# look at histogram of SPL
 
hist(alldata2019$SPL)
min(alldata2019$SPL)
max(alldata2019$SPL)
 
# Maybe break it into 10 sections?
spl.quantiles<-quantile(alldata2019$SPL,c(.10,.20,.30,.40,.50,.60,.70,.80,.90))

alldata2019<-alldata2019 %>%
  mutate(spl.group=case_when(
         SPL<=spl.quantiles[1]~1,
         SPL<=spl.quantiles[2] & SPL>spl.quantiles[1]~2,
         SPL<=spl.quantiles[3] & SPL>spl.quantiles[2]~3,
         SPL<=spl.quantiles[4] & SPL>spl.quantiles[3]~4,
         SPL<=spl.quantiles[5] & SPL>spl.quantiles[4]~5,
         SPL<=spl.quantiles[6] & SPL>spl.quantiles[5]~6,
         SPL<=spl.quantiles[7] & SPL>spl.quantiles[6]~7,
         SPL<=spl.quantiles[8] & SPL>spl.quantiles[7]~8,
         SPL<=spl.quantiles[9] & SPL>spl.quantiles[8]~9,
         SPL>spl.quantiles[9]~10))

# look at how many I've already done per "group"

(spl3<-alldata2019%>%
  group_by(spl.group)%>%
  summarize(min.done=sum(already.done)))

# Do 25 per group
mins.to.do_a<-alldata2019%>%
 filter(already.done!=1)
 
mins.to.do<-mins.to.do_a%>%
  group_by(spl.group)%>%
  sample_n(25,replace=FALSE)%>%
  ungroup()%>%
  separate(`Begin File`,
           into=c("st","datetime","ext"),
           sep=c(-16,-4),
           remove=FALSE)%>%
  mutate(file.startdate=ymd_hms(datetime),
         sec.into.file=SPL.start.time-file.startdate)%>%
  select(`Begin File`,sec.into.file)


write.csv(mins.to.do,"wdata/minutes_to_evaluate_master_sheet.csv")
 
 
ftm<-unique(mins.to.do$`Begin File`)
# 
# for (i in 1:length(ftm)){
#     filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
#       "D:/RCA_IN/April_July2019/toamplify")
# }
#
for(i in 1:length(ftm)){
  if(i==1)sfiles<-list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern = ftm[i])
  if(i!=1)sfiles<-c(sfiles,list.files("selection.tables/RCA_in_April_July2019_1342218252_updated",pattern = ftm[i]))
}
mins.to.do2<-imp_raven(path="selection.tables/RCA_in_April_July2019_1342218252_updated",
                       files=sfiles,
                       all.data = TRUE)


