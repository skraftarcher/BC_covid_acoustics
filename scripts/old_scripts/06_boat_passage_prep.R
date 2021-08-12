

#only have to run this bit once. If running a second time you can start on line 34
# functionize merging of selection tables
merge_selection_tabs <- function(folder_path){
  filelist <-list.files(folder_path, pattern = "*.txt")
  for(i in 1:length(filelist)){
    temp2<-Rraven::imp_raven(path = folder_path,
      files = filelist[i],
      all.data = TRUE) 
    if(i==1) new.files<-temp2
    if(i!=1) new.files<-dplyr::bind_rows(new.files,temp2)
  }
  new.files$stfile <- new.files[[11]]
  new.files$into.file <- new.files[[10]]
  new.files
}

# 2019 data
new.files <- merge_selection_tabs("selection.tables/RCA_in_April_July2019_1342218252_updated/")
write.table(new.files, file = "w.selection.tables/Autodetect_Updated_April_July2019_Jan2021.txt", 
  sep = "\t", row.names = FALSE, quote = FALSE)

# 2020 data
new.files2 <- merge_selection_tabs("selection.tables/RCA_In_200418_1205_5047/")
write.table(new.files2,file = "w.selection.tables/Autodetect_RCA_In_200418_1205_5047_Jan2021.txt", 
  sep = "\t", row.names = FALSE, quote = FALSE)

new.files3 <- merge_selection_tabs("selection.tables/RCA_In_200524_1149_5042/")
write.table(new.files3,file = "w.selection.tables/Autodetect_RCA_In_200524_1149_5042_Jan2021.txt", 
  sep = "\t", row.names = FALSE, quote = FALSE)


# if running a second time start below here
# Code to extract chosen boat passage "Intervals" from selection tables ----

library(tidyverse)
library(lubridate)

# load selected 5min periods for ferry analysis
periods <- read.csv("wdata/files_to_evaluate_all.csv") 
# allperiods <- allperiods %>% filter(Year==2020)

allperiods <- periods %>% mutate(
  dt1 = ymd_hms(strt), # to the second
  dt2 = dt1+300 # add 5min in seconds
) %>% 
  arrange(dt1) %>%
  select(-X, -stfile, -into.file, -timediff) %>% 
  distinct() %>%
  mutate(st = dt1, et = dt2) %>% # create the window to match to
  pivot_longer(st:et, names_to = "tt", values_to = "tint") %>% # turn it in to a long data format so we
  mutate(split_ids = row_number()) # hoping that morning passages are the even numbers following the odd for the prior midnight passage

# I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure
allperiods2 <- allperiods %>%
  select(-tint) %>%
  pivot_wider(names_from = tt, values_from = split_ids) %>%
  select(-et) %>%
  rename(period_id = st)

# if the number of columns changes above this needs to be updated
# load auto detector data
all2019 <- Rraven::imp_raven(path = "w.selection.tables/",
  files = "Autodetect_Updated_April_July2019_Jan2021.txt",
  all.data = TRUE)[,-19]

new.files2<-Rraven::imp_raven(path = "w.selection.tables/",
  files = "Autodetect_RCA_In_200418_1205_5047_Jan2021.txt",
  all.data = TRUE)[,-19]

new.files3<-Rraven::imp_raven(path = "w.selection.tables/",
  files = "Autodetect_RCA_In_200524_1149_5042_Jan2021.txt",
  all.data = TRUE)[,-19]

all2020 <- bind_rows(new.files2,new.files3)

# create a date time variable in each detection dataset
#all2019 <- new.files

all2019 <-separate(all2019, stfile,
  into = c("st","yr","m","d","hr","min","s","ext"), 
  sep = c(11,13,15,17,19,21,23), remove=FALSE, convert = TRUE)

all2020 <- separate(all2020, stfile, 
  into = c("st","yr","m","d","hr","min","s","ext"), 
  sep = c(5,7,9,11,13,15,17), remove=FALSE, convert = TRUE)

alldata <- bind_rows(all2019,all2020) %>%
  mutate(yr=paste0(20,yr),
    datetime=ymd_hms(paste(yr,m,d,hr,min,s)),
    datetime=datetime+into.file,
    yr=year(datetime),
    m=month(datetime),
    d=day(datetime),
    hr=hour(datetime),
    min=minute(datetime),
    s=second(datetime))%>%
  arrange(datetime)

# saveRDS(alldata, file = "wdata/auto_all_spring_w_noise.rds")

# link detections to chosen 5 min periods and keep only those periods
alldata$period_id <- findInterval(alldata$datetime, allperiods$tint)
linkdata <- left_join(alldata, allperiods2) %>% filter(!is.na(inter))

saveRDS(linkdata, file = "wdata/auto_all_periods_w_noise.rds")

linkdata <- linkdata %>% select(-strt) 
linkdata <- linkdata %>% mutate(
  timemin = ymd_hm(paste(yr,m,d,hr,min))
) 

# bring in SPL data 
spl<-readRDS("wdata/spl_by_min.rds")

year(spl$DateTime) <- spl$year
spl <- arrange(spl, DateTime) %>% # note: so far all datetime vars have different names
  filter(rca == "in") %>% select(-Year, -Deployment, -year)

spl$period_id <- findInterval(spl$DateTime, allperiods$tint)
spl_means <- spl %>% group_by(period_id) %>% summarise(spl_mean = mean(SPL, na.rm = T))
linkdata2 <- left_join(linkdata, spl_means)

spl$timemin <- spl$DateTime
second(spl$timemin) <- 60
spl_mins <- spl %>% select(timemin, SPL)

linkdata3 <- left_join(linkdata2, spl_mins)
saveRDS(linkdata3, file = "wdata/auto_all_periods_w_noise.rds")


fishonly <- filter(linkdata3, Class == "FS")
saveRDS(fishonly, file = "wdata/auto_all_periods_fishonly.rds")

