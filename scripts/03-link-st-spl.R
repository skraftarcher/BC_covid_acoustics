# A project to determine how COVID-19 has impacted the soundscape around Nanaimo, BC

# this script links soundtrap files to spl measurements

# bring in a list of soundtrap files
# change file paths to reflect your machine
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(lubridate))install.packages("lubridate");library(lubridate)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(filesstrings))install.packages("filesstrings");library(filesstrings)

# r19in<-list.files(path = "E:/RCA_IN/April_July2019/1342218252",pattern = "*.wav")

#for mac
# note for some reason my rca out file is empty in the #3 harddrive so I'll only work with the rca in data
# r19in<-list.files(path = "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252",pattern = "*.wav")
# write.csv(r19in,"wdata/rca_in_2019_stfiles.csv")
r19in<-read.csv("wdata/rca_in_2019_stfiles.csv")[,-1]

# r20in<-list.files(path = "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/",pattern = "*.wav")
# write.csv(r20in,"wdata/rca_in_2020_stfiles.csv")
r20in<-read.csv("wdata/rca_in_2020_stfiles.csv")[,-1]


# now create a data frame with year, month, day, hour, minute as well as the full date

r19in2<-tibble(stfile=r19in) %>%
  separate(stfile,into=c("st","y","m","d","h","min","s","e"),
           sep=c(11,13,15,17,19,21,23),
           remove=FALSE)%>%
  mutate(y=2019,
         site="RCA_In",
         dt=ymd_hms(paste(y,m,d,h,min,s)),
         ID=row_number())%>%
  select(stfile,dt,site,ID)


r20in2<-tibble(stfile=r20in) %>%
  separate(stfile,into=c("st","y","m","d","h","min","s","e"),
    sep=c(5,7,9,11,13,15,17),
    remove=FALSE)%>%
  mutate(y=2020,
    site="RCA_In",
    dt=ymd_hms(paste(y,m,d,h,min,s)),
    ID=row_number())%>%
  select(stfile,dt,site,ID)
         
# bring in minute by minute SPL data
in19 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")

# make DateTime have the correct year
year(in19$DateTime)<-2019

# link spl and soundtrap file
in19<-cbind(in19,ID=findInterval(in19$DateTime,r19in2$dt))%>%
  left_join(r19in2)%>%
  mutate(Deployment = 0) %>%
  select(-ID)


# repeat for 2020 if available
in20 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2020")
year(in20$DateTime)<-2020
in20<-cbind(in20,ID=findInterval(in20$DateTime,r20in2$dt))%>%
  left_join(r20in2)%>%
  select(-ID)


# # if you just have 2019
# in20 <- NULL

# if you have both years data
# check they match 
head(in19)
head(in20)
spl_data <- rbind(in19, in20)

# looks good?
head(spl_data)
tail(spl_data)

# save this for future uses
write_rds(spl_data,"wdata/spl_file.rds")

(in_sample_days <- spl_data %>% group_by(Year, Month, Day) %>% 
  mutate(mean_daily = median(SPL), 
    top95th_daily_spl = quantile(SPL, 0.95),
    top75th_daily_spl = quantile(SPL, 0.75),
    top25th_daily_spl = quantile(SPL, 0.25),
    top5th_daily_spl = quantile(SPL, 0.05),
    Week = lubridate::week(DateTime)) %>% 
  ungroup() %>%
  filter(
    (DateTime <= as.Date("2019-05-10") & DateTime>= as.Date("2019-04-14")) | 
      (DateTime>= as.Date("2019-05-20") & DateTime<= as.Date("2019-06-25")) |
      (DateTime <= as.Date("2020-05-10") & DateTime>= as.Date("2020-04-19"))
    ) %>% 
  group_by(Week) %>% 
  mutate(min_daily_spl = min(mean_daily), 
    min_daily_95th_spl= min(top95th_daily_spl),
    min_daily_75th_spl= min(top75th_daily_spl),
    min_daily_25th_spl= min(top25th_daily_spl),
    min_daily_5th_spl= min(top5th_daily_spl),
    n_days = n()/1440) %>% 
  # filter(top95th_daily_spl == min_daily_95th_spl) %>%
  # filter(top75th_daily_spl == min_daily_75th_spl) %>%
  # filter(mean_daily == min_daily_spl) %>%
    filter(top25th_daily_spl == min_daily_25th_spl) %>%
    # filter(top5th_daily_spl == min_daily_5th_spl) %>%
    # ggplot() + geom_histogram(aes(SPL)) + xlim(85,145)
    ungroup() 
  )

in_files <- in_sample_days %>% mutate(
  spl_hr = lubridate::hour(DateTime),
  file_hr = lubridate::hour(dt)
  ) %>% group_by(Week) %>% filter(file_hr == spl_hr) %>% 
  group_by(Week, file_hr) %>% mutate(min_file_time = min(dt)) %>% ungroup() %>% 
  filter(dt == min_file_time & n_days > 4) %>% 
  select(Deployment, Week, file_hr, stfile) %>% 
  distinct()

in19_files <- filter(in_files, Deployment == 0) 

# if you have both years
in20_files_1 <- filter(in_files, Deployment == 1) 
# in20_files_2 <- filter(in_files, Deployment == 2) # not used yet


### move selected files to new folder
### for mac, run just once
# for (i in 1:nrow(in19_files)){
# file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", in19_files$stfile[i]),
#   "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/quiet_days")
# }
# for (i in 1:nrow(in20_files_1)){
# file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", in20_files_1$stfile[i]),
#   "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/quiet_days_1")
# }
#
# for steph
# for (i in 1:nrow(in19_files)){
# file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",in19_files$stfile[i]),
#   "E:/RCA_IN/April_July2019/quiet_days")
# }
