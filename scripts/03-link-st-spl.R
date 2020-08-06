# A project to determine how COVID-19 has impacted the soundscape around Nanaimo, BC

# this script links soundtrap files to spl measurements

# bring in a list of soundtrap files
# change file paths to reflect your machine
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(lubridate))install.packages("lubridate");library(lubridate)
if(!require(readxl))install.packages("readxl");library(readxl)

#r19in<-list.files(path = "E:/RCA_IN/April_July2019/1342218252",pattern = "*.wav")
#write.csv(r19in,"wdata/rca_in_2019_stfiles.csv")
r19in<-read.csv("wdata/rca_in_2019_stfiles.csv")[,-1]
# note for some reason my rca out file is empty in the #3 harddrive so I'll only work with the rca in data

# now create a data frame with year, month, day, hour, minute as well as the full date

r19in2<-tibble(stfile=r19in) %>%
  separate(stfile,into=c("st","y","m","d","h","min","s","e"),
           sep=c(11,13,15,17,19,21,23),
           remove=FALSE)%>%
  select(-st,-e)%>%
  mutate(y=2019,
         site="RCA_In",
         DateTime=ymd_hms(paste(y,m,d,h,min,s)),
         etime=lead(DateTime,default=ymd_hms(20190625054114))-seconds(1),# the default statement here adds in the 
         # time 30 minutes after the last file name. if the recording interval is shorter for 2020
         # this should be adjusted. Also all attempts to extract that value automatically
         # produced errors for me so I had to enter it mannually.
         tint=interval(start=DateTime,end=etime))
 #        tint=interval(start=DateTime,end=etime))
# bring in minute by minute SPL data
in19 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
# make DateTime have the correct year
year(in19$DateTime)<-2019
# link spl and soundtrap file
# note this is slow and I am 100% sure there is a better way to do it but this is 
# what I got for now and it works...eventually
in19$stfile<-NA
for(i in 1:nrow(in19)){
  in19$stfile[i]<-r19in2[in19$DateTime[i] %within% r19in2$tint,1]
}


                