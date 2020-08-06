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
  mutate(y=2019,
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
  select(-ID,-dt)


             
  