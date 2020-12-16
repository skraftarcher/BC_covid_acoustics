# script to update automatic detection classifications

#bring in Xavier's selection tables
library(tidyverse)

#only have to run this bit once. If running a second time you can start on line 18
temp1<-list.files("selection.tables/RCA_in_April_July2019_1342218252_updated/", pattern = "*.txt")

for(i in 1:length(temp1)){
  temp2<-Rraven::imp_raven(path = "selection.tables/RCA_in_April_July2019_1342218252_updated/",
  files = temp1[i],
  all.data = TRUE) 
if(i==1) new.files<-temp2
if(i!=1) new.files<-bind_rows(new.files,temp2)
}

write.table(new.files,file = "w.selection.tables/Autodetect_Updated_April_July2019_Dec152020.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# if running a second time start below here

new.files<-Rraven::imp_raven(path = "w.selection.tables/",
                             files = "Autodetect_Updated_April_July2019_Dec152020.txt",
                             all.data = TRUE)

#bring in original manual review dataset

check19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_amp_10_Nov32020.txt",
                           all.data = TRUE) 

check.file<-unique((check19$`Begin File`))


# going to match new classifications by file offset and begin file

new.files2<-new.files[new.files[,9]%in%check.file,-16:-17]%>%
  select(`Begin File`,
         `File Offset (s)`,
         `Delta Time (s)`,
         `High Freq (Hz)`,
         updated.class=Class,
         updated.confidence=Confidence)%>%
  mutate(dtime=round(`Delta Time (s)`,2),
         fstime=round(`File Offset (s)`,2),
         hzs=round(`High Freq (Hz)`,3))%>%
  select(-`Delta Time (s)`,-`File Offset (s)`,-`High Freq (Hz)`)%>%
  distinct()

check19<-check19%>%
  mutate(dtime=round(`Delta Time (s)`,2),
         fstime=round(`File Offset (s)`,2),
         hzs=round(`High Freq (Hz)`,3))
  
u19<-left_join(check19,new.files2)%>%
  select(-dtime,-fstime,-hzs)

write.table(u19,file="w.selection.tables/boat_passage_random_selections_updated_Dec152020.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# forgot to update file path

u19<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE) 

u19[,9] <- paste0("D:/RCA_IN/April_July2019/amplified_10/",u19[,9])
u19<-u19[,-26:-27]
write.table(u19,file="w.selection.tables/boat_passage_random_selections_updated_Dec152020.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# I'm not convinced by my matching. So I'm going to make a dataset of his new classifications that
# only includes the files that I'm doing my checking on. Then to check the
# correlation between manual and automatic I will look at the # of calls per 
# minute in each dataset.

new.files3<-new.files[new.files[,9]%in%check.file,-16:-17]

write.table(new.files3,file="w.selection.tables/USE_THIS_FOR_CHECKING_AUTODETECTOR.txt", sep = "\t", row.names = FALSE, quote = FALSE)

