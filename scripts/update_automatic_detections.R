# script to update automatic detection classifications

#bring in Xavier's selection tables

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

# going to match new classifications by file offset and begin file

new.files2<-new.files%>%
  select("File Offset (s)",`Delta Time (s)`,"Begin File",updated.class=Class,updated.confidence=Confidence)

updated19<-left_join(check19,new.files2)

write.table(updated19,file="w.selection.tables/boat_passage_random_selections_updated_Dec152020.txt")


