#Make training data sets

am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)

# create the "answer" key

am.answer<-am.check%>%
  filter(`Manual Class`!="")%>%
  mutate(review.class=case_when(
    Class=="FS" & `Manual Class`=="F"~"F",
    Class=="FS" & `Manual Class`=="N"~"Auto.F",
    Class=="NN" & `Manual Class`=="F"~"F",
    Class=="NN" & `Manual Class`=="N"~"N",
    Class=="FS" & `Manual Class`=="FS"~"F",
    Class=="NN" & `Manual Class`=="FS"~"F",
    is.na(Class) & `Manual Class`=="F"~"F"))

am.training<-am.check%>%
  filter(`Manual Class`!="")%>%
  select(Selection,View,Channel,`Begin Time (s)`, `End Time (s)`,`Delta Time (s)`,
         `Low Freq (Hz)`,`High Freq (Hz)`,`Begin Path`,`File Offset (s)`,`Begin File`,
         interval,SPL,spl.group)%>%
  arrange(`Begin File`,`File Offset (s)`)%>%
  mutate(Selection=row_number(),Class="",Type="",comments="")

write.table(am.training,file="w.selection.tables/training_table.txt", sep = "\t", row.names = FALSE, quote = FALSE)


