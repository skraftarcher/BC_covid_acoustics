# script to compare automatic detections and mannual checking

# First- just some code to subset down to already made selections to check how the amplification worked.
library(tidyverse)

temp1<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                                files = "boat_passage_autoselect.txt",
                                all.data = TRUE) 

temp2<-temp1[!is.na(temp1[,21]),]

write.table(temp2, file = "w.selection.tables/boat_passage_check_amp.txt", sep = "\t", row.names = FALSE, quote = FALSE)
