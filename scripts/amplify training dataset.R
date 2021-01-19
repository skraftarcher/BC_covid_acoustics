# script to change file path and move files that need to be amplified
install.packages("Rraven")
s.table<-Rraven::imp_raven(path = "w.selection.tables/",#update this to reflect where you saved the training_table.txt file
                            files = "training_table.txt",
                            all.data = TRUE)
# from here on out your working with the file structure on the external hard drive so you should only have to change the drive (i.e., D:/ might become E:/)

s.table$`Begin Path`<-paste0("D:/RCA_IN/April_July2019/amplified_10/", #update this to reflect the file path for your computer
                             s.table$`Begin Path`)

ftm<-s.table$`Begin File`[!(s.table$`Begin File` %in% list.files("D:/RCA_IN/April_July2019/amplified_10/"))] # same as above
ftm<-unique(ftm)

#update the paths here to reflect your computer
# you might need to create a "toamplify" folder. If so, do that before running this code. If not, go in and within
# the "toamplify" folder create another folder called "old" and move the files in the "toamplify" folder into that folder
# before running this code
for (i in 1:length(ftm)){
  filesstrings::file.move(paste0("D:/RCA_IN/April_July2019/1342218252/",ftm[i]),
                          "D:/RCA_IN/April_July2019/toamplify")
}

# after you run this code we will move to raven pro to batch amplify the files that still need to be amplified