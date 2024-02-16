# Organize detector check data

# load packages
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="lubridate")
lp(pck="readxl")
lp("Rraven")

# load data
# one minute chunks
am.check<-Rraven::imp_raven(path = "w.selection.tables/",
                            files = "minutes_to_evaluate.txt",
                            all.data = TRUE)%>%
  select(-selec.file)
# five minute chunks
check19<-Rraven::imp_raven(path = "w.selection.tables",
                           files = "boat_passage_random_selections_updated_Dec152020.txt",
                           all.data = TRUE)%>%
  select(-selec.file)
#follow up chunks
follow.check<-Rraven::imp_raven(path = "w.selection.tables/",
                                files = "followup_minutes_to_evaluate.txt",
                                all.data = TRUE)%>%
  select(-selec.file)

#2020 chunks
check20<-Rraven::imp_raven(path = "w.selection.tables/",
                           files = "2020review.txt",
                           all.data = TRUE)%>%
  select(-selec.file)%>%
  filter(man.class!="")%>%
  filter(!is.na(man.class))

all19<-read_rds("wdata/alldata2019.rds")
all20<-read_rds("wdata/alldata2020.rds")


# organize data

#look and see if there are typos in the manual class
table(am.check$`Manual Class`) # all good

am2<-am.check%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="original")

table(check19$`Manual Class`) #all good

five2<-check19%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="boatpassage")

table(follow.check$man.class)# there's a few typos

follow2<-follow.check%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(man.class %in% c("F","G",""),1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="followup")

table(check20$man.class)# there's a few typos

man20<-check20%>%
  separate(`Begin File`,into=c("st","dtime","ext"),sep=c(-16,-4),remove=FALSE)%>%
  mutate(dtime=ymd_hms(dtime),
         dtime2=dtime+`File Offset (s)`,
         yr=year(dtime2),
         mnth=month(dtime2),
         d=day(dtime2),
         hr=hour(dtime2),
         mins=minute(dtime2),
         man=ifelse(man.class %in% c("F","K"),1,0))%>%
  group_by(yr,mnth,d,hr,mins,`Begin File`)%>%
  summarize(man.calls=sum(man))%>%
  mutate(reviewgroup="2020")

# group all 2019 

man19<-bind_rows(am2,five2,follow2)

# link autodetection stuff

dcheck19<-left_join(man19,all19)%>%
  filter(!is.na(SPL))
dcheck20<-left_join(man20,all20)%>%
  filter(!is.na(SPL))

#write out these datasets
write_rds(dcheck19,"wdata/detector_check_2019.rds")
write_rds(dcheck20,"wdata/detector_check_2020.rds")

# organize data to output for Xavier

all192<-all19%>%
  select(-fish.calls,-wind_spd,-wind_dir,-precip_amt,-tide,-wave.ht,
         -wave.prd,-wave.ht2,-wave.prd2,-temp)

all202<-all20%>%
  select(-fish.calls,-wind_spd,-wind_dir,-precip_amt,-tide,-wave.ht,
         -wave.prd,-wave.ht2,-wave.prd2,-temp)

xav19<-check19%>%
  select(-Class,-'Sound type',-'Software',-Confidence,-selec.file1,-Selection_X,
         -updated.class,-updated.confidence,-Inter,-Period,-`Manual Type`)%>%
  filter(`Manual Class`%in%c("NH","N","F","FS"))%>%
  separate(`Begin File`,into=c("pre","dt","ext"),sep="\\.",remove = FALSE)%>%
  separate(dt,into=c("yr","mnth","d","hr","mins","sec"),sep=c(2,4,6,8,10),convert = TRUE)%>%
  mutate(yr=yr+2000)%>%
  select(-sec,-pre,-ext)%>%
  left_join(all192)

xav20<-check20%>%
  select(-man.type,-Class,-`Sound type`,-Software,-Confidence,-stfile,
         -st,-s,-ext,-into.file,-datetime,-old.selection)%>%
  mutate(man.class=case_when(
    man.class=="F"~"F",
    man.class=="N"~"N",
    man.class=="B"~"N",
    man.class=="K"~"F",
    man.class=="FS"~"FS",
    man.class=="NF"~"N"))%>%
  rename(SPL.Int=spl.interval,`Manual Class`=man.class,mnth=m,mins=min,Comments=comments)%>%
  left_join(all202)

xav20b<-bind_rows(xav19[0,],xav20)

# get the SPL Interval for each file and SPL
spl.ints<-am.check[,colnames(am.check)%in%colnames(xav19)]%>%
  separate(`Begin File`,into=c("st","dt","ext"),sep="\\.",remove=FALSE)%>%
  separate(dt,into=c("yr","mnth","d","hr","mins","sec"),sep=c(2,4,6,8,10),convert=TRUE)%>%
  mutate(yr=yr+2000,
         dt=ymd_hms(paste(yr,mnth,d,hr,mins,sec)))%>%
  select(`Begin File`,dt,Selection)

spl.ints$spl.row<-findInterval(spl.ints$dt,all192$SPL.start.time)

spl.ints$SPL.Int<-all192$SPL.Int[spl.ints$spl.row]

spl.ints<-spl.ints[,c(-2,-4)]

xav192<-am.check[,colnames(am.check)%in%colnames(xav19)]%>%
  select(-SPL)%>%
  left_join(spl.ints)%>%
  left_join(all192)

xav192b<-bind_rows(xav19[0,],xav192)

xav193<-follow.check%>%
  rename(SPL.Int=spl.interval,`Manual Class`=man.class,mnth=m,mins=min)

xav193<-xav193[,colnames(xav193) %in% colnames(xav19)]%>%
  select(-yr,-mnth,-d,-mins,-hr)%>%
  left_join(all192)

xav193b<-bind_rows(xav19[0,],xav193)

xav2019<-bind_rows(xav19,xav192b,xav193b)%>%
  arrange(`Begin File`,`File Offset (s)`)%>%
  mutate(Selection=row.names(.))%>%
  distinct()

xav2020<-xav20b%>%
  arrange(`Begin File`,`File Offset (s)`)%>%
  mutate(Selection=row.names(.))%>%
  distinct()

write.table(xav2019,# the object you want to export as a selection table 
            file = "selection.tables.xavier/all2019selections.txt",# the path and file name using the file extension .txt
            sep = "\t", #how to delineate data
            row.names = FALSE, #row names will mess things up
            quote = FALSE)#putting things in quotes will mess things up.

write.table(xav2020,# the object you want to export as a selection table 
            file = "selection.tables.xavier/all2020selections.txt",# the path and file name using the file extension .txt
            sep = "\t", #how to delineate data
            row.names = FALSE, #row names will mess things up
            quote = FALSE)#putting things in quotes will mess things up.
