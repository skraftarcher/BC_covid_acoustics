# script to extract boat passages to examine further
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="lubridate")
lp(pck="readxl")
lp(pck="filesstrings")

### Taking ferry AIS approach----

# Bring in data ----
mv19 <- read_xlsx("odata/midnight_vessel.xlsx", sheet = "2019_RCA_In") # 2019 ais data
mv20 <- read_xlsx("odata/midnight_vessel.xlsx", sheet = "2020_RCA_In") # 2020 ais data
mv19am <- read_xlsx("odata/morning_vessel.xlsx", sheet = "2019_RCAIn") # 2020 ais data
mv20am <- read_xlsx("odata/morning_vessel.xlsx", sheet = "2020_RCAIN") # 2020 ais data
all2020<-read_rds("wdata/alldata2020.rds")
all2019<-read_rds("wdata/alldata2019.rds")

# Organize boat passage data ----
mnv <- bind_rows(mv19, mv20) %>%
  arrange(SPL.start.time) %>% # make sure dataset goes from earliest to latest
  # mutate(first_ferry=1) %>%
  mutate(st = SPL.start.time - minutes(60), et = SPL.start.time + minutes(60)) %>% # create the window to match to
  pivot_longer(st:et, names_to = "tt", values_to = "tint") %>% # turn it in to a long data format so we
  mutate(direction = "arriving") %>%
  mutate(inter = row_number()) %>%
  rename(boatdt = SPL.start.time) %>%
  select(-MMSI) # currently removing this because missing form morning data (note: 3 vessels included)

amv <- bind_rows(mv19am, mv20am) %>%
  filter(first_ferry == 1) %>%
  rename(SPL.start.time = SPL.start.time_PDT) %>%
  select(SPL.start.time, Distance) %>%
  # select(SPL.start.time,Distance,first_ferry)%>%
  arrange(SPL.start.time) %>% # make sure dataset goes from earliest to latest
  mutate(st = SPL.start.time - minutes(60), et = SPL.start.time + minutes(60)) %>% # create the window to match to
  pivot_longer(st:et, names_to = "tt", values_to = "tint") %>% # turn it in to a long data format so we
  mutate(direction = "departing") %>%
  rename(boatdt = SPL.start.time) %>%
  mutate(inter = row_number()) # hoping that morning passages are the even numbers following the odd for the prior midnight passage



### PM boat passage selection ----
### define odd inters for PM boat passages ----
all2019$inter <- findInterval(all2019$SPL.start.time, mnv$tint) # assign intervals for midnight passages
all2020$inter <- findInterval(all2020$SPL.start.time, mnv$tint) # assign intervals for midnight passages

alldata<-bind_rows(all2019,all2020)%>%
  mutate(Deployment=case_when(
    yr==2019~19.1,
    SPL.start.time <="2020-05-24"~20.1,
    SPL.start.time > "2020-05-24"~20.2))

# I'm 99% sure all intervals we want are odd, but I'm running this bit of code just to make sure
mnv2 <- mnv %>%
  select(-tint) %>%
  pivot_wider(names_from = tt, values_from = inter) %>%
  select(-et) %>%
  rename(inter = st)

# filter down only to intervals within 60 mins of the ferry passing (60 before or after)
mn_spl <- alldata %>%
  filter(inter %in% mnv2$inter) %>% 
  left_join(mnv2)

mn_spl2 <- mn_spl %>%
  mutate(Hr = hour(SPL.start.time)) %>% # create a date hr variable to link with wind
  group_by(inter) %>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2 = mean(wind_spd, na.rm = TRUE)) %>% # create a mean wind value for the interval
  filter(
    # !is.na(wsp2) & wsp2 <20 & # remove windy days 
    SPL.start.time < "2020-05-05" # remove non-continuous samples post 5/5/20
  ) %>%
  mutate(
    dt2 = boatdt - minutes(7),
    # create periods before the closest passage of the ferry and after
    tperiods = case_when(
      SPL.start.time < dt2 ~ "pre",
      SPL.start.time > boatdt ~ "post",
      SPL.start.time > dt2 & SPL.start.time < boatdt ~ "oth",
      SPL.start.time == boatdt ~ "oth"
    )
  ) %>% 
  group_by(inter, tperiods) %>%
  mutate(
    # create a ferry interval that is the 5 minutes before the closest passage
    ferry.int = interval(boatdt - minutes(6), boatdt - minutes(1)), 
    # create a new interval that indicates whether or not row is within the ferry interval
    ferry.prd = if_else(SPL.start.time %within% ferry.int, 1, 0), 
    pre.pst = case_when(
      tperiods == "pre" & ferry.prd != 1 & # find 5 minute periods in the period >5 min before closest passage
        zoo::rollmax(SPL, k = 5, fill = NA, align = "left") < 100 ~ 1, # where the maximum spl is < 100
      tperiods == "pre" & zoo::rollmax(SPL, k = 5, fill = NA, align = "left") > 100 ~ 0,
      tperiods == "pre" & is.na(zoo::rollmax(SPL, k = 5, fill = NA, align = "left")) ~ 0,
      tperiods == "pre" & ferry.prd == 1 ~ 0,
      tperiods == "post" ~ 0,
      tperiods == "oth" ~ 0
    ),
    post.pst = case_when(
      # find 5 minute periods in the post period where the maximum spl is < 100
      tperiods == "post" & zoo::rollmax(SPL, k = 5, fill = NA, align = "right") < 100 ~ 1, 
      tperiods == "post" & zoo::rollmax(SPL, k = 5, fill = NA, align = "right") > 100 ~ 0,
      tperiods == "post" & is.na(zoo::rollmax(SPL, k = 5, fill = NA, align = "right")) ~ 0,
      tperiods == "pre" ~ 0,
      tperiods == "oth" ~ 0
    ),
    # create a new variable that indicates whether or not the minute is part of a potential interval
    pt.int = ifelse(pre.pst == 1 | post.pst == 1, 1, 0) 
  ) %>%
  group_by(inter, tperiods, pt.int) %>%
  mutate(
    # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the pre-period
    pre.pst = ifelse(pre.pst == 1 & int_start(ferry.int) - SPL.start.time == min(int_start(ferry.int) - SPL.start.time), 1, 0), 
    # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the post-period
    post.pst = ifelse(post.pst == 1 & SPL.start.time - boatdt == min(SPL.start.time - boatdt), 1, 0)
  ) %>% 
  ungroup(tperiods, pt.int) %>%
  # only keep intervals where there is a qualifying period in both pre and post ferry periods
  mutate(keep.inter = ifelse(sum(pre.pst) != 0 & sum(post.pst) != 0, 1, 0)) %>% 
  filter(keep.inter == 1) %>%
  group_by(inter, tperiods)

mn_spl2a <- mn_spl2 %>%
  ungroup() %>%
  filter(ferry.prd == 1) %>%
  filter(Distance < 3000) %>% # only removes #57
  group_by(inter, yr, mnth, d) %>%
  summarize(ferryspl = max(SPL)) %>%
  filter(ferryspl > 105) # find intervals where the ferry passage interval has a mean spl over 105

mn_spl2 <- filter(mn_spl2, inter %in% mn_spl2a$inter) # subset down to those intervals

# calculate the pre-ferry period to analyze
mn_pre <- mn_spl2 %>%
  filter(pre.pst == 1) %>%
  ungroup() %>%
  select(inter, pre.dt = SPL.start.time) %>%
  mutate(pre.int = interval(pre.dt, pre.dt + minutes(5)))

# calculate the post-ferry period to analyze
mn_post <- mn_spl2 %>%
  filter(post.pst == 1) %>%
  ungroup() %>%
  select(inter, post.dt = SPL.start.time) %>%
  mutate(post.int = interval(post.dt - minutes(5), post.dt))

mn_spl2 <- mn_spl2 %>%
  ungroup() %>%
  left_join(mn_pre) %>%
  left_join((mn_post)) %>%
  mutate(
    pre.prd = if_else(SPL.start.time %within% pre.int, 1, 0), # assign a 1 to minutes within the pre period
    post.prd = if_else(SPL.start.time %within% post.int, 1, 0), # assign a 1 to minutes within the post period
    prd = case_when(
      pre.prd == 1 ~ "pre",
      post.prd == 1 ~ "post",
      ferry.prd == 1 ~ "ferry",
      pre.prd == 0 & post.prd == 0 & ferry.prd == 0 ~ "none"
    )
  ) # create a variable that indicates which period the minute belongs to

spl.inters <- mn_spl2 %>%
  filter(prd != "none")

# create a dataset of PM sound trap files to use ----
ftu <- spl.inters %>%
  dplyr::select(stfile=`Begin File`, SPL.start.time, inter, yr, Deployment, pre.int, ferry.int, post.int) %>%
  distinct() %>%
  separate(stfile, into=c("st","filedt","ext"),sep=c(-16,-4),remove=FALSE)%>%
  select(-st,-ext)%>%
  mutate(
    prd = case_when(
      SPL.start.time %within% pre.int ~ "pre",
      SPL.start.time %within% post.int ~ "post",
      SPL.start.time %within% ferry.int ~ "ferry"
    ),
    strt = case_when(
      prd == "pre" ~ int_start(pre.int),
      prd == "ferry" ~ int_start(ferry.int),
      prd == "post" ~ int_start(post.int)
    ),
    filedt=ymd_hms(filedt),
    into.file = strt - filedt
  ) %>%
  select(-pre.int, -ferry.int, -post.int)

# this is where it gets super inefficient
fcp <- ftu %>% # get the files to use
  ungroup() %>%
  select(inter, prd, strt) %>% # select only the interval, the period, and the start time
  distinct() %>% # go down to unique rows
  pivot_wider(names_from = prd, values_from = strt) %>% # make a wider data frame
  mutate(
    dymd = paste0(year(post), month(post), day(post)),
    pre2 = pre + minutes(5), # get the end of the pre-period
    ferry2 = ferry + minutes(5), # get the end of the ferry period
    pf = difftime(ferry, pre2, units = "mins"), # calculate the length of time between the end of the pre-period and the start of the ferry period
    fp = difftime(post, ferry2, units = "mins"), # calculate the length of time between the end of the ferry-period and the start of the post-period
    midtimediff = fp + 10,
    passlen = 15 + pf + fp
  ) %>% # calculate how long the boat period is
  select(inter, dymd, post, passlen, midtimediff) # only keep inter, post(start of the post period), and period length

### quantify longest periods of quiet
splq <- alldata %>%
  mutate(Hr = hour(SPL.start.time), dymd = paste0(yr, mnth, d)) %>% # create a date hr variable to link with wind
  group_by(Hr) %>%
  mutate(wsp2 = mean(wind_spd, na.rm = TRUE)) %>% # create a mean wind value for the interval
  filter(
    # !is.na(wsp2) & wsp2 <20 &
    SPL.start.time < "2020-05-05" & Hr < 5 & Hr >= 1
  ) %>%
  ungroup() %>%
  mutate(isq = ifelse(SPL < 100, 1, 0), inter = inter - 1) # assign isq (is quiet) a 1 if the spl is less than 105, 0 otherwise


### AM boat passage selection ----
### define even inters for AM boat passages ----
splam <- alldata %>% select(-inter)
# assign intervals such that morning passages are the even numbers following the odd for the prior midnight passage
splam$inter <- findInterval(splam$SPL.start.time, sort(amv$tint)) + 1 

# length(unique(mnv$inter))
# length(unique(amv$inter))

amv2 <- amv %>%
  select(-tint) %>%
  pivot_wider(names_from = tt, values_from = inter) %>% # code used to find now corrected typo in xlsx: values_fn = length
  mutate(inter = st + 1) %>%
  select(-et, -st)

# filter down only to intervals within 60 mins of the ferry passing (60 before or after)
am_spl <- splam %>%
  filter(inter %in% amv2$inter) %>% 
  left_join(amv2)

am_spl2 <- am_spl %>%
  mutate(Hr = hour(SPL.start.time)) %>% # create a date hr variable to link with wind
  group_by(inter) %>% # grouping by interval because some intervals span 2 hours
  mutate(wsp2 = mean(wind_spd, na.rm = TRUE)) %>% # create a mean wind value for the interval
  filter(
    # !is.na(wsp2) & wsp2 <20 &
    SPL.start.time < "2020-05-05"
  ) %>% 
  mutate(
    dt2 = boatdt - minutes(7),
    tperiods = case_when(
      # create periods before the closest passage of the ferry and after
      SPL.start.time < dt2 ~ "pre",
      SPL.start.time > boatdt ~ "post",
      SPL.start.time > dt2 & SPL.start.time < boatdt ~ "oth",
      SPL.start.time == boatdt ~ "oth"
    )
  ) %>% 
  group_by(inter, tperiods) %>%
  mutate(
    # create a ferry interval that is the 5 minutes before the closest passage
    ferry.int = interval(boatdt - minutes(6), boatdt - minutes(1)), 
    # create a new interval that indicates whether or not the minute is within the ferry interval
    ferry.prd = if_else(SPL.start.time %within% ferry.int, 1, 0), 
    pre.pst = case_when(
      tperiods == "pre" & ferry.prd != 1 & # find 5 minute periods in the period >5 min before closest passage
        zoo::rollmax(SPL, k = 5, fill = NA, align = "left") < 100 ~ 1, # where the maximum spl is < 100
      tperiods == "pre" & zoo::rollmax(SPL, k = 5, fill = NA, align = "left") > 100 ~ 0,
      tperiods == "pre" & is.na(zoo::rollmax(SPL, k = 5, fill = NA, align = "left")) ~ 0,
      tperiods == "pre" & ferry.prd == 1 ~ 0,
      tperiods == "post" ~ 0,
      tperiods == "oth" ~ 0
    ),
    post.pst = case_when(
      # find 5 minute periods in the post period where the maximum spl is < 100
      tperiods == "post" & zoo::rollmax(SPL, k = 5, fill = NA, align = "right") < 100 ~ 1, 
      tperiods == "post" & zoo::rollmax(SPL, k = 5, fill = NA, align = "right") > 100 ~ 0,
      tperiods == "post" & is.na(zoo::rollmax(SPL, k = 5, fill = NA, align = "right")) ~ 0,
      tperiods == "pre" ~ 0,
      tperiods == "oth" ~ 0
    ),
    # create a new variable that indicates whether or not the minute is part of a potential interval
    pt.int = ifelse(pre.pst == 1 | post.pst == 1, 1, 0) 
  ) %>%
  group_by(inter, tperiods, pt.int) %>%
  mutate(
    # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the pre-period
    pre.pst = ifelse(pre.pst == 1 & int_start(ferry.int) - SPL.start.time == min(int_start(ferry.int) - SPL.start.time), 1, 0), 
    # find the minute that starts the closest interval to the ferry passage where max spl < 100 in the post-period
    post.pst = ifelse(post.pst == 1 & SPL.start.time - boatdt == min(SPL.start.time - boatdt), 1, 0)
  ) %>% 
  ungroup(tperiods, pt.int) %>%
  # only keep intervals where there is a qualifying period in both pre and post ferry periods
  mutate(keep.inter = ifelse(sum(pre.pst) != 0 & sum(post.pst) != 0, 1, 0)) %>% 
  filter(keep.inter == 1) %>%
  group_by(inter, tperiods)

am_spl2a <- am_spl2 %>%
  ungroup() %>%
  filter(ferry.prd == 1) %>%
  filter(Distance < 3000) %>% # only removes #57
  group_by(inter, yr, mnth, d) %>%
  summarize(ferryspl = max(SPL)) %>%
  filter(ferryspl > 105) # find intervals where the ferry passage interval has a mean spl over 105

am_spl2 <- filter(am_spl2, inter %in% am_spl2a$inter) # subset down to those intervals

# calculate the pre-ferry period to analyze
am_pre <- am_spl2 %>%
  filter(pre.pst == 1) %>%
  ungroup() %>%
  select(inter, pre.dt = SPL.start.time) %>%
  mutate(pre.int = interval(pre.dt, pre.dt + minutes(5)))

# calculate the post-ferry period to analyze
am_post <- am_spl2 %>%
  filter(post.pst == 1) %>%
  ungroup() %>%
  select(inter, post.dt = SPL.start.time) %>%
  mutate(post.int = interval(post.dt - minutes(5), post.dt))

am_spl2 <- am_spl2 %>%
  ungroup() %>%
  left_join(am_pre) %>%
  left_join(am_post) %>%
  mutate(
    pre.prd = if_else(SPL.start.time %within% pre.int, 1, 0), # assign a 1 to minutes within the pre period
    post.prd = if_else(SPL.start.time %within% post.int, 1, 0), # assign a 1 to minutes within the post period
    prd = case_when(
      # create a variable that indicates which period the minute belongs to
      pre.prd == 1 ~ "pre",
      post.prd == 1 ~ "post",
      ferry.prd == 1 ~ "ferry",
      pre.prd == 0 & post.prd == 0 & ferry.prd == 0 ~ "none"
    )
  ) 


spl.inters.am <- am_spl2 %>%
  filter(prd != "none")


# create a dataset of AM sound trap files to use ----
ftuAM <- spl.inters.am %>%
  dplyr::select(stfile=`Begin File`, SPL.start.time, inter, yr, Deployment, pre.int, ferry.int, post.int) %>%
  distinct() %>%
  separate(stfile, into=c("st","filedt","ext"),sep=c(-16,-4),remove=FALSE)%>%
  select(-st,-ext)%>%
  mutate(
    prd = case_when(
      SPL.start.time %within% pre.int ~ "pre",
      SPL.start.time %within% post.int ~ "post",
      SPL.start.time %within% ferry.int ~ "ferry"
    ),
    strt = case_when(
      prd == "pre" ~ int_start(pre.int),
      prd == "ferry" ~ int_start(ferry.int),
      prd == "post" ~ int_start(post.int)
    ),
    filedt=ymd_hms(filedt),
    into.file = strt - filedt
  ) %>%
  select(-pre.int, -ferry.int, -post.int)
# wide list of dates with start of each AM passage sample in separate columns
fcpAM <- ftuAM %>% # get the files to use
  ungroup() %>%
  select(inter, prd, strt) %>% # select only the interval, the period, and the start time
  distinct() %>% # go town to unique rows
  pivot_wider(names_from = prd, values_from = strt) %>% # make a wider data frame
  mutate(
    dymd = paste0(year(post), month(post), day(post)),
    pre2 = pre + minutes(5), # get the end of the pre-period
    ferry2 = ferry + minutes(5), # get the end of the ferry period
    # calculate the length of time between the end of the pre-period and the start of the ferry period
    pf = difftime(ferry, pre2, units = "mins"), 
    # calculate the length of time between the end of the ferry-period and the start of the post-period
    fp = difftime(post, ferry2, units = "mins"), 
    midtimediff = fp + 10, # time diff between start of ferry and end of post
    passlenAM = round(15 + pf + fp) # calculate how long the boat period is
  ) %>% 
  select(dymd, inter, passlenAM, pre, ferry, post, midtimediff) # only keep inter, post(start of the post period), and period length

### START with AM ----
# select AM quiet control periods ----
splqAM <- splq %>% select(-Deployment, -inter)
dtl <- unique(splqAM$dymd) # the days to evaluate
splqAM$qpl <- NA # create the qpl (quiet period length) variable

# for loop to calculate length of different quiet period stretches
for (i in 1:length(dtl)) {
  t1 <- filter(splqAM, dymd == dtl[i]) # subset down to a single night
  for (j in 2:nrow(t1)) {
    t1$qpl[1] <- ifelse(t1$isq[1] == 0, 0, 1) # assign the first qpl a 0 if isq = 0 otherwise 1
    t1$qpl[j] <- ifelse(t1$isq[j] == 0, 0, 1 + t1$qpl[j - 1]) # add to qpl as long as isq = 1
  }
  # create splq2 first time through the loop otherwise append splq2
  ifelse(i == 1, splqAM2 <- t1, splqAM2 <- bind_rows(splqAM2, t1)) 
}

splqAMall <- splqAM2 %>%
  left_join(fcpAM) %>% # join in the file to keep
  filter(!is.na(post)) %>% # get rid of lines where there isn't a start to the post period
  group_by(dymd) %>% # group by day
  mutate(
    maxqpl = round(max(qpl, na.rm = T)),
    # assign a 1 to the end (last minute) of longest quiet period (eqp)
    eqp = if_else(qpl == maxqpl & maxqpl >= passlenAM, 1, 0),
    deltalen = maxqpl - passlenAM
  ) %>% 
  filter(eqp == 1) %>%
  group_by(dymd) %>% # group by day
  mutate(
    # find the time gap (tgap) between the end of the post period and the end of the quiet period
    tgap = round(difftime(pre, SPL.start.time, units = "mins")), 
    mintgap = ifelse(tgap == min(tgap), 1, 0),
    minquiet = qpl - passlenAM,
    midquiet = round(qpl - midtimediff),
    maxquiet = qpl - 5,
    # find intervals to keep, only keep those where the quiet period is at least as long as the boat pasage period
    keep = ifelse(maxqpl >= passlenAM & mintgap == 1, 1, 0)
  ) %>%
  select(inter, tgap, eqtime = SPL.start.time, qplength = maxqpl, passlen = passlenAM, minquiet, midquiet, maxquiet, keep)

ftuAM2 <- ftuAM %>%
  select(yr, Deployment, inter, prd, strt, boat.stfile = stfile, boat.intofile = into.file) %>%
  distinct() %>%
  left_join(splqAMall) %>%
  # filter(!is.na(dymd))%>%
  ungroup() %>%
  mutate(
    # calculate the start of of each control period (one for each pre,ferry,and post period)
    # use 6 with negative tgaps to give a min 1 min break between end of quiet-post sample and start of boat-pre sample
    quiet = if_else(tgap >= 0, strt - passlen - tgap - minutes(1), strt - passlen - minutes(6)),
    # # test should be equal to quiet only for pre period
    # quiet_test=if_else(tgap>=0, eqtime - passlen - minutes(5), strt - passlen - minutes(5)), 
    qstrt = quiet, # this is the start of the 5 minute period to analyze
    pend = quiet + minutes(5), # this is the end of the 5 minute period to analyze
    qp_strt = eqtime - minutes(qplength + 1), # this is the start of the quiet period being sampled
    qp_prior = round(difftime(qstrt, qp_strt, units = "mins")) # this is the length of quiet time prior to start of sample
  ) %>%
  # pivot longer so that the start and end times are in a single variable
  pivot_longer(qstrt:pend, names_to = "se", values_to = "quiet2") %>%
  filter(keep == 1)


### PM CODE resumed ----
### select PM quiet control periods ----
# quantify longest period of quiet after removing AM sampled times
# select quiet periods for only those passages retained
# inter.used <- unique(ftuAM2$inter)
fcp2 <- ftuAM2 %>%
  select(dymd, prd, quiet, passlen) %>%
  distinct()

# calculate the quiet periods analyzes for PM passage
fcp3 <- fcp2 %>%
  ungroup() %>%
  mutate(quiet.int = interval(quiet, quiet + minutes(5))) %>%
  select(-quiet) %>%
  pivot_wider(names_from = "prd", values_from = "quiet.int")

splqPM <- splq %>% # create a new splq dataset
  left_join(fcp3) %>% # join in the file to keep
  group_by(dymd) %>%
  mutate(
    passlength = if_else(!is.na(passlen), passlen, 0),
    # maxqpl= max(qpl, na.rm = T),
    # remainingqpl = if_else(!is.na(passlen), maxqpl-passlength-11, maxqpl), # subtract 11 for buffer
    # # truetgap=if_else(qpl==max(qpl, na.rm = T), tgap, NA_real_),
    # sampled = if_else( qpl>= max(qpl, na.rm = T)-passlength-11,1,0, missing = 0),
    sampled = if_else((SPL.start.time %within% pre | SPL.start.time %within% ferry | SPL.start.time %within% post), 1, 0, missing = 0), 
    isq = if_else(SPL < 100 & sampled == 0, 1, 0)
  ) %>%
  select(-Deployment, -inter, -passlen, -pre, -ferry, -post, passlenAM = passlength)

dtl <- unique(splqPM$dymd) # the days to evaluate
splqPM$qpl <- NA # create the qpl (quiet period length) variable

# for loop to calculate length of different quiet period stretches
for (i in 1:length(dtl)) {
  t1 <- filter(splqPM, dymd == dtl[i]) # subset down to a single night
  for (j in 2:nrow(t1)) {
    t1$qpl[1] <- ifelse(t1$isq[1] == 0, 0, 1) # assign the first qpl a 0 if isq = 0 otherwise 1
    t1$qpl[j] <- ifelse(t1$isq[j] == 0, 0, 1 + t1$qpl[j - 1]) # add to qpl as long as isq = 1
  }
  # create splq2 first time through the loop otherwise append splq2
  ifelse(i == 1, splqPM2 <- t1, splqPM2 <- bind_rows(splqPM2, t1)) 
}

splq3all <- splqPM2 %>% # create a new splq dataset
  group_by(dymd) %>% # group by day
  mutate(
    qplength = max(qpl),
    eqp = ifelse(qpl == max(qpl), 1, 0)
  ) %>% # assign a 1 to the end (last minute) of longest quiet period (eqp)
  filter(eqp == 1) %>% # only keep the end of the quiet period
  # select(-Deployment)%>%# remove deployment
  left_join(fcp) %>% # join in the file to keep
  filter(!is.na(post)) %>% # get rid of lines where there isn't a start to the post period
  mutate(
    epost = post + minutes(5), # calculate the end of the post period.
    # find the time gap (tgap) between the end of the post period and the end of the quiet period
    tgap = difftime(SPL.start.time, epost, units = "mins"), 
    mintgap = ifelse(tgap == min(tgap), 1, 0),
    # add in 6 min buffer at end of quiet period to keep separate from morning quiet period
    minquiet = qplength - passlen - 6, 
    midquiet = qplength - midtimediff - 6,
    maxquiet = qplength - 5 - 6,
    # find intervals to keep, only keep those where the quiet period is at least as long as the boat pasage period
    keep = ifelse(qpl >= (passlen + 6) & mintgap == 1, 1, 0)
  ) %>% 
  select(inter, tgap, eqtime = SPL.start.time, qplength, passlen, minquiet, midquiet, maxquiet, keep)

splq3 <- splq3all %>% filter(keep == 1) # remove deployment again

ftu2 <- ftu %>%
  select(yr, Deployment, inter, prd, strt, boat.stfile = stfile, boat.intofile = into.file) %>%
  # filter(inter !%in% )%>% #remove passages with overlap probles
  distinct() %>%
  left_join(splq3all) %>%
  filter(!is.na(dymd)) %>%
  ungroup() %>%
  mutate(
    # calculate the start of of each control period (one for each pre,ferry,and post period)
    quiet = strt + tgap - minutes(6), 
    qstrt = quiet, # this is the start of the 5 minute period to analyze
    pend = quiet + minutes(5),
    qp_strt = eqtime - minutes(qplength + 1),
    qp_prior = round(difftime(qstrt, qp_strt, units = "mins"))
  ) %>% # this is the end of the 5 minute period to analyze
  # pivot longer so that the start and end times are in a single variable called quiet2
  pivot_longer(qstrt:pend, names_to = "se", values_to = "quiet2") %>%
  filter(keep == 1) # remove deployment again


### COMBINE ALL SELECTIONS ----
# extract file names based on start and end times for quiet periods (quiet2)
spl_quiet2 <- alldata %>%
  mutate(quiet2 = SPL.start.time) %>%
  select(-inter)

# remove seconds for joining
second(spl_quiet2$quiet2) <- 0
second(ftu2$quiet2) <- 0
second(ftuAM2$quiet2) <- 0

# remove seconds for joining
ftu.b <- ftu2 %>%
  select(yr, Deployment, inter, prd, strt, stfile = boat.stfile, into.file = boat.intofile) %>%
  mutate(
    qp_prior = NA,
    type = "boat"
  ) %>%
  distinct()

ftu.q <- ftu2 %>%
  select(yr, Deployment, inter, prd, quiet, quiet2, qp_prior) %>%
  left_join(spl_quiet2) %>% # join to add file names and start times for quiet periods
  separate(`Begin File`, into=c("st","filedt","ext"),sep=c(-16,-4),remove=FALSE)%>%
  select(-st,-ext)%>%
  mutate(
    filedt=ymd_hms(filedt),
    into.file = round(quiet - filedt),
    type = "quiet"
  ) %>%
  select(yr, Deployment, inter, prd, strt = quiet, stfile=`Begin File`, into.file, qp_prior, type) %>%
  distinct()

# write.csv(ftu.b,"wdata/files_to_evaluate_boat_mn.csv")
# write.csv(ftu.q,"wdata/files_to_evaluate_quiet_mn.csv")

ftuAM.b <- ftuAM2 %>%
  select(yr, Deployment, inter, prd, strt, stfile = boat.stfile, into.file = boat.intofile) %>%
  mutate(
    qp_prior = NA,
    type = "boat"
  ) %>%
  distinct()

ftuAM.q <- ftuAM2 %>%
  select(inter, prd, quiet, quiet2, qp_prior) %>%
  left_join(spl_quiet2) %>% # join to add file names and start times for quiet periods
  separate(`Begin File`, into=c("st","filedt","ext"),sep=c(-16,-4),remove=FALSE)%>%
  select(-st,-ext)%>%
  mutate(filedt=ymd_hms(filedt),
         into.file = round(quiet - filedt), type = "quiet") %>%
  select(yr, Deployment, inter, prd, strt = quiet, stfile=`Begin File`, into.file, qp_prior, type) %>%
  distinct()

# write.csv(ftuAM.b,"wdata/files_to_evaluate_boat_am.csv")
# write.csv(ftuAM.q,"wdata/files_to_evaluate_quiet_am.csv")

allfiles <- bind_rows(ftu.b, ftu.q, ftuAM.b, ftuAM.q) %>%
  arrange(strt) %>%
  mutate(timediff = signif((strt - lag(strt)) / 1, digits = 4), overlap = if_else(timediff < 5 * 60 & timediff > 0, T, F, missing = F))


# check for overlap between selections ----
# none to drop if all working as planned :)
samples_to_drop <- filter(allfiles, overlap == T) 

allfiles2 <- allfiles %>%
  filter(!(inter %in% unique(samples_to_drop$inter))) %>%
  mutate(year = year(strt), month = month(strt), day = day(strt), dymd = paste0(year(strt), month(strt), day(strt)))

# write.csv(allfiles2,"wdata/files_to_evaluate_all.csv")


# FIGURES ----
# make figures to examine the spl profile of all periods
allinterspl <- alldata %>%
  select(SPL.start.time, SPL) %>%
  # bind_rows(mn_spl, am_spl) %>% select(inter, SPL.start.time, Time, SPL) %>%
  mutate(
    hr = hour(SPL.start.time),
    dymd = paste0(year(SPL.start.time), month(SPL.start.time), day(SPL.start.time))
  ) %>%
  filter(hr < 7)

interlist2 <- unique(allfiles2$dymd)

for (i in 1:length(interlist2)) {
  p1 <- allinterspl %>%
    filter(dymd == interlist2[i])

  p2 <- allfiles2 %>%
    filter(dymd == interlist2[i] & type == "quiet")

  p3 <- allfiles2 %>%
    filter(dymd == interlist2[i] & type == "boat") %>%
    mutate(prd = paste0("x", prd))

  inters <- sort(unique(p3$inter))

  ggplot(data = p1) +
    geom_line(aes(y = SPL, x = SPL.start.time)) +
    geom_segment(data = p2, aes(y = 88, yend = 88, x = strt, xend = (strt + minutes(5)), color = prd), size = 1.5, inherit.aes = F) +
    geom_segment(data = p3, aes(y = 88, yend = 88, x = strt, xend = (strt + minutes(5)), color = prd), size = 1.5, inherit.aes = F) +
    scale_color_manual(values = c("red", "orange", "orange", "black", "purple", "purple"), name = "Potential interval") +
    theme_bw() +
    # geom_vline(aes(xintercept=dt),color="red",linetype="dashed")+
    scale_x_datetime(date_minor_breaks = "5 mins") +
    coord_cartesian(ylim = c(85, 120)) +
    theme(legend.position = "none")
  ggsave(paste0("manual_figures/qfig_", paste0(inters[1]), "_", p2$year[1], p2$month[1], p2$day[1], ".jpg"))
}

# SAVE SELECTIONS ----
all19data <- allfiles2 %>%
  filter(Deployment == 19.1) %>%
  select(yr, Deployment, inter, prd, type, stfile, strt, into.file)
write.csv(all19data, "wdata/periods_to_evaluate_19.csv")

all20data <- allfiles2 %>%
  filter(Deployment != 19.1) %>%
  select(yr, Deployment, inter, prd, type, stfile, strt, into.file)
write.csv(all20data, "wdata/periods_to_evaluate_20.csv")

### MOVE SELECTED FILES to new folders ----
all19 <- allfiles2 %>%
  filter(Deployment == 0) %>%
  select(stfile) %>%
  distinct()

all20 <- allfiles2 %>%
  filter(Deployment == 1) %>%
  select(stfile) %>%
  distinct()

write.csv(all19, "wdata/files_list_for_2019.csv")
write.csv(all20, "wdata/files_list_for_2020.csv")

# ### for Philina, run just once
# for (i in 1:nrow(all19)){
#   file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/1342218252/", all19$stfile[i]),
#     "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/allboatpassage19")
# }
#
# for (i in 1:nrow(all20)){
#   file.move(paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/RCAin_200418_1505_5047/", all20$stfile[i]),
#     "/Volumes/SPERA_Rf_3_backup/RCA_IN_2020/allboatpassage20")
# }
#
#
# ### for Steph, run just once
# for (i in 1:nrow(all19)){
#   file.move(paste0("E:/RCA_IN/April_July2019/1342218252/",all19$stfile[i]),
#     "E:/RCA_IN/April_July2019/allboatpassage19")
# }


# Code to extract chosen "Intervals" from selection tables ----

allfiles2 <- read.csv("wdata/files_to_evaluate_all.csv")

# # code for use with old manual selection tables ?
# newinter <- filter(allfiles2, type== "boat" ) %>% select(inter, stfile) %>%
#   group_by(inter, stfile) %>%
#   distinct()
# 
# # remove morning passages since they weren't in first set of selections
# is.odd <- function(v) v %% 2 != 0
# newinter <- newinter[is.odd(newinter$inter),]
# 
# # load first selection table created (bp1)
# bp1 <- Rraven::imp_raven(path = here("w.selection.tables"),
#   # files = "boat_passage_prelim.txt",
#   files = "boat_passage_old.txt",
#   all.data = TRUE) 
# 
# bp1[21]<- bp1[14]
# colnames(bp1)[21] <- "stfile"
# 
# # remove intervals not to be used anymore
# matchedinter <- left_join(bp1, newinter) %>% filter(!is.na(inter)) 
# 
# # get selections from second selection table (bp2)
# bp2 <- Rraven::imp_raven(path = here("w.selection.tables"), files = "boat_passage_2nd.txt", all.data = TRUE) 
# bp2[21]<- bp2[14]
# colnames(bp2)[21] <- "stfile"
# 
# # filter out intervals already present in bp1
# oldinters <- unique(matchedinter$inter)
# matchedinter2 <- left_join(bp2, newinter) %>% filter(!(Interval %in% oldinters) & !is.na(inter)) 
# 
# # combine all selections 
# allmatchedinter <- bind_rows(matchedinter, matchedinter2) %>% 
#   # name column indicating which selection table row comes from
#   rename(which.table = selec.file) %>% 
#   # remove repetitive columns
#   select(-stfile, -inter)
# 
# # correct common typos
# allmatchedinter[16] <- case_when(
#   allmatchedinter[16]=="unkn"~"unkn",
#   allmatchedinter[16]=="knock"~"knock",
#   allmatchedinter[16]=="KNOCK"~"knock",
#   allmatchedinter[16]=="grunt"~"grunt",
#   allmatchedinter[16]=="grutn"~"grunt",
#   allmatchedinter[16]=="grrunt"~"grunt")
# 
# # save windows version
# write.table(allmatchedinter, file = "w.selection.tables/boat_passage_prelim_updated.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# ## code to change file paths for use on MAC ----
# 
# ### prelim manual selections
# allmatchedinter[,13] <- stringr::str_replace_all(allmatchedinter[,13], "E:\\\\RCA_IN\\\\April_July2019\\\\boat_passage\\\\","/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/allboatpassage19/")
# write.table(allmatchedinter, file = "w.selection.tables/boat_passage_prelim_updated_mac.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# sort(unique(allmatchedinter$Interval))
# 
### EDITING SELECTION TABLES
### prelim auto selections

# # extract from folder sent by X
# 
# mydir = paste0(here::here("/selection.tables/Raven_files_RCA_in_April_July2019_1342218252/"))
# myfiles <- list.files(path = mydir, pattern = "*.txt", full.names = F)
# myfiles
# allautoselect <- do.call(rbind, lapply(myfiles,  Rraven::imp_raven, path = mydir, warbler.format = FALSE, all.data = TRUE) )
# glimpse(allautoselect)
# 
# # rewrite X path to work on MAC
# allautoselect[,9] <- stringr::str_replace_all(allautoselect[,9], "1342218252\\\\",
#   "/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/allboatpassage19/")
# # # rewrite path to work on PC
# # allautoselect[,9] <- paste0("E:/RCA_IN/April_July2019/1342218252/",allautoselect[,9])
# 
# Stephs selection table
# allautoselect <- Rraven::imp_raven(path = here::here("selection.tables"),
#   files = "boat_passage_random_selections_amp_10_Nov32020.txt",
#   all.data = TRUE)
## allautoselect %>% filter(`Manual Class` == "F") %>% group_by(Inter,Period) %>% summarise(n_calls = n())

# # rewrite path to work on MAC
# allautoselect[,9] <- paste0("/Volumes/SPERA_Rf_3_backup/RCA_IN/April_July2019/allboatpassage19x10db/", allautoselect[,11])
# 
# # renumber selections to load in a way consistent with manual selection workspace and period start times
# orderautoselect <- allautoselect %>% arrange(`Begin File`, `File Offset (s)`) %>%
#   mutate(Selection_X = Selection, Selection = row_number())
# 
# # save .txt
# #write.table(orderautoselect, file = "w.selection.tables/boat_passage_autoselect_mac.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# write.table(orderautoselect, file = "w.selection.tables/boat_passage_autoselect_Nov3.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# # subset to only fish sounds
# orderautoselect <- orderautoselect %>% filter(Class == "FS")
# write.table(orderautoselect, file = "w.selection.tables/boat_passage_autoFS_mac.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 
# # confirm that files are exactly the same as for manual selection workspace 
# all19_2 <- filter(allfiles2, yr == "2019")
# myfilelist <- unique(as.character(all19_2$stfile))
# 
# # allautoFS <- filter(allautoselect, Class == "FS")
# Xfiles <- allautoFS$selec.file
# Xfilelist <- unique(gsub(".chan1.Table.1.selections.txt", "", Xfiles))
# 
# # if returns character(0) than lists match exactly
# setdiff(myfilelist, Xfilelist)
# 
# # Xfilelist[! Xfilelist %in% myfilelist]
# # myfilelist[! myfilelist %in% Xfilelist]


## code to extract date from file names
# mutate(dy2md = gsub("1342218252.", "", stfile), dy2md = substr(dy2md,1,nchar(dy2md)-10))

# earlier check that the inter assignments hadn't changed
# # oldinter <- read_xlsx("files_to_evaluate_boat_061920.xlsx", sheet = "files_to_evaluate_boat_061920") %>%  
# oldinter <- read_xlsx("wdata/files_to_evaluate_boat.xlsx", sheet = "files_to_evaluate_boat") %>%    
#   select(oldinter = inter, stfile = stfile.boat) %>% 
#   select(-stfile) %>% distinct()
# oldinter
# newinter <- newinter %>% select(-stfile) %>% distinct()
# matchedinter <- left_join(newinter, oldinter)
# unique(matchedinter$oldinter)
# # stephdid <- c(29, 91, 105, 113, 117,125)
# matchedinter <- filter(matchedinter, oldinter %in% stephdid) 
# matchedinter[is.odd(matchedinter$newinter),]
