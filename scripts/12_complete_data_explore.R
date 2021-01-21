#script to do an initial exploration on the full dataset 
fish<-read_rds("wdata/all_fish_calls_with_weather.rds")
# first going to look at relationship between wave height and period measurements

ggplot(data=fish%>%
         filter(!is.na(wave.ht2))%>%
         select(wave.ht,wave.ht2)%>%
         distinct())+
  geom_point(aes(x=wave.ht,y=wave.ht2))

# wave.ht is definitely the way to go. 

# now look at period data
ggplot(data=fish%>%
         filter(!is.na(wave.prd2))%>%
         select(wave.prd,wave.prd2)%>%
         distinct())+
  geom_point(aes(x=wave.prd,y=wave.prd2))
# wave.prd is also definitely the way to go.

# now trying to make figure of fish calls by time of day/day
# need to create two variables: a decimal hour, and a day of year variable

# going to look at distribution on the number of fish calls first
hist(fish$ncall)
# looks like there's a bit of an outlier somewhere.

# for now I will also rescale so that anything that is over 30 gets a value of 50
fish2<-fish %>%
  mutate(doy = strftime(datetime, format = "%j"),
         hr.min=hr+min/60,
         ncall2=ifelse(ncall<30,ncall,30))

ggplot(fish2)+
  geom_tile(aes(x=doy,y=hr.min,color=ncall2),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_color_viridis_c()
  

# hard to visualize with the whole dataset because the second part of
# the 2020 dataset is 15 min on 15 off and there's the big gap.
# first I'm going to subset down to only doy in the 2020 dataset

doytokeep<-fish2 %>%
  filter(yr==2020)%>%
  ungroup()%>%
  select(doy)%>%
  distinct()

fish3<-fish2 %>%
  filter(doy %in% doytokeep$doy)

ggplot(fish3)+
  geom_tile(aes(x=doy,y=hr.min,fill=ncall2),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()

# I'm now going to go with average call per minute per hour to get rid of gaps
fish4<-fish3 %>%
  group_by(yr,doy,hr)%>%
  summarize(mean.call=mean(ncall,rm.na=TRUE),
            mean.spl=mean(spl,rm.na=TRUE),
            mean.wvht=mean(wave.ht,rm.na=TRUE),
            mean.wvprd=mean(wave.prd,rm.na=TRUE))

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.call),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.spl),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()

ggplot(fish4)+
  geom_tile(aes(x=doy,y=hr,fill=mean.wvht),size=.5)+
  scale_y_reverse()+
  facet_wrap(facets="yr",nrow=2)+
  scale_fill_viridis_c()
