# script to extract boat passages to examine further

library(tidyverse)
library(lubridate)

splm<-read_rds("wdata/spl_by_min.rds")%>%
  mutate(hr=hour(Time),
         mn=minute(Time),
         dspl=lead(SPL)-SPL)%>%
  filter(hr <= 3)

# quick example plot to look at data
ggplot(data=splm%>%
         filter(Month==5 & Day <=1& Year ==2019& hr <1),aes(y=dspl,x=DateTime))+
  geom_line(aes(group=grp,color=grp))#+
#  facet_wrap(~Year,scales="free",ncol=1)