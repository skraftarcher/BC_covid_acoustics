# visualize relationship between manual review, detector check and explanatory variables

# load packages
#load packages
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="patchwork")

# load data
dcheck19<-read_rds("wdata/detector_check_2019.rds")
dcheck20<-read_rds("wdata/detector_check_2020.rds")

# make some plots
p19<-ggplot(data=dcheck19,aes(x=fish.calls,y=man.calls))
p20<-ggplot(data=dcheck20,aes(x=fish.calls,y=man.calls))

#by spl
(p19a<-p19+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=SPL))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by wind
(p19b<-p19+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=wind_spd))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by precip
(p19c<-p19+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=precip_amt))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by tide
(p19d<-p19+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=tide))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))


(p19a+p19b)/(p19c+p19d)

ggsave("figures/detector_review_2019.jpg")

# 2020
#by spl
(p20a<-p20+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=SPL))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by wind
(p20b<-p20+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=wind_spd))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by precip
(p20c<-p20+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=precip_amt))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))

#by tide
(p20d<-p20+
    geom_abline(aes(intercept=0,slope=1))+
    geom_jitter(aes(color=tide))+
    scale_color_viridis_c()+
    facet_wrap(~reviewgroup))


(p20a+p20b)/(p20c+p20d)

ggsave("figures/detector_review_2020.jpg")
