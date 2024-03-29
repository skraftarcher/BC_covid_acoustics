#boat passage analysis script

#load packages
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="DHARMa")
lp(pck="glmmTMB")
lp(pck="patchwork")

# bring in data
boatpass<-read_rds("wdata/all_boat_passage_data.rds")
boatpass$prd<-factor(boatpass$prd,levels=c("pre","ferry","post"))
boatpass$type<-factor(boatpass$type,levels=c("quiet","boat"))

# custom function for checking for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# it looks like the number of fish calls go down in when the boat is passing
# and that the number of fish calls is always less in the boat periods vs
# the quiet periods. 
# it also looks like there are fewer fish calls in 2020, but don't trust this
# until the detector gets corrected for 2020

#try out a model to examine this. 
#fish calls are counts so start out with poisson
# poisson residuals are not too bad but not normal moving on to nbinom1
# nbinom1 looks great
# going to use nbinom1

bp.glm1<-glmmTMB(fish.calls~prd*type+(1|inter),
                 data=boatpass,
                 family=nbinom1)

bp.glm1_simres <- simulateResiduals(bp.glm1) 
testDispersion(bp.glm1_simres) 
plot(bp.glm1_simres)

summary(bp.glm1)

# moral of the story -
# there are slight differences between periods in quiet times
# with pre having more calls than post (beta: -0.15 stderror: 0.06, p = 0.007)
# overall all boat periods have fewer calls (beta: -0.35, stderror: 0.06, p< 0.001)
# in boat periods there is a significant decrease in calls when
# the ferry is passing (beta: -0.43, stderror 0.09, p < 0.001)
# the interaction between bota and post ferry passage is signficant
# largely gets rid of the decrease seen in quiet periods (beta: 0.18,
# stderror: 0.09, p=0.04)

# make a plot to look at this
theme_set(theme_bw())
theme_update(panel.grid = element_blank())
col.pal=c("#403891ff","#de7065ff","6b4596ff")

boatpass2<-boatpass%>%
  select(inter,prd,yr,type,fish.calls)%>%
  pivot_wider(names_from = type,values_from = fish.calls)%>%
  mutate(fc.diff=quiet-boat)

(p1<-ggplot(data=boatpass%>%
              filter(type=="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=150,label="Boat Period"),size=5)+
    ylab("Total number of fish calls \n Boat Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p2<-ggplot(data=boatpass%>%
              filter(type!="boat"))+
    geom_boxplot(aes(x=prd,y=fish.calls,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=5)+
    ylab("Total number of fish calls\n Quiet Period")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

(p3<-ggplot(data=boatpass2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
    geom_boxplot(aes(x=prd,y=fc.diff,fill=prd))+
    scale_fill_manual(values = col.pal,name="Passage Interval",labels=c("Pre-Ferry","Ferry passing","Post-Ferry"))+
    #geom_text(aes(x=0.75,y=225,label="Quiet Period"),size=8)+
    ylab("Difference in fish calls \n (Quiet - Boat)")+
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          legend.position = "bottom"))

p1 / p2 / p3 / plot_layout(guides = 'collect')&
  theme(legend.position='bottom')

ggsave("figures/boat_passage_results.jpg",height = 9,width=4)

# do analysis with trimmed data

fcpowera<-read_rds("wdata/all_boat_passage_data_power.rds")

# get rid of periods where masking in ferry passage is so high there are too few fish calls

rminter<-fcpowera%>%
  filter(type=="boat")%>%
  filter(prd=="ferry")%>%
  group_by(inter,yr)%>%
  summarize(fc=n())%>%
  filter(fc<=5)

fcpower<-filter(fcpowera, !inter %in% rminter$inter)

all.periods<-fcpower%>%
  select(yr,inter,prd,type)%>%
  distinct()
#get mean spl per passage/type

write_rds(fcpower%>%
  group_by(yr,prd,type)%>%
  summarize(spl=mean(SPL.mean)),"wdata/boatpassage_meanspls.rds")

fcpower.sum<-fcpower%>%
  filter(type=="boat")%>%
  filter(prd=="ferry")%>%
  group_by(inter)%>%
  summarise(inband.q1=quantile(inband.power,.25),peak.q1=quantile(peak.power,.25))



fcp.ib<-fcpower%>%
  left_join(fcpower.sum)%>%
  filter(inband.power>=inband.q1)%>%
  group_by(inter,prd,type,yr)%>%
  summarise(fish.calls=n())

fcp.ib<-left_join(all.periods,fcp.ib)%>%
  mutate(fish.calls=ifelse(is.na(fish.calls),0,fish.calls))

fcp.ib$prd<-factor(fcp.ib$prd,levels = c("pre","ferry","post"))
fcp.ib$type<-factor(fcp.ib$type,levels = c("quiet","boat"))

write_rds(fcp.ib,"wdata/boat_passage_trimmed_inbandpower.rds")

fcp.pp<-fcpower%>%
  left_join(fcpower.sum)%>%
  filter(peak.power>=peak.q1)%>%
  group_by(inter,prd,type,yr)%>%
  summarise(fish.calls=n())

fcp.pp<-left_join(all.periods,fcp.pp)%>%
  mutate(fish.calls=ifelse(is.na(fish.calls),0,fish.calls),
         yr=case_when(
           !is.na(yr)~yr,
           is.na(yr) & inter <200~2019,
           is.na(yr) & inter > 200 ~2020))

fcp.pp$prd<-factor(fcp.pp$prd,levels = c("pre","ferry","post"))
fcp.pp$type<-factor(fcp.pp$type,levels = c("quiet","boat"))

write_rds(fcp.pp,"wdata/boat_passage_trimmed_peakpower.rds")

# do analysis with filtered dataset and q1 for each interval

ipglm<-glmmTMB(fish.calls~prd*type+(1|inter),
                           data=fcp.ib,
                           family=nbinom1)

ipglm_simres <- simulateResiduals(ipglm) 
testDispersion(ipglm_simres) 
plot(ipglm_simres)

summary(ipglm)

# same results as before

# now doing the analysis with peak power as the filter
ppglm<-glmmTMB(fish.calls~prd*type+(1|inter)+(1|yr),
               data=fcp.pp,
               family=nbinom1)

ppglm_simres <- simulateResiduals(ppglm) 
testDispersion(ppglm_simres) 
plot(ppglm_simres)

summary(ppglm)

# also same results as before





