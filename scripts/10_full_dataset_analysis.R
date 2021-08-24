# script to start data analysis

source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="DHARMa")
lp(pck="glmmTMB")
lp(pck="patchwork")

# custom function for checking for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# load data
all<-read_rds("wdata/alldata_bothyears_withtod.rds")%>%
  filter(!is.na(wind_spd))

# first scale some predictors
all<-all%>%
  ungroup()%>%
  distinct()%>%
  mutate(spl.sc=scale(SPL),
         wind.sc=scale(wind_spd),
         precip.sc=scale(precip_amt),
         wave.sc=scale(wave.ht),
         tide.sc=scale(tide))%>%
  filter(!is.na(wave.ht))

all$ship.close<-factor(all$ship.close,levels=c("no.ship","approaching","close"))
all$tod<-factor(all$tod,levels=c("day","dusk","night","dawn"))
all$yr<-factor(all$yr)
# trying a full model with poisson model and a repeated measures structure
# poisson residuals are horrid

full.glmm<-glmmTMB(fish.calls~spl.sc*yr+ship.close*yr+
                     tod*yr+ moon.phase*yr+tide.sc*yr,
                   data=all,
                   family=nbinom1)

full_simres <- simulateResiduals(full.glmm) 
testDispersion(full_simres) 
plot(full_simres)

summary(full.glmm)

full20.glmm<-glmmTMB(fish.calls~spl.sc*yr+ship.close*yr+
                     tod*yr+ moon.phase*yr+tide.sc*yr,
                   data=all%>%
                     mutate(yr=relevel(yr,ref="2020")),
                   family=nbinom1)

summary(full20.glmm)
