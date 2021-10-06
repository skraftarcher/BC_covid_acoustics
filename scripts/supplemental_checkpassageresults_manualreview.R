#script to double check power findings using only manually detected calls

# bring in manual detection check from 5 minute periods
source("scripts/install_packages_function.R")
lp(pck="tidyverse")
lp(pck="lubridate")
lp(pck="readxl")
lp(pck="DHARMa")
lp(pck="glmmTMB")

# mrbp<-Rraven::imp_raven(path = here::here("w.selection.tables"),
#                     files = "boat_passage_random_selections_updated_Dec152020.txt",
#                     all.data = TRUE)%>%
#   select(-selec.file)%>%
#   filter(!is.na(Inter))
# 
# mrbp$`Begin Path`<-paste0("E:/RCA_IN/April_July2019/amplified_10/",mrbp$`Begin Path`)
# 
# write.table(mrbp, file = "w.selection.tables/boat_passage_forpowerfilter.txt", sep = "\t", row.names = FALSE, quote = FALSE)

mrbp2<-Rraven::imp_raven(path = here::here("w.selection.tables"),
                         files = "boat_passage_forpowerfilter.txt",
                         all.data = TRUE)%>%
  select(-selec.file)

mrbp2$Period<-factor(mrbp2$Period,levels=c("pre","ferry","post"))

cuts<-mrbp2%>%
  filter(Period=="ferry")%>%
  filter(`Manual Class`=="F")%>%
  mutate(ibpf=quantile(`Inband Power (dB FS)`,.25),
         ppf=quantile(`Peak Power Density (dB FS)`,.25))%>%
  select(ibpf,ppf)%>%
  distinct()

mrbp2<-mrbp2%>%
  mutate(FC=ifelse(`Manual Class`=="F",1,0),
         ibpf=cuts$ibpf,
         ppf=cuts$ppf)

# custom function for checking for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

ggplot(mrbp2)+
  geom_boxplot(aes(y=`Inband Power (dB FS)`,fill=Period))

ggplot(mrbp2)+
  geom_boxplot(aes(y=`Peak Power Density (dB FS)`,fill=Period))

ggplot(mrbp2%>%
#         filter(`Inband Power (dB FS)`>= ibpf)%>%
         group_by(Inter,Period)%>%
         summarize(ns=sum(FC)))+
  geom_boxplot(aes(y=ns,fill=Period))

ggplot(mrbp2%>%
         filter(`Inband Power (dB FS)`>= ibpf)%>%
         group_by(Inter,Period)%>%
         summarize(ns=sum(FC)))+
  geom_boxplot(aes(y=ns,fill=Period))

ggplot(mrbp2%>%
         filter(`Peak Power Density (dB FS)`>= ppf)%>%
         group_by(Inter,Period)%>%
         summarize(ns=sum(FC)))+
  geom_boxplot(aes(y=ns,fill=Period))

# do stats
# when filtering by inband

ibf.bp<-mrbp2%>%
  filter(`Inband Power (dB FS)`>= ibpf)%>%
  group_by(Inter,Period)%>%
  summarize(fish.calls=sum(FC))

ibf.glm<-glmmTMB(fish.calls~Period,
                          data=ibf.bp%>%
                   mutate(Period=relevel(Period,ref="ferry")),
                          family=nbinom1)

ibf.glm1_simres <- simulateResiduals(ibf.glm) 
testDispersion(ibf.glm1_simres) 
plot(ibf.glm1_simres)

summary(ibf.glm)

#filtering by peak power
ppf.bp<-mrbp2%>%
  filter(`Peak Power Density (dB FS)`>= ppf)%>%
  group_by(Inter,Period)%>%
  summarize(fish.calls=sum(FC))

ppf.glm<-glmmTMB(fish.calls~Period,
                 data=ppf.bp%>%
                   mutate(Period=relevel(Period,ref="ferry")),
                 family=nbinom1)

ppf.glm1_simres <- simulateResiduals(ppf.glm) 
testDispersion(ppf.glm1_simres) 
plot(ppf.glm1_simres)

summary(ppf.glm)
