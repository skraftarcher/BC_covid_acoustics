if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(DHARMa))install.packages("DHARMa");library(DHARMa)
# if(!require(lmerTest))install.packages("lmerTest");library(lmerTest)
if(!require(glmmTMB))install.packages("glmmTMB");library(glmmTMB)



fishonly <- readRDS("wdata/auto_all_periods_fishonly.rds")
inter_dates <- fishonly %>% select(inter, dt1)


d <- fishonly %>% filter(Confidence >= 0.50) %>% 
  group_by(period_id, inter, year, Deployment, type, prd, spl_mean, qp_prior) %>%
  summarise(fish_count = n()) 

d <- left_join(d, inter_dates) %>% mutate(doy = strftime(dt1, format = "%j"),
  prd = factor(prd, levels = c("pre", "ferry", "post"))
)


ggplot(d, aes(spl_mean, fish_count, colour=spl_mean)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(type~prd)

ggplot(d, aes(qp_prior, fish_count, colour=spl_mean)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_colour_viridis_c()

ggplot(d, aes(prd, fish_count)) +
  geom_boxplot() + 
  facet_wrap(~type, nrow = 2)

ggplot(d, aes(doy, fish_count, colour=spl_mean)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_wrap(~year, nrow = 2)

# seasonal effect removed for clearer between year comparison
d1 <- filter(d, doy < 125)
ggplot(d1, aes(spl_mean, fish_count, colour = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_viridis_d(end = 0.5) 


d2 <- filter(d, !(prd == "ferry" & type == "boat")) %>% mutate(
  prd_type = case_when(
    prd == "pre" & type == "boat" ~ "pre-passage",
    prd == "post" & type == "boat" ~ "post-passage",
    type == "quiet" ~ "quiet"
  ), 
  prd_type = factor(prd_type, levels = c("pre-passage", "post-passage", "quiet"))
)

ggplot(d2, aes(prd_type, fish_count)) +
  geom_violin()

ggplot(filter(d2, !(prd == "ferry" & type == "quiet")), 
  aes(prd, fish_count)) +
  geom_boxplot() +
  facet_wrap(~type)


aov1 <- lm(fish_count~prd_type + spl_mean, data = d2 )
# anova(aov1)
summary(aov1)


mmod <- glmmTMB::glmmTMB(fish_count ~ prd*type*as.factor(year) + 
    spl_mean + (spl_mean|inter), 
  family = nbinom2, # best one
  # family = nbinom1,
  # family = poisson,
  data = filter(d2, !(prd == "ferry" & type == "quiet")))
summary(mmod)

# check residuals: not amazing, but not terrible
mmod_simres <- simulateResiduals(mmod) 
testDispersion(mmod_simres) 
plot(mmod_simres)





lm1 <- lm(fish_count~prd_type, data = d2 )
anova(aov1)
summary(aov1)
