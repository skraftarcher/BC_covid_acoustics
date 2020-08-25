# script to extract boat passages to examine further
library(tidyverse)
library(lubridate)

splm<-read_rds("wdata/spl_by_min.rds")%>%
  mutate(hr=hour(Time),
         mn=minute(Time),
         am_pm=if_else(hr<12, "am", "pm"),
         dspl=lead(SPL)-SPL) %>%
  filter(hr <= 6 | hr >= 21) %>% # 9 pm to 7 am
  group_by(Year, Month, rca) %>% 
  mutate(spl_scaled=scale(SPL)) %>% # scale SPL within site
  ungroup()

# quick example plot to look at data
# change from prior min
ggplot(data=splm%>%
    filter(Month==5 & Day ==2 & Year==2019 & hr<2),
  aes(y=dspl,x=DateTime))+
  geom_line(aes(group=grp,color=grp))#+
#  facet_wrap(~Year,scales="free",ncol=1)

# vs raw SPL timeseries
ggplot(data=splm%>%
    filter(Month==5 & Day==2 & Year==2019 & hr<2),
  aes(y=SPL,x=DateTime))+
  geom_line(aes(group=grp,color=grp))

# compared with scaled version of spl
ggplot(data=splm%>%
    filter(Month==5 & Day==2 & Year==2019 & hr<2),
  aes(y=spl_scaled,x=DateTime))+
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)

# maybe smaller blip shows up nearly simultaneously 

###############
### view multiple days at same time 
### includes scheduled ferry passage in both covid and normal times
###############

# daily 10:45 departure from tsawwassen should arrive around 12:45 am (passage ~12:30)
ggplot(data=splm%>%
    filter(Month==4 & Day >=22 & Year ==2019 & 
        hr <2 & 
        rca == "in"), 
  aes(y=spl_scaled,x=DateTime))+  
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day >=20 & Day <=28 & 
        Year ==2020 & 
        hr <2 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")


# daily 5:15 departure from duke point (peak passage ~5:30)
ggplot(data=splm%>%
    filter(Month==4 & Day >=22 & Year ==2019 & 
        hr>3 & hr<7 &
        rca == "in"), 
  aes(y=spl_scaled,x=DateTime))+  
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day >=20 & Day <=28 & 
        Year ==2020 & 
        hr>3 & hr<7 &
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")


# Mon-Fri 8:15 departure from Tsawwassen should pass around 10 pm and 10:45 departure from duke point (peak passage ~10:55 pm?) 
ggplot(data=splm%>%
    filter(Month==4 & Day>=20 & Day<=28 & 
        Year ==2019 & 
        hr >= 21 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

ggplot(data=splm%>%
    filter(Month==4 & Day>=20 & Day<=28 & 
        Year ==2020 & 
        hr >= 22 & 
        rca == "in"),
  aes(y=spl_scaled,x=DateTime))+
  coord_cartesian(ylim=c(-2,4)) +
  geom_line(aes(group=grp,color=grp))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5, lty = "dashed")+
  geom_hline(yintercept = -0.5, lty = "dashed")+
  facet_wrap(~Day, scales="free_x")

################
#### try tsoutliers package to detect peaks 
################
splm2hr <- splm %>%
  filter(rca=="in" & Month==5 & Day ==2& Year ==2019& hr <4)

if(!require(tsoutliers)) install.packages("tsoutliers"); library("tsoutliers")
dat.ts<- ts(splm2hr$spl_scaled,frequency=1, deltat = 1/240)
plot(dat.ts)
data.ts.outliers <- tso(dat.ts, delta = 0.05, types = "TC", check.rank = T)
data.ts.outliers
plot(data.ts.outliers)


################
### simple function that finds peaks
################
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

# plot(splm2hr$SPL, type="l")
# p=find_peaks(splm2hr$SPL, m = 20)
# p2=find_peaks(-splm2hr$SPL, m = 20)
# points(p, splm2hr$SPL[p], col="red")
# points(p2, splm2hr$SPL[p2])

d <- c(20,21)
d <- c(21,22)
d <- c(22,23)
d <- c(23,24)
year <- 2019
year <- 2020

splm2hr <- splm %>%
  filter(rca=="in" & (
    Month==4 & Day==d[1]& Year==year & hr>=10 | 
      Month==4 & Day==d[2]& Year==year & hr<=6
  ))

splmIN <- splm %>% filter(rca=="in")

plot(splm2hr$SPL, type="l")
p=find_peaks(splm2hr$SPL, m = 20)
p <- p[ p> 20 & p < (max(p)-20)]
threshold <- quantile(splmIN$SPL, 0.75)
# threshold <- mean(splm$SPL)+ 2*(sd(splm$SPL))
# quiet_threshold <- mean(splm$SPL)
quiet_threshold <- quantile(splmIN$SPL, 0.25)
abline(h = threshold)
abline(h = quiet_threshold)
p <- p[splm2hr$SPL[p] > threshold]
points(p, splm2hr$SPL[p], col="red")

# p2=find_peaks(-splm2hr$SPL, m = 20)
# points(p2, splm2hr$SPL[p2])

################
# ### quantmod function that finds peaks ### NOT GOOD
# if(!require(quantmod)) install.packages("quantmod")
# 
# plot(splm2hr$spl_scaled, type="l")
# p=quantmod::findPeaks(splm2hr$spl_scaled, 0.5)
# points(p, splm2hr$spl_scaled[p])

### complex function with smoothing... but throws error
# myfindPeaks <- function (x, 
#   thresh=0.05, span=0.25, lspan=0.05, 
#   noisey=TRUE
# ){
#   n <- length(x)
#   y <- x
#   mu.y.loc <- y
#   if(noisey)
#   {
#     mu.y.loc <- (x[1:(n-2)] + x[2:(n-1)] + x[3:n])/3
#     mu.y.loc <- c(mu.y.loc[1], mu.y.loc, mu.y.loc[n-2])
#   }
#   y.loess <- loess(x~I(1:n), span=span)
#   y <- y.loess[[2]]
#   sig.y <- var(y.loess$resid, na.rm=TRUE)^0.5
#   DX.1 <- sign(diff(mu.y.loc, na.pad = FALSE))
#   pks <- which(diff(DX.1, na.pad = FALSE) < 0 & DX.1[-(n-1)] > 0) + 1
#   out <- pks
#   if(noisey)
#   {
#     n.w <- floor(lspan*n/2)
#     out <- NULL
#     # browser()
#     for(pk in pks)
#     {
#       inner <- (pk-n.w):(pk+n.w)
#       outer <- c((pk-2*n.w):(pk-n.w),(pk+2*n.w):(pk+n.w))
#       mu.y.outer <- mean(y[outer])
#       if(!is.na(mu.y.outer)) 
#         if (mean(y[inner])-mu.y.outer > thresh*sig.y) out <- c(out, pk)
#     }
#   }
#   out
# }



