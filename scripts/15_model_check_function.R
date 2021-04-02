# Function for 10-fold cross validation

# cleaning up 14_visualize_detector_check

#Load packages
library(tidyverse)
library(ggeffects)
library(lubridate)
library(mgcv)
library(mgcViz)
library(glmmTMB)

#read in data
all.data<-read_rds("wdata/training_test_data.rds")%>%
  mutate(ID=rownames(.))



ten.fold.gam <- function(data,prop.d) { 
  output.info = list(
      fit = data.frame(iteration = NA, MSE = NA,R2=NA)[-1, ],
      pred.values = data.frame(man.fish = NA, pred.fish = NA)[-1, ])
  for (i in 1:10) {
    train.data <- data %>%
      group_by(dataset) %>%
      slice_sample(prop = prop.d, replace = FALSE)
    
    test.data <- data %>%
      filter(!ID %in% train.data$ID)
    
    mod2<-gamm(man.fish ~ te(spl.sc, log.fish.sc, bs = "ts") +
                 te(wind_dir, wind.spd, bs = c("cc", "ts")),
               family=nbinom2, 
               data=train.data)

    t1 <- data.frame(iteration = i, 
                     MSE = mean((train.data$man.fish-exp(predict(
      mod2$gam, newdata = train.data, re.form = NA )))),
      R2=summary(mod2$gam)$r.sq)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                 pred.fish = exp(predict(
                   mod2$gam, newdata = test.data, re.form = NA)))
    
    output.info[[1]] <- bind_rows(output.info[[1]], t1)
    output.info[[2]] <- bind_rows(output.info[[2]], t2)
  }
  return(output.info)
}

check.gam.all<-ten.fold.gam(data=all.data,prop.d=.9)

hist(check.gam.all[[1]]$MSE)
hist(check.gam.all[[1]]$R2)
plot(check.gam.all[[2]]$man.fish~check.gam.all[[2]]$pred.fish)

check.gam.19<-ten.fold.gam(data=all.data%>%
                             filter(yr==2019),prop.d=.9)

hist(check.gam.19[[1]]$MSE)
hist(check.gam.19[[1]]$R2)
plot(check.gam.19[[2]]$man.fish~check.gam.19[[2]]$pred.fish)


check.gam.20<-ten.fold.gam(data=all.data%>%
                             filter(yr==2020),prop.d=.9)

hist(check.gam.20[[1]]$MSE)
hist(check.gam.20[[1]]$R2)
plot(check.gam.20[[2]]$man.fish~check.gam.20[[2]]$pred.fish)


# to use with linear models
mod<-lm(man.fish~log.fish.sc+spl.sc,data=all.data)

ten.fold.lm <- function(data,prop.d,model,logs=F) { 
  output.info = list(
    fit = data.frame(iteration = NA, MSE = NA,R2=NA)[-1, ],
    pred.values = data.frame(man.fish = NA, pred.fish = NA)[-1, ])
  for (i in 1:10) {
    train.data <- data %>%
      group_by(dataset) %>%
      slice_sample(prop = prop.d, replace = FALSE)
    
    test.data <- data %>%
      filter(!ID %in% train.data$ID)
    
    mod2<-update(model,.~.,data=train.data)
    
    if(logs==F){t1 <- data.frame(iteration = i, 
                     MSE = mean((train.data$man.fish-predict(
                       mod2, newdata = train.data, re.form = NA ))),
                     R2=summary(mod2)$adj.r.squared)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = predict(
                       mod2, newdata = test.data, re.form = NA))
    }
    if(logs==T){t1 <- data.frame(iteration = i, 
                                 MSE = mean((train.data$man.fish-exp(predict(
                                   mod2, newdata = train.data, re.form = NA )))),
                                 R2=summary(mod2)$adj.r.squared)
    
    t2 <- data.frame(man.fish = test.data$man.fish,
                     pred.fish = exp(predict(
                       mod2, newdata = test.data, re.form = NA)))
    }   
    
    output.info[[1]] <- bind_rows(output.info[[1]], t1)
    output.info[[2]] <- bind_rows(output.info[[2]], t2)
  }
  return(output.info)
}

mod<-lm(man.fish~log.fish.sc+spl.sc,data=all.data)
mod2<-lm(man.fish~auto.fish.sc+auto.fish.sc2+spl.sc,data=all.data)
mod3<-lm(log(man.fish+1)~log.fish.sc+spl.sc+spl.sc2,data=all.data)

check.simplelm.19<-ten.fold.lm(data=all.data%>%filter(yr==2019),model=mod,prop.d = .9)
check.simplelm.192<-ten.fold.lm(data=all.data%>%filter(yr==2019),model=mod2,prop.d = .9)
check.simplelm.193<-ten.fold.lm(data=all.data%>%filter(yr==2019),model=mod3,prop.d = .9,logs=T)


hist(check.simplelm.19[[1]]$MSE)
hist(check.simplelm.19[[1]]$R2)
plot(check.simplelm.19[[2]]$man.fish,check.simplelm.19[[2]]$pred.fish)

hist(check.simplelm.192[[1]]$MSE)
hist(check.simplelm.192[[1]]$R2)
plot(check.simplelm.192[[2]]$man.fish,check.simplelm.192[[2]]$pred.fish)

hist(check.simplelm.193[[1]]$MSE)
hist(check.simplelm.193[[1]]$R2)
plot(check.simplelm.193[[2]]$man.fish,check.simplelm.193[[2]]$pred.fish)


check.simplelm.20<-ten.fold.lm(data=all.data%>%filter(yr==2020),model=mod,prop.d = .9)
check.simplelm.202<-ten.fold.lm(data=all.data%>%filter(yr==2020),model=mod2,prop.d = .9)
check.simplelm.203<-ten.fold.lm(data=all.data%>%filter(yr==2020),model=mod3,prop.d = .9,logs=T)


hist(check.simplelm.20[[1]]$MSE)
hist(check.simplelm.20[[1]]$R2)
plot(check.simplelm.20[[2]]$man.fish,check.simplelm.20[[2]]$pred.fish)

hist(check.simplelm.202[[1]]$MSE)
hist(check.simplelm.202[[1]]$R2)
plot(check.simplelm.202[[2]]$man.fish,check.simplelm.202[[2]]$pred.fish)

hist(check.simplelm.203[[1]]$MSE)
hist(check.simplelm.203[[1]]$R2)
plot(check.simplelm.203[[2]]$man.fish,check.simplelm.203[[2]]$pred.fish)
     