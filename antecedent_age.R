#knitr::opts_chunk$set(cache=TRUE, echo=TRUE)
options(mc.cores = parallel::detectCores())
options(scipen=999)
library(dplyr)
#library(foreign)
#library(anytime)
#library(plyr)
#library(psych)
#library(naniar)
#library(nlme)
#library(DataCombine)
library(brms)
#library(ggplot2)
library(cmdstanr)
library(parallel)
#library(cowplot)
#library(brmstools)

data.nssi.thgts <- read.table("/mmfs1/home/jdora/data.nssi.thgts_ant.csv", row.names = NULL, header = T, sep = ",", na.strings=c("","NA"))

data.nssi.thgts$PID<-as.numeric(data.nssi.thgts$PID)

data.nssi.thgts$Study <- as.factor(as.character(data.nssi.thgts$Study))
levels(data.nssi.thgts$Study) <- c("Kiekens et al., 2020", "Kuehn et al., in Prep", "Lear et al., 2019", "Bresin et al., 2013", "Selby et al., 2018", "Hochard et al., 2015")

### need to turn this into a factor before fitting model!

data.nssi.thgts$NSSI_thgts <- as.factor(as.character(data.nssi.thgts$NSSI_thgts))

###

data.nssi.bhx <- read.table("/mmfs1/home/jdora/data.nssi.bhx_ant.csv", row.names = NULL, header = T, sep = ",", na.strings=c("","NA"))

data.nssi.bhx$PID<-as.numeric(data.nssi.bhx$PID)

data.nssi.bhx$Study <- as.factor(as.character(data.nssi.bhx$Study))
levels(data.nssi.bhx$Study) <- c("Armey et al., 2011", "Kiekens et al., 2020", "Kuehn et al., in prep", "Lear et al., 2019", "Muehlenkamp et al., 2009", "Santangelo et al., 2017", "Selby et al., 2013", "Bresin et al., 2013", "Selby et al., 2018", "Vansteelandt et al., 2017", "Wolford-Clevenger et al., 2019", "Czyz et al., 2017", "Hochard et al., 2015", "Houben et al., 2017")

### need to turn this into a factor before fitting model!

data.nssi.bhx$NSSI_bhx <- as.factor(as.character(data.nssi.bhx$NSSI_bhx))

###

data.sui.thgts <- read.table("/mmfs1/home/jdora/data.sui.thgts_ant.csv", row.names = NULL, header = T, sep = ",", na.strings=c("","NA"))

data.sui.thgts$PID<-as.numeric(data.sui.thgts$PID)

data.sui.thgts$Study <- as.factor(as.character(data.sui.thgts$Study))

levels(data.sui.thgts$Study) <- c("Kiekens et al., 2020", "Kleiman et al., 2017", "Kleiman et al., 2018", "Kuehn et al., in prep", "Peters et al., 2020", "Salim et al., 2019", "Bresin et al., 2013", "Wolford-Clevenger et al., 2019", "Czyz et al., 2017", "Forkmann et al., 2018", "Husky et al., 2017", "Kaurin et al., under review", "Kaurin et al., 2020")

### need to turn this into a factor before fitting model!

data.sui.thgts$suicidal_thgts <- as.factor(as.character(data.sui.thgts$suicidal_thgts))

data.nssi.thgts$mean.age <- ifelse(data.nssi.thgts$Study == "Kiekens et al., 2020", 20.1, ifelse(
  data.nssi.thgts$Study == "Kuehn et al., in Prep", 18.58, ifelse(
    data.nssi.thgts$Study == "Lear et al., 2019", 19.87, ifelse(
      data.nssi.thgts$Study == "Bresin et al., 2013", 19.58, ifelse(
        data.nssi.thgts$Study == "Selby et al., 2018", 19.07, 21.04
      )
    )
  )
))

data.nssi.thgts$mean.age <- scale(data.nssi.thgts$mean.age, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_thgts1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "mean.age"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_thgts1:mean.age"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_nssi_thgts_antecedent_age <-brm(NA.standard.CWP.lag ~ 0 + Intercept + NSSI_thgts*mean.age + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + NSSI_thgts | Study) + ( 0 + NSSI_thgts | Study:PID), prior=prior, data=data.nssi.thgts, control = list(adapt_delta = 0.99, max_treedepth=15),
                                          sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_nssi_thgts_antecedent_age)

data.nssi.bhx$mean.age <- ifelse(data.nssi.bhx$Study == "Armey et al., 2011", 18.7, ifelse(
  data.nssi.bhx$Study == "Kiekens et al., 2020", 20.1, ifelse(
    data.nssi.bhx$Study == "Kuehn et al., in Prep", 18.58, ifelse(
      data.nssi.bhx$Study == "Lear et al., 2019", 19.87, ifelse(
        data.nssi.bhx$Study == "Muehlenkamp et al., 2009", 25.3, ifelse(
          data.nssi.bhx$Study == "Santangelo et al., 2017", 15.9, ifelse(
            data.nssi.bhx$Study == "Selby et al., 2013", 28.84, ifelse(
              data.nssi.bhx$Study == "Bresin et al., 2013", 19.58, ifelse(            
                data.nssi.bhx$Study == "Selby et al., 2018", 19.07, ifelse(
                  data.nssi.bhx$Study == "Vansteelandt et al., 2017", 28, ifelse(
                    data.nssi.bhx$Study == "Wolford-Clevenger et al., 2019", 19.11, ifelse(
                      data.nssi.bhx$Study == "Czyz et al., 2017", 15.5, ifelse(
                        data.nssi.bhx$Study == "Hochard et al., 2015", 21.04, 29.03)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
)

data.nssi.bhx$mean.age <- scale(data.nssi.bhx$mean.age, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_bhx1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "mean.age"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_bhx1:mean.age"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_NSSI_bhx_antecedent_age <-brm(NA.standard.CWP.lag ~ 0 + Intercept + NSSI_bhx*mean.age + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + NSSI_bhx | Study) + ( 0 + NSSI_bhx | Study:PID), prior=prior, data=data.nssi.bhx, control = list(adapt_delta = 0.99, max_treedepth=15),
                                        sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_NSSI_bhx_antecedent_age)

data.sui.thgts$mean.age <- ifelse(data.sui.thgts$Study == "Kiekens et al., 2020", 20.1, ifelse(
  data.sui.thgts$Study == "Kleiman et al., 2017", 23.52, ifelse(
    data.sui.thgts$Study == "Kleiman et al., 2018", 23.28, ifelse(
      data.sui.thgts$Study == "Kuehn et al., in Prep", 18.58, ifelse(
        data.sui.thgts$Study == "Peters et al., 2020", 36.3, ifelse(
          data.sui.thgts$Study == "Salim et al., 2019", 24, ifelse(
            data.sui.thgts$Study == "Bresin et al., 2013", 19.58, ifelse(
              data.sui.thgts$Study == "Wolford-Clevenger et al., 2019", 19.11, ifelse(
                data.sui.thgts$Study == "Czyz et al., 2017", 15.5, ifelse(
                  data.sui.thgts$Study == "Forkmann et al., 2018", 37.6, ifelse(
                    data.sui.thgts$Study == "Husky et al., 2017", 37.9, ifelse(
                      data.sui.thgts$Study == "Kaurin et al., under review", 33.32, 33.43)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
)

data.sui.thgts$mean.age <- scale(data.sui.thgts$mean.age, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "suicidal_thgts1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "mean.age"),
           set_prior("normal(0, 0.5)", class = "b", coef = "suicidal_thgts1:mean.age"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_suicidal_thgts_antecedent_age <-brm(NA.standard.CWP.lag ~ 0 + Intercept + suicidal_thgts*mean.age + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + suicidal_thgts | Study) + ( 0 + suicidal_thgts | Study:PID), prior=prior, data=data.sui.thgts, control = list(adapt_delta = 0.99, max_treedepth=15),
                                              sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_suicidal_thgts_antecedent_age)

save.image("antecedent_age.RData", compress = "xz")