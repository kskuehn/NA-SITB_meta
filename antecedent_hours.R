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

data.nssi.thgts$hours.between.prompts <- ifelse(data.nssi.thgts$Study == "Kiekens et al., 2020", 1.5, ifelse(
  data.nssi.thgts$Study == "Kuehn et al., in Prep", 4.32, ifelse(
    data.nssi.thgts$Study == "Lear et al., 2019", 24, ifelse(
      data.nssi.thgts$Study == "Bresin et al., 2013", 24, ifelse(
        data.nssi.thgts$Study == "Selby et al., 2018", 2.5, 8
      )
    )
  )
))

data.nssi.thgts$hours.between.prompts <- scale(data.nssi.thgts$hours.between.prompts, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_thgts1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "hours.between.prompts"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_thgts1:hours.between.prompts"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_nssi_thgts_antecedent_hours <-brm(NA.standard.CWP.lag ~ 0 + Intercept + NSSI_thgts*hours.between.prompts + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + NSSI_thgts | Study) + ( 0 + NSSI_thgts | Study:PID), prior=prior, data=data.nssi.thgts, control = list(adapt_delta = 0.99, max_treedepth=15),
                                        sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_nssi_thgts_antecedent_hours)

data.nssi.bhx$hours.between.prompts <- ifelse(data.nssi.bhx$Study == "Armey et al., 2011", 2, ifelse(
  data.nssi.bhx$Study == "Kiekens et al., 2020", 1.5, ifelse(
    data.nssi.bhx$Study == "Kuehn et al., in Prep", 4.32, ifelse(
      data.nssi.bhx$Study == "Lear et al., 2019", 24, ifelse(
        data.nssi.bhx$Study == "Muehlenkamp et al., 2009", 24, ifelse(
          data.nssi.bhx$Study == "Santangelo et al., 2017", 1, ifelse(
            data.nssi.bhx$Study == "Selby et al., 2013", NA, ifelse(
              data.nssi.bhx$Study == "Bresin et al., 2013", 24, ifelse(            
                data.nssi.bhx$Study == "Selby et al., 2018", 2.5, ifelse(
                  data.nssi.bhx$Study == "Vansteelandt et al., 2017", 1.3, ifelse(
                    data.nssi.bhx$Study == "Wolford-Clevenger et al., 2019", 24, ifelse(
                      data.nssi.bhx$Study == "Czyz et al., 2017", 24, ifelse(
                        data.nssi.bhx$Study == "Hochard et al., 2015", 8, 1.3)
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

data.nssi.bhx$hours.between.prompts <- scale(data.nssi.bhx$hours.between.prompts, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_bhx1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "hours.between.prompts"),
           set_prior("normal(0, 0.5)", class = "b", coef = "NSSI_bhx1:hours.between.prompts"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_NSSI_bhx_antecedent_hours <-brm(NA.standard.CWP.lag ~ 0 + Intercept + NSSI_bhx*hours.between.prompts + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + NSSI_bhx | Study) + ( 0 + NSSI_bhx | Study:PID), prior=prior, data=data.nssi.bhx, control = list(adapt_delta = 0.99, max_treedepth=15),
                                      sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_NSSI_bhx_antecedent_hours)

data.sui.thgts$hours.between.prompts <- ifelse(data.sui.thgts$Study == "Kiekens et al., 2020", 1.5, ifelse(
  data.sui.thgts$Study == "Kleiman et al., 2017", 24, ifelse(
    data.sui.thgts$Study == "Kleiman et al., 2018", 7.95, ifelse(
      data.sui.thgts$Study == "Kuehn et al., in Prep", 4.32, ifelse(
        data.sui.thgts$Study == "Peters et al., 2020", 5.5, ifelse(
          data.sui.thgts$Study == "Salim et al., 2019", 24, ifelse(
            data.sui.thgts$Study == "Bresin et al., 2013", 24, ifelse(
              data.sui.thgts$Study == "Wolford-Clevenger et al., 2019", 24, ifelse(
                data.sui.thgts$Study == "Czyz et al., 2017", 24, ifelse(
                  data.sui.thgts$Study == "Forkmann et al., 2018", 0.5, ifelse(
                    data.sui.thgts$Study == "Husky et al., 2017", 2.8, ifelse(
                      data.sui.thgts$Study == "Kaurin et al., under review", NA, NA)
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

data.sui.thgts$hours.between.prompts <- scale(data.sui.thgts$hours.between.prompts, center = T, scale = F)

prior <- c(set_prior("normal(0, 1)", class = "b", coef = "Intercept"),
           set_prior("normal(0, 0.5)", class = "b", coef = "suicidal_thgts1"), 
           set_prior("normal(0, 0.5)", class = "b", coef = "hours.between.prompts"),
           set_prior("normal(0, 0.5)", class = "b", coef = "suicidal_thgts1:hours.between.prompts"),
           set_prior("student_t(3,0,2.5)", class="sd"),
           set_prior("student_t(3,0,2.5", class="sigma"))

model_suicidal_thgts_antecedent_hours <-brm(NA.standard.CWP.lag ~ 0 + Intercept + suicidal_thgts*hours.between.prompts + ( 0 + Intercept | Study) + ( 0 + Intercept | Study:PID) + ( 0 + suicidal_thgts | Study) + ( 0 + suicidal_thgts | Study:PID), prior=prior, data=data.sui.thgts, control = list(adapt_delta = 0.99, max_treedepth=15),
                                              sample_prior = TRUE, iter = 6000, chains = 4, backend = "cmdstanr", threads = threading(9))

summary(model_suicidal_thgts_antecedent_hours)

save.image("antecedent_hours.RData", compress = "xz")