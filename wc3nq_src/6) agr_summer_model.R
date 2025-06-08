#####Summer Model with Agonistic Network####
####Date: Jan 8, 2021
####Code written by: Anita Montero
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we run linear model 

#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/')
library(optimx)
library(lme4)
library(lmerTest)
library(MuMIn)

####Prepare Data File####
#Read in csv file
sur_data <- read.csv("agr_NoSurvNAs.csv")
#Ensure variables are read in properly
sur_data$pup_summer_surv <- as.factor(sur_data$pup_summer_surv)
sur_data$pup_yrborn <- as.factor(sur_data$pup_yrborn)
sur_data$overall.index <- as.factor(sur_data$overall.index)
sur_data$pup_emerjdate <- as.factor(sur_data$pup_emerjdate)
sur_data$pup_sex <- as.factor(sur_data$pup_sex)
sur_data$pup_massaug <- as.factor(sur_data$pup_massaug)
sur_data$pup_littersizeborn <- as.factor(sur_data$pup_littersizeborn)
sur_data$valley <- as.factor(sur_data$valley)
sur_data$mother_age <- as.factor(sur_data$mother_age)
sur_data$mother_uid <- as.factor(sur_data$mother_uid)
sur_data$mass_gain.day <- as.factor(sur_data$mass_gain.day)

sur_data[ is.na(sur_data) ] <- NA #remove NAs
na.exclude(sur_data)
sur_data <- subset(sur_data, pup_yrborn != "2011")#Remove rows where pups were born in 2011 to allow models to converge


#Ensure rest of variables are read in properly
sur_data$pup_massaug <- as.numeric(sur_data$pup_massaug)
sur_data$pup_littersizeborn <- as.numeric(sur_data$pup_littersizeborn)
sur_data$group_size <- as.numeric(sur_data$group_size)
sur_data$mother_age <- as.numeric(sur_data$mother_age)
sur_data$mass_gain.day <- as.numeric(sur_data$mass_gain.day)
sur_data$pup_emerjdate <- as.numeric(sur_data$pup_emerjdate)
sur_data$network_size <- as.numeric(sur_data$network_size)

#Log transform
sur_data$log_mass_gain.day <- log(sur_data$mass_gain.day)

#Basic model, excluding year- look to see if one year has too few observations to be run
m0 <- glmer(pup_summer_surv~ + valley + overall.index + pup_sex + pup_littersizeborn + group_size  +  mother_age + pup_emerjdate + log_mass_gain.day + (1|mother_uid) + (1|pup_yrborn), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=sur_data, family= binomial)
summary(m0)

####Models by social network measure####
#1. Indegree
m1 <- glmer(pup_summer_surv~indegree + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*indegree + valley*indegree + (1|mother_uid) + (1|pup_yrborn), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=sur_data, family= binomial)
summary(m1)

#checking assumptions 

par(mfrow=c(2,2))

library(ggplot2)

r <- residuals(m1)
f <- fitted(m1)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m1)
f <- fitted(m1)

hist(r)

qqnorm(r)

plot(f,r)
results_m1o <- summary(m1)$coefficients[,1]
results_m1o <- as.data.frame(results_m1o)

#2. Outdegree
m2 <- glmer(pup_summer_surv~outdegree + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*outdegree + valley*outdegree + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m2)

#checking assumptions

par(mfrow=c(2,2))

r <- residuals(m2)
f <- fitted(m2)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m2)
f <- fitted(m2)

hist(r)

qqnorm(r)

plot(f,r)
results_m2o <- summary(m2)$coefficients[,1]
results_m2o <- as.data.frame(results_m2o)

#3. Betweenness
m3 <- glmer(pup_summer_surv~betweenness + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*betweenness + valley*betweenness + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=25e5)), family= binomial)
summary(m3)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m3)
f <- fitted(m3)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m3)
f <- fitted(m3)

hist(r)

qqnorm(r)

plot(f,r)
results_m3o <- summary(m3)$coefficients[,1]
results_m3o <- as.data.frame(results_m3o)

#4. Outcloseness
m4 <- glmer(pup_summer_surv~outcloseness + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*outcloseness + valley*outcloseness + (1|mother_uid) + (1|pup_yrborn), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=sur_data, family= binomial)
summary(m4)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m4)
f <- fitted(m4)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m4)
f <- fitted(m4)

hist(r)

qqnorm(r)

plot(f,r)
results_m4o <- summary(m4)$coefficients[,1]
results_m4o <- as.data.frame(results_m4o)

#5. Incloseness
m5 <- glmer(pup_summer_surv~incloseness + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*incloseness + valley*incloseness + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m5)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m5)
f <- fitted(m5)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m5)
f <- fitted(m5)

hist(r)

qqnorm(r)
plot(f,r)
results_m5o <- summary(m5)$coefficients[,1]
results_m5o <- as.data.frame(results_m5o)

#6. Local clustering
m6 <- glmer(pup_summer_surv~local_clustering + valley+ overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*local_clustering + valley*local_clustering +  (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m6)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m6)
f <- fitted(m6)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m6)
f <- fitted(m6)

hist(r)

qqnorm(r)

plot(f,r)
results_m6o <- summary(m6)$coefficients[,1]
results_m6o <- as.data.frame(results_m6o)

#7. Negative average shortest path 
m7 <- glmer(pup_summer_surv~neg_ave_shortest_path + overall.index + valley + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*neg_ave_shortest_path + valley*neg_ave_shortest_path + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m7)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m7)
f <- fitted(m7)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m7)
f <- fitted(m7)

hist(r)

qqnorm(r) 

plot(f,r)

results_m7o <- summary(m7)$coefficients[,1]
results_m7o <- as.data.frame(results_m7o)

#8. Eigenvector centrality 
m8 <- glmer(pup_summer_surv~eigenv + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*eigenv + valley*eigenv + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m8)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m8)
f <- fitted(m8)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m8)
f <- fitted(m8)

hist(r)

qqnorm(r)

plot(f,r)
results_m8o <- summary(m8)$coefficients[,1]
results_m8o <- as.data.frame(results_m8o)

#9. Outstrength
m9 <- glmer(pup_summer_surv~outstrength + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*outstrength + valley*outstrength + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m9)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m9)
f <- fitted(m9)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m9)
f <- fitted(m9)

hist(r)

qqnorm(r)

plot(f,r)
results_m9o <- summary(m9)$coefficients[,1]
results_m9o <- as.data.frame(results_m9o)

#10. Instrength
m10 <- glmer(pup_summer_surv~instrength + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_mass_gain.day + overall.index*instrength + valley*instrength + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m10)

#checking assumptions 

par(mfrow=c(2,2))

r <- residuals(m10)
f <- fitted(m10)
d <- data.frame(f=f, r=r)

ggplot(d, aes(x=f, y=r)) + geom_point() +
  stat_summary_bin(fun.y="mean", geom="point", color="red")  + 
  theme(text=element_text(size=16, family="Times"))

r <- residuals(m10)
f <- fitted(m10)

hist(r)

qqnorm(r)

plot(f,r)
results_m10o <- summary(m10)$coefficients[,1]
results_m10o <- as.data.frame(results_m10o)

#Calculate R squared for each model
r.squaredGLMM(m1) #indegree
r.squaredGLMM(m2) #outdegree
r.squaredGLMM(m3) #betweenness
r.squaredGLMM(m4) #outcloseness
r.squaredGLMM(m5) #incloseness
r.squaredGLMM(m6) #local clustering
r.squaredGLMM(m7) #average shortest path length
r.squaredGLMM(m8) #eigenvector
r.squaredGLMM(m9) #outstrength
r.squaredGLMM(m10) #instrength

#Summaries of each model 
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10)

#Calculate r-squared of random effect
library(MuMIn)

r.squaredGLMM(m1)
re <- ranef(m1)

r.squaredGLMM(m2)
re <- ranef(m2)

r.squaredGLMM(m3)
re <- ranef(m3)

r.squaredGLMM(m4)
re <- ranef(m4)

r.squaredGLMM(m5)
re <- ranef(m5)

r.squaredGLMM(m6)
re <- ranef(m6)

r.squaredGLMM(m7)
re <- ranef(m7)

r.squaredGLMM(m8)
re <- ranef(m8)

r.squaredGLMM(m9)
re <- ranef(m9)

r.squaredGLMM(m10)
re <- ranef(m10)
