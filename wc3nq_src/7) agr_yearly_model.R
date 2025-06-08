#####Yearly Model with Agonistic Network####
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
sur_data$pup_year_surv <- as.factor(sur_data$pup_year_surv)
sur_data$pup_yrborn <- as.factor(sur_data$pup_yrborn)
sur_data$overall.index <- as.factor(sur_data$overall.index)
sur_data$pup_sex <- as.factor(sur_data$pup_sex)
sur_data$valley <- as.factor(sur_data$valley)
sur_data$mother_uid <- as.factor(sur_data$mother_uid)
sur_data$pup_massaug <- as.factor(sur_data$pup_massaug)
sur_data$pup_littersizeborn <- as.factor(sur_data$pup_littersizeborn)
sur_data$mother_age <- as.factor(sur_data$mother_age)
sur_data$pup_emerjdate <- as.factor(sur_data$pup_emerjdate)
sur_data$network_size <- as.factor(sur_data$network_size)

sur_data[ is.na(sur_data) ] <- NA #remove NAs
na.exclude(sur_data)
sur_data <- subset(sur_data, pup_yrborn != "2011")#Remove rows where pups were born in 2011 to allow models to converge

#Ensure rest of variables are read in properly
sur_data$pup_massaug <- as.numeric(sur_data$pup_massaug)
sur_data$pup_littersizeborn <- as.numeric(sur_data$pup_littersizeborn)
sur_data$group_size <- as.numeric(sur_data$group_size)
sur_data$network_size <- as.numeric(sur_data$network_size)

sur_data$mother_age <- as.numeric(sur_data$mother_age)
sur_data$pup_emerjdate <- as.numeric(sur_data$pup_emerjdate)
sur_data$network_size <- as.numeric(sur_data$network_size)

#Log transform
sur_data$log_pup_massaug <- log(sur_data$pup_massaug)


####Models by social network measure####
#1. Indegree
m1 <- glmer(pup_year_surv~indegree + valley + overall.index + pup_sex + pup_littersizeborn + mother_age + network_size + pup_emerjdate + overall.index*indegree + valley*indegree + (1|mother_uid) + (1|pup_yrborn), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=sur_data, family= binomial)
summary(m1)
#checking assumptions 
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
m2 <- glmer(pup_year_surv~outdegree + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_pup_massaug + overall.index*outdegree + valley*outdegree + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m2)

#checking assumptions 
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
m3 <- glmer(pup_year_surv~betweenness + valley + overall.index + pup_sex + pup_littersizeborn + mother_age + network_size + pup_emerjdate + overall.index*betweenness + valley*betweenness + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m3)

#checking assumptions 
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
m4 <- glmer(pup_year_surv~outcloseness + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_pup_massaug + overall.index*outcloseness + valley*outcloseness + (1|mother_uid) + (1|pup_yrborn), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=sur_data, family= binomial)
summary(m4)

#checking assumptions 
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
m5 <- glmer(pup_year_surv~incloseness + valley + overall.index + pup_sex + pup_littersizeborn + network_size  + mother_age + pup_emerjdate + overall.index*incloseness + valley*incloseness + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m5)

#checking assumptions 
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
m6 <- glmer(pup_year_surv~local_clustering + valley + overall.index + pup_sex + pup_littersizeborn + network_size  +  mother_age + pup_emerjdate + log_pup_massaug + overall.index*local_clustering + valley*local_clustering + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m6)

#checking assumptions 
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
m7 <- glmer(pup_year_surv~neg_ave_shortest_path + valley + overall.index + pup_sex +  pup_littersizeborn + network_size + mother_age + pup_emerjdate + overall.index*neg_ave_shortest_path + valley*neg_ave_shortest_path + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m7)

#checking assumptions 
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
m8 <- glmer(pup_year_surv~eigenv + valley + overall.index + pup_sex + pup_littersizeborn +  network_size  + mother_age + pup_emerjdate + log_pup_massaug + overall.index*eigenv + valley*eigenv + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m8)

#checking assumptions 
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
m9 <- glmer(pup_year_surv~outstrength + valley + overall.index + pup_sex + pup_littersizeborn + network_size  + mother_age + pup_emerjdate + overall.index*outstrength + valley*outstrength + (1|mother_uid) + (1|pup_yrborn), data=sur_data,control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m9)

#checking assumptions 
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
m10 <- glmer(pup_year_surv~instrength + valley + overall.index + pup_sex + pup_littersizeborn + network_size  + mother_age + pup_emerjdate + overall.index*instrength + valley*instrength + (1|mother_uid) + (1|pup_yrborn), data=sur_data, control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), family= binomial)
summary(m10)

#checking assumptions 
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


#Check singularity
isSingular(m9, tol = 1e-05)
isSingular(m10, tol = 1e-05)

#Summaries of all models
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

#Calculating r-squared of random effect
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


