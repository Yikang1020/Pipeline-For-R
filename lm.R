# set working dir
setwd('D:/yikang1020/yikangsystem1020/3_Code/Eisenberg2020/Data/Complete_02-16-2019')
# load packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)

demo = read.csv('demographics.csv')
dv_ddm = read.csv('meaningful_variables.csv')
dv_noddm = read.csv('meaningful_variables_noDDM.csv')


df = data.frame(demo[1],
                factor(demo$Sex),
                dv_noddm$stroop.congruent_rt,
                dv_noddm$stroop.incongruent_rt,
                dv_ddm$stroop.hddm_drift)
df.long = df %>%
  rename(congruent=dv_noddm.stroop.congruent_rt,
         incongruent=dv_noddm.stroop.incongruent_rt,
         drift_rate = dv_ddm.stroop.hddm_drift,
         subject=X,
         sex = factor.demo.Sex.) %>%
  pivot_longer(c(congruent,incongruent),names_to = "condition", values_to = "rt")

df.long$condition = factor(df.long$condition)
df.long$subject = factor(df.long$subject)
##  lm0   #########################################################
lm0 = lm(rt ~ drift_rate+condition, data =df.long)
summary(lm0)

ggplot(df.long,
       aes(x = drift_rate, 
           y = rt,
           group=condition,
           color = condition
           ))+
  geom_point()+
  geom_abline(intercept =c(coef(lm0)[1],coef(lm0)[1]+coef(lm0)[3]),
              slope = c(coef(lm0)[2], coef(lm0)[2]))
##  lm1   #########################################################
lm1 = lm(rt ~ drift_rate+condition+drift_rate:condition, data =df.long)
summary(lm1)

ggplot(df.long,
       aes(x = drift_rate, 
           y = rt,
           group=condition,
           color = condition
       ))+
  geom_point()+
  geom_abline(intercept =c(coef(lm1)[1],coef(lm1)[1]+coef(lm1)[3]),
              slope = c(coef(lm1)[2], coef(lm1)[2]+coef(lm1)[4]))
##  lm2   #########################################################
lm2 = lmer( rt ~ drift_rate + condition + drift_rate:condition +( 1 |subject ), data = df.long)
summary(lm2)
ggplot(df.long,
       aes(x = drift_rate, 
           y = rt,
           color =condition,
       ))+
  geom_point()+
  geom_abline(intercept =coef(lm2)$subject[1][1],
              slope = coef(lm2)$subject[2][1])

##  lm3   #########################################################
lm3 = lmer( rt ~ drift_rate + condition + drift_rate:condition +( 0 + drift_rate |sex), data = df.long)
summary(lm3)

ggplot(df.long,
       aes(x = drift_rate, 
           y = rt,
           group=condition,
           color = condition
       ))+
  geom_point()+
  geom_abline(intercept =c(coef(lm1)[1],coef(lm0)[1]+coef(lm1)[3]),
              slope = c(coef(lm1)[2], coef(lm0)[2]+coef(lm1)[4]))

##  lm4   #########################################################
lm4 = lmer( rt ~ drift_rate + condition + drift_rate:condition +( 1 + drift_rate |sex), data = df.long)
summary(lm4)

ggplot(df.long,
       aes(x = drift_rate, 
           y = rt,
           group=condition,
           color = condition
       ))+
  geom_point()+
  geom_abline(intercept =c(coef(lm1)[1],coef(lm0)[1]+coef(lm1)[3]),
              slope = c(coef(lm1)[2], coef(lm0)[2]+coef(lm1)[4]))
#write.table(df.long,"example9.csv",row.names=FALSE,col.names=TRUE,sep=",")