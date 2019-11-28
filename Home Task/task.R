getwd()
setwd("/home/petr/Рабочий стол/R/Home Task")
rm(list=ls())

#install.packages("tidyverse")
#install.packages("psych")
#install.packages("corrgram")
#install.packages("caret")

data(DNase) 
DNase$Run = NULL

#h1 <- hist(DNase$density, freq = F) 
#curve(dnorm(x, mean=mean(DNase$density), sd=sd(DNase$density)), add = TRUE)

#DNase_log <- log(DNase)

#h2 <- hist(DNase_log$density, freq = FALSE)
#curve(dnorm(x, mean=mean(DNase_log$density), sd=sd(DNase_log$density)), add = TRUE)

library(psych)
describe(DNase)

library(caret)
lin_mod <- lm(density~conc, data = DNase)
print(lin_mod)
summary(lin_mod)

#lin_mod_log <- lm(density~conc, data = DNase_log)
#print(lin_mod)
#summary(lin_mod)
