# Problem Set 7
# Michael Yuhas


#Change working directory for use in OSCER

dird = "C://Users//MJYuh//Documents//OU 2017-2018//Spring 2018//ECON 5970//PS 7"
setwd(dird)

#Libraries
library(MixedDataImpute)
library(mice)
library(stargazer)

wage = read.csv("wages.csv")
head(wage)

wage.1 = wage[!(is.na(wage$hgc)),] 
wage.1 = wage[!(is.na(wage$tenure)),]

#Latex output for data summary
stargazer(wage.1)

#gives number of missing observations
lw.miss = list(wage[(is.na(wage$logwage)),])

#Number of missing over total observations
lw.rate = 560/2246
lw.rate
