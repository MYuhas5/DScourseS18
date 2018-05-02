# Problem Set 12
#Author: Michael Yuhas

#Change working directory for use in OSCER

dird = "C://Users//MJYuh//DScourseS18//Problem Sets//PS12"
setwd(dird)

# Libraries
library(sampleSelection)
library(tidyverse)
library(stargazer)

# Load in the Data
wage = read.csv("wages12.csv")

wage$college = factor(wage$college, labels = c("no", "yes"))
wage$married = factor(wage$married, labels = c("no", "yes"))
wage$union = factor(wage$union, labels = c("no", "yes"))

#Stargazer output production
stargazer(wage)


# Listwise deletion
wage.listwise = lm(logwage ~ hgc + union + college + exper + I(exper^2), data=wage, na.action=na.omit)
print(summary(wage.listwise))


# Mean imputation
wage$logwage_mean_imp = wage$logwage
wagebar = mean(wage$logwage,na.rm=T)
wage$logwage_mean_imp[is.na(wage$logwage_mean_imp)] = wagebar

wage.mean.imp = lm(logwage_mean_imp ~ hgc + union + college + exper + I(exper^2), data=wage, na.action=na.omit)
print(summary(wage.mean.imp))

#Non Missing accounability
valid = is.na(wage$logwage)


#Probit Estimation
wage.probit = glm(union ~ hgc + college + exper + married + kids, family = binomial(link="probit"), data =wage)
print(summary(wage.probit))
## Counterfactual Policy

# Compute Predicted Probabilities
wage$predProbit = predict(wage.probit, newdata = wage, type = "response")
print(summary(wage$predProbit))

#Change coefficients to zero
wage.probit$coefficients["kids"]= 0
wage.probit$coefficients["married"]=0

#Compute new predicted probabilities
wage$counterprobit = predict(wage.probit, newdata = wage, type = "response")
diff = wage$predProbit-wage$counterprobit
summary(diff)
