# Author: Michael Yuhas
# Final project

dird = "C://Users//MJYuh//DScourseS18//FinalProject//Yuhas"

setwd(dird)
set.seed(150)

# Libraries
library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(ggplot2)
# Data obtained from: https://www.kaggle.com/nickhould/craft-cans/data
beer = read.csv("beers.csv")

###### Variables
# Numerical Indicator of beer
# abv = ABV of Beer(Alcohol by Volume) - continuous
# ibu = International bittering Unit - string
# id = identification number assigned to beer - discrete
# name = name of beer - string
# style = style of beer - string
# brewery_id = brewery identification number or cross reference with a brewery dataset - Discrete
# ounces = size of beer in ounces - continuous
######

#####
#Data Cleaning
#####
#Drop Unused Variables
beer$ibu        = NULL
beer$name       = NULL
beer$brewery_id = NULL
beer$X          = NULL
beer$id         = NULL

# Confirm Continuous Variables
beer$abv    = as.numeric(beer$abv)
beer$ounces = as.numeric(beer$ounces)


#Drop Missing ABV and Style values
beer = beer[!is.na(beer$abv) & !is.na(beer$style) & !is.na(beer$ounces),]

# Break up data for training
n = nrow(beer)
train = sample(n, size = .8*n)
test  = setdiff(1:n, train)
beer.train = beer[train,]
beer.test  = beer[test, ]

#Creation of task

# Configure MLR for possible errors due to factor sampling
configureMlr(on.learner.error = "warn")

beer.Task = makeClassifTask(id = "taskname", data = beer.train, target = "style", fixup.data = "no", check.data = FALSE)

### Create prediction algorithms

#KKNN
pred.kknn = makeLearner("classif.kknn", predict.type = "response")
#SVM
pred.svm = makeLearner("classif.svm", predict.type = "response")



# Set resampling strategy (10-fold CV)
resamplestrat = makeResampleDesc(method = "CV", iters = 10)


# Take 10 random guess tuning strat 
tunemethod = makeTuneControlRandom(maxit = 10L)

### Create Parameters

#KKNN
param_kknn = makeParamSet(makeIntegerParam("k", lower = 1,upper = 40))

#SVM
param_svm =  makeParamSet(makeDiscreteParam("cost", values = 2^c(-2,-1, 0, 1, 2, 10)),
                         makeDiscreteParam("gamma", values = 2^c(-2,-1, 0, 1, 2, 10)))

### Tuning

#KKNN
tunedmodel_kknn = tuneParams(learner = pred.kknn,
                            task = beer.Task,
                            resampling = resamplestrat,
                            measures = list(acc),      
                            par.set = param_kknn,
                            control = tunemethod,
                            show.info = TRUE)



#SVM
tunedmodel_svm = tuneParams(learner = pred.svm,
                           task = beer.Task,
                           resampling = resamplestrat,
                           measures = list(acc),      
                           par.set = param_svm,
                           control = tunemethod,
                           show.info = TRUE)

### Training Models

# Optimal Tuning Parameters
pred.kknn = setHyperPars(learner = pred.kknn,par.vals = tunedmodel_kknn$x)

pred.svm = setHyperPars(learner = pred.svm,par.vals = tunedmodel_svm$x)

# Verify Performance
performance.kknn = resample(pred.kknn,beer.Task,resamplestrat,measures=list(acc))

performance.svm = resample(pred.svm,beer.Task,resamplestrat,measures=list(acc))

# Train Final Model
finalModel.kknn = train(learner=pred.kknn,task=beer.Task)

finalModel.svm = train(learner=pred.svm,task=beer.Task)

# Make Predictions
prediction.kknn = predict(finalModel.kknn,newdata=beer.test)

prediction.svm = predict(finalModel.svm,newdata=beer.test)


# Test out of sample performance
perf.kknn = performance(prediction.kknn, measures = list(acc))

perf.svm = performance(prediction.svm, measures  = list(acc))

perf.kknn
perf.svm

# Creation of modelling figures for writeup
plot(beer$abv, beer$style)

#Create summary table for writeup
beer.table = stargazer(beer)


