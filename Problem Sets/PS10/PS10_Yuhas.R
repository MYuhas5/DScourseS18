# Problem Set 9
#Author: Michael Yuhas
#Collaborators: Alex Skipper and Conrad Polkosnik


#Change working directory for use in OSCER

dird = "C://Users//MJYuh//DScourseS18//Problem Sets//PS10"
setwd(dird)

# Libraries
library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)


#####
#PS Start by Dr. Tyler Ransom
####

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#############
#End of Dr. Ransom's Work
############


# Create the Task 
theTask <- makeClassifTask(id = "taskname", data = income.train, target = "high.earner")

##########
# Create prediction algorithms with mlr
##########
#TREE
pred.tree <- makeLearner("classif.rpart", predict.type = "response")
#Logistic Regression 
pred.logic <- makeLearner("classif.glmnet", predict.type = "response")
#Neural Network
pred.neural <- makeLearner("classif.nnet", predict.type = "response")
#Naive Bayes 
pred.bayes <- makeLearner("classif.naiveBayes", predict.type = "response")
#KKNN
pred.kknn <- makeLearner("classif.kknn", predict.type = "response")
#SVM
pred.svm <- makeLearner("classif.svm", predict.type = "response")



# Set resampling strategy (3-fold CV)
resamplestrat <- makeResampleDesc(method = "CV", iters = 3)


# Take 10 random guess tuning strat 
tunemethod <- makeTuneControlRandom(maxit = 10L)


########################################################
########################################################


#Part 6 
#Setting up the parameters 

#TREE 
param_tree<- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50), 
  makeIntegerParam("minbucket", lower = 5, upper = 50), 
  makeNumericParam("cp", lower = 0.001, upper = 0.2)) 

#Logistic Regression 
param_regression <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                                makeNumericParam("alpha",lower=0,upper=1))

#Neural Network
param_neural<- makeParamSet(makeIntegerParam("size", lower = 1,upper = 10),
                           makeNumericParam("decay",lower = 0.1,upper = 0.5),
                           makeIntegerParam("maxit",lower = 1000,upper = 1000))
#kknn
param_kknn<-makeParamSet(makeIntegerParam("k", lower = 1,upper = 30))

#SVM
param_svm<- makeParamSet(makeDiscreteParam("cost", values = 2^c(-2,-1, 0, 1, 2, 10)),
                        makeDiscreteParam("gamma", values = 2^c(-2,-1, 0, 1, 2, 10)))





########################################################
########################################################


#Part 7                
# Do the tuning

#Tree model with F1 score 
tunedmodel_tree <- tuneParams(learner = pred.tree,
                              task = theTask,
                              resampling = resamplestrat,
                              measures =list(f1,gmean),
                              par.set = param_tree,
                              control = tunemethod,
                              show.info = TRUE)


#Logistic Regression 
tunedmodel_logic<- tuneParams(learner = pred.logic,
                              task = theTask,
                              resampling = resamplestrat,
                              measures = list(f1,gmean),      
                              par.set = param_regression,
                              control = tunemethod,
                              show.info = TRUE)


#Neural Network 

tunedmodel_neural<-tuneParams(learner = pred.neural,
                              task = theTask,
                              resampling = resamplestrat,
                              measures = list(f1,gmean),      
                              par.set = param_neural,
                              control = tunemethod,
                              show.info = TRUE)



#kknn
tunedmodel_kknn<-tuneParams(learner = pred.kknn,
                            task = theTask,
                            resampling = resamplestrat,
                            measures = list(f1,gmean),      
                            par.set = param_kknn,
                            control = tunemethod,
                            show.info = TRUE)



#svm
tunedmodel_svm<-tuneParams(learner = pred.svm,
                           task = theTask,
                           resampling = resamplestrat,
                           measures = list(f1,gmean),      
                           par.set = param_svm,
                           control = tunemethod,
                           show.info = TRUE)


########################################################
########################################################

#Training and predicting 


#Applying the optimal tuning parameter to each algorith


pred.tree<-setHyperPars(learner = pred.tree,par.vals = tunedmodel_tree$x)
pred.logic<-setHyperPars(learner = pred.logic,par.vals = tunedmodel_logic$x)
pred.neural<-setHyperPars(learner = pred.neural,par.vals = tunedmodel_neural$x)
pred.kknn<-setHyperPars(learner = pred.kknn,par.vals = tunedmodel_kknn$x)
pred.svm<-setHyperPars(learner = pred.svm,par.vals = tunedmodel_svm$x)

# Verify performance on cross validated sample sets
performance.tree<-resample(pred.tree,theTask,resamplestrat,measures=list(f1,gmean))
performance.logic<-resample(pred.logic,theTask,resamplestrat,measures=list(f1,gmean))
performance.neural<-resample(pred.neural,theTask,resamplestrat,measures=list(f1,gmean))
performance.kknn<-resample(pred.kknn,theTask,resamplestrat,measures=list(f1,gmean))
performance.svm<-resample(pred.svm,theTask,resamplestrat,measures=list(f1,gmean))
performance.bayes<-resample(pred.bayes,theTask,resamplestrat,measures = list(f1,gmean))
# Train the final model
finalModel.tree <- train(learner = pred.tree, task = theTask)
finalModel.logic<-train(learner=pred.logic,task=theTask)
finalModel.neural<-train(learner=pred.neural,task=theTask)
finalModel.kknn<-train(learner=pred.kknn,task=theTask)
finalModel.svm<-train(learner=pred.svm,task=theTask)
finalModel.bayes<-train(learner=pred.bayes,task=theTask)
# Predict in test set!
prediction.tree <- predict(finalModel.tree, newdata = income.test)
prediction.logic<-predict(finalModel.logic,newdata=income.test)
prediction.neural<-predict(finalModel.neural,newdata=income.test)
prediction.kknn<-predict(finalModel.kknn,newdata=income.test)
prediction.svm<-predict(finalModel.svm,newdata=income.test)
prediction.bayes<-predict(finalModel.bayes,newdata=income.test)


prediction.tree
prediction.Logic
prediction.Neural
prediction.KKNN
prediction.SVM
prediction.Bayes


#out of sample performance 
performance(prediction.tree, measures = list(f1, gmean))
performance(prediction.logic, measures = list(f1, gmean))
performance(prediction.neural, measures = list(f1, gmean))
performance(prediction.kknn, measures = list(f1, gmean))
performance(prediction.svm, measures = list(f1, gmean))
performance(prediction.bayes, measures = list(f1, gmean))

