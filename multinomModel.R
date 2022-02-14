library(dplyr)
library(ggplot2)
library(caret)
library(nnet)
library(MLmetrics)

#this is a script for a regression model
local <- getwd()

#load cleaned data from CleanDataProject script 
load(file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))

#we will use here an intepretable transformation of LOS
#breaks = c(0, 0.5, 1, 5, 20, 117),
#labels = c("twelve_hours", "twentyfour_hours", "few_days", "many_days", "extended_stay"),

#interpretable take 2 
#breaks = c(0, 1, 2, 3, 5, 102),
#labels = c("twentyfour_hours", "fourtyeight_hours", "seventytwo_hours", "few_days", "many_days"),

#can look at a table of the two variables that interest us. 
table(noNeonateData$InterpLos, noNeonateData$binaryLang)

#check significance between two categorial variables
chisq.test(noNeonateData$InterpLos, noNeonateData$binaryLang)

################
#caret addition
################
#since the MLN function doesn't require a tuning parameter, but if we want to apply regularlized regression
#we can add this if we use caret which under the hoos is using nnet

#Bottom line though, that is doesn't imporve the model
trainControl_MNL <- trainControl(method = "cv", #cross validation resampling method
                                 number = 10,   #number of resampling iterations
                                 search = "grid",
                                 classProbs = TRUE,
                                 summaryFunction = multiClassSummary) #alternatie performance summaries

tuneGrid_MNL <- expand.grid(decay = seq(0, 1, by = 0.1)) #11 values for decay
#regularized paramater to avoid over-fitting 

#set seed so partition we will use for training and test will always be the same 
set.seed(2612) 

#we use caret's package function, bec it leaves the same initial proportions of the variable we are interested in
#for both the test and train 
data.index <- caret::createDataPartition(noNeonateData$InterpLos,
                                         p = 0.7, #the percentage of data that goes to training
                                         list =FALSE) #automatically returns a list
#seperate to train and test 
train_data <- noNeonateData[data.index, ]

test_data <- noNeonateData[-data.index, ]

###################
#caret continuation
###################
#MNL model which includes parameter 
MNL_model <- caret::train(InterpLos ~ gender + binaryLang +
                        first_admit_age  + simpleEthnic +
                        insurance + sofa + sapsii,
                        method = "multinom",
                        data = train_data,
                        maxit = 100,
                        trace = FALSE, #we dont want to output the iterations
                        tuneGrid = tuneGrid_MNL, #a df with columns for each tuning parameter
                        trControl = trainControl_MNL)
#get best value for decay
MNL_model$bestTune

#get the AUC and accuracy for each decay
MNL_model$results %>% select(decay, AUC, Accuracy)

#test the test data and get a confusion matrix
caret::confusionMatrix(predict(MNL_model,
                               newdata = test_data,
                               type = "raw"),
                       reference = test_data$InterpLos)


#Conclusion, even with the additional paramaters I get the same accuarcy and the model can't predict 24hours!
#####################
##MNL model with nnet
#####################
# MNL model using nnet directly, with parameters
MNL_model <- multinom(TakeTwo_InterpLos ~ gender + binaryLang +
                            first_admit_age  + simpleEthnic +
                            insurance + sofa + sapsii,
                            data = train_data)

#get the summary of the model
summary(MNL_model)
#the reported residual deviance is final negative log-likelihood multiplied by two 

#extarct the coefficients from the model 
exp(coef(MNL_model))

head(prob.tableTrain <- fitted(MNL_model))

##################################
#check acccuracy for training data
###################################
train_data$predicted <- predict(MNL_model, newdata = train_data, "class")

cm_tableTrain <- table(train_data$TakeTwo_InterpLos, train_data$predicted, dnn = c("actual", "predicted"))

accuracyTrain <- round((sum(diag(cm_tableTrain))/sum(cm_tableTrain))*100,2)

################################
#check accuracy for testing data
###############################
test_data$predicted <- predict(MNL_model, newdata = test_data, "class")

cm_tableTest <- table(test_data$TakeTwo_InterpLos, test_data$predicted, 
                      dnn = c("actual", "predicted"))

accuracyTest <- round((sum(diag(cm_tableTest))/sum(cm_tableTest))*100,2)


#since they both come out about the same, with a 70% accuracy, let's take just the training set and examine closer
#get the summary of the model
train_summary <- summary(MNL_model)

#calculate z-staistics and p values 
z <- train_summary$coefficients/train_summary$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test

#seperate each "length of stay" to display all of the details
los_12h <- rbind(train_summary$coefficients[1, ], train_summary$standard.errors[1, ], z[1, ], p[1, ])
rownames(los_12h) <- c("Coefficient","Std. Errors","z stat","p value")
los_12h <- as.data.frame(round(t(los_12h),4))
#write.csv(los_12h, file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/los12h_summaryStatistics.csv"))

los_24h <- rbind(train_summary$coefficients[2, ], train_summary$standard.errors[2, ], z[2, ], p[2, ])
rownames(los_24h) <- c("Coefficient","Std. Errors","z stat","p value")
los_24h <- as.data.frame(round(t(los_24h),4))
#write.csv(los_24h, file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/los24h_summaryStatistics.csv"))


many_days <- rbind(train_summary$coefficients[3, ], train_summary$standard.errors[3, ], z[3, ], p[3, ])
rownames(many_days) <- c("Coefficient","Std. Errors","z stat","p value")
many_days <- as.data.frame(round(t(many_days),4))
#write.csv(many_days, file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/losmany_days_summaryStatistics.csv"))


extended_stay <- rbind(train_summary$coefficients[4, ], train_summary$standard.errors[4, ], z[4, ], p[4, ])
rownames(extended_stay) <- c("Coefficient","Std. Errors","z stat","p value")
extended_stay <- as.data.frame(round(t(extended_stay),4))
#write.csv(extended_stay, file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/losextended_stay_summaryStatistics.csv"))






