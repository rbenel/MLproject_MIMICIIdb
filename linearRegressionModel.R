library(ggcorrplot)
library(caret)


#load cleaned data from CleanDataProject script 
load(file = paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))

table(noNeonateData$roundedlogLOS)

#as log of 4 and 5 are such small groups, I think it's best to combine them... 
noNeonateData$roundedlogLOS[noNeonateData$roundedlogLOS == 5] <- 4
                            
table(noNeonateData$roundedlogLOS)

#predictor variables, check correlations between numeric variables 

cormat <- round(cor(as.matrix(noNeonateData[, c("first_admit_age", "sofa", "sapsii", "roundedlogLOS")])), 2)
cormat[upper.tri(cormat)] <- ""
#cormat <- as.data.frame(cormat) %>% select(-roundedlogLOS)

ggcorrplot::ggcorrplot(round(cor(as.matrix(noNeonateData[, c("first_admit_age", "sofa", "sapsii", "roundedlogLOS")])), 2), 
                       p.mat = ggcorrplot::cor_pmat(as.matrix(noNeonateData[, c("first_admit_age", "sofa", "sapsii", "roundedlogLOS")])),
                       hc.order = TRUE, 
                       #type = "lower",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_minimal,
                       colors = c("#cf222c", "white", "#3a2d7f")
                        )

# first_admit_age  sofa sapsii roundedlogLOS
# first_admit_age            1.00 -0.04   0.29   0.02
# sofa                      -0.04  1.00   0.32   0.21
# sapsii                     0.29  0.32   1.00   0.17
# roundedlogLOS                     0.02  0.21   0.17   1.00


######################
#TRAIN AND TEST DATA
######################
set.seed(1234) #set seed so we always get the same sample train/test
train <- sample(nrow(noNeonateData), 0.7*nrow(noNeonateData)) #get 70%
train.df <- noNeonateData[train, ] #divide the data 

test.df <- noNeonateData[-train, ] #everything we didnt take in the training place into test

table(train.df$roundedlogLOS)
table(test.df$roundedlogLOS)

########################
#linear regression model
#######################
#create a model with the variables we are interested in looking at 
# model.lm <- caret::train(roundedlogLOS ~ gender + binaryLang + 
#                            first_admit_age  + simpleEthnic + marital_status + 
#                            insurance + sofa + sapsii, 
#                            data = train.df, 
#                            method = "lm")

#which model is correct???
model.lm <- lm(roundedlogLOS ~ gender + binaryLang + 
                           first_admit_age  + simpleEthnic + marital_status + 
                           insurance + sofa + sapsii, 
                           data = noNeonateData)
summary(model.lm)
step(model.lm)
prob <- predict(model.lm, test.df)

#see ML workshop hw#3 question #3
table(test.df$roundedlogLOS, prob,
      dnn=c("Actual", "Predicted"))


