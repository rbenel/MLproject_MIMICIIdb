#this is an attempt to use random forest to answer my research question. 
#I am still not sure that this indeed does what I am asking, becuase this predicts the dependent variable 
#but doesn't actually look at the specific relationship between the two varibales I am interested in.. 

#to do that we need to do anova between the two variables? 

library(randomForest)
library(car)

local <- getwd()

#load cleaned data from CleanDataProject script 
load(file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))

table(noNeonateData$InterpLos)

######################
#TRAIN AND TEST DATA
######################
set.seed(1234) #set seed so we always get the same sample train/test
train <- sample(nrow(noNeonateData), 0.7*nrow(noNeonateData)) #get 70%
train.df <- noNeonateData[train, ] #divide the data 

test.df <- noNeonateData[-train, ] #everything we didnt take in the training place into test

table(train.df$InterpLos)

table(test.df$InterpLos)


#################################
#RANDOM FOREST LOGISITC REGRESSION
#################################
fit.forest <- randomForest(InterpLos ~ gender + binaryLang + 
                             first_admit_age  + simpleEthnic + marital_status + 
                             insurance + sofa + sapsii,
                              data = train.df, importance = TRUE)

importance(fit.forest, type = 2)

forest.pred <- predict(fit.forest, test.df)

forest.preformance <- table(test.df$InterpLos, forest.pred)


###########
#ANOVA TEST
###########
#compute a one-way anova test, using logLOS bec it is numeric
res.anova <- aov(logLOS ~ binaryLang , data = noNeonateData)

summary(res.anova)

#bec the anova result is signifcant can look at which groups differ 
#Tukey HSD 
TukeyHSD(res.anova)

#check that the anova asumptions are valid, check the homogeneity of variance assumption 
plot(res.anova, 1)

leveneTest(logLOS ~ binaryLang, data = noNeonateData) #uh oh this came out significant which means that 
#we have violated the homogeneity of varience 

#Welch one-way test), that does not require there to be equal variance for all groups 
oneway.test(logLOS ~ binaryLang, data = noNeonateData)
kruskal.test(logLOS ~ binaryLang, data = noNeonateData)


