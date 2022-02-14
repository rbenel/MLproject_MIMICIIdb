library(nnet) #allows for multinom glm
library(stargazer) #for pvalue for the multinom glm
library(knitr)
library(quantreg) #for quantile regression


#load cleaned data from CleanDataProject script 
load(file = paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))

################################
#Try to fit a multinomial model
################################
#run a multinom model, because we have septiles as the dependent variable. 
fit.uniqueData <- nnet::multinom(losQuantile ~ gender + binaryLang + 
                        first_admit_age  + simpleEthnic + marital_status + 
                        insurance + sofa + sapsii, data = noNeonateData) 
#but this function doesn't have p-values so we need to calculate them 
summary.output <- summary(fit.uniqueData)

#predict the dependent variable based off of the model used
predict(fit.uniqueData, noNeonateData)

#try to reduce the model?
fit.uniqueData.reduced <- step(fit.uniqueData)

#miscalculation error
misCalcError <- table(predict(fit.uniqueData), noNeonateData$losQuantile)
print(misCalcError)

#find out percentage of time that the model is correct
1-sum(diag(misCalcError))/sum(misCalcError)

#Z statistics are simply ratios of model coefficients and standard errors
z <- summary.output$coefficients/summary.output$standard.errors
#we can get the p-values using the standard normal distribution.
p <- (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test

#make a table for the first quantile
Pclass1 <- rbind(summary.output$coefficients[1,],summary.output$standard.errors[1,],z[1,],p[1,])
rownames(Pclass1) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass1)

#make a table for the third quantile
Pclass3 <- rbind(summary.output$coefficients[2,],summary.output$standard.errors[2,],z[2,],p[2,])
rownames(Pclass3) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass3)

#make a table for the fourth quantile
Pclass4 <- rbind(summary.output$coefficients[3,],summary.output$standard.errors[3,],z[3,],p[3,])
rownames(Pclass4) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass4)

########################################
#Try to fit a quantile regression model
########################################
quantreg25 <- rq(los ~ gender + binaryLang + 
                   first_admit_age  + simpleEthnic + marital_status + 
                   insurance + sofa + sapsii, data = noNeonateData, tau = 0.25)
summary(quantreg25)

quantreg50 <- rq(los ~ gender + binaryLang + 
                   first_admit_age  + simpleEthnic + marital_status + 
                   insurance + sofa + sapsii, data = noNeonateData, tau = 0.50)
summary(quantreg50)

quantreg75 <- rq(los ~ gender + binaryLang + 
                      first_admit_age  + simpleEthnic + marital_status + 
                      insurance + sofa + sapsii, data = noNeonateData, tau = 0.75)
summary(quantreg75)

anova(quantreg25, quantreg50, quantreg75, joint = F)

#plotting data 
quantreg.all <- rq(los ~ gender + binaryLang + 
                    first_admit_age  + simpleEthnic + marital_status + 
                    insurance + sofa + sapsii, data = noNeonateData, tau = seq(0.05, 0.95, by = 0.05))

quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)
