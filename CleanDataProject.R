library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#############
#INPUT FILES
#############
#the IDs are unqiue to a subject ID is unique to a patient and icu is unique to a patients ICU stay
#this .csv comes from the `getTrialData.sql` script. 
sqlData <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/TrialDataIncludeICUid.csv"), 
         sep = ",", header = T, stringsAsFactors = T) #stringsAsFactors prevents strings being converted to factors
#can use the lubridate package to arrrange as "date"
#read in the date and time so we can order 
#sqlData$first_admittime <- ymd_hms(sqlData$first_admittime)

sqlData$subject_id <- as.factor(sqlData$subject_id)
##################
##SEVERITY SCORES
##################
#this is from the repository that willie shared with me 
sofa <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/severityscores/sofa.csv"))
sofa$subject_id <- as.factor(sofa$subject_id)

sapsii <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/severityscores/sapsii.csv"))
sapsii$subject_id <- as.factor(sapsii$subject_id)

#We want to add this sofa score to our sqlData 
subsetSofa <- sofa[ , names(sofa) %in% c("subject_id", "sofa")]

#We want to add this sofa score to our sqlData 
subsetSapsii <- sapsii[ , names(sapsii) %in% c("subject_id", "sapsii")]

#join the two dfs togther
sql_severityData <- sqlData %>% left_join(subsetSofa, by = "subject_id") %>% left_join(subsetSapsii, by = "subject_id")

#just a check for me that we have all complete.cases
#complete.casesSQLSeverity <- sql_severityData[complete.cases(sql_severityData), ]
#nrow(complete.casesSQLSeverity)

#order the data by los and subject id, bec there are multiple entires per person? 
#sort by first admit.
#orderAdmitSQL_severityData <- sql_severityData[order(sql_severityData$first_admittime), ]

#lets not use order! use arrange from dplyr
orderAdmitSQL_severityData <- sql_severityData %>% arrange(first_admittime)

#get only the first ordered row per subject_id, so this would mean taking the longest los
uniqueData <- orderAdmitSQL_severityData[!duplicated(orderAdmitSQL_severityData$subject_id), ]
#can't use the function unique to subset/index data!

#or use tidyverse's function distinct
#uniqueData <- distinct(orderAdmitSQL_severityData, subject_id, .keep_all = TRUE)
#nrow(uniqueData)

#we are going to try something new that does everything
uniqueData <- sql_severityData %>% group_by(subject_id) %>%  #group per subject id
  arrange(first_admittime, desc(los)) %>%  #take first admittime (which are all the same) and then longest los
  slice(1) %>% #take first occurance 
  ungroup() #turn back to DF

#########################
#LANGUAGE - ENGL AND NON
#########################
#binary language, english V. all the rest 
uniqueData$binaryLang <- as.factor(ifelse(uniqueData$language == "ENGL", "ENGL", "NON_ENGL"))

############
#INSURANCE
############
levels(uniqueData$insurance)[levels(uniqueData$insurance)=="Self Pay"] <- "Self_Pay"
#relvel so private is the reference
uniqueData$insurance = relevel(as.factor(uniqueData$insurance), ref = "Private")

#add a simplifies variable of insurance private, goverment, or self pay, we may need for later 
#this is the same distiniction tha willie boag used 

# #add the types of goverment assistance to a vector 
# typesGovInsurance <- c("Government", "Medicaid", "Medicare")
# 
# #copy the data from the insurance column
# uniqueData$simpleInsurance <- uniqueData$insurance
# #replace goverment types with just goverment 
# 
# uniqueData$simpleInsurance[uniqueData$simpleInsurance %in% typesGovInsurance] <- "Goverment"
# 
# #to keeo things consistent, turn into a factor 
# uniqueData$simpleInsurance <- as.factor(uniqueData$simpleInsurance)
# #relvel so private is the reference
# uniqueData$simpleInsurance = relevel(as.factor(uniqueData$simpleInsurance), ref = "Private")

########################
#ETHNICITY - SIMPLIFIED
########################
#ethnicity often has a "-" so we convert this to "/" so we can strsplit later
uniqueData$ethnicity <- gsub("-", "/", uniqueData$ethnicity, fixed = TRUE)
#this specific entry has the name "OR", but if i grep for "OR" is substitutes pORtugese as well
uniqueData$ethnicity <- gsub("HISPANIC OR LATINO", "HISPANIC", uniqueData$ethnicity, fixed = TRUE)

#simplify ethnicity data by taking only first entry before the "/"
uniqueData$simpleEthnic <- sapply(strsplit(as.character(uniqueData$ethnicity), "/"), '[', 1)
#trim white space
uniqueData$simpleEthnic <- trimws(uniqueData$simpleEthnic)

#if I have less than 100 data points remove from the final df i.e. "MIDDLE EASTERN", all other 
table(uniqueData$simpleEthnic)

#unknown, or unavailable
majorEthnicGroups <- c("WHITE", "BLACK", "HISPANIC", "ASIAN", "PORTUGUESE")

#subset for only major ethnic groups
#after meeting with Uri and Dany, advised not to remove smaller populations, rather turn to "other" group
#majorEthnicGroupData <- subset(uniqueData, uniqueData$simpleEthnic %in% majorEthnicGroups)
uniqueData$simpleEthnic[!uniqueData$simpleEthnic %in% majorEthnicGroups] <- "OTHER"

#turn into a factor now, so it has less levels, only the ones relevant to us
uniqueData$simpleEthnic <- as.factor(uniqueData$simpleEthnic)

#view table following simplification
table(uniqueData$simpleEthnic)

#relevel so the ref is white
uniqueData$simpleEthnic = relevel(uniqueData$simpleEthnic, ref = "WHITE")
###############
#MARITAL STATUS
###############
#since "other" is not a defined group in marial status, first need to convert to character 
#this was we can change the status not belonging to "majorMaritalGroups" to other, and then 
#convert back
uniqueData$marital_status <- as.character(uniqueData$marital_status)
#major marital status points, also used at least 100 as a filter... so life partner was not included.
majorMaritalGroups <- c("DIVORCED", "MARRIED", "SEPARATED", "SINGLE", "WIDOWED")

#anybody not found in the majorMaritalGroups will be categorized as other 
uniqueData$marital_status[!uniqueData$marital_status %in% majorMaritalGroups] <- "OTHER"

table(uniqueData$marital_status)
#turn back to factor
uniqueData$marital_status <- as.factor(uniqueData$marital_status)
#check the table to double check "other" is still listed 
table(uniqueData$marital_status)

#relevel for model the marital status
uniqueData$marital_status = relevel(uniqueData$marital_status, ref = "MARRIED")
#############
#AGE GROUPS
#############
#after meeting with Uri and Dany, I need to remove the neonates from the data, and anyone over 89 will be considered 90.
#the rest of the population will be numeric 
#remove the 14 neonates
noNeonateData <- subset(uniqueData, uniqueData$age_group != "neonate")

#change all of the people over 89 in first_admit_age to 90
noNeonateData$first_admit_age[noNeonateData$age_group == ">89"] <- 90

#because the age has two decimanl places we are going to round to the nearest whole number
noNeonateData$first_admit_age <- round(noNeonateData$first_admit_age)

###############
#LOS QUANTILES
###############
#maybe need to do this last, because we have finished filtering the data
#round the los data
noNeonateData$roundedLOS <- round(noNeonateData$los)

noNeonateData$roundedlogLOS <- round(log(noNeonateData$roundedLOS + 1))

#after talking with uri and dany, using quantiles seems to make more sense
#but after talking to haim it seems log of los or an interpretable los may be better
# Using cut to simplify the range of los
noNeonateData$InterpLos <- cut(noNeonateData$los,
                               breaks = c(0, 0.5, 1, 5, 20, 120),
                               labels = c("twelve_hours", "twentyfour_hours", "few_days", "many_days", "extended_stay"),
                                right = FALSE)

#after running the model, I realized that although these are intuitive definitions the groups are too imbalanced
#so we are going to try again
noNeonateData$TakeTwo_InterpLos <- cut(noNeonateData$los,
                               breaks = c(0, 1, 2, 3, 5, 120),
                               labels = c("twentyfour_hours", "fourtyeight_hours", "seventytwo_hours", "few_days", "many_days"),
                               right = FALSE)


#we do this here bec the multinomial model depends on separating to quantiles ahead of time
noNeonateData$losQuantile <- cut(noNeonateData$los, 
                              breaks = quantile(noNeonateData$los), 
                              labels = c(1:4), include.lowest = T)
                              

#noNeonateData$losSeptile <- cut(noNeonateData$los,
#                              breaks = quantile(noNeonateData$los, probs = seq(0,1, length = 8)), #for septiles
#                              labels = c(1:7), include.lowest = T)

##################
#Relevel factors
###################
#relevel the reference type to be the middle quantile
#relvel the rest of the variables 
noNeonateData$losQuantile = relevel(noNeonateData$losQuantile, ref = "2")

#this variables, relevel here bec they are not tampered with 
noNeonateData$gender = relevel(noNeonateData$gender, ref = "M")

#relvel to few days? It is the largest group, what should be the reference 
noNeonateData$InterpLos = relevel(noNeonateData$InterpLos, ref = "few_days")

#save(noNeonateData, file = paste0(local, "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))

##################################
#DATA FOR THE FINAL PROJECT TABLE
##################################
#let's make a table that summarizes the stratification by language for the final report. 

#find median age and los for the two populations 
noNeonateData %>% group_by(binaryLang) %>% 
  summarize(mean_los = mean(los), sd_los = sd(los),  mean_age = mean(first_admit_age), sd_age = sd(first_admit_age))

#find freq and number of insurance in the two populations 
noNeonateData %>%  group_by(binaryLang, insurance) %>% summarize(n = n()) %>% 
  mutate(freq = n/sum(n)) 
#spread(binaryLang, n)

#find the freq and number of gender in the two populations 
noNeonateData %>%
  group_by(binaryLang, gender) %>% summarize(n = n()) %>% 
  mutate(freq = n/sum(n))
  #spread(binaryLang, n)


#for every one of the variables we should run this to check that indeed it is significant 
#first we should test that language and los are *not* dependent on one another 
cont.table <- table(noNeonateData$binaryLang,  noNeonateData$los)
#run a chi-squared test on the contigency test
chisq.test(cont.table) #shows that there is a significant difference between eng v non eng and los

