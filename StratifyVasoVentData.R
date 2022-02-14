library(ggplot2)
library(dplyr)

#############
#INPUT FILES
#############
#this .csv comes from the `getTrialData.sql` script. 
#import all of the data from the MIMIC db 
sqlData <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/TrialDataIncludeICUid.csv"), 
                    sep = ",", header = T, stringsAsFactors = T) #stringsAsFactors prevents strings being converted to factors

vasopressor <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/durations/vasopressor-durations.csv"),
                        sep = ",", header = T, stringsAsFactors = T)

ventilation <- read.csv(paste0( "/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/durations/ventilation-durations.csv"),
                        sep = ",", header = T, stringsAsFactors = T)

#Severity scores
#this is from the repository that willie shared with me from his research 
sofa <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/severityscores/sofa.csv"))
sapsii <- read.csv(paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/data/repository/severityscores/sapsii.csv"))

#We want to add this vasopressor data to our sqlData 
#add the correct columns to our trail data 
subsetVasopressor <- vasopressor[ , names(vasopressor) %in% c("icustay_id", "duration_hours")]
colnames(subsetVasopressor) <- c("icustay_id", "vasopressor_duration")

#We also want to add this ventilation score to our sqlData 
subsetVentilation <- ventilation[ , names(ventilation) %in% c("icustay_id", "duration_hours")]
colnames(subsetVentilation) <- c("icustay_id", "ventillation_duarion")

#join the two dfs togther to include the duration info
sql_durationData <- sqlData %>% left_join(subsetVasopressor, by = "icustay_id") %>% left_join(subsetVentilation, by = "icustay_id")

#remove any rows with NAs
durartionData <- sql_durationData[complete.cases(sql_durationData), ]

##################
##SEVERITY SCORES
##################
#two types of severity scores, sofa and sapsii
#We want to add this sofa score to our sqlData 
subsetSofa <- sofa[ , names(sofa) %in% c("subject_id", "sofa")]

#We want to add this sofa score to our sqlData 
subsetSapsii <- sapsii[ , names(sapsii) %in% c("subject_id", "sapsii")]

#join the two dfs togther
SOL_durartionData <- durartionData %>% left_join(subsetSofa, by = "subject_id") %>% left_join(subsetSapsii, by = "subject_id")

#Research has determined it is best to sort by first admit. 
orderDurartionData <- SOL_durartionData[order(SOL_durartionData$first_admittime, SOL_durartionData$icustay_id), ]

#get only the first ordered row per icustay_id, so this would mean taking the longest los
uniqueData <- orderDurartionData[!duplicated(orderDurartionData$icustay_id), ]

nrow(uniqueData)
#########################
#LANGUAGE - ENGL AND NON
#########################
#binary language, English V. all the rest 
uniqueData$binaryLang <- as.factor(ifelse(uniqueData$language == "ENGL", "ENGL", "NON_ENGL"))

####################
#Risk Stratification
###################
uniqueData$cutSAPSII <- cut(uniqueData$sapsii, breaks = 3, labels = c("low", "medium", "high"))

#cut the data into unqiue sets depending on risk
uniqueData$WilliecutSAPSII <- cut(uniqueData$sapsii, 
                                breaks = c(0, 37, 48, 118), 
                                labels = c("low", "medium", "high"))

################################
#Change duration in hours to min
################################
#since willie's paper is in minutes, convert hours to min
uniqueData$vasopressor_duration <- (as.numeric(uniqueData$vasopressor_duration))*60

uniqueData$ventillation_duarion <- (as.numeric(uniqueData$ventillation_duarion))*60


#let's summarize what our findings are, group by insurance and lang and see if median of los is different 
#first let's make a small table with the distributions bec it is easier to work with ggplot 
tblGraph <- uniqueData %>% group_by(binaryLang) %>% 
  summarize(median_ventillation = median(vasopressor_duration), 
            median_vasopressor = median(ventillation_duarion))

#see if the means are the same
# A tibble: 2 x 3
# binaryLang median_ventillation median_vasopressor
# <fct>                    <dbl>              <dbl>
#   1 ENGL                      479.              1123.
# 2 NON_ENGL                  582               1378.


#check if significant for vasopressor
EngSpeakers <- uniqueData[uniqueData$binaryLang == "ENGL", ]

NonEngSpeakers <- uniqueData[uniqueData$binaryLang == "NON_ENGL", ]

wilcox.test(EngSpeakers$vasopressor_duration, NonEngSpeakers$vasopressor_duration)

# Wilcoxon rank sum test with continuity correction
# 
# data:  EngSpeakers$vasopressor_duration and NonEngSpeakers$vasopressor_duration
# W = 4265522, p-value = 0.004861
# alternative hypothesis: true location shift is not equal to 0

#do the same for ventillation 
wilcox.test(EngSpeakers$ventillation_duarion, NonEngSpeakers$ventillation_duarion)

# Wilcoxon rank sum test with continuity correction
# 
# data:  EngSpeakers$ventillation_duarion and NonEngSpeakers$ventillation_duarion
# W = 4157941, p-value = 3.376e-05
# alternative hypothesis: true location shift is not equal to 0

#######################
#Plot cumulative prob
######################
#try to add inlets with the coor_cartesian? 
typesRisk <- c("low", "medium", "high")

for(i in typesRisk) {
  
  #separate the data per insurance
  subsetUniqueData <- uniqueData[uniqueData$WilliecutSAPSII == i, ]
  
  subsetUniqueDataGraph <- subsetUniqueData %>% group_by(WilliecutSAPSII, binaryLang) %>% 
    summarize(median_vaso = median(vasopressor_duration), median_vent = median(ventillation_duarion))
  
ggplot(subsetUniqueData, aes(x = vasopressor_duration)) +
  stat_ecdf(aes(color = binaryLang, lty = binaryLang), size = 1) +
  labs(x = "Vasopressor Duration (minutes)", y = "Cumaltive Probability") +
  geom_vline(xintercept = subsetUniqueDataGraph$median_vaso, color = c("#F8766D", "#00bfc4", "grey80")) + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1500)) +
  scale_linetype_manual(values=c("dotted", "solid")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size = 14, face = "plain")) +
  theme( axis.title.x = element_text(family="sans",size = 14, face="bold", hjust=0.5, vjust=-0.5),
         axis.title.y = element_text(family="sans",size = 14, angle=90, face="bold", hjust=0.5, vjust=1)) +
  theme( axis.text.x = element_text(family = "sans",size = 14, angle=0, face='plain', colour="#353535",   hjust=1, vjust=1) ) +
  theme( axis.text.y = element_text(family = "sans",size = 14, face='plain', colour="#353535",  vjust=0.5) ) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.background = element_rect()) + 
  theme(legend.position="right") 


#graph for ventillation 
ggplot(subsetUniqueData, aes(x = ventillation_duarion)) +
  stat_ecdf(aes(color = binaryLang, lty = binaryLang), size = 1) +
  labs(x = "Ventillation Duration (minutes)", y = "Cumaltive Probability") +
  theme_bw() + 
  coord_cartesian(xlim = c(0, 700)) +
  geom_vline(xintercept = subsetUniqueDataGraph$median_vent, color = c("#F8766D", "#00bfc4", "grey80")) + 
  scale_linetype_manual(values=c("dotted", "solid")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size = 14, face = "plain")) +
  theme( axis.title.x = element_text(family="sans",size = 14, face="bold", hjust=0.5, vjust=-0.5),
         axis.title.y = element_text(family="sans",size = 14, angle=90, face="bold", hjust=0.5, vjust=1)) +
  theme( axis.text.x = element_text(family = "sans",size = 14, angle=0, face='plain', colour="#353535",   hjust=1, vjust=1) ) +
  theme( axis.text.y = element_text(family = "sans",size = 14, face='plain', colour="#353535",  vjust=0.5) ) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.background = element_rect()) + 
  theme(legend.position="right") 

}