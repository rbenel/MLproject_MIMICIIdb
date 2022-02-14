
#I want to make graphs for the project presentation

#load cleaned data from CleanDataProject script 
load(file = paste0("/Bigdata/Dropbox (Technion Dropbox)/Rina_Benel/Home/MachineLearningMedicine/results/cleanData.RData"))


#density plot los V. language 
#for the purpose of the plot, anything that is a log of 0 will be -Inf, so let's add 1 to all of the values. 
#this graph uses the *original* los values!!
plot <- ggplot(noNeonateData, aes(log(los+1), fill = binaryLang)) + geom_density(alpha = 0.35) +
  xlab("Length of Stay (log)") +
  theme(panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) 
plot + scale_fill_manual(values= c("#00bfc4", "#F8766D")) + guides(fill=guide_legend(title=" "))


#histogram 
hist <- ggplot(noNeonateData, aes(log(los+1), fill = binaryLang)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), colour="black", position = 'identity', binwidth = 0.35) +
  xlab("Length of Stay (log)") + ylab("") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour="black", size = 20, face = "plain")) +
  theme( axis.title.x = element_text(family="sans",size = 20, face="bold", hjust=0.5, vjust=-0.5),
         axis.title.y = element_text(family="sans",size = 20, angle=90, face="bold", hjust=0.5, vjust=1)) +
  theme( axis.text.x = element_text(family = "sans",size = 14, angle=0, face='plain', colour="#353535",   hjust=1, vjust=1) ) +
  theme( axis.text.y = element_text(family = "sans",size = 14, face='plain', colour="#353535",  vjust=0.5) ) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.background = element_rect()) + 
  theme(legend.position="top") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  panel.background = element_blank()) 

hist + guides(fill=guide_legend(title=" ")) + scale_fill_manual(values= c("#00bfc4", "#F8766D"))
