###packages install
install.packages("devtools")
devtools::install_github("vqv/ggbiplot")

###microbiome (https://microbiome.github.io/tutorials/)
library(devtools) # Load the devtools package
install_github("microbiome/microbiome") # Install the package

install.packages("reshape2")

###packages
library("ggplot2")	
library("GGally")	
library("cowplot") 	
library("microbiome")  
library("reshape2")		
library("tidyverse")
library(dplyr)

###importing data
data <- read.csv("Bubble_chart.csv", header = TRUE, na.strings = "NA")
z = data[,3:ncol(data)]

###transformation to Z-score
zscore <- microbiome::transform(z, "Z")

write.csv(zscore, "z-score.csv")

###setting data frame
data.scores = as.data.frame(zscore)
data.scores$Zone = data$Zone
data.scores$Size = data$Size

data.scores$Zone <- factor(data.scores$Zone, levels = c("SPZ-pico/nano", "AAZ-pico/nano", "PFZ-pico/nano", "SAFZ-pico/nano", "STFZ-pico/nano", "SPZ-micro", "AAZ-micro", "PFZ-micro", "SAFZ-micro", "STFZ-micro"))
data.scores <- data.scores %>% arrange(Zone)


#convert data frame from a "wide" format to a "long" format
data.scores = melt(data.scores, id = c("Zone", "Size"))

data.scores$Zone <- factor(data.scores$Zone,levels=unique(data.scores$Zone))
data.scores$Size <- factor(data.scores$Size,levels=unique(data.scores$Size))



###making the figure
plot = ggplot(data.scores, aes(x=variable, y=Zone)) + 
  geom_point(aes(size = value, fill = Size), alpha = 0.75, shape = 21) + 
  scale_size_continuous(limits = c(0, 3), range = c(1, 9), breaks = c(0,1,2,3)) + 
  labs( x= "", y = "", size = "Z-score (> 0)", fill = "grey") + 
  theme(legend.key=element_blank(), 
  axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
  axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
  legend.text = element_text(size = 10, face ="bold", colour ="black"), 
  legend.title = element_text(size = 12, face = "bold"), 
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
  legend.position = "bottom") +  
  scale_fill_manual(values = c("blue", "red"), guide = "none") + 
  scale_x_discrete(limits = rev(levels(data.scores$variable))) 


ggsave("plot_zscore.pdf", plot, height=10, width=16, unit="cm")



###hierarchical cluster analysis
library("stats")
library("dendextend")

zclust <- read.csv("clust.csv", header = TRUE, na.strings = "NA")
zclust = zclust[,2:ncol(zclust)]

result <- hclust(dist(zclust), method = "ward.D2")
plot(set(as.dendrogram(result),'branches_k_color', k = 3), horiz = TRUE)
