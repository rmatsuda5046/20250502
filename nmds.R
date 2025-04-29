#パッケージ起動
library("ggplot2")	
library("GGally")	
library("cowplot") 	
library("vegan") 	

#Importing the original data
data <- read.csv("OTU_table.csv", header = TRUE, na.strings = "NA")
nmds = data[,4:ncol(data)]

#Rarefied the reads
raremax <- min(rowSums(nmds))
rared <- rrarefy(nmds, raremax)
rared_sp <- specnumber(rared)

#Rarecurve
par(mfrow = c(1,2))
plot(sp, rared_sp, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(nmds, step = 20, sample = raremax, col = "blue", cex = 0.6)

#NMDS analysis
mat_rared<-as.matrix(rared)
nmds<-metaMDS(mat_rared, distance = "bray")

#Labeling "Size" and "Zone"
data.scores = as.data.frame(scores(nmds)$sites)
data.scores$Size = data$Size
data.scores$Zone = data$Zone

fill_order <- c("STFZ", "SAFZ", "PFZ", "AAZ", "SPZ")

#Making the figure
plot = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = Size, shape = factor(Zone, fill_order))) +
  geom_point(size = 3) + theme_bw(12)+theme(axis.ticks=element_line(colour = "black"), axis.text=element_text(colour = "black"), legend.position = ("right"), legend.text = element_text(size = 10), axis.title = element_text(size = 10))+labs(color = "Size", shape = "Zone")+scale_colour_manual(values = c("red", "blue"))

ggsave("plot.pdf", plot_all, height=8, width=11, unit="cm")


#PERMANOVA test
adonis2(formula = rared ~ data.scores$Zone, permutations = 999, method = "bray")
adonis2(formula = rared ~ data.scores$Size, permutations = 999, method = "bray")

