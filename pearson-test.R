###相関分析
data <- read.csv("pca-v2.csv", header = TRUE, na.strings = "NA")

#正規性の確認
shapiro.test(data$L.sha)

#Pearson test
cor.test(data$Lat, data$diatom.micro.chao, method = "pearson")
