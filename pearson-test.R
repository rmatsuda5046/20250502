###相関分析
PC <- read.csv("pca-v2.csv", header = TRUE, na.strings = "NA")

#正規性の確認
shapiro.test(PC$L.sha)

#Pearson test
cor.test(PC$Lat, PC$diatom.micro.chao, method = "pearson")