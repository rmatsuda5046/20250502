#パッケージのインストール
library(CCA)
library(CCP)
library("ggplot2")	
library("GGally")	
library("cowplot") 	
library("vegan") 	
library("ggbiplot") 
library("ggthemes") 


###除歪対応分析（DCA）と正準相関分析（CCA）の方法
###欠損値を補完する方法があるらしいが、今回は欠損値を含むサンプルを除いた。

##データのインポート
data <- read.csv("OTU_table_micro.csv")
para <- read.csv("parameter_micro.csv")

##文字列の削除
data_r = data[,3:ncol(data)]	
para_r = para[,2:ncol(para)]


###歪除対応分析, DCA1のAxis lengthsが4以下ならPCAやRDA解析、4以上ならCCA解析、要は目的変数を環境パラメータで説明する時に2次元で説明できるのかそうでないのかの判別
decorana(veg=data_r)

# Hellinger transformation
rared <- decostand(data_r, method = "hellinger")
para_r <- as.data.frame(scale(para_r)) 

###Canonical coresspondence analysis
result.cca <- cca(rared ~ ., data=para_r)

###CCA解析結果の詳細
summary(result.cca)


###CCAの解析結果をggplotで表現する
#extracting the data as data frame; env data
veg_1 = as.data.frame(result.cca$CCA$biplot)
veg_1["env"] = row.names(veg_1)

#extracting the data; Areau
veg_2 = as.data.frame(result.cca$CCA$u)
veg_2["OTU"] = row.names(veg_2)
veg_2$Zone = data$Zone

fill_order <- c("STFZ", "SAFZ", "PFZ", "AAZ", "SPZ")

#環境変数の有意差検定
anova_result <- anova(result.cca, by = "term")

#extracting significant environmental parameter（p value < 0.05）
significant_vars <- rownames(anova_result)[anova_result$`Pr(>F)` < 0.05]
veg_1_significant <- veg_1[veg_1$env %in% significant_vars, ]

#CCAの作図
plot <- ggplot() + 
  geom_point(data = veg_2, aes(x = CCA1, y = CCA2, shape = factor(Zone, fill_order), color = "red"), alpha = 0.9) +
  geom_point(data = veg_1_significant, aes(x = CCA1, y = CCA2), size = 0.6, alpha = 0.9, color = "black") +
  geom_segment(data = veg_1_significant, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), 
               arrow = arrow(length = unit(0.25, "cm"))) + 
  geom_text(data = veg_1_significant, aes(x = CCA1, y = CCA2, label = env), 
            nudge_y = -0.2, nudge_x = -0.1, color = "black", size = 3) +
  labs(shape = "Zone") +
  scale_colour_manual(values = c("red")) +
  xlab("CCA1 (22.3%)") +
  ylab("CCA2 (9.7%)") +
  theme_bw(12) + 
  theme(axis.ticks = element_line(colour = "black"), 
        axis.text = element_text(colour = "black"), 
        legend.position = "right", 
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

ggsave("plot_micro_250409.pdf", plot, height=6, width=9, unit="cm")
