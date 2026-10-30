library(MatchIt)
library(openxlsx)
library(tableone)
library(Maaslin2)
library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(reshape2)
library(viridis)
library(ggpubr)

maa=as.data.frame(read.csv("7cohort_motusp_MAA.csv"))
maa=maa[maa$cate=="5137",]
plots <- list()
tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Non_PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Non_PAF orFDR0.1"),
       x = "coef_Non_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[1]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAF orFDR0.1"),
       x = "coef_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[2]] <- p


tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="IHD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs IHD orFDR0.1"),
       x = "coef_IHD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[3]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="CHF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs CHF orFDR0.1"),
       x = "coef_CHF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[4]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAD orFDR0.1"),
       x = "coef_PAD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[5]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Stroke",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Stroke orFDR0.1"),
       x = "coef_Stroke", y = paste0("coef_AF")) +
  theme_minimal()

plots[[6]] <- p
pdf("motu_maa_5137_AF_vs_6cohort_orFDR01_scatterplots.pdf", width = 6, height = 6)  # 每页大小可以根据需要调整

for (p in plots) {
  print(p)   # 必须 print，否则不会绘制
}
dev.off()

plots <- list()
tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Non_PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Non_PAF orP0.05"),
       x = "coef_Non_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[1]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAF orP0.05"),
       x = "coef_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[2]] <- p


tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="IHD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs IHD orP0.05"),
       x = "coef_IHD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[3]] <- p


tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="CHF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs CHF orP0.05"),
       x = "coef_CHF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[4]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAD orP0.05"),
       x = "coef_PAD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[5]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Stroke",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Stroke orP0.05"),
       x = "coef_Stroke", y = paste0("coef_AF")) +
  theme_minimal()

plots[[6]] <- p
pdf("motu_maa_5137_AF_vs_6cohort_orP005_scatterplots.pdf", width = 6, height = 6)  # 每页大小可以根据需要调整

for (p in plots) {
  print(p)   # 必须 print，否则不会绘制
}
dev.off()





plots <- list()
tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Non_PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Non_PAF ALL"),
       x = "coef_Non_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[1]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAF ALL"),
       x = "coef_PAF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[2]] <- p


tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="IHD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs IHD ALL"),
       x = "coef_IHD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[3]] <- p


tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="CHF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs CHF ALL"),
       x = "coef_CHF", y = paste0("coef_AF")) +
  theme_minimal()

plots[[4]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="PAD",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs PAD ALL"),
       x = "coef_PAD", y = paste0("coef_AF")) +
  theme_minimal()

plots[[5]] <- p

tempfix=maa[maa$metadata=="AF",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="Stroke",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef AF vs Stroke ALL"),
       x = "coef_Stroke", y = paste0("coef_AF")) +
  theme_minimal()

plots[[6]] <- p



tempfix=maa[maa$metadata=="IHD",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$metadata=="CHF",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef HF vs IHD ALL"),
       x = "coef_HF", y = paste0("coef_IHD")) +
  theme_minimal()

plots[[7]] <- p


tempfix=maa[maa$metadata=="IHD",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=read.csv("world_cohort_motusp_MAA_20260110.csv")
tempmaa = tempmaa[, c(11,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x =coef_Stroke , y = coef_AF)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "spearman", size = 5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef IHD China vs JAP ALL"),
       x = "coef_China_IHD", y = paste0("coef_JAP_IHD")) +
  theme_minimal()

plots[[8]] <- p




pdf("motu_maa_5137_AF_vs_6cohort_ALL_scatterplots.pdf", width = 6, height = 6)  # 每页大小可以根据需要调整

for (p in plots) {
  print(p)   # 必须 print，否则不会绘制
}
dev.off()

