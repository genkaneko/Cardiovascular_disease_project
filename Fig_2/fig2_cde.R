setwd("/Users/gizen/Documents/AF/20250810/")
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
library(svglite)

maa=as.data.frame(read.csv("7cohort_motusp_MAA_20250810.csv"))
maa=maa[maa$metadata=="AF",]
plots <- list()
tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs 5137 orFDR0.1"),
       x = "coef_PSM_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[1]] <- p

tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="AFvsHealth",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs Health orFDR0.1"),
       x = "coef_PSM_AF", y = paste0("coef_Health_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[2]] <- p


tempfix=maa[maa$cate=="AFvsHealth",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]


colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef 5137 vs Health orFDR0.1"),
       x = "coef_Health_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[3]] <- p

library(svglite)

# 保存为SVG格式
for (i in 1:length(plots)) {
  ggsave(paste0("plot_orFDR01_", i, ".svg"), 
         plot = plots[[i]],
         width = 2, height = 2.2, 
         units = "in")
}




plots <- list()
tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs 5137 orP005"),
       x = "coef_PSM_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[1]] <- p

tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="AFvsHealth",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs Health orP005"),
       x = "coef_PSM_AF", y = paste0("coef_Health_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[2]] <- p


tempfix=maa[maa$cate=="AFvsHealth",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]


colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef 5137 vs Health orP005"),
       x = "coef_Health_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[3]] <- p

# 保存为SVG格式
for (i in 1:length(plots)) {
  ggsave(paste0("plot_orP005_", i, ".svg"), 
         plot = plots[[i]],
         width = 2, height = 2.2, 
         units = "in")
}




plots <- list()
tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs 5137 Allcoef"),
       x = "coef_PSM_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[1]] <- p

tempfix=maa[maa$cate=="PSM",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="AFvsHealth",]
tempmaa = tempmaa[, c(1,3,5,6)]

colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef PSM vs Health Allcoef"),
       x = "coef_PSM_AF", y = paste0("coef_Health_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[2]] <- p


tempfix=maa[maa$cate=="AFvsHealth",]
tempfix = tempfix[, c(1,3,5,6)]
tempmaa=maa[maa$cate=="5137",]
tempmaa = tempmaa[, c(1,3,5,6)]


colnames(tempfix) = c("name", "coef_AF", "pval_AF", "qval_AF")
colnames(tempmaa) = c("name", "coef_Stroke", "pval_Stroke", "qval_Stroke")
temp = merge(tempfix, tempmaa, by = "name")
# temp=temp[(temp$qval_AF<0.1)|(temp$qval_Stroke<0.1),]
# temp=temp[(temp$pval_AF<0.05)|(temp$pval_Stroke<0.05),]
p <- ggplot(temp, aes(x = coef_AF, y = coef_Stroke)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue",linewidth = 0.5) +
  stat_cor(method = "spearman", size = 3.5) +
  labs(title = paste0( " ", nrow(temp), " motus MAA coef 5137 vs Health Allcoef"),
       x = "coef_Health_AF", y = paste0("coef_5137_AF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()  # 去掉副网格线
  )

plots[[3]] <- p

# 保存为SVG格式
for (i in 1:length(plots)) {
  ggsave(paste0("plot_allcoef_", i, ".svg"), 
         plot = plots[[i]],
         width = 2, height = 2.2, 
         units = "in")
}