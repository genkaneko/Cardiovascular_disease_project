setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

# setwd("/Users/gizen/Documents/AF/20250813/")
library(vegan)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(dplyr)
library(patchwork)
library(parallel)

meta5137=as.data.frame(read.csv("oral_AF_meta_nonpaf_newHTN.csv",row.names = 1))
rownames(meta5137)=meta5137$metas
fdata=as.data.frame(t(read.xlsx("saliva_240410.motus3.sp.relative_abundance.xlsx",rowNames = T,check.names = F,sep.names = " ")))
fdata=fdata[,colSums(fdata)>0]
diskey=c("AF",
         "IHD",
         "CHF"
)
modify_rownames <- function(df) {
  old_names <- rownames(df)
  new_names <- sapply(old_names, function(name) {
    # 尝试转换为数字
    num_name <- suppressWarnings(as.numeric(name))
    
    # 如果能转换为数字且不是NA
    if (!is.na(num_name)) {
      return(as.character(num_name))  # 去掉前导0
    } else {
      return(name)  # 保持原字符串
    }
  })
  
  rownames(df) <- new_names
  return(df)
}
#METAdataとの順番の揃いと確認
fdata=modify_rownames(fdata)
pr=data.frame()

set.seed(123)
meta_data <- meta5137  # ファイル名を適宜変更
data=fdata[rownames(meta5137),]
data=data[,colSums(data)>0]
identical(rownames(data),rownames(meta5137))

pr=NULL

# 初始化结构
alpha_list <- list()
all_pvals <- data.frame()
plot_lists_by_index <- list(
  Shannon = list(),
  Simpson = list(),
  Pielou = list(),
  Richness = list()
)

# 定义多样性指标计算函数
calc_alpha_indices <- function(mat) {
  shannon <- diversity((mat), index = "shannon")
  simpson <- diversity((mat), index = "simpson")
  richness <- rowSums(mat > 0)
  pielou <- shannon / log(richness + 1e-10)  # 防止除以0
  return(data.frame(
    Shannon = shannon,
    Simpson = simpson,
    Pielou = pielou,
    Richness = richness
  ))
}

# 遍历每个疾病变量
for (i in diskey) {
  message("Processing: ", i)
  
  tempmeta <- meta5137
  tempmeta=tempmeta[!is.na(tempmeta[,i]),]
  common_ids <- intersect(rownames(tempmeta), rownames(data))
  tempmeta <- tempmeta[common_ids, ]
  tempdata <- data[common_ids, ]
  
  group_var <- tempmeta[[i]]
  if (length(unique(group_var)) != 2) {
    warning(paste("Variable", i, "does not have exactly 2 groups. Skipping."))
    next
  }
  
  # 计算 alpha 多样性指标
  alpha_indices <- calc_alpha_indices(tempdata)
  alpha_indices$ID <- rownames(alpha_indices)
  alpha_indices$Group <- factor(group_var)
  # 保存 sheet 数据
  alpha_list[[i]] <- alpha_indices
  
  # 转长格式用于绘图
  long_alpha <- melt(alpha_indices,
                     id.vars = c("ID", "Group"),
                     variable.name = "Index",
                     value.name = "Value")
  
  # 每个指标计算 p 值
  pvals <- long_alpha %>%
    group_by(Index) %>%
    summarise(
      P_value = wilcox.test(Value ~ Group)$p.value
    ) %>%
    mutate(Disease = i)
  
  all_pvals <- rbind(all_pvals, pvals)
  
  # 按 alpha 指标分组作图
  for (index_name in unique(long_alpha$Index)) {
    subdata <- long_alpha[long_alpha$Index == index_name, ]
    
    # 确保 Group 是 factor，并设置顺序 0, 1
    subdata$Group <- factor(subdata$Group, levels = c("0", "1"))
    
    # 自定义颜色
    #group_colors <- c("0" = "skyblue", "1" = "pink")
    group_colors <- c("0" = "#00BFC4", "1" = "#F8766D")
    
    # 计算 p 值（注意这里是针对当前子集）
    p_test <- wilcox.test(Value ~ Group, data = subdata)
    p_value_label <- paste0("P = ", signif(p_test$p.value, 3))  # 保留3位有效数字
    
    # 绘图
    p_single <- ggplot(subdata, aes(x = Group, y = Value, fill = Group)) +
      geom_boxplot(width = 0.5, outlier.shape = NA) +
      geom_jitter(aes(color = Group, alpha = Group),
                  width = 0.2, size = 0.2, stroke = 0.1) +
      scale_color_manual(values = c("0" = "grey50", "1" = "black")) +
      scale_fill_manual(values = group_colors) +
      scale_alpha_manual(values = c("0" = 0.2, "1" = 1)) +
      annotate("text",
               x = 1.5,
               y = max(subdata$Value, na.rm = TRUE) * 1.05,
               label = p_value_label,
               size = 10 / .pt) +  # annotate 用 pt 转换
      labs(title = paste0(index_name, " - ", i),
           x = NULL,
           y = index_name) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
      )
    plot_lists_by_index[[index_name]][[i]] <- p_single
  }
}

# 写入 Excel（每个疾病一个 sheet）
write.xlsx(alpha_list, file = "oral_alpha_diversity_6cohort_20250809.xlsx")



for (index_name in names(plot_lists_by_index)) {
  plots <- plot_lists_by_index[[index_name]]
  pdf_file <- paste0("oral_alpha_violin_", tolower(index_name), ".pdf")
  
  # 拼接成横排
  combined_plot <- wrap_plots(plots, nrow = 1)  # 所有 plot 一行展示
  
  # 动态调整宽度
  plot_width <- 1.6 * length(plots)
  plot_height <- 1.8  # 一行
  
  pdf(pdf_file, width = plot_width, height = plot_height)
  print(combined_plot)
  dev.off()
}


# ここから先はbetaなので改変なし

beta_results <- list()

# # 2. 一次性计算 Bray-Curtis 距离
bc_dist <- vegdist(data, method = "bray")

# 3. 转换为矩阵，并保存为 CSV（保留样本名）
bc_matrix <- as.matrix(bc_dist)
write.csv(bc_matrix, "oral_bc_distance.csv", row.names = TRUE)

# 4. 读取保存的距离矩阵（并转为矩阵对象）
bc_matrix_read <- read.csv("oral_bc_distance.csv", row.names = 1, check.names = FALSE)
bc_matrix_read <- as.matrix(bc_matrix_read)

# 5. 循环每个疾病变量
for (i in diskey) {
  message("Beta diversity - Processing: ", i)
  set.seed(123)
  
  # 筛选元数据中当前变量非 NA 的样本
  tempmeta <- meta5137
  tempmeta <- tempmeta[!is.na(tempmeta[, i]), ]
  
  # 找出当前变量相关的样本交集
  common_ids <- intersect(rownames(tempmeta), rownames(bc_matrix_read))
  tempmeta <- tempmeta[common_ids, ]
  
  group_var <- tempmeta[[i]]
  
  # 跳过非二分类变量
  if (length(unique(group_var)) != 2) {
    warning(paste("Variable", i, "does not have exactly 2 groups. Skipping."))
    next
  }
  
  # ✅ 从读取的完整矩阵中提取子矩阵
  bc_submatrix <- bc_matrix_read[common_ids, common_ids]
  

  # 转为 dist 对象用于后续分析
  bc_dist_sub <- as.dist(bc_submatrix)
  
  # Adonis2 分析
  adonis_res <- adonis2(bc_dist_sub ~ group_var, data = tempmeta, permutations = 999,parallel = 10)
  p_value <- adonis_res$`Pr(>F)`[1]
  set.seed(123)
  # MDS (PCoA)
  mds <- cmdscale(bc_dist_sub, k = 2, eig = TRUE)
  mds_df <- as.data.frame(mds$points)
  colnames(mds_df) <- c("MDS1", "MDS2")
  mds_df$Group <- factor(group_var)
  mds_df$SampleID <- rownames(mds_df)
  
  # 保存每个变量的结果
  beta_results[[i]] <- list(
    adonis = adonis_res,
    p_value = p_value,
    mds_df = mds_df
  )
}
# 保存整个结果列表，供绘图用
saveRDS(beta_results, "oral_beta_diversity_results.rds")


beta_results <- readRDS("oral_beta_diversity_results.rds")
beta_plots <- list()

for (i in names(beta_results)) {
  mds_df <- beta_results[[i]]$mds_df
  p_value <- beta_results[[i]]$p_value
  p_label <- paste0("P = ", signif(p_value, 3))
  p_mds <- ggplot(mds_df, aes(x = MDS1, y = MDS2)) +
    # 先绘制组别0的点（在下层）
    geom_point(data = subset(mds_df, Group == "0"), 
               aes(color = Group), 
               alpha = 0.4, size = 0.01, stroke = 0.01) +
    # 再绘制组别1的点（在上层）
    geom_point(data = subset(mds_df, Group == "1"), 
               aes(color = Group), 
               alpha = 0.8, size = 0.01, stroke = 0.01) +
    # 组别0的椭圆
    stat_ellipse(data = subset(mds_df, Group == "0"),
                 aes(color = Group), 
                 level = 0.95, type = "norm", linetype = 2, alpha = 0.4) +
    # 组别1的椭圆
    stat_ellipse(data = subset(mds_df, Group == "1"),
                 aes(color = Group), 
                 level = 0.95, type = "norm", linetype = 2, alpha = 0.8) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "tomato")) +
    labs(
      title = paste("Beta Diversity (", i, ")", sep = ""),
      subtitle = p_label,
      x = "MDS1", y = "MDS2"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
  
  beta_plots[[i]] <- p_mds
}

# 导出 PDF
cairo_pdf("oral_beta_diversity_7cohort_20250809_mds_plots.pdf", width = 2.3, height = 2.1)
for (plot in beta_plots) {
  print(plot)
}
dev.off()




beta_results <- list()
checkkey=  c("Age",
                  "sex",
                  "BMI",
                  "Proton.pump.inhibitors",
                  "newHTN",
                  "cur.ever.smoking",
                  "Alcohol",
                  "Antibiotics",
                  "Diabetes.mellitus",
                  "Dyslipidemia"
                  
                  
)
# # 2. 一次性计算 Bray-Curtis 距离
# bc_dist <- vegdist(data, method = "bray")
# 
# # 3. 转换为矩阵，并保存为 CSV（保留样本名）
# bc_matrix <- as.matrix(bc_dist)
# write.csv(bc_matrix, "oral_bc_distance.csv", row.names = TRUE)

# 4. 读取保存的距离矩阵（并转为矩阵对象）
bc_matrix_read <- read.csv("oral_bc_distance.csv", row.names = 1, check.names = FALSE)
bc_matrix_read <- as.matrix(bc_matrix_read)

# 5. 循环每个疾病变量
for (i in checkkey) {
  message("Beta diversity - Processing: ", i)
  set.seed(123)
  
  # 筛选元数据中当前变量非 NA 的样本
  tempmeta <- meta5137
  tempmeta <- tempmeta[!is.na(tempmeta[, i]), ]
  
  # 找出当前变量相关的样本交集
  common_ids <- intersect(rownames(tempmeta), rownames(bc_matrix_read))
  tempmeta <- tempmeta[common_ids, ]
  
  group_var <- tempmeta[[i]]
  
  # 跳过非二分类变量
  if (length(unique(group_var)) != 2) {
    warning(paste("Variable", i, "does not have exactly 2 groups. Skipping."))
    next
  }
  
  # ✅ 从读取的完整矩阵中提取子矩阵
  bc_submatrix <- bc_matrix_read[common_ids, common_ids]
  
  
  # 转为 dist 对象用于后续分析
  bc_dist_sub <- as.dist(bc_submatrix)
  
  # Adonis2 分析
  adonis_res <- adonis2(bc_dist_sub ~ group_var, data = tempmeta, permutations = 999,parallel = 10)
  p_value <- adonis_res$`Pr(>F)`[1]
  set.seed(123)
  # MDS (PCoA)
  mds <- cmdscale(bc_dist_sub, k = 2, eig = TRUE)
  mds_df <- as.data.frame(mds$points)
  colnames(mds_df) <- c("MDS1", "MDS2")
  mds_df$Group <- factor(group_var)
  mds_df$SampleID <- rownames(mds_df)
  
  # 保存每个变量的结果
  beta_results[[i]] <- list(
    adonis = adonis_res,
    p_value = p_value,
    mds_df = mds_df
  )
}
# 保存整个结果列表，供绘图用
saveRDS(beta_results, "oral_beta_diversity_10factor_results.rds")


beta_results <- readRDS("oral_beta_diversity_10factor_results.rds")
beta_plots <- list()

for (i in names(beta_results)) {
  mds_df <- beta_results[[i]]$mds_df
  p_value <- beta_results[[i]]$p_value
  p_label <- paste0("Adonis P = ", signif(p_value, 3))
  
  p_mds <- ggplot(mds_df, aes(x = MDS1, y = MDS2, color = Group)) +
    geom_point(size = 0.2, alpha = 0.5) +
    stat_ellipse(level = 0.95, type = "norm", linetype = 2) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "tomato")) +
    labs(
      title = paste("Beta Diversity (", i, ")", sep = ""),
      subtitle = p_label,
      x = "MDS1", y = "MDS2"
    ) +
    theme_minimal(base_size = 10) +  # 基础字体 10pt
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
  
  beta_plots[[i]] <- p_mds
}

# 导出 PDF
cairo_pdf("oral_beta_diversity_20250809_mds_plots.pdf", width = 3, height = 3)
for (plot in beta_plots) {
  print(plot)
}
dev.off()

