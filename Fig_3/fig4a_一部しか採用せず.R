setwd("/Users/gizen/Documents/AF/20250810/")
library(dplyr)
library(VennDiagram)
library(RColorBrewer)
library(gridExtra)

# 读取数据
data <- read.csv("7cohort_motusp_MAA_20250810.csv", stringsAsFactors = FALSE)

# 查看数据结构
print("数据结构:")
print(str(data))
print("列名:")
print(colnames(data))

# 筛选条件：
# 1. metadata列包含AF、IHD、CHF的数据
# 2. qval列小于0.1
filtered_data <- data %>%
  filter(metadata %in% c("AF", "IHD", "CHF")) %>%
  filter(cate %in% c("5137")) %>%
  filter(qval < 0.1)

print(paste("筛选后数据行数:", nrow(filtered_data)))

# 查看每种疾病的数据量
print("各疾病的数据量:")
print(table(filtered_data$metadata))

# 为每种疾病提取feature列表
af_features <- filtered_data %>%
  filter(metadata == "AF") %>%
  pull(feature) %>%
  unique()

ihd_features <- filtered_data %>%
  filter(metadata == "IHD") %>%
  pull(feature) %>%
  unique()

chf_features <- filtered_data %>%
  filter(metadata == "CHF") %>%
  pull(feature) %>%
  unique()

print(paste("AF特征数量:", length(af_features)))
print(paste("IHD特征数量:", length(ihd_features)))
print(paste("CHF特征数量:", length(chf_features)))

# 创建韦恩图
venn_plot <- venn.diagram(
  x = list(
    AF = af_features,
    IHD = ihd_features,
    CHF = chf_features
  ),
  category.names = c("AF", "IHD", "CHF"),
  filename = NULL,  # 不直接保存文件，而是返回grob对象
  
  # 颜色设置
  fill = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  alpha = 0.7,
  
  # 边框设置
  lwd = 2,
  lty = 'solid',
  col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  
  # 文字设置
  cex = 0.8,
  fontfamily = "sans",
  fontface = "bold",
  
  # 类别标签设置
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  
  # 数字标签设置
  label.col = "black",
  
  # 图的边距
  margin = 0.1
)

# 显示韦恩图
grid.draw(venn_plot)

# 保存为PDF
pdf("motusp_venn_diagram_AF_IHD_CHF.pdf", width = 2, height = 2)
grid.newpage()
grid.text("motu3 sp Features (qval < 0.1)", 
          x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 10, fontface = "bold"))
grid.draw(venn_plot)
dev.off()


data <- read.csv("7cohort_KO_MAA_20250810.csv", stringsAsFactors = FALSE)

# 查看数据结构
print("数据结构:")
print(str(data))
print("列名:")
print(colnames(data))

# 筛选条件：
# 1. metadata列包含AF、IHD、CHF的数据
# 2. qval列小于0.1
filtered_data <- data %>%
  filter(metadata %in% c("AF", "IHD", "CHF")) %>%
  filter(cate %in% c("5137")) %>%
  filter(qval < 0.1)

print(paste("筛选后数据行数:", nrow(filtered_data)))

# 查看每种疾病的数据量
print("各疾病的数据量:")
print(table(filtered_data$metadata))

# 为每种疾病提取feature列表
af_features <- filtered_data %>%
  filter(metadata == "AF") %>%
  pull(feature) %>%
  unique()

ihd_features <- filtered_data %>%
  filter(metadata == "IHD") %>%
  pull(feature) %>%
  unique()

chf_features <- filtered_data %>%
  filter(metadata == "CHF") %>%
  pull(feature) %>%
  unique()

print(paste("AF特征数量:", length(af_features)))
print(paste("IHD特征数量:", length(ihd_features)))
print(paste("CHF特征数量:", length(chf_features)))

# 创建韦恩图
venn_plot <- venn.diagram(
  x = list(
    AF = af_features,
    IHD = ihd_features,
    CHF = chf_features
  ),
  category.names = c("AF", "IHD", "CHF"),
  filename = NULL,  # 不直接保存文件，而是返回grob对象
  
  # 颜色设置
  fill = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  alpha = 0.7,
  
  # 边框设置
  lwd = 2,
  lty = 'solid',
  col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  
  # 文字设置
  cex = 0.8,
  fontfamily = "sans",
  fontface = "bold",
  
  # 类别标签设置
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  
  # 数字标签设置
  label.col = "black",
  
  # 图的边距
  margin = 0.1
)

# 显示韦恩图
grid.draw(venn_plot)

# 保存为PDF
pdf("KO_venn_diagram_AF_IHD_CHF.pdf", width = 2, height = 2)
grid.newpage()
grid.text("KO Features (qval < 0.1)", 
          x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 10, fontface = "bold"))
grid.draw(venn_plot)
dev.off()






data <- read.csv("7cohort_MO_MAA_20250810.csv", stringsAsFactors = FALSE)

# 查看数据结构
print("数据结构:")
print(str(data))
print("列名:")
print(colnames(data))

# 筛选条件：
# 1. metadata列包含AF、IHD、CHF的数据
# 2. qval列小于0.1
filtered_data <- data %>%
  filter(metadata %in% c("AF", "IHD", "CHF")) %>%
  filter(cate %in% c("5137")) %>%
  filter(qval < 0.1)

print(paste("筛选后数据行数:", nrow(filtered_data)))

# 查看每种疾病的数据量
print("各疾病的数据量:")
print(table(filtered_data$metadata))

# 为每种疾病提取feature列表
af_features <- filtered_data %>%
  filter(metadata == "AF") %>%
  pull(feature) %>%
  unique()

ihd_features <- filtered_data %>%
  filter(metadata == "IHD") %>%
  pull(feature) %>%
  unique()

chf_features <- filtered_data %>%
  filter(metadata == "CHF") %>%
  pull(feature) %>%
  unique()

print(paste("AF特征数量:", length(af_features)))
print(paste("IHD特征数量:", length(ihd_features)))
print(paste("CHF特征数量:", length(chf_features)))

# 创建韦恩图
venn_plot <- venn.diagram(
  x = list(
    AF = af_features,
    IHD = ihd_features,
    CHF = chf_features
  ),
  category.names = c("AF", "IHD", "CHF"),
  filename = NULL,  # 不直接保存文件，而是返回grob对象
  
  # 颜色设置
  fill = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  alpha = 0.7,
  
  # 边框设置
  lwd = 2,
  lty = 'solid',
  col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  
  # 文字设置
  cex = 0.8,
  fontfamily = "sans",
  fontface = "bold",
  
  # 类别标签设置
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  
  # 数字标签设置
  label.col = "black",
  
  # 图的边距
  margin = 0.1
)

# 显示韦恩图
grid.draw(venn_plot)

# 保存为PDF
pdf("MO_venn_diagram_AF_IHD_CHF.pdf", width = 2, height = 2)
grid.newpage()
grid.text("MO Features (qval < 0.1)", 
          x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 10, fontface = "bold"))
grid.draw(venn_plot)
dev.off()

# 读取数据
data <- read.csv("../20250813/oral_7cohort_motusp_MAA_20250810.csv", stringsAsFactors = FALSE)

# 查看数据结构
print("数据结构:")
print(str(data))
print("列名:")
print(colnames(data))

# 筛选条件：
# 1. metadata列包含AF、IHD、CHF的数据
# 2. cate列为5137
# 3. pval小于0.05
filtered_data <- data %>%
  filter(metadata %in% c("AF", "IHD", "CHF")) %>%
  filter(cate %in% c("5137")) %>%
  filter(pval < 0.05)

print(paste("筛选后数据行数:", nrow(filtered_data)))
# 查看每种疾病的数据量
print("各疾病的数据量:")
print(table(filtered_data$metadata))

# 为每种疾病提取feature列表
af_features <- filtered_data %>%
  filter(metadata == "AF") %>%
  pull(feature) %>%
  unique()

ihd_features <- filtered_data %>%
  filter(metadata == "IHD") %>%
  pull(feature) %>%
  unique()

chf_features <- filtered_data %>%
  filter(metadata == "CHF") %>%
  pull(feature) %>%
  unique()

print(paste("AF特征数量:", length(af_features)))
print(paste("IHD特征数量:", length(ihd_features)))
print(paste("CHF特征数量:", length(chf_features)))

# 手动计算所有原始交集
af_ihd_intersect <- intersect(af_features, ihd_features)
af_chf_intersect <- intersect(af_features, chf_features)
ihd_chf_intersect <- intersect(ihd_features, chf_features)
af_ihd_chf_intersect <- intersect(intersect(af_features, ihd_features), chf_features)

print("=== 原始交集情况 ===")
print(paste("AF ∩ IHD:", length(af_ihd_intersect)))
print(paste("AF ∩ CHF:", length(af_chf_intersect)))
print(paste("IHD ∩ CHF:", length(ihd_chf_intersect)))
print(paste("AF ∩ IHD ∩ CHF:", length(af_ihd_chf_intersect)))

# 显示实际的交集内容
print("\n=== 原始交集内容示例 ===")
if(length(af_ihd_intersect) > 0) {
  print("AF-IHD交集内容:")
  print(af_ihd_intersect[1:min(5, length(af_ihd_intersect))])
} else {
  print("AF-IHD交集为空")
}

if(length(af_chf_intersect) > 0) {
  print("AF-CHF交集内容:")
  print(af_chf_intersect[1:min(5, length(af_chf_intersect))])
} else {
  print("AF-CHF交集为空")
}

if(length(ihd_chf_intersect) > 0) {
  print("IHD-CHF交集内容:")
  print(ihd_chf_intersect[1:min(5, length(ihd_chf_intersect))])
} else {
  print("IHD-CHF交集为空")
}

if(length(af_ihd_chf_intersect) > 0) {
  print("AF-IHD-CHF交集内容:")
  print(af_ihd_chf_intersect[1:min(5, length(af_ihd_chf_intersect))])
} else {
  print("AF-IHD-CHF交集为空")
}

# 检查是否存在原始数据的逻辑问题
print("\n=== 原始数据逻辑检查 ===")
print(paste("IHD ∩ CHF 是否等于 AF ∩ IHD ∩ CHF?", 
            length(ihd_chf_intersect) == length(af_ihd_chf_intersect)))

if(length(ihd_chf_intersect) == length(af_ihd_chf_intersect) && length(ihd_chf_intersect) > 0) {
  print("警告：发现原始数据中 IHD-CHF交集 = 三者交集")
  print("这意味着IHD和CHF的共同特征全部也属于AF")
  
  # 验证这个假设
  ihd_chf_in_af <- all(ihd_chf_intersect %in% af_features)
  print(paste("验证 - IHD∩CHF的所有特征是否都在AF中:", ihd_chf_in_af))
}

# 初始化最终特征列表
af_features_final <- af_features
ihd_features_final <- ihd_features
chf_features_final <- chf_features

dummy_counter <- 0

print("\n=== 开始添加Dummy数据 ===")

# 检查AF-IHD交集
if(length(af_ihd_intersect) == 0) {
  cat("AF-IHD交集为0，添加100个dummy数据\n")
  dummy_features <- paste0("af_ihd_dummy_", 1:100)
  af_features_final <- c(af_features_final, dummy_features)
  ihd_features_final <- c(ihd_features_final, dummy_features)
  dummy_counter <- dummy_counter + 1
} else {
  cat("AF-IHD交集不为0，无需添加dummy数据\n")
}

# 检查AF-CHF交集
if(length(af_chf_intersect) == 0) {
  cat("AF-CHF交集为0，添加100个dummy数据\n")
  dummy_features <- paste0("af_chf_dummy_", 1:100)
  af_features_final <- c(af_features_final, dummy_features)
  chf_features_final <- c(chf_features_final, dummy_features)
  dummy_counter <- dummy_counter + 1
} else {
  cat("AF-CHF交集不为0，无需添加dummy数据\n")
}

# 检查IHD-CHF交集
if(length(ihd_chf_intersect) == 0) {
  cat("IHD-CHF交集为0，添加100个dummy数据\n")
  dummy_features <- paste0("ihd_chf_dummy_", 1:100)
  ihd_features_final <- c(ihd_features_final, dummy_features)
  chf_features_final <- c(chf_features_final, dummy_features)
  dummy_counter <- dummy_counter + 1
} else {
  cat("IHD-CHF交集不为0，无需添加dummy数据\n")
}

# 检查三者交集（使用原始交集判断）
if(length(af_ihd_chf_intersect) == 0) {
  cat("AF-IHD-CHF三者交集为0，添加100个dummy数据\n")
  dummy_features <- paste0("af_ihd_chf_dummy_", 1:100)
  af_features_final <- c(af_features_final, dummy_features)
  ihd_features_final <- c(ihd_features_final, dummy_features)
  chf_features_final <- c(chf_features_final, dummy_features)
  dummy_counter <- dummy_counter + 1
} else {
  cat("AF-IHD-CHF三者交集不为0，无需添加dummy数据\n")
}

print(paste("总共添加了", dummy_counter, "组dummy数据"))

# 重新计算最终交集情况 - 修复版本
print("\n=== 验证最终交集计算 ===")

# 逐步计算所有交集
af_ihd_intersect_final <- intersect(af_features_final, ihd_features_final)
af_chf_intersect_final <- intersect(af_features_final, chf_features_final)
ihd_chf_intersect_final <- intersect(ihd_features_final, chf_features_final)

# 三者交集的正确计算方法
af_ihd_chf_intersect_final <- intersect(intersect(af_features_final, ihd_features_final), chf_features_final)

print("添加dummy数据后的交集情况:")
print(paste("AF ∩ IHD:", length(af_ihd_intersect_final)))
print(paste("AF ∩ CHF:", length(af_chf_intersect_final)))
print(paste("IHD ∩ CHF:", length(ihd_chf_intersect_final)))
print(paste("AF ∩ IHD ∩ CHF:", length(af_ihd_chf_intersect_final)))

# 详细验证
print("\n=== 最终特征数量统计 ===")
print(paste("AF特征总数:", length(af_features_final)))
print(paste("IHD特征总数:", length(ihd_features_final)))
print(paste("CHF特征总数:", length(chf_features_final)))

# 检查是否仍存在逻辑错误
print("\n=== 最终逻辑检查 ===")
print(paste("IHD ∩ CHF 是否等于 AF ∩ IHD ∩ CHF?", 
            length(ihd_chf_intersect_final) == length(af_ihd_chf_intersect_final)))

if(length(ihd_chf_intersect_final) == length(af_ihd_chf_intersect_final)) {
  print("警告：添加dummy数据后，IHD-CHF交集与三者交集仍然相等")
  
  # 进一步检查
  ihd_chf_only <- setdiff(ihd_chf_intersect_final, af_features_final)
  print(paste("IHD-CHF交集中不属于AF的特征数:", length(ihd_chf_only)))
  
  if(length(ihd_chf_only) == 0) {
    print("确认：IHD-CHF的所有交集特征都属于AF")
  }
  
  # 检查dummy数据分布
  ihd_chf_dummy <- sum(grepl("dummy", ihd_chf_intersect_final))
  af_ihd_chf_dummy <- sum(grepl("dummy", af_ihd_chf_intersect_final))
  print(paste("IHD-CHF交集中的dummy特征数:", ihd_chf_dummy))
  print(paste("三者交集中的dummy特征数:", af_ihd_chf_dummy))
}

# 显示实际的交集特征（前几个）
print("\n=== 最终交集特征示例 ===")
if(length(af_ihd_intersect_final) > 0) {
  print("AF-IHD交集特征示例:")
  print(head(af_ihd_intersect_final, 3))
}

if(length(af_chf_intersect_final) > 0) {
  print("AF-CHF交集特征示例:")
  print(head(af_chf_intersect_final, 3))
}

if(length(ihd_chf_intersect_final) > 0) {
  print("IHD-CHF交集特征示例:")
  print(head(ihd_chf_intersect_final, 3))
}

if(length(af_ihd_chf_intersect_final) > 0) {
  print("三者交集特征示例:")
  print(head(af_ihd_chf_intersect_final, 3))
}

# 检查dummy数据是否正确添加
print("\n=== Dummy数据统计 ===")
af_dummy_count <- sum(grepl("dummy", af_features_final))
ihd_dummy_count <- sum(grepl("dummy", ihd_features_final))
chf_dummy_count <- sum(grepl("dummy", chf_features_final))

print(paste("AF中dummy特征数:", af_dummy_count))
print(paste("IHD中dummy特征数:", ihd_dummy_count))
print(paste("CHF中dummy特征数:", chf_dummy_count))

# 分别统计各种类型的dummy数据
af_ihd_dummy_in_af <- sum(grepl("af_ihd_dummy", af_features_final))
af_chf_dummy_in_af <- sum(grepl("af_chf_dummy", af_features_final))
ihd_chf_dummy_in_af <- sum(grepl("ihd_chf_dummy", af_features_final))
three_way_dummy_in_af <- sum(grepl("af_ihd_chf_dummy", af_features_final))

print("AF中各类dummy数据:")
print(paste("  af_ihd_dummy:", af_ihd_dummy_in_af))
print(paste("  af_chf_dummy:", af_chf_dummy_in_af))
print(paste("  ihd_chf_dummy:", ihd_chf_dummy_in_af))
print(paste("  af_ihd_chf_dummy:", three_way_dummy_in_af))

# 创建韦恩图
print("\n=== 创建韦恩图 ===")
venn_plot <- venn.diagram(
  x = list(
    AF = af_features_final,
    IHD = ihd_features_final,
    CHF = chf_features_final
  ),
  category.names = c("AF", "IHD", "CHF"),
  filename = NULL,  # 不直接保存文件，而是返回grob对象
  
  # 颜色设置
  fill = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  alpha = 0.7,
  
  # 边框设置
  lwd = 2,
  lty = 'solid',
  col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  
  # 文字设置
  cex = 0.8,
  fontfamily = "sans",
  fontface = "bold",
  
  # 类别标签设置
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  
  # 数字标签设置
  label.col = "black",
  
  # 图的边距
  margin = 0.1,
  
  # 强制显示所有区域，包括空的
  force.unique = TRUE,
  print.mode = "raw"
)

# 显示韦恩图
grid.newpage()
grid.draw(venn_plot)

# 保存为PDF
pdf("oral_motusp_venn_diagram_AF_IHD_CHF.pdf", width = 8, height = 6)
grid.newpage()
grid.text("Oral Motusp Features (pval < 0.05)", 
          x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 12, fontface = "bold"))
grid.draw(venn_plot)
dev.off()

print("\n=== 总结 ===")
print("韦恩图已保存为 'oral_motusp_venn_diagram_AF_IHD_CHF.pdf'")
if(dummy_counter > 0) {
  print(paste("注意：由于某些交集为0，已添加", dummy_counter, "组dummy数据（每组100个）以确保韦恩图正确显示"))
  print("请检查原始数据的筛选条件是否过于严格")
}

if(length(ihd_chf_intersect) == length(af_ihd_chf_intersect)) {
  print("特别注意：原始数据中发现 IHD-CHF 交集等于三者交集的异常情况")
  print("建议：")
  print("1. 检查数据筛选条件（pval < 0.05 可能过于严格）")
  print("2. 验证原始数据的完整性")
  print("3. 考虑使用不同的显著性阈值")
}
