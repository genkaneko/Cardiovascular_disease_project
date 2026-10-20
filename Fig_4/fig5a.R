setwd("/Users/gizen/Documents/AF/fig_cytokine_box/")

library(openxlsx)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(ComplexHeatmap)

par(family = "Helvetica")

# ============================================================
# 1. 加载数据
# ============================================================

metadata <- as.data.frame(
  read.csv("../5137_AF_meta_nonpaf_newHTN.csv", row.names = 1)
)

cytokine <- as.data.frame(read.xlsx(
  "../../smoking/6803/■1740_1813_Cytokine_ForPaper_260209.xlsx",
  rowNames = TRUE,
  check.names = FALSE,
  sep.names = " ",
  sheet = 1
))

rownames(cytokine) <- cytokine$METAF
rownames(cytokine) <- sapply(rownames(cytokine), function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!is.na(v)) as.character(v) else x
})

cytokine <- cytokine[, -c(1:3)]

metabo <- as.data.frame(read.xlsx(
  "../../smoking/6803/■■1713MetabLifestylePapers250209.xlsx",
  check.names = FALSE,
  sep.names = " "
))

rownames(metabo) <- metabo$METAF
rownames(metabo) <- sapply(rownames(metabo), function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!is.na(v)) as.character(v) else x
})

metabo <- metabo[, c(-1, -2, -3)]

# ============================================================
# 2. 指定变量
# ============================================================

target_cytokines <- c(
  "TNF_alpha",
  "IL8_CXCL8",
  "CCL2",
  "CXCL9",
  "IP10_CXCL10",
  "CXCL11"
)

all_targets <- target_cytokines
disease_list <- c("AF", "IHD", "CHF")

# ============================================================
# 3. 颜色设置
# ============================================================

COLOR_DECREASE <- "#90CAD0"  # 水色
COLOR_INCREASE <- "#FAB1AD"  # 桃色
COLOR_NS       <- "white"

fs <- 5
cell_size <- unit(2.5, "mm")

# ============================================================
# 4. 计算每个 disease × cytokine 的 mean log2FC / p / n
# ============================================================

calc_cytokine_summary <- function(data_df, metadata_df, targets, disease_list) {
  
  res_list <- list()
  
  common_ids <- intersect(rownames(data_df), rownames(metadata_df))
  
  data_df <- data_df[common_ids, , drop = FALSE]
  metadata_df <- metadata_df[common_ids, , drop = FALSE]
  
  for (dis in disease_list) {
    
    if (!(dis %in% colnames(metadata_df))) {
      stop(paste0("metadata 中不存在列: ", dis))
    }
    
    disease_vec <- metadata_df[, dis]
    disease_vec <- as.numeric(as.character(disease_vec))
    
    for (tgt in targets) {
      
      if (!(tgt %in% colnames(data_df))) {
        next
      }
      
      vals <- data_df[, tgt]
      vals <- suppressWarnings(as.numeric(vals))
      
      min_nz <- suppressWarnings(min(vals[vals > 0], na.rm = TRUE))
      
      if (!is.finite(min_nz)) {
        res_list[[paste(dis, tgt, sep = "_")]] <- data.frame(
          Disease = dis,
          Cytokine = tgt,
          n_total = 0,
          n_disease1 = 0,
          n_disease0 = 0,
          mean_disease0 = NA,
          mean_disease1 = NA,
          median_disease0 = NA,
          median_disease1 = NA,
          log2FC_mean_1_vs_0 = NA,
          p_value = NA,
          heatmap_group = "ns"
        )
        next
      }
      
      vals[vals == 0] <- min_nz / 2
      
      df <- data.frame(
        value = vals,
        disease_status = disease_vec
      ) %>%
        filter(
          !is.na(value),
          !is.na(disease_status),
          value > 0,
          disease_status %in% c(0, 1)
        )
      
      df0 <- df$value[df$disease_status == 0]
      df1 <- df$value[df$disease_status == 1]
      
      n_total <- nrow(df)
      n_disease1 <- length(df1)
      n_disease0 <- length(df0)
      
      mean0 <- mean(df0, na.rm = TRUE)
      mean1 <- mean(df1, na.rm = TRUE)
      
      med0 <- median(df0, na.rm = TRUE)
      med1 <- median(df1, na.rm = TRUE)
      
      log2fc_mean <- ifelse(
        is.na(mean0) | is.na(mean1) | mean0 <= 0 | mean1 <= 0,
        NA,
        log2(mean1 / mean0)
      )
      
      p_val <- tryCatch(
        {
          if (length(unique(df$disease_status)) < 2) {
            NA
          } else {
            wilcox.test(
              df$value[df$disease_status == 0],
              df$value[df$disease_status == 1]
            )$p.value
          }
        },
        error = function(e) NA
      )
      
      heatmap_group <- ifelse(
        is.na(p_val) | p_val > 0.05 | is.na(log2fc_mean),
        "ns",
        ifelse(log2fc_mean > 0, "increase", "decrease")
      )
      
      res_list[[paste(dis, tgt, sep = "_")]] <- data.frame(
        Disease = dis,
        Cytokine = tgt,
        n_total = n_total,
        n_disease1 = n_disease1,
        n_disease0 = n_disease0,
        mean_disease0 = mean0,
        mean_disease1 = mean1,
        median_disease0 = med0,
        median_disease1 = med1,
        log2FC_mean_1_vs_0 = log2fc_mean,
        p_value = p_val,
        heatmap_group = heatmap_group
      )
    }
  }
  
  do.call(rbind, res_list)
}

summary_cytokine <- calc_cytokine_summary(
  data_df = cytokine,
  metadata_df = metadata,
  targets = target_cytokines,
  disease_list = disease_list
)

# 确认到底有没有显著的 increase / decrease
print(table(summary_cytokine$heatmap_group, useNA = "ifany"))

# ============================================================
# 5. 输出 summary dataframe
# ============================================================

write.csv(
  summary_cytokine,
  "cytokine_disease_log2FC_summary.csv",
  row.names = FALSE
)

write.xlsx(
  summary_cytokine,
  "cytokine_disease_log2FC_summary.xlsx",
  rowNames = FALSE
)

# ============================================================
# 6. 构建 heatmap matrix
#    注意：这里一定不要转 factor
# ============================================================

mat <- matrix(
  "ns",
  nrow = length(disease_list),
  ncol = length(target_cytokines)
)

rownames(mat) <- disease_list
colnames(mat) <- target_cytokines

for (i in seq_len(nrow(summary_cytokine))) {
  
  dis <- summary_cytokine$Disease[i]
  tgt <- summary_cytokine$Cytokine[i]
  
  mat[dis, tgt] <- as.character(summary_cytokine$heatmap_group[i])
}

mat <- as.matrix(mat)

# 确认 heatmap matrix 里面有没有颜色分类
print(mat)
print(table(as.vector(mat), useNA = "ifany"))

# ============================================================
# 7. heatmap color
# ============================================================

col_fun <- c(
  "decrease" = COLOR_DECREASE,
  "increase" = COLOR_INCREASE,
  "ns"       = COLOR_NS
)

# ============================================================
# 8. 画 heatmap
#    每个 square = 2.5 mm
#    font = Helvetica, 5 pt
# ============================================================

ht <- Heatmap(
  mat,
  name = "Direction",
  col = col_fun,
  na_col = "white",
  
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  
  show_row_names = TRUE,
  show_column_names = TRUE,
  
  row_names_gp = gpar(fontsize = fs, fontfamily = "Helvetica"),
  column_names_gp = gpar(fontsize = fs, fontfamily = "Helvetica"),
  
  width = cell_size * ncol(mat),
  height = cell_size * nrow(mat),
  
  rect_gp = gpar(col = "black", lwd = 0.3),
  
  heatmap_legend_param = list(
    title_gp = gpar(fontsize = fs, fontfamily = "Helvetica"),
    labels_gp = gpar(fontsize = fs, fontfamily = "Helvetica"),
    grid_width = unit(2.5, "mm"),
    grid_height = unit(2.5, "mm"),
    at = c("increase", "decrease", "ns"),
    labels = c("increase", "decrease", "ns")
  )
)

pdf(
  "cytokine_disease_direction_heatmap.pdf",
  width = 2,
  height = 1.3,
  family = "Helvetica"
)

draw(
  ht,
  heatmap_legend_side = "right"
)

dev.off()