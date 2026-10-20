# setwd("/Users/gizen/Documents/AF/fig_cytokine_box/")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

library(openxlsx)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

# ============================================================
# 1. 加载数据
# ============================================================

metadata <- as.data.frame(
  read.csv("5137_AF_meta_nonpaf_newHTN.csv", row.names = 1)
)

cytokine <- as.data.frame(read.xlsx(
  "■1740_1813_Cytokine_ForPaper_260209.xlsx",
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
  "■■1713MetabLifestylePapers250209.xlsx",
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
#  "TNF_alpha",
#  "IL8_CXCL8",
#  "CCL2",
#  "CXCL9",
  "IP10_CXCL10"
#  "CXCL11"
)

# 如果以后要画代谢物，再打开这里
# target_metabos <- c(
#   "Butyrate",
#   "Acetate",
#   "Propionate"
# )

all_targets <- target_cytokines

disease_list <- c("AF", "IHD", "CHF")

# ============================================================
# 3. 颜色与显著性标记
# ============================================================

# COLOR_HEALTH1 <- "#FAB1AD"
# COLOR_HEALTH0 <- "#90CAD0"

COLOR_HEALTH1 <- "#F8766D"
COLOR_HEALTH0 <- "#00BFC4"

sig_label <- function(p) {
  if (is.na(p))  return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# ============================================================
# 4. 单个 panel 绘图函数
#    修改点：
#    每个图用 health 0 / 1 两组中位数的平均值作为中心
#    并在 log10 空间里调整 y 轴范围
# ============================================================

make_panel <- function(
    tgt,
    df_plot,
    p_val,
    y_quantile_lower = 0.1,
    y_quantile_upper = 0.9,
    y_expand = 1.01
) {
  
  if (nrow(df_plot) == 0) {
    return(nullGrob())
  }
  
  df_plot <- df_plot %>%
    filter(!is.na(value), !is.na(health), value > 0)
  
  if (nrow(df_plot) == 0) {
    return(nullGrob())
  }
  
  if (length(unique(df_plot$health)) < 2) {
    return(nullGrob())
  }
  
  # ------------------------------------------------------------
  # y轴范围计算：
  # 1. 用两组中位数的平均值作为图的中心
  # 2. 用分位数范围代替 min/max，忽略极端值
  # 3. 在 log10 空间里保持上下视觉距离对称
  # ------------------------------------------------------------
  
  median_h1 <- median(df_plot$value[df_plot$health == 1], na.rm = TRUE)
  median_h0 <- median(df_plot$value[df_plot$health == 0], na.rm = TRUE)
  
  center_y <- mean(c(median_h1, median_h0), na.rm = TRUE)
  
  # 用分位数忽略极端值
  data_lower <- as.numeric(
    quantile(df_plot$value, y_quantile_lower, na.rm = TRUE)
  )
  
  data_upper <- as.numeric(
    quantile(df_plot$value, y_quantile_upper, na.rm = TRUE)
  )
  
  # 保险：如果分位数计算异常，则退回到 min/max
  if (
    !is.finite(data_lower) ||
    !is.finite(data_upper) ||
    data_lower <= 0 ||
    data_upper <= data_lower
  ) {
    data_lower <- min(df_plot$value, na.rm = TRUE)
    data_upper <- max(df_plot$value, na.rm = TRUE)
  }
  
  if (
    !is.finite(center_y) ||
    !is.finite(data_lower) ||
    !is.finite(data_upper) ||
    center_y <= 0 ||
    data_lower <= 0 ||
    data_upper <= 0 ||
    data_upper <= data_lower
  ) {
    return(nullGrob())
  }
  
  log_center <- log10(center_y)
  log_lower  <- log10(data_lower)
  log_upper  <- log10(data_upper)
  
  log_range <- max(
    abs(log_center - log_lower),
    abs(log_upper - log_center),
    na.rm = TRUE
  )
  
  if (!is.finite(log_range) || log_range <= 0) {
    log_range <- 0.5
  }
  
  # 稍微留白
  log_range <- log_range * y_expand
  
  y_lower <- 10^(log_center - log_range)
  y_upper <- 10^(log_center + log_range)
  
  # 显著性标记放在当前 y 轴范围上方附近
  sig_y <- 10^(
    log10(y_upper) -
      0.08 * (log10(y_upper) - log10(y_lower))
  )
  
  violin_plot <- ggplot() +
    geom_violin(
      data = subset(df_plot, health == 1),
      aes(x = 1, y = value),
      fill = COLOR_HEALTH1,
      color = NA,
      position = position_nudge(x = 0.12),
      width = 0.4,
      alpha = 0.6
    ) +
    geom_violin(
      data = subset(df_plot, health == 0),
      aes(x = 1, y = value),
      fill = COLOR_HEALTH0,
      color = NA,
      position = position_nudge(x = -0.12),
      width = 0.4,
      alpha = 0.6
    ) +
    geom_boxplot(
      data = subset(df_plot, health == 1),
      aes(x = 1, y = value),
      width = 0.15,
      position = position_nudge(x = 0.12),
      outlier.shape = NA,
      linewidth = 0.3
    ) +
    geom_boxplot(
      data = subset(df_plot, health == 0),
      aes(x = 1, y = value),
      width = 0.15,
      position = position_nudge(x = -0.12),
      outlier.shape = NA,
      linewidth = 0.3
    ) +
    annotate(
      "text",
      x = 1,
      y = sig_y,
      label = sig_label(p_val),
      size = 5 / 2.845
    ) +
    scale_y_log10() +
    scale_x_continuous(breaks = NULL) +
    coord_cartesian(ylim = c(y_lower, y_upper)) +
    theme_minimal(base_size = 5) +
    theme(
      axis.title       = element_blank(),
      axis.text.x      = element_blank(),
      axis.text        = element_text(size = 5),
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.4
      )
    )
  
  title_grob <- grobTree(
    rectGrob(gp = gpar(fill = "#E6E6E6", col = "black", lwd = 0.6)),
    textGrob(tgt, gp = gpar(fontsize = 5, fontface = "bold"))
  )
  
  p_grob <- grobTree(
    rectGrob(gp = gpar(fill = "#FFFFFF", col = "black", lwd = 0.6)),
    textGrob(
      paste0("p=", formatC(p_val, digits = 2, format = "e")),
      gp = gpar(fontsize = 5)
    )
  )
  
  arrangeGrob(
    title_grob,
    p_grob,
    violin_plot,
    ncol = 1,
    heights = c(0.12, 0.10, 0.78)
  )
}

# ============================================================
# 5. 对指定疾病变量画图
# ============================================================

plot_violin <- function(data_df, metadata_df, targets, dis) {
  
  if (!(dis %in% colnames(metadata_df))) {
    stop(paste0("metadata 中不存在列: ", dis))
  }
  
  targets_exist <- intersect(targets, colnames(data_df))
  
  plot_list <- list()
  
  common_ids <- intersect(rownames(data_df), rownames(metadata_df))
  
  data_df <- data_df[common_ids, , drop = FALSE]
  health_vec <- metadata_df[common_ids, dis]
  
  for (tgt in targets_exist) {
    
    vals <- data_df[, tgt]
    vals <- suppressWarnings(as.numeric(vals))
    
    min_nz <- suppressWarnings(min(vals[vals > 0], na.rm = TRUE))
    
    if (!is.finite(min_nz)) {
      plot_list[[tgt]] <- nullGrob()
      next
    }
    
    vals[vals == 0] <- min_nz / 2
    
    df_plot <- data.frame(
      value  = vals,
      health = health_vec
    ) %>%
      filter(!is.na(value) & !is.na(health) & value > 0)
    
    # health 统一转成 0 / 1
    df_plot$health <- as.numeric(as.character(df_plot$health))
    
    df_plot <- df_plot %>%
      filter(health %in% c(0, 1))
    
    p_val <- tryCatch(
      {
        if (length(unique(df_plot$health)) < 2) {
          NA
        } else {
          wilcox.test(
            df_plot$value[df_plot$health == 0],
            df_plot$value[df_plot$health == 1]
          )$p.value
        }
      },
      error = function(e) NA
    )
    
    plot_list[[tgt]] <- make_panel(tgt, df_plot, p_val)
  }
  
  return(plot_list)
}

# ============================================================
# 6. 分别生成 AF / IHD / CHF 的所有图
# ============================================================

plots_by_dis <- list()

for (dis in disease_list) {
  
  plots_cytokine <- plot_violin(
    data_df     = cytokine,
    metadata_df = metadata,
    targets     = target_cytokines,
    dis         = dis
  )
  
  # 如果以后要画 metabo，再打开这里
  # plots_metabo <- plot_violin(
  #   data_df     = metabo,
  #   metadata_df = metadata,
  #   targets     = target_metabos,
  #   dis         = dis
  # )
  # plots_by_dis[[dis]] <- c(plots_cytokine, plots_metabo)
  
  plots_by_dis[[dis]] <- plots_cytokine
}

# ============================================================
# 7. 第一行：AF / IHD / CHF 疾病标题
# ============================================================

make_dis_title <- function(label) {
  grobTree(
    rectGrob(gp = gpar(fill = "#D9D9D9", col = "black", lwd = 0.8)),
    textGrob(label, gp = gpar(fontsize = 8, fontface = "bold"))
  )
}

title_row <- arrangeGrob(
  grobs = list(
    make_dis_title("AF"),
    make_dis_title("IHD"),
    make_dis_title("CHF")
  ),
  ncol = 3
)

# ============================================================
# 8. 后续行：
#    AF 一列，IHD 一列，CHF 一列
#    每一行是同一个 target
# ============================================================

row_list <- list()

for (tgt in all_targets) {
  
  p_af  <- plots_by_dis[["AF"]][[tgt]]
  p_ihd <- plots_by_dis[["IHD"]][[tgt]]
  p_hf  <- plots_by_dis[["CHF"]][[tgt]]
  
  if (is.null(p_af))  p_af  <- nullGrob()
  if (is.null(p_ihd)) p_ihd <- nullGrob()
  if (is.null(p_hf))  p_hf  <- nullGrob()
  
  # 防止 list 嵌套导致 arrangeGrob 报错
  if (is.list(p_af) && !inherits(p_af, "grob") && !inherits(p_af, "gtable")) {
    p_af <- p_af[[1]]
  }
  if (is.list(p_ihd) && !inherits(p_ihd, "grob") && !inherits(p_ihd, "gtable")) {
    p_ihd <- p_ihd[[1]]
  }
  if (is.list(p_hf) && !inherits(p_hf, "grob") && !inherits(p_hf, "gtable")) {
    p_hf <- p_hf[[1]]
  }
  
  row_list[[tgt]] <- arrangeGrob(
    grobs = list(p_af, p_ihd, p_hf),
    ncol = 3
  )
}

# ============================================================
# 9. 输出到同一个 PDF
# ============================================================

pdf(
  "cytokine_violin_AF_IHD_CHF_centered_by_median.pdf",
  width  = 1.75,
  height = 5
)

final_grobs <- c(list(title_row), unname(row_list))

grid.arrange(
  grobs = final_grobs,
  ncol = 1,
  heights = c(
    0.35,
    rep(1, length(row_list))
  )
)

dev.off()

