# setwd("/Users/gizen/Documents/AF/fig_cytokine_box/")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

library(openxlsx)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

set.seed(123)

# ============================================================
# 1. データ読み込み
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
# 2. 対象変数・表示設定
# ============================================================

target_cytokines <- c(
  # "TNF_alpha",
  # "IL8_CXCL8",
  # "CCL2",
  # "CXCL9",
  "IP10_CXCL10"
  # "CXCL11"
)

# 代謝物も描画する場合は有効化
# target_metabos <- c(
#   "Butyrate",
#   "Acetate",
#   "Propionate"
# )

all_targets <- target_cytokines

# 代謝物も描画する場合はこちらに変更
# all_targets <- c(target_cytokines, target_metabos)

disease_list <- c("AF", "IHD", "CHF")

# 元の表示範囲を維持
# 全データを表示したい場合は lower = 0、upper = 1 に変更
Y_QUANTILE_LOWER <- 0.1
Y_QUANTILE_UPPER <- 0.9
Y_EXPAND <- 1.01

# ============================================================
# 3. 色・有意差ラベル
# ============================================================

COLOR_HEALTH1 <- "#F8766D"
COLOR_HEALTH0 <- "#00BFC4"

group_colors <- c(
  "0" = COLOR_HEALTH0,
  "1" = COLOR_HEALTH1
)

point_colors <- c(
  "0" = "grey50",
  "1" = "black"
)

point_alpha <- c(
  "0" = 0.2,
  "1" = 1
)

sig_label <- function(p) {
  if (is.na(p))  return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# ============================================================
# 4. 単一パネルの描画関数
#    箱ひげ図 + 生データ
#    y軸はlog10表示、範囲は自動設定
# ============================================================

make_panel <- function(tgt, df_plot, p_val) {
  
  if (nrow(df_plot) == 0) {
    return(nullGrob())
  }
  
  df_plot <- df_plot %>%
    filter(
      is.finite(value),
      !is.na(health),
      value > 0,
      health %in% c(0, 1)
    )
  
  if (
    nrow(df_plot) == 0 ||
    length(unique(df_plot$health)) < 2
  ) {
    return(nullGrob())
  }
  
  # 横軸を 0 → 1 の順に固定
  df_plot$Group <- factor(
    df_plot$health,
    levels = c(0, 1),
    labels = c("0", "1")
  )
  
  # log10空間で、最大値より少し上に有意差ラベルを配置
  log_values <- log10(df_plot$value)
  log_span <- diff(range(log_values))
  
  if (!is.finite(log_span) || log_span <= 0) {
    log_span <- 0.5
  }
  
  sig_y <- 10^(
    max(log_values) + 0.08 * log_span
  )
  
  # 箱ひげ図 + 生データ
  box_plot <- ggplot(
    df_plot,
    aes(x = Group, y = value, fill = Group)
  ) +
    geom_boxplot(
      width = 0.5,
      outlier.shape = NA,
      linewidth = 0.3
    ) +
    geom_point(
      aes(color = Group, alpha = Group),
      position = position_jitter(
        width = 0.2,
        height = 0,
        seed = 123
      ),
      size = 0.2,
      stroke = 0.1
    ) +
    scale_color_manual(values = point_colors) +
    scale_fill_manual(values = group_colors) +
    scale_alpha_manual(values = point_alpha) +
    annotate(
      "text",
      x = 1.5,
      y = sig_y,
      label = sig_label(p_val),
      size = 5 / 2.845
    ) +
    scale_y_log10() +
    scale_x_discrete(drop = FALSE) +
    theme_minimal(base_size = 5) +
    theme(
      axis.title       = element_blank(),
      axis.text        = element_text(size = 5),
      axis.text.x      = element_text(size = 5),
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.4
      )
    )
  
  title_grob <- grobTree(
    rectGrob(
      gp = gpar(
        fill = "#E6E6E6",
        col = "black",
        lwd = 0.6
      )
    ),
    textGrob(
      tgt,
      gp = gpar(fontsize = 5, fontface = "bold")
    )
  )
  
  p_grob <- grobTree(
    rectGrob(
      gp = gpar(
        fill = "#FFFFFF",
        col = "black",
        lwd = 0.6
      )
    ),
    textGrob(
      paste0("p=", formatC(p_val, digits = 2, format = "e")),
      gp = gpar(fontsize = 5)
    )
  )
  
  arrangeGrob(
    title_grob,
    p_grob,
    box_plot,
    ncol = 1,
    heights = c(0.12, 0.10, 0.78)
  )
}

# ============================================================
# 5. 疾患ごとのデータ処理・検定・描画
# ============================================================

plot_boxplot <- function(data_df, metadata_df, targets, dis) {
  
  if (!(dis %in% colnames(metadata_df))) {
    stop(paste0("metadata に列がありません: ", dis))
  }
  
  targets_exist <- intersect(targets, colnames(data_df))
  
  plot_list <- list()
  
  common_ids <- intersect(
    rownames(data_df),
    rownames(metadata_df)
  )
  
  data_df <- data_df[common_ids, , drop = FALSE]
  health_vec <- metadata_df[common_ids, dis]
  
  for (tgt in targets_exist) {
    
    vals <- data_df[, tgt]
    vals <- suppressWarnings(as.numeric(vals))
    
    min_nz <- suppressWarnings(
      min(vals[vals > 0], na.rm = TRUE)
    )
    
    if (!is.finite(min_nz)) {
      plot_list[[tgt]] <- nullGrob()
      next
    }
    
    # 元の処理を維持：ゼロ値を最小正値の半分に置換
    vals[vals == 0] <- min_nz / 2
    
    df_plot <- data.frame(
      value = vals,
      health = health_vec
    ) %>%
      filter(!is.na(value), !is.na(health), value > 0)
    
    # health を数値の 0 / 1 に統一
    df_plot$health <- suppressWarnings(
      as.numeric(as.character(df_plot$health))
    )
    
    df_plot <- df_plot %>%
      filter(health %in% c(0, 1))
    
    p_val <- tryCatch(
      {
        if (length(unique(df_plot$health)) < 2) {
          NA_real_
        } else {
          wilcox.test(
            df_plot$value[df_plot$health == 0],
            df_plot$value[df_plot$health == 1]
          )$p.value
        }
      },
      error = function(e) NA_real_
    )
    
    plot_list[[tgt]] <- make_panel(tgt, df_plot, p_val)
  }
  
  return(plot_list)
}

# ============================================================
# 6. AF / IHD / CHF の各パネルを生成
# ============================================================

plots_by_dis <- list()

for (dis in disease_list) {
  
  plots_cytokine <- plot_boxplot(
    data_df = cytokine,
    metadata_df = metadata,
    targets = target_cytokines,
    dis = dis
  )
  
  # 代謝物も描画する場合は有効化
  # plots_metabo <- plot_boxplot(
  #   data_df = metabo,
  #   metadata_df = metadata,
  #   targets = target_metabos,
  #   dis = dis
  # )
  # plots_by_dis[[dis]] <- c(plots_cytokine, plots_metabo)
  
  plots_by_dis[[dis]] <- plots_cytokine
}

# ============================================================
# 7. 最上段：AF / IHD / CHF のタイトル
# ============================================================

make_dis_title <- function(label) {
  grobTree(
    rectGrob(
      gp = gpar(
        fill = "#D9D9D9",
        col = "black",
        lwd = 0.8
      )
    ),
    textGrob(
      label,
      gp = gpar(fontsize = 8, fontface = "bold")
    )
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
# 8. 各行に同じ target を配置
#    左から AF / IHD / CHF
# ============================================================

row_list <- list()

for (tgt in all_targets) {
  
  p_af  <- plots_by_dis[["AF"]][[tgt]]
  p_ihd <- plots_by_dis[["IHD"]][[tgt]]
  p_hf  <- plots_by_dis[["CHF"]][[tgt]]
  
  if (is.null(p_af))  p_af  <- nullGrob()
  if (is.null(p_ihd)) p_ihd <- nullGrob()
  if (is.null(p_hf))  p_hf  <- nullGrob()
  
  # list の入れ子を回避
  if (
    is.list(p_af) &&
    !inherits(p_af, "grob") &&
    !inherits(p_af, "gtable")
  ) {
    p_af <- p_af[[1]]
  }
  
  if (
    is.list(p_ihd) &&
    !inherits(p_ihd, "grob") &&
    !inherits(p_ihd, "gtable")
  ) {
    p_ihd <- p_ihd[[1]]
  }
  
  if (
    is.list(p_hf) &&
    !inherits(p_hf, "grob") &&
    !inherits(p_hf, "gtable")
  ) {
    p_hf <- p_hf[[1]]
  }
  
  row_list[[tgt]] <- arrangeGrob(
    grobs = list(p_af, p_ihd, p_hf),
    ncol = 3
  )
}

# ============================================================
# 9. PDF出力
# ============================================================

pdf(
  "cytokine_boxplot_AF_IHD_CHF_centered_by_median.pdf",
  width = 1.75,
  height = 2
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
